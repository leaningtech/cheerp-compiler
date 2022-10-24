//===-- SIMDLowering.cpp - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/SIMDLowering.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/IRBuilder.h"

using namespace llvm;

struct VectorParts
{
	SmallVector<Value*, 16> values;

	bool isNull()
	{
		return (values.size() == 0);
	}
};

struct SIMDLoweringVisitor: public InstVisitor<SIMDLoweringVisitor, VectorParts>
{
	bool changed;
	SmallVector<Instruction*, 16> toDelete;
	SmallVector<PHINode*, 16> PHIs;
	DenseMap<Value*, VectorParts> cache;
	Type* Int32Ty;
	Type* Int64Ty;
	AllocaInst* bitcastAlloca;
	const bool lowerAll;

	SIMDLoweringVisitor(Module& M, bool lowerAll): changed(false), bitcastAlloca(nullptr), lowerAll(lowerAll)
	{
		Int32Ty = IntegerType::get(M.getContext(), 32);
		Int64Ty = IntegerType::get(M.getContext(), 64);
	}

	~SIMDLoweringVisitor()
	{
		for (Instruction* I: toDelete)
		{
			I->replaceAllUsesWith(UndefValue::get(I->getType()));
			I->eraseFromParent();
		}
		toDelete.clear();
	}

	bool shouldLower(const Type* type)
	{
		if (!type->isVectorTy())
			return false;
		const FixedVectorType* vecType = cast<FixedVectorType>(type);
		const unsigned vectorSize = vecType->getNumElements() * vecType->getScalarSizeInBits();
		if (vectorSize == 128 && !lowerAll)
			return false;
		if (vecType->getScalarSizeInBits() == 1 && !lowerAll)
			return false;
		return true;
	}

	VectorParts visitPHINode(PHINode& I)
	{
		if (!shouldLower(I.getType()))
			return VectorParts();

		const FixedVectorType* vecType = cast<FixedVectorType>(I.getType());
		Type* elType = vecType->getElementType();
		const unsigned amount = vecType->getNumElements();

		IRBuilder<> Builder(&I);
		unsigned inc = I.getNumIncomingValues();
		VectorParts result;

		// Create the PHI instructions, but populate them later
		// because PHIs can recursively depend on each other.
		for (unsigned i = 0; i < amount; i++)
		{
			Value* newPHI = Builder.CreatePHI(elType, inc);
			result.values.push_back(newPHI);
		}
		PHIs.push_back(&I);
		return result;
	}

	VectorParts visitLoadInst(LoadInst& I)
	{
		if (!shouldLower(I.getType()))
			return VectorParts();

		const FixedVectorType* vecType = cast<FixedVectorType>(I.getType());
		Type* elType = vecType->getElementType();
		const unsigned num = vecType->getNumElements();
		VectorParts p = visitValue(I.getOperand(0));
		assert(p.values[0]->getType() == elType->getPointerTo() &&
				p.values.size() == num);

		VectorParts result;
		IRBuilder<> Builder(&I);
		for (unsigned i = 0; i < num; i++)
		{
			Value* load = Builder.CreateLoad(elType, p.values[i]);
			result.values.push_back(load);
		}
		toDelete.push_back(&I);
		changed = true;
		return result;
	}

	VectorParts visitStoreInst(StoreInst& I)
	{
		if (!shouldLower(I.getValueOperand()->getType()))
			return VectorParts();

		const FixedVectorType* vecType = cast<FixedVectorType>(I.getValueOperand()->getType());
		const unsigned num = vecType->getNumElements();
		VectorParts v = visitValue(I.getValueOperand());
		VectorParts p = visitValue(I.getPointerOperand());

		VectorParts result;
		IRBuilder<> Builder(&I);
		for (unsigned i = 0; i < num; i++)
		{
			Value* store = Builder.CreateStore(v.values[i], p.values[i]);
			result.values.push_back(store);
		}
		toDelete.push_back(&I);
		changed = true;
		return VectorParts();
	}

	VectorParts visitBitCastInst(BitCastInst& I)
	{
		if (I.getDestTy()->isPointerTy())
		{
			const Type* pointedType = I.getDestTy()->getPointerElementType();
			if (!shouldLower(pointedType))
				return VectorParts();

			// If we're casting to a pointer to a vector, instead create
			// pointers to the separate elements.
			const FixedVectorType* vecType = cast<FixedVectorType>(pointedType);
			const unsigned num = vecType->getNumElements();
			Type* elType = vecType->getElementType()->getPointerTo();
			IRBuilder<> Builder(&I);
			VectorParts result;
			Value* firstValue = I.getOperand(0);
			if (elType != firstValue->getType())
				firstValue = Builder.CreateBitCast(firstValue, elType);
			result.values.push_back(firstValue);

			for (unsigned i = 1; i < num; i++)
			{
				Value* index[] = {ConstantInt::get(Int32Ty, i)};
				Value* gep = Builder.CreateInBoundsGEP(vecType->getElementType(), firstValue, index);
				result.values.push_back(gep);
			}
			toDelete.push_back(&I);
			changed = true;
			return result;
		}
		if (!shouldLower(I.getDestTy()))
			return VectorParts();

		// Assert we're casting from one type of vector to another.
		assert(I.getDestTy()->isVectorTy() && I.getSrcTy()->isVectorTy());
		const FixedVectorType* vecDestTy = cast<FixedVectorType>(I.getDestTy());
		const unsigned destN = vecDestTy->getNumElements();
		Type* destType = vecDestTy->getElementType();
		const FixedVectorType* vecSrcTy = cast<FixedVectorType>(I.getSrcTy());
		const unsigned srcN = vecSrcTy->getNumElements();
		Type* srcType = vecSrcTy->getElementType();
		IRBuilder<> Builder(&I);
		VectorParts srcVec = visitValue(I.getOperand(0));
		VectorParts result;

		// Make an alloca, if one doesn't exist for this function.
		if (bitcastAlloca == nullptr)
		{
			Function* F = I.getFunction();
			Builder.SetInsertPoint(F->getEntryBlock().getFirstNonPHI());
			bitcastAlloca = Builder.CreateAlloca(Int64Ty);
			Builder.SetInsertPoint(&I);
		}
		// Prepare store and load locations before the loop
		SmallVector<Value*, 8> srcLocations;
		Value* firstSrcElement = bitcastAlloca;
		if (srcType != Int64Ty)
			firstSrcElement = Builder.CreateBitCast(bitcastAlloca, srcType->getPointerTo());
		srcLocations.push_back(firstSrcElement);
		for (unsigned i = 1; i < srcN / 2; i++)
		{
			Value* index[] = {ConstantInt::get(Int32Ty, i)};
			srcLocations.push_back(Builder.CreateInBoundsGEP(srcType, firstSrcElement, index));
		}
		SmallVector<Value*, 8> destLocations;
		Value* firstDestElement = bitcastAlloca;
		if (destType != Int64Ty)
			firstDestElement = Builder.CreateBitCast(bitcastAlloca, destType->getPointerTo());
		destLocations.push_back(firstDestElement);
		for (unsigned i = 1; i < destN / 2; i++)
		{
			Value* index[] = {ConstantInt::get(Int32Ty, i)};
			destLocations.push_back(Builder.CreateInBoundsGEP(destType, firstDestElement, index));
		}
		// Now do this process twice (we're dealing with a 64-bit alloca)
		for (unsigned i = 0; i < 2; i++)
		{
			unsigned srcOffset = i * (srcN / 2);
			// Store the elements that fit.
			for (unsigned j = 0; j < srcN / 2; j++)
				Builder.CreateStore(srcVec.values[srcOffset + j], srcLocations[j]);
			// Load the elements.
			for (unsigned j = 0; j < destN / 2; j++)
				result.values.push_back(Builder.CreateLoad(destType, destLocations[j]));
		}
		toDelete.push_back(&I);
		changed = true;
		return result;
	}

	VectorParts visitSizeChangingCast(CastInst& I)
	{
		// This function lowers size changing casts for vectors even when SIMD is enabled,
		// because they are not (currently) supported by the Wasm SIMD instructions.
		const FixedVectorType* destType = cast<FixedVectorType>(I.getDestTy());
		const unsigned amount = destType->getNumElements();
		const unsigned destWidth = amount * destType->getScalarSizeInBits();
		Type* newType = destType->getElementType();
		const FixedVectorType* srcType = cast<FixedVectorType>(I.getSrcTy());
		const unsigned srcWidth = amount * srcType->getScalarSizeInBits();
		IRBuilder<> Builder(&I);

		if (destWidth == 128)
		{
			// Are we casting to 128 bit? Create a vector with the elements from the previous instruction.
			VectorParts v = visitValue(I.getOperand(0));
			// If the previous instruction was not lowered, ignore this cast.
			if (v.values.size() == 0)
				return VectorParts();
			Value* newVector = UndefValue::get(I.getDestTy());
			for (unsigned i = 0; i < amount; i++)
			{
				Value* cast = Builder.CreateCast(I.getOpcode(), v.values[i], newType);
				newVector = Builder.CreateInsertElement(newVector, cast, i);
			}
			I.replaceAllUsesWith(newVector);
			toDelete.push_back(&I);
			changed = true;
			return VectorParts();
		}
		else if (srcWidth == 128)
		{
			// Are we casting from 128 bit? Extract all the elements and put into a VectorParts struct.
			VectorParts result;
			for (unsigned i = 0; i < amount; i++)
			{
				Value* extract = Builder.CreateExtractElement(I.getOperand(0), i);
				Value* cast = Builder.CreateCast(I.getOpcode(), extract, newType);
				result.values.push_back(cast);
			}
			toDelete.push_back(&I);
			changed = true;
			return result;
		}
		// This is a size-changing cast where both src and dest are not 128 bit. Lower completely.
		VectorParts v = visitValue(I.getOperand(0));
		VectorParts result;
		for (unsigned i = 0; i < amount; i++)
		{
			Value* cast = Builder.CreateCast(I.getOpcode(), v.values[i], newType);
			result.values.push_back(cast);
		}
		toDelete.push_back(&I);
		changed = true;
		return result;
	}

	VectorParts visitCastInst(CastInst& I)
	{
		if (!lowerAll && I.getType()->isVectorTy() && (isa<SExtInst>(I) || isa<ZExtInst>(I) || isa<TruncInst>(I) || isa<FPExtInst>(I) || isa<FPTruncInst>(I)))
			return visitSizeChangingCast(I);
		if (!shouldLower(I.getDestTy()) && !shouldLower(I.getSrcTy()))
			return VectorParts();

		const FixedVectorType* vecType = cast<FixedVectorType>(I.getDestTy());
		Type* newType = vecType->getElementType();
		VectorParts v = visitValue(I.getOperand(0));
		unsigned amount = v.values.size();
		IRBuilder<> Builder(&I);

		VectorParts result;
		for (unsigned i = 0; i < amount; i++)
		{
			Value* cast = Builder.CreateCast(I.getOpcode(), v.values[i], newType);
			result.values.push_back(cast);
		}
		toDelete.push_back(&I);
		changed = true;
		return result;
	}

	VectorParts visitAllocaInst(AllocaInst& I)
	{
		if (!shouldLower(I.getType()->getPointerElementType()))
			return VectorParts();

		const FixedVectorType* vecType = cast<FixedVectorType>(I.getType()->getPointerElementType());
		Type* newType = vecType->getElementType();
		unsigned amount = vecType->getNumElements();
		IRBuilder<> Builder(&I);
		VectorParts result;

		// Allocate an array of values instead of a vector.
		ArrayType* arr = ArrayType::get(newType, amount);
		Value* alloca = Builder.CreateAlloca(arr);
		Value* bitcast = Builder.CreateBitCast(alloca, newType->getPointerTo());
		result.values.push_back(bitcast);
		for (unsigned i = 1; i < amount; i++)
		{
			Value* index[] = {ConstantInt::get(Int32Ty, i)};
			Value* gep = Builder.CreateInBoundsGEP(newType, bitcast, index);
			result.values.push_back(gep);
		}
		toDelete.push_back(&I);
		changed = true;
		return result;
	}

	VectorParts visitInsertElementInst(InsertElementInst& I)
	{
		if (!shouldLower(I.getType()))
			return VectorParts();

		VectorParts v = visitValue(I.getOperand(0));
		Value* val = I.getOperand(1);
		const ConstantInt* ci = cast<ConstantInt>(I.getOperand(2));
		const unsigned idx = ci->getZExtValue();
		const unsigned num = v.values.size();
		assert(idx < num);

		VectorParts result;
		// Simply propagate the values being passed on,
		// and change the value at idx.
		for (unsigned i = 0; i < num; i++)
		{
			if (i == idx)
				result.values.push_back(val);
			else
				result.values.push_back(v.values[i]);
		}
		toDelete.push_back(&I);
		changed = true;
		return result;
	}

	VectorParts visitExtractElementInst(ExtractElementInst& I)
	{
		if (!shouldLower(I.getOperand(0)->getType()))
			return VectorParts();

		VectorParts v = visitValue(I.getOperand(0));
		const ConstantInt* ci = cast<ConstantInt>(I.getOperand(1));
		const unsigned idx = ci->getZExtValue();
		const unsigned num = v.values.size();
		assert(idx < num);

		// Replace the use of this instruction with the value at place idx.
		I.replaceAllUsesWith(v.values[idx]);
		toDelete.push_back(&I);
		changed = true;
		return VectorParts();
	}

	VectorParts visitShuffleVectorInst(ShuffleVectorInst& I)
	{
		if (!shouldLower(I.getType()) && !shouldLower(I.getOperand(0)->getType()))
			return VectorParts();

		VectorParts v1 = visitValue(I.getOperand(0));
		VectorParts v2 = visitValue(I.getOperand(1));
		auto shuffleMask = I.getShuffleMask();
		VectorParts result;

		const int v1size = v1.values.size();
		const int v2size = v2.values.size();
		const unsigned num = shuffleMask.size();
		// We only need to shuffle the elements from the inputs into the output.
		for (unsigned i = 0; i < num; i++)
		{
			const int idx = shuffleMask[i];
			assert(idx != UndefMaskElem && idx < v1size + v2size);
			if (idx < v1size)
				result.values.push_back(v1.values[idx]);
			else
				result.values.push_back(v2.values[idx - v1size]);
		}
		toDelete.push_back(&I);
		changed = true;
		return result;
	}

	VectorParts visitGetElementPtrInst(GetElementPtrInst& I)
	{
		if (!I.getType()->isVectorTy())
			return VectorParts();

		const FixedVectorType* vecType = cast<FixedVectorType>(I.getType());
		const unsigned num = vecType->getNumElements();
		IRBuilder<> Builder(I.getNextNode());
		VectorParts result;

		// We always lower vector GEP instructions.
		// If the result is 128-bits wide, and SIMD is supported, we pack it back into a vector.
		for (unsigned i = 0; i < num; i++)
		{
			Value* pointerOp = I.getPointerOperand();
			if (I.getPointerOperand()->getType()->isVectorTy())
				pointerOp = Builder.CreateExtractElement(I.getPointerOperand(), i);
			SmallVector<Value*, 4> indices;
			for (Value* v: I.indices())
			{
				if (v->getType()->isVectorTy())
				{
					if (isa<Instruction>(v))
					{
						Value* extract = Builder.CreateExtractElement(v, i);
						indices.push_back(extract);
					}
					else
					{
						const ConstantDataVector* cdv = cast<ConstantDataVector>(v);
						indices.push_back(cdv->getAggregateElement(i));
					}
				}
				else
					indices.push_back(v);
			}
			if (I.isInBounds())
				result.values.push_back(Builder.CreateInBoundsGEP(I.getResultElementType(), pointerOp, indices));
			else
				result.values.push_back(Builder.CreateGEP(I.getResultElementType(), pointerOp, indices));
		}

		toDelete.push_back(&I);
		changed = true;
		if (lowerAll || cheerp::getVectorBitwidth(vecType) != 128)
			return result;

		Value* newValue = UndefValue::get(I.getType());
		for (unsigned i = 0; i < result.values.size(); i++)
			newValue = Builder.CreateInsertElement(newValue, result.values[i], i);
		I.replaceAllUsesWith(newValue);
		return VectorParts();
	}

	VectorParts visitBinaryOperator(BinaryOperator& I)
	{
		if (!shouldLower(I.getType()))
			return VectorParts();

		const FixedVectorType* vecType = cast<FixedVectorType>(I.getType());
		const unsigned num = vecType->getNumElements();
		IRBuilder<> Builder(&I);
		VectorParts lhs = visitValue(I.getOperand(0));
		VectorParts rhs = visitValue(I.getOperand(1));

		VectorParts result;
		for (unsigned i = 0; i < num; i++)
		{
			Value* binop = Builder.CreateBinOp(I.getOpcode(), lhs.values[i], rhs.values[i]);
			result.values.push_back(binop);
		}
		toDelete.push_back(&I);
		changed = true;
		return result;
	}

	VectorParts visitCmpInst(CmpInst& I)
	{
		if (!shouldLower(I.getType()))
			return VectorParts();

		const FixedVectorType* vecType = cast<FixedVectorType>(I.getType());
		const unsigned num = vecType->getNumElements();
		IRBuilder<> Builder(&I);
		VectorParts lhs = visitValue(I.getOperand(0));
		VectorParts rhs = visitValue(I.getOperand(1));
		CmpInst::Predicate Pred = I.getPredicate();

		VectorParts result;
		for (unsigned i = 0; i < num; i++)
		{
			Value* cmp = Builder.CreateCmp(Pred, lhs.values[i], rhs.values[i]);
			result.values.push_back(cmp);
		}
		toDelete.push_back(&I);
		changed = true;
		return result;
	}

	VectorParts visitSelectInst(SelectInst& I)
	{
		if (!shouldLower(I.getType()))
			return VectorParts();

		const FixedVectorType* vecType = cast<FixedVectorType>(I.getType());
		const unsigned num = vecType->getNumElements();
		IRBuilder<> Builder(&I);
		VectorParts trueVector = visitValue(I.getTrueValue());
		VectorParts falseVector = visitValue(I.getFalseValue());
		VectorParts condition = visitValue(I.getCondition());
		if (condition.values.size() == 0)
		{
			// The condition was a boolean instead of a vector boolean.
			// Create a vector from the single condition.
			Value* cond = I.getCondition();
			for (unsigned i = 0; i < num; i++)
				condition.values.push_back(cond);
		}

		VectorParts result;
		for (unsigned i = 0; i < num; i++)
		{
			Value* select = Builder.CreateSelect(condition.values[i], trueVector.values[i], falseVector.values[i]);
			result.values.push_back(select);
		}
		toDelete.push_back(&I);
		changed = true;
		return result;
	}

	VectorParts lowerReduceIntrinsic(IntrinsicInst& I)
	{
		// Reduce intrinsics take a vector and return a scalar.
		// Lower this to do the operation on the separate elements.
		Intrinsic::ID id = I.getIntrinsicID();
		assert(id == Intrinsic::vector_reduce_mul || id == Intrinsic::vector_reduce_add);
		const FixedVectorType* vecType = cast<FixedVectorType>(I.getOperand(0)->getType());
		const unsigned num = vecType->getNumElements();
		IRBuilder<> Builder(&I);
		VectorParts v = visitValue(I.getOperand(0));

		Value* total = v.values[0];
		for (unsigned i = 1; i < num; i++)
		{
			if (id == Intrinsic::vector_reduce_add)
				total = Builder.CreateAdd(total, v.values[i]);
			else
				total = Builder.CreateMul(total, v.values[i]);
		}
		I.replaceAllUsesWith(total);
		toDelete.push_back(&I);
		changed = true;
		return VectorParts();
	}

	VectorParts lowerSplatIntrinsic(IntrinsicInst& I)
	{
		// Splat intrinsics take a scalar and return a vector of elements
		// with all elements being the scalar.
		const FixedVectorType* vecType = cast<FixedVectorType>(I.getType());
		const unsigned num = vecType->getNumElements();
		IRBuilder<> Builder(&I);

		VectorParts result;
		for (unsigned i = 0; i < num; i++)
			result.values.push_back(I.getOperand(0));
		toDelete.push_back(&I);
		changed = true;
		return result;
	}

	VectorParts lowerShiftIntrinsic(IntrinsicInst& I)
	{
		// Lower the bitshift intrinsics to separate bitshifts.
		const FixedVectorType* vecType = cast<FixedVectorType>(I.getType());
		const unsigned num = vecType->getNumElements();
		IRBuilder<> Builder(&I);
		VectorParts v = visitValue(I.getOperand(0));
		Value* amount = I.getOperand(1);

		Instruction::BinaryOps opcode;
		Intrinsic::ID id = I.getIntrinsicID();
		if (id == Intrinsic::cheerp_wasm_shl)
			opcode = Instruction::Shl;
		else if (id == Intrinsic::cheerp_wasm_shr_s)
			opcode = Instruction::AShr;
		else
			opcode = Instruction::LShr;
		VectorParts result;
		for (unsigned i = 0; i < num; i++)
		{
			Value* shift = Builder.CreateBinOp(opcode, v.values[i], amount);
			result.values.push_back(shift);
		}
		toDelete.push_back(&I);
		changed = true;
		return result;

	}

	VectorParts visitIntrinsicInst(IntrinsicInst& I)
	{
		if (!lowerAll)
			return VectorParts();

		// Here we are looking for a few specific intrinsics.
		Intrinsic::ID id = I.getIntrinsicID();
		if (id == Intrinsic::vector_reduce_mul ||
			id == Intrinsic::vector_reduce_add ||
			id == Intrinsic::vector_reduce_fmul ||
			id == Intrinsic::vector_reduce_fadd)
			return lowerReduceIntrinsic(I);
		if (id == Intrinsic::cheerp_wasm_splat)
			return lowerSplatIntrinsic(I);
		if (id == Intrinsic::cheerp_wasm_shl ||
			id == Intrinsic::cheerp_wasm_shr_s ||
			id == Intrinsic::cheerp_wasm_shr_u)
			return lowerShiftIntrinsic(I);

		return VectorParts();
	}

	VectorParts visitInstruction(Instruction& I)
	{
		if (shouldLower(I.getType()))
		{
			llvm::errs() << I << "\n";
			llvm::report_fatal_error("Missing instruction");
		}
		return VectorParts();
	}

	VectorParts visit(Instruction& I)
	{
		if (cache.count(&I))
			return VectorParts();

		VectorParts ret = InstVisitor::visit(I);
		if (!ret.isNull())
			cache.insert(std::make_pair(&I, ret));
		return ret;
	}

	void visit(Function& F)
	{
		InstVisitor::visit(F);
		for (PHINode* p: PHIs)
		{
			VectorParts newPHIs = visitValue(p);
			const unsigned numElements = newPHIs.values.size();
			const unsigned incomingAmount = p->getNumIncomingValues();
			for (unsigned i = 0; i < incomingAmount; i++)
			{
				VectorParts orig = visitValue(p->getIncomingValue(i));
				BasicBlock* BB = p->getIncomingBlock(i);
				for (unsigned j = 0; j < numElements; j++)
				{
					PHINode* newPHI = cast<PHINode>(newPHIs.values[j]);
					newPHI->addIncoming(orig.values[j] , BB);
				}
			}
			toDelete.push_back(p);
		}
	}
	using InstVisitor::visit;

	VectorParts visitValue(Value* V)
	{
		if (!shouldLower(V->getType()) &&
			!(V->getType()->isPointerTy() && shouldLower(V->getType()->getPointerElementType())))
			return VectorParts();

		auto it = cache.find(V);
		if (it != cache.end())
			return it->second;

		VectorParts ret;
		if (Instruction* I = dyn_cast<Instruction>(V))
		{
			ret = visit(*I);
			return ret;
		}
		else if (const ConstantAggregateZero* caz = dyn_cast<ConstantAggregateZero>(V))
		{
			Constant* el = caz->getSequentialElement();
			const unsigned amount = caz->getElementCount().getFixedValue();
			for (unsigned i = 0; i < amount; i++)
				ret.values.push_back(el);
		}
		else if (const ConstantDataVector* cdv = dyn_cast<ConstantDataVector>(V))
		{
			const FixedVectorType* vecType = cdv->getType();
			const unsigned amount = vecType->getNumElements();
			for (unsigned i = 0; i < amount; i++)
				ret.values.push_back(cdv->getElementAsConstant(i));
		}
		else if (const ConstantVector* cv = dyn_cast<ConstantVector>(V))
		{
			const FixedVectorType* vecType = cv->getType();
			const unsigned amount = vecType->getNumElements();
			for (unsigned i = 0; i < amount; i++)
				ret.values.push_back(cv->getAggregateElement(i));
		}
		else if (const UndefValue* uv = dyn_cast<UndefValue>(V))
		{
			UndefValue* uvElement = uv->getSequentialElement();
			const unsigned amount = uv->getNumElements();
			for (unsigned i = 0; i < amount; i++)
				ret.values.push_back(uvElement);
		}
		else
		{
			llvm::errs() << *V << "\n";
			llvm::report_fatal_error("not implemented yet");
		}
		if (!ret.isNull())
			cache.insert(std::make_pair(V, ret));
		return ret;
	}
};

namespace cheerp
{

PreservedAnalyses SIMDLoweringPass::run(Function& F, FunctionAnalysisManager& FAM)
{
	if (!WasmSIMD)
	{
		// Check if the parameters or return type are vector.
		assert(!F.getReturnType()->isVectorTy());
		for (auto it = F.arg_begin(); it != F.arg_end(); it++)
			assert(!it->getType()->isVectorTy());
	}

	SIMDLoweringVisitor Visitor(*F.getParent(), !WasmSIMD);
	Visitor.visit(F);

	if (!Visitor.changed)
		return PreservedAnalyses::all();
	PreservedAnalyses PA;
	PA.preserve<cheerp::GlobalDepsAnalysis>();
	PA.preserve<cheerp::LinearMemoryAnalysis>();
	return PA;
}

}

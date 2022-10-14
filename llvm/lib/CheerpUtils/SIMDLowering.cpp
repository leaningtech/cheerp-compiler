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
	DenseMap<Value*, VectorParts> cache;
	Type* Int32Ty;
	Type* Int64Ty;
	AllocaInst* bitcastAlloca;

	SIMDLoweringVisitor(Module& M): changed(false), bitcastAlloca(nullptr)
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

	VectorParts visitLoadInst(LoadInst& I)
	{
		if (!I.getType()->isVectorTy())
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
		if (!I.getValueOperand()->getType()->isVectorTy())
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
			if (!pointedType->isVectorTy())
				return VectorParts();

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
		if (!I.getType()->isVectorTy())
			return VectorParts();

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

	VectorParts visitInsertElementInst(InsertElementInst& I)
	{
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
		if (!I.getResultElementType()->isVectorTy())
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
		if (!I.getType()->isVectorTy())
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

	VectorParts visitInstruction(Instruction& I)
	{
		if (I.getType()->isVectorTy())
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
	}
	using InstVisitor::visit;

	VectorParts visitValue(Value* V)
	{
		if (!V->getType()->isVectorTy() &&
			!(V->getType()->isPointerTy() && V->getType()->getPointerElementType()->isVectorTy()))
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
	SIMDLoweringVisitor Visitor(*F.getParent());
	Visitor.visit(F);

	if (!Visitor.changed)
		return PreservedAnalyses::all();
	PreservedAnalyses PA;
	PA.preserve<cheerp::GlobalDepsAnalysis>();
	PA.preserve<cheerp::LinearMemoryAnalysis>();
	return PA;
}

}

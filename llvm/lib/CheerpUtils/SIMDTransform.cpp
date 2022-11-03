//===-- SIMDTransform.cpp - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/SIMDTransform.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRBuilder.h"

using namespace llvm;

namespace cheerp
{

void SIMDTransformPass::checkVectorCorrectness(Instruction& I)
{
	assert(isa<FixedVectorType>(I.getType()));
	const FixedVectorType* vecTy = cast<FixedVectorType>(I.getType());
	const unsigned vectorBitwidth = getVectorBitwidth(vecTy);
	if (vectorBitwidth != 128)
	{
		// Verify that 64 is produced by a load, and only used to extend.
		if (vectorBitwidth == 64)
		{
			assert(isa<LoadInst>(I) || isa<BinaryOperator>(I) || isa<IntrinsicInst>(I) || isa<FPTruncInst>(I) || isa<InsertElementInst>(I) || isa<ShuffleVectorInst>(I));
			for (User* U: I.users())
			{
				assert(isa<Instruction>(U));
				const Instruction* useI = cast<Instruction>(U);
				assert(isa<SExtInst>(useI) || isa<ZExtInst>(useI) || isa<TruncInst>(useI) || isa<FPExtInst>(useI) || isa<BinaryOperator>(useI) || isa<CmpInst>(useI) || isa<IntrinsicInst>(useI) || isa<ExtractElementInst>(useI) || isa<StoreInst>(useI) || isa<ShuffleVectorInst>(useI));
			}
			return ;
		}
		// Verify that if the element size is 1, it's from a comparison,
		// and this result is only used in select instructions.
		if (vecTy->getScalarSizeInBits() == 1)
		{
			assert(isa<CmpInst>(I) || isa<PHINode>(I) || isa<BinaryOperator>(I));
			for (User* U: I.users())
				assert(isa<SelectInst>(U) || isa<SExtInst>(U) || isa<BitCastInst>(U) || isa<PHINode>(U) || isa<BinaryOperator>(U) || isa<ExtractElementInst>(U));
			return ;
		}
		if (vectorBitwidth == 32 && (isa<TruncInst>(I) || isa<SelectInst>(I) || isa<PHINode>(I) || isa<BinaryOperator>(I) || isa<InsertElementInst>(I) || isa<ShuffleVectorInst>(I)))
			return ;
		if (vectorBitwidth == 256 && isa<ZExtInst>(I))
			return ;
		assert(false);
	}
}

bool SIMDTransformPass::lowerExtractOrInsert(Instruction& I)
{
	bool extract = I.getOpcode() == Instruction::ExtractElement;
	Function* F = I.getFunction();
	Value* vec = I.getOperand(0);
	assert(vec->getType()->isVectorTy());
	Type* elementType = cast<VectorType>(vec->getType())->getElementType();
	Value *variableOp = extract ? I.getOperand(1) : I.getOperand(2);
	IRBuilder<> Builder(&I);
	if (extractInsertAlloca == nullptr)
	{
		Builder.SetInsertPoint(F->getEntryBlock().getFirstNonPHI());
		extractInsertAlloca = Builder.CreateAlloca(vec->getType());
		Builder.SetInsertPoint(&I);
	}
	Value* indexes[] = { variableOp };
	Value* bitcast = Builder.CreateBitCast(extractInsertAlloca, elementType->getPointerTo());
	Value* gep = Builder.CreateInBoundsGEP(elementType, bitcast, indexes);
	Builder.CreateStore(vec, extractInsertAlloca);
	Value* load;
	if (extract)
		load = Builder.CreateLoad(elementType, gep);
	else
	{
		Builder.CreateStore(I.getOperand(1), gep);
		load = Builder.CreateLoad(vec->getType(), extractInsertAlloca);
	}
	I.replaceAllUsesWith(load);
	deleteList.push_back(&I);
	return false;
}

bool SIMDTransformPass::lowerReduceIntrinsic(IntrinsicInst& I)
{
	// Reduce intrinsics are either a simple binary operation done multiple times,
	// or an intrinsic done multiple times. None are supported in SIMD. We extract
	// all of the elements, and do the operations on the elements.
	Intrinsic::ID id = I.getIntrinsicID();
	bool fBinOp = (id == Intrinsic::vector_reduce_fadd || id == Intrinsic::vector_reduce_fmul);
	Value* vecOp = fBinOp ? I.getOperand(1) : I.getOperand(0);
	const FixedVectorType* vecType = cast<FixedVectorType>(vecOp->getType());
	const unsigned amount = vecType->getNumElements();
	Type* elType = vecType->getElementType();
	IRBuilder<> Builder(&I);
	SmallVector<Value*, 16> values;
	for (unsigned i = 0; i < amount; i++)
		values.push_back(Builder.CreateExtractElement(vecOp, i));
	Value* total = fBinOp ? I.getOperand(0) : values[0];
	bool binop = (id == Intrinsic::vector_reduce_fadd || id == Intrinsic::vector_reduce_fmul ||
				id == Intrinsic::vector_reduce_add || id == Intrinsic::vector_reduce_mul ||
				id == Intrinsic::vector_reduce_and || id == Intrinsic::vector_reduce_or ||
				id == Intrinsic::vector_reduce_xor);
	if (binop)
	{
		Instruction::BinaryOps opcode;
		if (id == Intrinsic::vector_reduce_fadd)
			opcode = Instruction::FAdd;
		else if (id == Intrinsic::vector_reduce_fmul)
			opcode = Instruction::FMul;
		else if (id == Intrinsic::vector_reduce_add)
			opcode = Instruction::Add;
		else if (id == Intrinsic::vector_reduce_mul)
			opcode = Instruction::Mul;
		else if (id == Intrinsic::vector_reduce_and)
			opcode = Instruction::And;
		else if (id == Intrinsic::vector_reduce_or)
			opcode = Instruction::Or;
		else if (id == Intrinsic::vector_reduce_xor)
			opcode = Instruction::Xor;
		unsigned start = fBinOp ? 0 : 1;
		for (unsigned i = start; i < amount; i++)
			total = Builder.CreateBinOp(opcode, total, values[i]);
	}
	else
	{
		Intrinsic::ID loweredID;
		if (id == Intrinsic::vector_reduce_smax)
			loweredID = Intrinsic::smax;
		else if (id == Intrinsic::vector_reduce_smin)
			loweredID = Intrinsic::smin;
		else if (id == Intrinsic::vector_reduce_umax)
			loweredID = Intrinsic::umax;
		else if (id == Intrinsic::vector_reduce_umin)
			loweredID = Intrinsic::umin;
		else if (id == Intrinsic::vector_reduce_fmax)
			loweredID = Intrinsic::maxnum;
		else if (id == Intrinsic::vector_reduce_fmin)
			loweredID = Intrinsic::minnum;
		else
			llvm::report_fatal_error("Unrecognized intrinsic");
		std::vector<Type *> argTypes = { elType };
		Function* intrinsic = Intrinsic::getDeclaration(I.getModule(), loweredID, argTypes);
		for (unsigned i = 1; i < amount; i++)
			total = Builder.CreateCall(intrinsic, {total, values[i]});
	}
	I.replaceAllUsesWith(total);
	deleteList.push_back(&I);
	return false;
}

bool SIMDTransformPass::lowerBinaryIntrinsic(IntrinsicInst& I)
{
	// Lower this to extract elements on both input vectors.
	// Then do the intrinsic on the individual elements.
	// Then insert element into a result vector.
	Intrinsic::ID id = I.getIntrinsicID();
	const FixedVectorType* vecType = cast<FixedVectorType>(I.getType());
	Type* elType = vecType->getElementType();
	const unsigned amount = vecType->getNumElements();
	IRBuilder<> Builder(&I);
	std::vector<Type *> argTypes = { elType };
	Function* intrinsic = Intrinsic::getDeclaration(I.getModule(), id, argTypes);
	Value* newVector = UndefValue::get(I.getType());
	for (unsigned i = 0; i < amount; i++)
	{
		Value* extract1 = Builder.CreateExtractElement(I.getOperand(0), i);
		Value* extract2 = Builder.CreateExtractElement(I.getOperand(1), i);
		Value* call = Builder.CreateCall(intrinsic, {extract1, extract2});
		newVector = Builder.CreateInsertElement(newVector, call, i);
	}
	I.replaceAllUsesWith(newVector);
	deleteList.push_back(&I);
	return false;
}

bool SIMDTransformPass::lowerUnaryIntrinsic(IntrinsicInst& I)
{
	// Lower this to extract every element, do the intrinsic on
	// the individual elements, and insert into a result vector.
	Intrinsic::ID id = I.getIntrinsicID();
	const FixedVectorType* vecType = cast<FixedVectorType>(I.getType());
	Type* elType = vecType->getElementType();
	const unsigned amount = vecType->getNumElements();
	IRBuilder<> Builder(&I);
	std::vector<Type *> argTypes = { elType };
	Function* intrinsic = Intrinsic::getDeclaration(I.getModule(), id, argTypes);
	Value* newVector = UndefValue::get(I.getType());
	for (unsigned i = 0; i < amount; i++)
	{
		Value* extract = Builder.CreateExtractElement(I.getOperand(0), i);
		Value* call = Builder.CreateCall(intrinsic, {extract});
		newVector = Builder.CreateInsertElement(newVector, call, i);
	}
	I.replaceAllUsesWith(newVector);
	deleteList.push_back(&I);
	return false;
}

bool SIMDTransformPass::lowerIntrinsic(Instruction& I)
{
	// This function will pick up on intrinsics that are not supported
	// by Wasm SIMD and lower them.
	IntrinsicInst& ii = cast<IntrinsicInst>(I);
	switch (ii.getIntrinsicID())
	{
		case Intrinsic::vector_reduce_fadd:
		case Intrinsic::vector_reduce_fmul:
		case Intrinsic::vector_reduce_add:
		case Intrinsic::vector_reduce_mul:
		case Intrinsic::vector_reduce_and:
		case Intrinsic::vector_reduce_or:
		case Intrinsic::vector_reduce_xor:
		case Intrinsic::vector_reduce_smax:
		case Intrinsic::vector_reduce_smin:
		case Intrinsic::vector_reduce_umax:
		case Intrinsic::vector_reduce_umin:
		case Intrinsic::vector_reduce_fmax:
		case Intrinsic::vector_reduce_fmin:
			return lowerReduceIntrinsic(ii);
		case Intrinsic::smax:
		case Intrinsic::smin:
		case Intrinsic::umax:
		case Intrinsic::umin:
		{
			// Max and min intrinsics are not supported for i64x2 vectors.
			if (!ii.getType()->isVectorTy())
				return false;
			const FixedVectorType* vecType = cast<FixedVectorType>(ii.getType());
			if (!vecType->getElementType()->isIntegerTy(64))
				return false;
			return lowerBinaryIntrinsic(ii);
		}
		case Intrinsic::pow:
		case Intrinsic::copysign:
			if (!ii.getType()->isVectorTy())
				return false;
			return lowerBinaryIntrinsic(ii);
		case Intrinsic::sin:
		case Intrinsic::cos:
		case Intrinsic::exp:
		case Intrinsic::exp2:
		case Intrinsic::log:
		case Intrinsic::log10:
		case Intrinsic::log2:
			if (!ii.getType()->isVectorTy())
				return false;
			return lowerUnaryIntrinsic(ii);
	}
	return false;
}

bool SIMDTransformPass::lowerBitShift(Instruction& I)
{
	// This function will lower bit shift operations that take a vector as their second operand,
	// since WebAssembly does not support this.
	Value* secondOp = I.getOperand(1);
	if (!secondOp->getType()->isVectorTy())
		return false;
	Intrinsic::ID intrID;
	unsigned opcode = I.getOpcode();
	if (opcode == Instruction::Shl)
		intrID = Intrinsic::cheerp_wasm_shl;
	else if (opcode == Instruction::AShr)
		intrID = Intrinsic::cheerp_wasm_shr_s;
	else
		intrID = Intrinsic::cheerp_wasm_shr_u;
	assert(isa<ConstantDataVector>(secondOp) || isa<Instruction>(secondOp));
	IRBuilder<> Builder(&I);
	const ConstantDataVector* cdv = dyn_cast<ConstantDataVector>(secondOp);
	const CallInst* callInst = dyn_cast<CallInst>(secondOp);
	if (cdv && cdv->isSplat())
	{
		ConstantInt* splatValue = cast<ConstantInt>(cdv->getSplatValue());
		ConstantInt* shiftValue = Builder.getInt32(splatValue->getZExtValue());
		std::vector<Type *> argTypes = { I.getType(), I.getType(), shiftValue->getType() };
		Function* intrinsic = Intrinsic::getDeclaration(I.getModule(), intrID, argTypes);
		CallInst* ci = Builder.CreateCall(intrinsic, {I.getOperand(0), shiftValue});
		I.replaceAllUsesWith(ci);
		deleteList.push_back(&I);
	}
	else if (callInst && callInst->getIntrinsicID() == Intrinsic::cheerp_wasm_splat)
	{
		std::vector<Type *> argTypes = { I.getType(), I.getType(), callInst->getOperand(0)->getType() };
		Function* intrinsic = Intrinsic::getDeclaration(I.getModule(), intrID, argTypes);
		CallInst* ci = Builder.CreateCall(intrinsic, {I.getOperand(0), callInst->getOperand(0) });
		I.replaceAllUsesWith(ci);
		deleteList.push_back(&I);
		if (secondOp->hasOneUse())
			deleteList.push_back(cast<Instruction>(secondOp));
	}
	else
		// General lower case.
		return lowerGeneralUnsupportedVectorOperation(I);
	return false;
}

bool SIMDTransformPass::lowerSplat(Instruction &I)
{
	// Try to see if this instruction was originally a splat.
	// We know this if we find a shufflevector instruction, that has a zero mask, and it's first operand
	// is an insert element used only once, which inserts into the first element of a vector.
	const ShuffleVectorInst& svi = cast<ShuffleVectorInst>(I);
	if (svi.isZeroEltSplat())
	{
		Value* firstOp = svi.getOperand(0);
		if (!firstOp->hasOneUse())
			return false;
		if (const InsertElementInst* iei = dyn_cast<InsertElementInst>(firstOp))
		{
			if (const ConstantInt* index = dyn_cast<ConstantInt>(iei->getOperand(2)))
			{
				if (index->getZExtValue() != 0)
					return false;
				IRBuilder<> Builder((Instruction*)iei);
				std::vector<Type *> argTypes { iei->getType(), iei->getOperand(1)->getType() };
				Function* splatIntrinsic = Intrinsic::getDeclaration(I.getModule(), Intrinsic::cheerp_wasm_splat, argTypes);
				CallInst* ci = Builder.CreateCall(splatIntrinsic, { iei->getOperand(1) });
				I.replaceAllUsesWith(ci);
				deleteList.push_back(&I);
				deleteList.push_back((Instruction*)firstOp);
			}
		}
	}
	return false;
}

bool SIMDTransformPass::lowerGeneralUnsupportedVectorOperation(Instruction& I)
{
	// This function will lower instructions that are not defined to be done on 2 vectors in SIMD.
	// It will extract values 1 by 1 from both vectors, do the operation, and store the results.
	unsigned opcode = I.getOpcode();
	IRBuilder<> Builder(&I);
	Value* firstOp = I.getOperand(0);
	Value* secondOp = I.getOperand(1);
	Type* elementType = cast<VectorType>(firstOp->getType())->getElementType();
	assert(elementType->isIntegerTy());
	int amount;
	if (elementType->isIntegerTy())
		amount = 128 / elementType->getIntegerBitWidth();
	else if (elementType->isFloatTy())
		amount = 4;
	else if (elementType->isDoubleTy())
		amount = 2;
	else
		llvm::report_fatal_error("Unknown elementwidth");
	Value* newVec;
	for (int i = 0; i < amount; i++)
	{
		Value* subOp1 = Builder.CreateExtractElement(firstOp, i);
		Value* subOp2 = Builder.CreateExtractElement(secondOp, i);
		Value* shiftedValue = Builder.CreateBinOp((Instruction::BinaryOps)opcode, subOp1, subOp2);
		if (i == 0)
		{
			std::vector<Type *> argTypes { secondOp->getType(), elementType };
			Function* splatIntrinsic = Intrinsic::getDeclaration(I.getModule(), Intrinsic::cheerp_wasm_splat, argTypes);
			newVec = Builder.CreateCall(splatIntrinsic, { shiftedValue });
		}
		else
			newVec = Builder.CreateInsertElement(newVec, shiftedValue, i);
	}
	I.replaceAllUsesWith(newVec);
	deleteList.push_back(&I);
	return false;
}

bool SIMDTransformPass::lowerVectorBooleanBitcast(Instruction& I)
{
	// This function will lower a bitcast from a vector of booleans to an integer type.
	IRBuilder<> Builder(&I);
	Value* srcVec = I.getOperand(0);
	const FixedVectorType* vecType = cast<FixedVectorType>(srcVec->getType());
	const unsigned num = vecType->getNumElements();
	Type* destType = I.getType();
	assert(vecType->getScalarSizeInBits() == 1);
	assert(num == destType->getIntegerBitWidth());

	// We need to extract 'num' elements from the vector first.
	SmallVector<Value*, 16> values;
	for (unsigned i = 0; i < num; i++)
	{
		Value* extract = Builder.CreateExtractElement(srcVec, i);
		values.push_back(extract);
	}

	// Then we zero-extend each element to the right size, bitshift them and OR them together.
	Value* newValue = Builder.CreateZExt(values[num - 1], destType);
	for (unsigned i = 1; i < num; i++)
	{
		Value* currentValue = Builder.CreateZExt(values[num - 1 - i], destType);
		Value* bitshift = Builder.CreateShl(currentValue, i);
		newValue = Builder.CreateOr(newValue, bitshift);
	}
	// Replace the uses of the bitcast with the newly created value.
	I.replaceAllUsesWith(newValue);
	deleteList.push_back(&I);
	return false;
}

bool SIMDTransformPass::isVariableExtractOrInsert(Instruction& I)
{
	return (I.getOpcode() == Instruction::ExtractElement && !isa<ConstantInt>(I.getOperand(1))) || 
			(I.getOpcode() == Instruction::InsertElement && !isa<ConstantInt>(I.getOperand(2)));
}

PreservedAnalyses SIMDTransformPass::run(Function& F, FunctionAnalysisManager& FAM)
{
	extractInsertAlloca = nullptr;
	deleteList.clear();
	bool needToBreak = false;
	for (auto it = F.begin(); it != F.end(); it++)
	{
		BasicBlock& BB = *it;
		for (Instruction& I: BB)
		{
			// Check correctness of vector types. Can only be 128 bits, or in some cases 64.
#ifndef NDEBUG
			if (I.getType()->isVectorTy())
				checkVectorCorrectness(I);
#endif
			// This will find certain instructions that do not allow variables as lane indexes and
			// instead use a store and a load.
			if (isVariableExtractOrInsert(I))
				needToBreak = lowerExtractOrInsert(I);
			else if (isa<IntrinsicInst>(I))
				needToBreak = lowerIntrinsic(I);
			else if ((I.getOpcode() == Instruction::Shl || I.getOpcode() == Instruction::AShr ||
					I.getOpcode() == Instruction::LShr) && I.getType()->isVectorTy())
				needToBreak = lowerBitShift(I);
			else if (I.getOpcode() == Instruction::ShuffleVector)
				needToBreak = lowerSplat(I);
			else if ((I.getOpcode() == Instruction::SDiv || I.getOpcode() == Instruction::UDiv)
					&& I.getType()->isVectorTy())
				needToBreak = lowerGeneralUnsupportedVectorOperation(I);
			else if (isa<BitCastInst>(I) && I.getOperand(0)->getType()->isVectorTy()
					&& !I.getType()->isVectorTy())
				needToBreak = lowerVectorBooleanBitcast(I);
			if (needToBreak)
				break ;
		}
	}
	for (Instruction* I: deleteList)
		I->eraseFromParent();
	if (deleteList.empty())
		return PreservedAnalyses::all();
	PreservedAnalyses PA;
	PA.preserve<cheerp::GlobalDepsAnalysis>();
	PA.preserve<cheerp::LinearMemoryAnalysis>();
	return PA;
}

}

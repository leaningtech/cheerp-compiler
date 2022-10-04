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
#include "llvm/IR/Instruction.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/IRBuilder.h"

using namespace llvm;

namespace cheerp
{

void SIMDLoweringPass::checkVectorCorrectness(Instruction& I)
{
	assert(isa<FixedVectorType>(I.getType()));
	const FixedVectorType* vecTy = cast<FixedVectorType>(I.getType());
	const unsigned vectorBitwidth = vecTy->getNumElements() * vecTy->getScalarSizeInBits();
	if (vectorBitwidth != 128)
	{
		// Verify that 64 is produced by a load, and only used to extend.
		if (vectorBitwidth == 64)
		{
			assert(isa<LoadInst>(I));
			for (User* U: I.users())
			{
				assert(isa<Instruction>(U));
				const Instruction* useI = cast<Instruction>(U);
				assert(useI->getOpcode() == Instruction::SExt || useI->getOpcode() == Instruction::ZExt);
			}
			return ;
		}
		// Verify that if the element size is 1, it's from a comparison,
		// and this result is only used in select instructions.
		if (vecTy->getScalarSizeInBits() == 1)
		{
			assert(isa<CmpInst>(I));
			for (User* U: I.users())
				assert(isa<SelectInst>(U) || isa<SExtInst>(U));
			return ;
		}
		assert(false);
	}
}
bool SIMDLoweringPass::lowerExtractOrInsert(Instruction& I)
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

bool SIMDLoweringPass::lowerReduceIntrinsic(Instruction& I)
{
	Value *vec = I.getOperand(0);
	const int amount = 128 / vec->getType()->getScalarSizeInBits();
	IRBuilder<> Builder(&I);
	std::vector<Value*> values;
	for (int i = 0; i < amount; i++)
		values.push_back(Builder.CreateExtractElement(vec, i));
	Intrinsic::ID id = cast<CallInst>(I).getCalledFunction()->getIntrinsicID();
	bool add = (id == Intrinsic::vector_reduce_add || id == Intrinsic::vector_reduce_fadd) ? true : false;
	// Only accept normal, not floats, for now.
	assert(id == Intrinsic::vector_reduce_add || id == Intrinsic::vector_reduce_mul);
	Value* total = values[0];
	for (int i = 1; i < amount; i++)
	{
		if (add)
			total = Builder.CreateAdd(total, values[i]);
		else
			total = Builder.CreateMul(total, values[i]);
	}
	I.replaceAllUsesWith(total);
	deleteList.push_back(&I);
	return false;
}

bool SIMDLoweringPass::lowerBitShift(Instruction& I)
{
	// This function will lower bit shift operations that take a vector as their second operand,
	// since WebAssembly does not support this.
	Value* secondOp = I.getOperand(1);
	if (!secondOp->getType()->isVectorTy() || !secondOp->hasOneUse())
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
	}
	else
		// General lower case.
		return lowerGeneralUnsupportedVectorOperation(I);
	return false;
}

bool SIMDLoweringPass::lowerSplat(Instruction &I)
{
	// Try to see if this instruction was originally a splat.
	// We know this if we find a shufflevector instruction, that has a zero mask, and it's first operand
	// is an insert element used only once, and which has an undef/poison operand.
	const ShuffleVectorInst& svi = cast<ShuffleVectorInst>(I);
	if (svi.isZeroEltSplat())
	{
		Value* firstOp = svi.getOperand(0);
		if (!firstOp->hasOneUse())
			return false;
		if (const InsertElementInst* iei = dyn_cast<InsertElementInst>(firstOp))
		{
			if (isa<UndefValue>(iei->getOperand(0)))
			{
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

bool SIMDLoweringPass::lowerGeneralUnsupportedVectorOperation(Instruction& I)
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

bool SIMDLoweringPass::isVariableExtractOrInsert(Instruction& I)
{
	return (I.getOpcode() == Instruction::ExtractElement && !isa<ConstantInt>(I.getOperand(1))) || 
			(I.getOpcode() == Instruction::InsertElement && !isa<ConstantInt>(I.getOperand(2)));
}

bool SIMDLoweringPass::isReduceIntrinsic(Instruction& I)
{
	if (I.getOpcode() != Instruction::Call)
		return false;
	Intrinsic::ID id = cast<CallInst>(I).getCalledFunction()->getIntrinsicID();
	return (id == Intrinsic::vector_reduce_mul ||
		id == Intrinsic::vector_reduce_add ||
		id == Intrinsic::vector_reduce_fmul ||
		id == Intrinsic::vector_reduce_fadd);
}

PreservedAnalyses SIMDLoweringPass::run(Function& F, FunctionAnalysisManager& FAM)
{
	if (!WasmSIMD)
		return PreservedAnalyses::all();
	extractInsertAlloca = nullptr;
	deleteList.clear();
	bool needToBreak;
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
			// instead add all the versions of these instructions with a switch.
			if (isVariableExtractOrInsert(I))
				needToBreak = lowerExtractOrInsert(I);
			else if (isReduceIntrinsic(I))
				needToBreak = lowerReduceIntrinsic(I);
			else if ((I.getOpcode() == Instruction::Shl || I.getOpcode() == Instruction::AShr ||
					I.getOpcode() == Instruction::LShr) && I.getType()->isVectorTy())
				needToBreak = lowerBitShift(I);
			else if (I.getOpcode() == Instruction::ShuffleVector)
				needToBreak = lowerSplat(I);
			else if ((I.getOpcode() == Instruction::SDiv || I.getOpcode() == Instruction::UDiv)
					&& I.getType()->isVectorTy())
				needToBreak = lowerGeneralUnsupportedVectorOperation(I);
			if (needToBreak)
				break ;
		}
	}
	for (Instruction* I: deleteList)
		I->eraseFromParent();
	PreservedAnalyses PA;
	PA.preserve<cheerp::GlobalDepsAnalysis>();
	PA.preserve<cheerp::LinearMemoryAnalysis>();
	return PA;
}

}

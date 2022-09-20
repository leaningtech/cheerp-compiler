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

bool SIMDLoweringPass::lowerExtractOrInsert(Instruction& I)
{
	bool extract = I.getOpcode() == Instruction::ExtractElement;
	Function* F = I.getFunction();
	Value* vec = I.getOperand(0);
	assert(vec->getType()->isVectorTy());
	Type* elementType = cast<VectorType>(vec->getType())->getElementType();
	Value *variableOp = extract ? I.getOperand(1) : I.getOperand(2);
	IRBuilder<> Builder(&I);
	Builder.SetInsertPoint(F->getEntryBlock().getFirstNonPHI());
	AllocaInst* ai = Builder.CreateAlloca(vec->getType());
	Builder.SetInsertPoint(&I);
	Value* indexes[] = { variableOp };
	Value* bitcast = Builder.CreateBitCast(ai, elementType->getPointerTo());
	Value* gep = Builder.CreateInBoundsGEP(elementType, bitcast, indexes);
	Builder.CreateStore(vec, ai);
	Value* load;
	if (extract)
		load = Builder.CreateLoad(elementType, gep);
	else
	{
		Builder.CreateStore(I.getOperand(1), gep);
		load = Builder.CreateLoad(vec->getType(), ai);
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
	deleteList.clear();
	bool needToBreak;
	for (auto it = F.begin(); it != F.end(); it++)
	{
		BasicBlock& BB = *it;
		for (Instruction& I: BB)
		{
			// This will find certain instructions that do not allow variables as lane indexes and
			// instead add all the versions of these instructions with a switch.
			if (isVariableExtractOrInsert(I))
				needToBreak = lowerExtractOrInsert(I);
			else if (isReduceIntrinsic(I))
				needToBreak = lowerReduceIntrinsic(I);
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

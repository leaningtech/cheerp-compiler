//===-- CFGPasses.cpp - Optimizations on the control flow graph --------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/CFGPasses.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/InvokeWrapping.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

namespace cheerp {

bool RemoveFwdBlocks::runOnFunction(Function& F)
{
	bool changed = false;
	for (auto BB = F.begin(); BB!=F.end(); BB++)
	{
		// We are only interested in blocks containing only the terminator
		if (!BB->begin()->isTerminator()) continue;
		Instruction* term = BB->getTerminator();
		// We are only interested in unconditional branches
		if (term->getNumSuccessors() != 1) continue;
		BasicBlock* predBB = BB->getSinglePredecessor();
		// We need exactly one predecessor
		// TODO: loop over all predecessors
		if (!predBB) continue;
		BasicBlock* succBB = term->getSuccessor(0);
		// TODO: relax the following constraint (handle switches)
		if (predBB->getTerminator()->getNumSuccessors() != 2) continue;

		// If the successor block has a phi node, abort
		// TODO: relax this a bit?
		if (succBB->getFirstNonPHI() != &*succBB->begin()) continue;

		Instruction* predTerm = predBB->getTerminator();
		// If the predecessor's terminator now branches only to the current
		// block, turn it into an unconditional branch
		bool unconditional = true;
		for (uint32_t i = 0; i < predTerm->getNumSuccessors(); i++)
		{
			if (predTerm->getSuccessor(i) == &*BB)
			{
				predTerm->setSuccessor(i, succBB);
			}
			else if (predTerm->getSuccessor(i) != succBB)
			{
				unconditional = false;
			}
		}
		if (unconditional)
		{
			BranchInst* newTerm = BranchInst::Create(succBB);
			ReplaceInstWithInst(predTerm, newTerm);
		}

		// Finally, replace this block with the successor
		BB->replaceAllUsesWith(predBB);
		BB->eraseFromParent();
		BB = predBB->getIterator();

		changed = true;
	}
	return changed;
}

PreservedAnalyses RemoveFwdBlocksPass::run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM)
{
		RemoveFwdBlocks inner;

		if (!inner.runOnFunction(F))
			return PreservedAnalyses::all();
		else
		{
		PreservedAnalyses PA;
		PA.preserve<cheerp::PointerAnalysis>();
		PA.preserve<cheerp::RegisterizeAnalysis>();
		PA.preserve<cheerp::GlobalDepsAnalysis>();
		PA.preserve<cheerp::InvokeWrappingAnalysis>();
		PA.preserve<cheerp::LinearMemoryAnalysis>();
		return PA;
		}
}

void LowerAndOrBranches::fixTargetPhis(llvm::BasicBlock* originalPred, llvm::BasicBlock* newBlock, llvm::BasicBlock* singleBlock, llvm::BasicBlock* doubleBlock)
{
	// Fix PHI nodes in target blocks
	auto trueIt = singleBlock->begin();
	while(llvm::PHINode* phi = dyn_cast<llvm::PHINode>(&*trueIt))
	{
		++trueIt;
		int predIndex = phi->getBasicBlockIndex(originalPred);
		assert(predIndex >= 0);
		phi->setIncomingBlock(predIndex, newBlock);
	}
	auto falseIt = doubleBlock->begin();
	while(llvm::PHINode* phi = dyn_cast<llvm::PHINode>(&*falseIt))
	{
		++falseIt;
		int predIndex = phi->getBasicBlockIndex(originalPred);
		assert(predIndex >= 0);
		llvm::Value* incoming = phi->getIncomingValue(predIndex);
		phi->addIncoming(incoming, newBlock);
	}
}

bool LowerAndOrBranches::runOnFunction(Function& F)
{
	bool Changed = false;

	const Type* boolean = Type::getInt1Ty(F.getContext());
	std::vector<Instruction*> toBeErased;
	for (llvm::BasicBlock& BB: F)
	{
		for (Instruction& I : BB)
		{
			if (I.getType() != boolean)
				continue;
			if (I.getOpcode() != Instruction::And)
				continue;

			ICmpInst* A = dyn_cast<ICmpInst>(I.getOperand(0));
			ICmpInst* B = dyn_cast<ICmpInst>(I.getOperand(1));

			//Consider only integer aritmethics
			if (A && B)
			{
				//Consider only ( a==b && c==d)
				if (A->getPredicate() != ICmpInst::ICMP_EQ)
					continue;
				if (B->getPredicate() != ICmpInst::ICMP_EQ)
					continue;

				//We may consider pointer operands only in linear addressing mode (since it requires a PtrToInt
				if ((A->getOperand(0)->getType()->isPointerTy() || B->getOperand(0)->getType()->isPointerTy()) &&
					(F.getSection() != StringRef("asmjs")))
					continue;

				auto requiredSize = [](const Type* type) -> int
				{
					if (type->isPointerTy())
						return 32;
					if (type->isIntegerTy(32))
						return 32;
					if (type->isIntegerTy(64))
						return 64;
					if (type->isIntegerTy(16))
						return 16;
					if (type->isIntegerTy(8))
						return 8;
					return 32;
				};

				Type* targetType = Type::getInt32Ty(F.getContext());
				int maxSize = std::max(requiredSize(A->getOperand(0)->getType()), requiredSize(B->getOperand(0)->getType()));
				if (maxSize == 64)
					targetType = Type::getInt64Ty(F.getContext());

				auto getOperandToIntegerType = [&](Value* value) -> Value*
				{
					if (value->getType()->isPointerTy())
						return new PtrToIntInst(value, Type::getInt32Ty(F.getContext()), value->getName()+".toint", &I);
					return value;
				};

				//Gather the 4 operands (possibly after performing PtrToInt on them)
				Value *A0 = getOperandToIntegerType(A->getOperand(0));
				Value *A1 = getOperandToIntegerType(A->getOperand(1));
				Value *B0 = getOperandToIntegerType(B->getOperand(0));
				Value *B1 = getOperandToIntegerType(B->getOperand(1));

				auto doXorAndExtend = [&](Value* a, Value* b) -> Value*
				{
					bool isZero = false;
					if (Constant* C = dyn_cast<Constant>(b))
						if (C->isZeroValue())
							isZero = true;

					Value* res = nullptr;
					if (isZero)
						res = a;
					else
						res = BinaryOperator::CreateXor(a, b, "", &I);

					if (res->getType() != targetType)
						res = new ZExtInst(res, targetType, "", &I);

					return res;
				};

				//Xor the couple of values, so if (a == b) is the same as ((a^b) == 0)
				Value* xor1 = doXorAndExtend(A0, A1);
				Value* xor2 = doXorAndExtend(B0, B1);

				//Or the xors, since only if both were 0 the orOfXor would be 0, and >0 in all other cases
				Instruction * orOfXor = BinaryOperator::CreateOr(xor1, xor2, "", &I);

				//New comparions, orOfXor against 0
				ICmpInst * compare = new ICmpInst(ICmpInst::ICMP_EQ, orOfXor, ConstantInt::get(targetType, 0));
				compare->insertAfter(orOfXor);

				I.replaceAllUsesWith(compare);
				toBeErased.push_back(&I);

				if (A->use_empty())
					A->eraseFromParent();
				if (B->use_empty())
					B->eraseFromParent();

				Changed = true;
			}
		}
	}

	for (Instruction* I : toBeErased)
	{
		I->eraseFromParent();
	}

	// Gather all the BranchInst we need to analyze, this vector can grow over time
	llvm::SmallVector<llvm::BranchInst*, 4> condBraches;
	for(llvm::BasicBlock& BB: F)
	{
		llvm::Instruction* ti = BB.getTerminator();
		llvm::BranchInst* bi = dyn_cast<llvm::BranchInst>(ti);
		if(bi == nullptr || bi->isUnconditional())
			continue;
		condBraches.push_back(bi);
	}
	for(uint32_t i=0;i<condBraches.size();i++)
	{
		llvm::BranchInst* bi = condBraches[i];
		llvm::Value* cond = bi->getCondition();
		// Only interested in And/Or instructions
		llvm::Instruction* condI = dyn_cast<llvm::Instruction>(cond);
		if(condI == nullptr)
			continue;
		if(condI->getOpcode() == llvm::Instruction::And)
		{
			Changed = true;
			// Split in 2 branches, only reach the target if both are true
			llvm::BasicBlock* trueBlock = bi->getSuccessor(0);
			llvm::BasicBlock* falseBlock = bi->getSuccessor(1);
			llvm::BasicBlock* newBlock = llvm::BasicBlock::Create(F.getContext(), "andbranch", &F);
			llvm::BranchInst* firstBranch = llvm::BranchInst::Create(newBlock, falseBlock, condI->getOperand(0), bi);
			llvm::BranchInst* secondBranch = llvm::BranchInst::Create(trueBlock, falseBlock, condI->getOperand(1), newBlock);
			fixTargetPhis(bi->getParent(), newBlock, trueBlock, falseBlock);
			// Enqueue the new branches in the vector, we may need to split them again
			condBraches.push_back(firstBranch);
			condBraches.push_back(secondBranch);
			assert(bi->use_empty());
			// The old branch can be removed
			bi->eraseFromParent();
			// The old condition may still be used elsewhere
			if(condI->use_empty())
				condI->eraseFromParent();
		}
		else if(condI->getOpcode() == llvm::Instruction::Or)
		{
			Changed = true;
			// Split in 2 branches, reach the target if either is true
			llvm::BasicBlock* trueBlock = bi->getSuccessor(0);
			llvm::BasicBlock* falseBlock = bi->getSuccessor(1);
			llvm::BasicBlock* newBlock = llvm::BasicBlock::Create(F.getContext(), "orbranch", &F);
			llvm::BranchInst* firstBranch = llvm::BranchInst::Create(trueBlock, newBlock, condI->getOperand(0), bi);
			llvm::BranchInst* secondBranch = llvm::BranchInst::Create(trueBlock, falseBlock, condI->getOperand(1), newBlock);
			fixTargetPhis(bi->getParent(), newBlock, falseBlock, trueBlock);
			// Enqueue the new branches in the vector, we may need to split them again
			condBraches.push_back(firstBranch);
			condBraches.push_back(secondBranch);
			assert(bi->use_empty());
			// The old branch can be removed
			bi->eraseFromParent();
			// The old condition may still be used elsewhere
			if(condI->use_empty())
				condI->eraseFromParent();
		}
	}
	return Changed;
}

llvm::PreservedAnalyses LowerAndOrBranchesPass::run(Function& F, FunctionAnalysisManager& FAM)
{
	LowerAndOrBranches inner;
	if (inner.runOnFunction(F))
		return PreservedAnalyses::none();
	return PreservedAnalyses::all();
}

}

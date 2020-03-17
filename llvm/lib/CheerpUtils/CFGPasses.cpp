#include "llvm/Cheerp/CFGPasses.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

namespace llvm {

bool RemoveFwdBlocks::runOnFunction(Function& F)
{
	bool changed = false;
	for (auto BB = F.begin(); BB!=F.end(); BB++)
	{
		// We are only interested in blocks containing only the terminator
		if (!BB->begin()->isTerminator()) continue;
		TerminatorInst* term = BB->getTerminator();
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

		TerminatorInst* predTerm = predBB->getTerminator();
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
StringRef RemoveFwdBlocks::getPassName() const
{
	return "RemoveFwdBlocks";
}

char RemoveFwdBlocks::ID = 0;

void RemoveFwdBlocks::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addPreserved<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::Registerize>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	llvm::Pass::getAnalysisUsage(AU);
}

FunctionPass *createRemoveFwdBlocksPass() { return new RemoveFwdBlocks(); }

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
	// Gather all the BranchInst we need to analyze, this vector can grow over time
	llvm::SmallVector<llvm::BranchInst*, 4> condBraches;
	for(llvm::BasicBlock& BB: F)
	{
		llvm::TerminatorInst* ti = BB.getTerminator();
		llvm::BranchInst* bi = dyn_cast<llvm::BranchInst>(ti);
		if(bi == nullptr || bi->isUnconditional())
			continue;
		condBraches.push_back(bi);
	}
	bool Changed = false;
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

StringRef LowerAndOrBranches::getPassName() const
{
	return "LowerAndOrBranches";
}

char LowerAndOrBranches::ID = 0;

FunctionPass *createLowerAndOrBranchesPass() { return new LowerAndOrBranches(); }

}

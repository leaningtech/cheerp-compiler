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
		if (succBB->getFirstNonPHI() != succBB->begin()) continue;

		TerminatorInst* predTerm = predBB->getTerminator();
		// If the predecessor's terminator now branches only to the current
		// block, turn it into an unconditional branch
		bool unconditional = true;
		for (uint32_t i = 0; i < predTerm->getNumSuccessors(); i++)
		{
			if (predTerm->getSuccessor(i) == BB)
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
const char* RemoveFwdBlocks::getPassName() const
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
}

//===-- Cheerp/FixIrreducibleControlFlow.h - Cheerp optimization pass ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_FIX_IRREDUCIBLE_CONTROL_FLOW_H
#define _CHEERP_FIX_IRREDUCIBLE_CONTROL_FLOW_H

#include "llvm/Pass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/raw_ostream.h"

#include <unordered_map>
#include <unordered_set>

namespace llvm
{

/**
 * Remove allocas to asmjs types and add stack manipulation intrinsics
 */
class FixIrreducibleControlFlow: public FunctionPass
{
public:
	static char ID;
	explicit FixIrreducibleControlFlow() : FunctionPass(ID) { }
	bool runOnFunction(Function &F) override;
	const char *getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
private:
	/// A generalization of a basic block, containing either a single block, a loop,
	/// or a set of metablocks dominated by the Entry
	class MetaBlock {
		BasicBlock *Entry;
		// The original predecessors of this metablock. The actual predecessor will
		// eventually be the dispatch block
		SmallPtrSet<BasicBlock *, 2> Preds;
		// The actual successors of this metablock. They will be the original successors
		// at the beginning, and the forward blocks in a second phase
		SmallPtrSet<BasicBlock *, 2> Succs;
		// The forward blocks that logically lead TOWARDS this metablock
		SmallPtrSet<BasicBlock *, 2> Forwards;
		SmallPtrSet<BasicBlock *, 2> Blocks;

	public:
		explicit MetaBlock(BasicBlock *BB)
			: Entry(BB), Preds(pred_begin(BB), pred_end(BB)),
			Succs(succ_begin(BB), succ_end(BB))
		{
			Blocks.insert(BB);
		}

		explicit MetaBlock(Loop *L) : Entry(L->getHeader()), Blocks(L->block_begin(), L->block_end())
		{
			SmallVector<BasicBlock*, 2> tmp;
			L->getExitBlocks(tmp);
			Succs.insert(tmp.begin(), tmp.end());
			for (pred_iterator pit = pred_begin(Entry), pet = pred_end(Entry); pit != pet; pit++)
			{
				Preds.insert(*pit);
			}
		}

		BasicBlock *getEntry() const { return Entry; }

		const SmallPtrSetImpl<BasicBlock*>& getBlocks() const {
			return Blocks;
		}
		const SmallPtrSetImpl<BasicBlock *> &predecessors() const {
			return Preds;
		}
		const SmallPtrSetImpl<BasicBlock *> &successors() const {
			return Succs;
		}
		const SmallPtrSetImpl<BasicBlock *> &forwards() const {
			return Forwards;
		}

		void mergeMetaBlock(const MetaBlock& New) {
			bool wasSucc = Succs.erase(New.getEntry());
			assert(wasSucc);
			Succs.insert(New.Succs.begin(), New.Succs.end());
			Blocks.insert(New.Blocks.begin(), New.Blocks.end());
		}
		void updateSuccessor(BasicBlock* Old, BasicBlock* New) {
			Succs.erase(Old);
			Succs.insert(New);
		}
		void addForwardBlock(BasicBlock* Fwd) {
			Forwards.insert(Fwd);
		}
		bool contains(const BasicBlock* Target) const {
			return Blocks.count(const_cast<BasicBlock*>(Target)) != 0;
		}
		bool isSuccessor(BasicBlock* S) const {
			return Succs.count(S) != 0;
		}

		bool operator==(const BasicBlock* BB) const { return Entry == BB; }
		bool operator==(const MetaBlock &MBB) const { return Entry == MBB.Entry; }
		bool operator!=(const MetaBlock &MBB) const { return Entry != MBB.Entry; }
		bool operator<(const MetaBlock &MBB) const { return Entry < MBB.Entry; }

		void dump() const
		{
			llvm::errs() << "META ---------------------\n";
			for (const auto& BB: Blocks)
			{
				BB->dump();
			}
			llvm::errs() << "ENDMETA ---------------------\n";
		}
	};
	/// Utility class that performs the FixIrreducibleControlFlow logic for every
	/// loop in the function, including the nullptr loop representing the function
	/// itself
	class LoopVisitor {
	public:
		LoopVisitor(Function &F, LoopInfo &LI, Loop *L): F(F), LI(LI), L(L)
		{}
		bool VisitLoop();

	private:
		// True if the block is a forward block coming to the dispatcher from inside the loop
		bool comingFromLoop(BasicBlock* B);
		// Get the metablock this block belongs to, or nullptr
		MetaBlock* getParentMetaBlock(BasicBlock* BB);
		// Create the forward blocks and wire them to the dispatcher
		void fixPredecessor(MetaBlock& Meta, BasicBlock* Pred, MetaBlock* PredMeta);
		// Move the PHIs at the entry of a metablock into the dispatcher
		void makeDispatchPHIs(const MetaBlock& Meta);
		// Fix a use that is not dominated by its definition anymore
		void fixUse(Use& U);
		// Main processing function
		void processBlocks(SetVector<BasicBlock*>& Heads);

	private:
		Function &F;
		LoopInfo &LI;
		Loop *L;
		DominatorTree DT;
		// The metabloks corresponding to the irreducible loop we identified
		std::vector<MetaBlock> MetaBlocks;
		// The new block that will become the single entry of the new loop
		BasicBlock* Dispatcher;
		// The value used by the dispatcher for forwarding to the next metablock
		PHINode* Label;
		// Map that associate the entries of the metablocks with their index in the
		// switch instruction in the dispatcher
		DenseMap<BasicBlock*, unsigned> Indices;
		// PHIs that were in the entry block of a metablock, and are now lifted
		// in the dispatcher
		std::unordered_set<PHINode*> DispatchPHIs;
		// Instructions inside the metablocks that need a corresponding PHI in
		// the dispatcher to fix domination issues
		std::unordered_map<Instruction*, PHINode*> DomPHIs;
	};
};

//===----------------------------------------------------------------------===//
//
// FixIrreducibleControlFlow
//
FunctionPass *createFixIrreducibleControlFlowPass();

}

#endif

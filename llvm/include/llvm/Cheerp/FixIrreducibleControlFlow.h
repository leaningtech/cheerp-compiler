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
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/raw_ostream.h"

#include <unordered_map>
#include <unordered_set>
#include <queue>

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
		// The forward blocks that logically lead TOWARDS this metablock
		SmallPtrSet<BasicBlock *, 2> Forwards;
		// The blocks included in this metablock
		SmallPtrSet<BasicBlock *, 2> Blocks;

		void addBlocks(DomTreeNode* Node, std::unordered_set<BasicBlock*> Group)
		{
			for (auto N: make_range(df_begin(Node), df_end(Node)))
			{
				if (Group.count(N->getBlock()))
					Blocks.insert(N->getBlock());
			}
		}
		void addPreds()
		{
			for (auto Pred: make_range(pred_begin(Entry), pred_end(Entry)))
			{
				if (!Blocks.count(Pred))
				{
					Preds.insert(Pred);
				}
			}
		}
	public:
		explicit MetaBlock(DomTreeNode* Node, std::unordered_set<BasicBlock*> Group): Entry(Node->getBlock())
		{
			addBlocks(Node, Group);
			addPreds();
		}

		BasicBlock *getEntry() const { return Entry; }

		const SmallPtrSetImpl<BasicBlock*>& getBlocks() const {
			return Blocks;
		}
		const SmallPtrSetImpl<BasicBlock *> &predecessors() const {
			return Preds;
		}
		const SmallPtrSetImpl<BasicBlock *> &forwards() const {
			return Forwards;
		}

		void addForwardBlock(BasicBlock* Fwd) {
			Forwards.insert(Fwd);
		}
		bool contains(const BasicBlock* Target) const {
			return Blocks.count(const_cast<BasicBlock*>(Target)) != 0;
		}

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
public:
	class SubGraph;
	struct GraphNode {
		BasicBlock* Header;
		std::vector<BasicBlock*> Succs;
		SubGraph& Graph;
		explicit GraphNode(BasicBlock* BB, SubGraph& Graph);
	};
	class SubGraph {
		typedef std::unordered_set<BasicBlock*> BlockSet;
		BasicBlock* Entry;
		BlockSet Blocks;
		std::unordered_map<BasicBlock*, GraphNode> Nodes;
	public:
		explicit SubGraph(BasicBlock* Entry, BlockSet Blocks): Entry(Entry), Blocks(std::move(Blocks))
		{
		}
		BasicBlock* getEntry() { return Entry; }
	private:
		GraphNode* getOrCreate(BasicBlock* BB)
		{
			auto it = Nodes.find(BB);
			if (it == Nodes.end())
			{
				it = Nodes.emplace(BB, GraphNode(BB, *this)).first;
			}
			return &it->second;
		}
		friend struct GraphTraits<SubGraph*>;
		friend struct GraphNode;
	};
private:
	/// Utility class that performs the FixIrreducibleControlFlow logic for every
	/// loop in the function, including the nullptr loop representing the function
	/// itself
	class SCCVisitor {
	public:
		SCCVisitor(Function &F, const std::vector<GraphNode*>& SCC)
			: F(F), SCC(SCC)
		{}
		void run(std::queue<SubGraph>& Queue);

	private:
		// Create the forward blocks and wire them to the dispatcher
		void fixPredecessor(MetaBlock& Meta, BasicBlock* Pred);
		// Move the PHIs at the entry of a metablock into the dispatcher
		void makeDispatchPHIs(const MetaBlock& Meta);
		// Fix a use that is not dominated by its definition anymore
		void fixUse(Use& U);
		// Main processing function
		void processBlocks();

	private:
		Function &F;
		DominatorTree DT;
		const std::vector<GraphNode*>& SCC;
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
		// Map to store the original PHIs that were not completely removed by
		// the DispatchPHIs, and need to update the uses
		std::unordered_map<PHINode*, PHINode*> DelayedFixes;
	};
	bool visitSubGraph(Function& F, std::queue<SubGraph>& Queue);
};

template <> struct GraphTraits<FixIrreducibleControlFlow::SubGraph*> {
	typedef FixIrreducibleControlFlow::GraphNode NodeType;
	typedef mapped_iterator<std::vector<BasicBlock*>::iterator, std::function<FixIrreducibleControlFlow::GraphNode*(BasicBlock*)>> ChildIteratorType;

	static NodeType *getEntryNode(FixIrreducibleControlFlow::SubGraph* G) { return G->getOrCreate(G->Entry); }
	static inline ChildIteratorType child_begin(NodeType *N) {
		return ChildIteratorType(N->Succs.begin(), [N](BasicBlock* BB){ return N->Graph.getOrCreate(BB);});
	}
	static inline ChildIteratorType child_end(NodeType *N) {
		return ChildIteratorType(N->Succs.end(), [N](BasicBlock* BB){ return N->Graph.getOrCreate(BB);});
	}
};


//===----------------------------------------------------------------------===//
//
// FixIrreducibleControlFlow
//
FunctionPass *createFixIrreducibleControlFlowPass();

}

#endif

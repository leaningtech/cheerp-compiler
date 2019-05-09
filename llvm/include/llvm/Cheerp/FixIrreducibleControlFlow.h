//===-- Cheerp/FixIrreducibleControlFlow.h - Cheerp optimization pass ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2018-2019 Leaning Technologies
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
#include "llvm/Cheerp/DeterministicPtrSet.h"

#include <unordered_map>
#include <unordered_set>
#include <queue>

namespace llvm
{

/**
 * Transform multiple entry loops in single entry ones
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
	typedef cheerp::DeterministicPtrSet<BasicBlock *> DeterministicBBSet;
	/// An header of a (possibly) multi-header loop
	class Header {
		BasicBlock *BB;
		// The original predecessors of this header. The actual predecessor will
		// eventually be the dispatch block
		DeterministicBBSet Preds;
		// The forward blocks that logically lead TOWARDS this header
		DeterministicBBSet Forwards;
	public:
		explicit Header(BasicBlock* BB, DominatorTree& DT): BB(BB)
		{
			for (auto Pred: llvm::predecessors(BB))
			{
				// Do not consider backedges of inner loops dominated by the header
				if (!DT.dominates(BB, Pred))
				{
					Preds.insert(Pred);
				}
			}
		}

		BasicBlock *getBB() const { return BB; }

		const DeterministicBBSet &predecessors() const {
			return Preds;
		}
		const DeterministicBBSet &forwards() const {
			return Forwards;
		}

		void addForwardBlock(BasicBlock* Fwd) {
			Forwards.insert(Fwd);
		}
	};
public:
	class SubGraph;
	struct GraphNode {
		BasicBlock* BB;
		SmallVector<BasicBlock*, 2> Succs;
		SubGraph& Graph;
		explicit GraphNode(BasicBlock* BB, SubGraph& Graph);
	};
	class SubGraph {
	public:
		typedef DeterministicBBSet BlockSet;
		typedef std::unordered_map<BasicBlock*, GraphNode> NodeMap;

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

		BasicBlock* Entry;
		BlockSet Blocks;
		NodeMap Nodes;
	};
private:
	/// Utility class that performs the FixIrreducibleControlFlow logic on the
	// provided SCC
	class SCCVisitor {
	public:
		SCCVisitor(Function &F, const std::vector<GraphNode*>& SCC)
			: F(F), SCC(SCC)
		{}
		std::pair<SubGraph, bool> run();

	private:
		// Create the forward blocks and wire them to the dispatcher
		void fixPredecessor(Header& H, BasicBlock* Pred);
		// Move the PHIs in the header into the dispatcher
		void makeDispatchPHIs(const Header& H);
		// Main processing function
		void processBlocks();

	private:
		Function &F;
		DominatorTree DT;
		const std::vector<GraphNode*>& SCC;
		// The headers of the irreducible loop we identified
		std::vector<Header> Headers;
		// The new block that will become the single entry of the new loop
		BasicBlock* Dispatcher;
		// The value used by the dispatcher for forwarding to the next header
		PHINode* Label;
		// Map that associates the headers with their index in the
		// switch instruction in the dispatcher
		DenseMap<BasicBlock*, unsigned> Indices;
	};
};

template <> struct GraphTraits<FixIrreducibleControlFlow::SubGraph*> {
	typedef FixIrreducibleControlFlow::GraphNode NodeType;
	typedef mapped_iterator<SmallVectorImpl<BasicBlock*>::iterator, std::function<FixIrreducibleControlFlow::GraphNode*(BasicBlock*)>> ChildIteratorType;

	static NodeType *getEntryNode(FixIrreducibleControlFlow::SubGraph* G) { return G->getOrCreate(G->Entry); }
	static inline ChildIteratorType child_begin(NodeType *N) {
		return ChildIteratorType(N->Succs.begin(), [N](BasicBlock* BB){ return N->Graph.getOrCreate(BB);});
	}
	static inline ChildIteratorType child_end(NodeType *N) {
		return ChildIteratorType(N->Succs.end(), [](BasicBlock* BB){ llvm_unreachable("dereferencing past-the-end iterator");return nullptr;});
	}
};


//===----------------------------------------------------------------------===//
//
// FixIrreducibleControlFlow
//
FunctionPass *createFixIrreducibleControlFlowPass();

}

#endif

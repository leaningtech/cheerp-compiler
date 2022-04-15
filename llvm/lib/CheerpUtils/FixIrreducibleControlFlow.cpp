//===-- FixIrreducibleControlFlow.cpp - Cheerp optimization pass --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2018-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "CheerpFixIrreducibleControlFlow"
#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/FixIrreducibleControlFlow.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/InvokeWrapping.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Verifier.h"

using namespace llvm;
namespace cheerp
{

/**
 * Transform multiple entry loops in single entry ones
 */
class FixIrreducibleControlFlow
{
public:
	static char ID;
	explicit FixIrreducibleControlFlow() { }
	bool runOnFunction(Function &F);

private:
	typedef cheerp::DeterministicUnorderedSet<BasicBlock *, cheerp::RestrictionsLifted::NoErasure> DeterministicBBSet;
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

}
using namespace cheerp;
namespace llvm{

template <> struct GraphTraits<cheerp::FixIrreducibleControlFlow::SubGraph*> {
	typedef cheerp::FixIrreducibleControlFlow::GraphNode NodeType;
	typedef NodeType* NodeRef;
	typedef mapped_iterator<SmallVectorImpl<BasicBlock*>::iterator, std::function<cheerp::FixIrreducibleControlFlow::GraphNode*(BasicBlock*)>> ChildIteratorType;

	static NodeType *getEntryNode(cheerp::FixIrreducibleControlFlow::SubGraph* G) { return G->getOrCreate(G->Entry); }
	static inline ChildIteratorType child_begin(NodeType *N) {
		return ChildIteratorType(N->Succs.begin(), [N](BasicBlock* BB){ return N->Graph.getOrCreate(BB);});
	}
	static inline ChildIteratorType child_end(NodeType *N) {
		return ChildIteratorType(N->Succs.end(), [](BasicBlock* BB){ llvm_unreachable("dereferencing past-the-end iterator");return nullptr;});
	}
};
}
namespace cheerp {

void FixIrreducibleControlFlow::SCCVisitor::fixPredecessor(Header& H, BasicBlock* Pred)
{
	BasicBlock* BB = H.getBB();
	Function& F = *BB->getParent();
	auto& Context = F.getParent()->getContext();
	IntegerType* Int32Ty = IntegerType::getInt32Ty(Context);
	Instruction* Term = Pred->getTerminator();
	BasicBlock *Fwd = nullptr;
	for (size_t i = 0; i < Term->getNumSuccessors(); i++)
	{
		if (Term->getSuccessor(i) != BB) continue;
		if (Fwd == nullptr)
		{
			Fwd = BasicBlock::Create(Context,Twine(Pred->getName()) + "." + BB->getName() + ".forward", &F);
			H.addForwardBlock(Fwd);
			BranchInst::Create(Dispatcher, Fwd);
			Label->addIncoming(ConstantInt::get(Int32Ty,Indices[BB]), Fwd);
		}
		Term->setSuccessor(i, Fwd);
	}
}
void FixIrreducibleControlFlow::SCCVisitor::makeDispatchPHIs(const Header& H)
{

	BasicBlock* BB = H.getBB();
	for (BasicBlock::iterator I = BB->begin(), IE = BB->end(); I != IE;) {
		PHINode* P = dyn_cast<PHINode>(I);
		if (P == nullptr)
			break;
		PHINode* NewP = PHINode::Create(P->getType(), Label->getNumIncomingValues(), {P->getName(),".dispatch"}, &*Dispatcher->getFirstInsertionPt());
		for (auto F: H.forwards())
		{
			BasicBlock* Pred = F->getUniquePredecessor();
			assert(Pred);
			Value* V = P->getIncomingValueForBlock(Pred);
			NewP->addIncoming(V, F);
		}
		for (auto Pred = pred_begin(Dispatcher), PredE = pred_end(Dispatcher); Pred != PredE; Pred++)
		{
			if (NewP->getBasicBlockIndex(*Pred) == -1)
			{
				if (DT.dominates(P, *Pred))
					NewP->addIncoming(P, *Pred);
				else if (DT.dominates(NewP, *Pred))
					NewP->addIncoming(NewP, *Pred);
				else
					NewP->addIncoming(UndefValue::get(NewP->getType()), *Pred);
			}
		}

		++I;

		// Update the predecessors, removing the ones not part of the
		// metablock
		for (size_t i = 0; i < P->getNumIncomingValues();)
		{
			BasicBlock* Incoming = P->getIncomingBlock(i);
			if (!DT.dominates(BB, Incoming))
			{
				P->removeIncomingValue(i, false);
			}
			else
			{
				i++;
			}
		}
		if (P->getNumIncomingValues() == 0)
		{
			// The only predecessor is the dispatcher, completely remove
			// P and replace the uses with NewP
			assert(BB->getSinglePredecessor()==Dispatcher);
			BasicBlock::iterator ii(P);
			ReplaceInstWithValue(P->getParent()->getInstList(), ii, NewP);
		}
		else
		{
			// Add NewP as incoming from the dispatcher
			P->addIncoming(NewP, Dispatcher);
		}
	}
}


void FixIrreducibleControlFlow::SCCVisitor::processBlocks()
{
	auto& Context = F.getParent()->getContext();
	// Ok. We have irreducible control flow! Create a dispatch block which will
	// contains a jump table to any block in the problematic set of blocks.
	Dispatcher = BasicBlock::Create(Context, "dispatcher", &F);

	// Add the jump table.
	IntegerType* Int32Ty = IntegerType::getInt32Ty(Context);
	IRBuilder<> Builder(Dispatcher);
	Label = Builder.CreatePHI(Int32Ty, Headers.size(), "label");
	auto hit = Headers.begin(), he = Headers.end();
	Indices.insert(std::make_pair(hit->getBB(), -1));
	SwitchInst* Switch = Builder.CreateSwitch(Label, hit->getBB());
	hit++;
	int Index = 0;
	for(auto& H: make_range(hit, he))
	{
		Indices.insert(std::make_pair(H.getBB(), Index++));
		Switch->addCase(ConstantInt::get(Int32Ty, Switch->getNumCases()), H.getBB());
	}

	// Fix the control flow
	for (auto& H: Headers)
	{
		for (auto *Pred: H.predecessors())
		{
			fixPredecessor(H, Pred);
		}
	}

	// CFG is fixed from now on. Get the updated domination tree
	DT.recalculate(F);

	// Create all the DispatchPHIs and replace uses where appropriate
	for (const auto& H: Headers)
	{
		makeDispatchPHIs(H);
	}
}

FixIrreducibleControlFlow::GraphNode::GraphNode(BasicBlock* BB, SubGraph& Graph): BB(BB), Graph(Graph)
{
	for (auto Succ: successors(BB))
	{
		// Skip edges that go outside of the SubGraph, or that loop back to the entry
		if (!Graph.Blocks.count(Succ) || Succ == Graph.Entry)
			continue;
		Succs.push_back(Succ);
	}
}

std::pair<FixIrreducibleControlFlow::SubGraph, bool> FixIrreducibleControlFlow::SCCVisitor::run()
{
	bool Irreducible = false;
	DT.recalculate(F);
	SubGraph::BlockSet Group;
	for(auto& GN: SCC)
	{
		Group.insert(GN->BB);
	}
	for(auto BB: Group)
	{
		for (auto Pred: predecessors(BB))
		{
			if (!Group.count(Pred))
			{
				Headers.emplace_back(BB, DT);
				break;
			}
		}
	}
	BasicBlock* Entry = nullptr;
	if (Headers.size() != 1)
	{
		Irreducible = true;
		processBlocks();
		Entry = Dispatcher;
	}
	else
	{
		Entry = Headers.front().getBB();
	}
	Group.insert(Entry);
	SubGraph SG(Entry, std::move(Group));

	return std::make_pair(std::move(SG), Irreducible);
}

bool FixIrreducibleControlFlow::runOnFunction(Function& F)
{
	bool Changed = false;

	SubGraph::BlockSet BBs;
	for (auto& BB: F)
		BBs.insert(&BB);
	std::queue<SubGraph> Queue;
	SubGraph SG(&*F.begin(), std::move(BBs));
	Queue.push(std::move(SG));

	while(!Queue.empty())
	{
		SubGraph SG = std::move(Queue.front());
		Queue.pop();
		for (auto& SCC: make_range(scc_begin(&SG), scc_end(&SG)))
		{
			if (SCC.size() != 1)
			{
				SCCVisitor V(F, SCC);
				auto res = V.run();
				Queue.push(std::move(res.first));
				Changed |= res.second;
			}
		}
	}

#ifndef  NDEBUG
	bool v = verifyFunction(F, &llvm::errs());
	if (v)
	{
		F.dump();
		report_fatal_error("failed verification");
	}
#endif

	return Changed;
}

PreservedAnalyses FixIrreducibleControlFlowPass::run(Function& F, FunctionAnalysisManager& FAM)
{
	FixIrreducibleControlFlow inner;
	if (!inner.runOnFunction(F))
		return PreservedAnalyses::all();
	{
	PreservedAnalyses PA;
	PA.preserve<cheerp::GlobalDepsAnalysis>();
	PA.preserve<cheerp::InvokeWrappingAnalysis>();
	return PA;
	}
}

}

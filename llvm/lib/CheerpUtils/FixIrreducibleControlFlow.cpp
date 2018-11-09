//===-- FixIrreducibleControlFlow.cpp - Cheerp optimization pass --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "CheerpFixIrreducibleControlFlow"
#include "llvm/Cheerp/FixIrreducibleControlFlow.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/SCCIterator.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Verifier.h"

namespace llvm {

void FixIrreducibleControlFlow::SCCVisitor::fixPredecessor(MetaBlock& Meta, BasicBlock* Pred)
{
	BasicBlock* BB = Meta.getEntry();
	Function& F = *BB->getParent();
	auto& Context = F.getParent()->getContext();
	IntegerType* Int32Ty = IntegerType::getInt32Ty(Context);
	TerminatorInst* Term = Pred->getTerminator();
	BasicBlock *Fwd = nullptr;
	for (size_t i = 0; i < Term->getNumSuccessors(); i++)
	{
		if (Term->getSuccessor(i) != BB) continue;
		if (Fwd == nullptr)
		{
			Fwd = BasicBlock::Create(Context,Twine(Pred->getName()) + "." + BB->getName() + ".forward", &F);
			Meta.addForwardBlock(Fwd);
			BranchInst::Create(Dispatcher, Fwd);
			Label->addIncoming(ConstantInt::get(Int32Ty,Indices[BB]), Fwd);
		}
		Term->setSuccessor(i, Fwd);
	}
}
void FixIrreducibleControlFlow::SCCVisitor::makeDispatchPHIs(const MetaBlock& Meta)
{

	BasicBlock* BB = Meta.getEntry();
	for (BasicBlock::iterator I = BB->begin(), IE = BB->end(); I != IE;) {
		PHINode* P = dyn_cast<PHINode>(I);
		if (P == nullptr)
			break;
		PHINode* NewP = PHINode::Create(P->getType(), Label->getNumIncomingValues(), {P->getName(),".dispatch"}, Dispatcher->getFirstInsertionPt());
		for (auto F: Meta.forwards())
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
	Label = Builder.CreatePHI(Int32Ty, MetaBlocks.size(), "label");
	auto mit = MetaBlocks.begin(), me = MetaBlocks.end();
	Indices.insert(std::make_pair(mit->getEntry(), -1));
	SwitchInst* Switch = Builder.CreateSwitch(Label, mit->getEntry());
	mit++;
	int Index = 0;
	for(auto& Meta: make_range(mit, me))
	{
		Indices.insert(std::make_pair(Meta.getEntry(), Index++));
		Switch->addCase(ConstantInt::get(Int32Ty, Switch->getNumCases()), Meta.getEntry());
	}

	// Fix the control flow
	for (auto& Meta: MetaBlocks)
	{
		for (auto *Pred: Meta.predecessors())
		{
			fixPredecessor(Meta, Pred);
		}
	}

	// CFG is fixed from now on. Get the updated domination tree
	DT.recalculate(F);

	// Create all the DispatchPHIs, without fixing uses
	for (const auto& Meta: MetaBlocks)
	{
		makeDispatchPHIs(Meta);
	}
}

FixIrreducibleControlFlow::GraphNode::GraphNode(BasicBlock* BB, SubGraph& Graph): Header(BB), Graph(Graph)
{
	for (auto Succ: make_range(succ_begin(Header), succ_end(Header)))
	{
		if (!Graph.Blocks.count(Succ) || Succ == Graph.Entry)
			continue;
		Succs.push_back(Succ);
	}
}

bool FixIrreducibleControlFlow::visitSubGraph(Function& F, std::queue<SubGraph>& Queue)
{
	SubGraph SG = std::move(Queue.front());
	Queue.pop();
	bool Irreducible = false;
	for (auto& SCC: make_range(scc_begin(&SG), scc_end(&SG)))
	{
		if (SCC.size() != 1)
		{
			SCCVisitor V(F, SCC);
			Irreducible |= V.run(Queue);
		}
	}
	return Irreducible;
}

bool FixIrreducibleControlFlow::SCCVisitor::run(std::queue<SubGraph>& Queue)
{
	bool Irreducible = false;
	DT.recalculate(F);
	std::unordered_set<BasicBlock*> Group;
	for(auto& GN: SCC)
	{
		Group.insert(GN->Header);
	}
	for(auto& GN: SCC)
	{
		auto Node = DT.getNode(GN->Header);
		auto IDom = Node->getIDom();
		if (!Group.count(IDom->getBlock()))
		{
			MetaBlocks.emplace_back(GN->Header, DT);
		}
	}
	if (MetaBlocks.size() != 1)
	{
		Irreducible = true;
		processBlocks();
	}
	for (MetaBlock& Meta: MetaBlocks)
	{
		SubGraph::BlockSet BBs;
		auto Node = DT.getNode(Meta.getEntry());
		for (auto N: make_range(df_begin(Node), df_end(Node)))
		{
			if (Group.count(N->getBlock()))
				BBs.insert(N->getBlock());
		}
		SubGraph SG(Meta.getEntry(), std::move(BBs));
		Queue.push(std::move(SG));
	}
	return Irreducible;
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
		if (visitSubGraph(F, Queue))
			Changed = true;
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

const char* FixIrreducibleControlFlow::getPassName() const
{
	return "FixIrreducibleControlFlow";
}

char FixIrreducibleControlFlow::ID = 0;

void FixIrreducibleControlFlow::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	llvm::Pass::getAnalysisUsage(AU);
}

FunctionPass *createFixIrreducibleControlFlowPass() { return new FixIrreducibleControlFlow(); }

}

using namespace llvm;

INITIALIZE_PASS_BEGIN(FixIrreducibleControlFlow, "FixIrreducibleControlFlow", "Transform multiple entry loops in single entry ones",
                      false, false)
INITIALIZE_PASS_END(FixIrreducibleControlFlow, "FixIrreducibleControlFlow", "Transform multiple entry loops in single entry ones",
                    false, false)

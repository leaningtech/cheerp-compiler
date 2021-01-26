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
#include "llvm/InitializePasses.h"
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

StringRef FixIrreducibleControlFlow::getPassName() const
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

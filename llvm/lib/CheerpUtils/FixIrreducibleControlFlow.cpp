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

bool FixIrreducibleControlFlow::SCCVisitor::comingFromLoop(BasicBlock* B)
{
	for (auto& M: MetaBlocks)
	{
		if (M.isSuccessor(B))
			return true;
	}
	return false;
}
FixIrreducibleControlFlow::MetaBlock* FixIrreducibleControlFlow::SCCVisitor::getParentMetaBlock(BasicBlock* BB)
{
	for (auto& M: MetaBlocks)
	{
		if (M.contains(BB))
			return &M;
	}
	return nullptr;
}
void FixIrreducibleControlFlow::SCCVisitor::fixPredecessor(MetaBlock& Meta, BasicBlock* Pred, MetaBlock* PredMeta)
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
		if (PredMeta)
			PredMeta->updateSuccessor(BB, Fwd);
	}
}
void FixIrreducibleControlFlow::SCCVisitor::makeDispatchPHIs(const MetaBlock& Meta)
{

	BasicBlock* BB = Meta.getEntry();
	for (BasicBlock::iterator I = BB->begin(), IE = BB->end(); I != IE;) {
		PHINode* P = dyn_cast<PHINode>(I);
		if (P == nullptr)
			break;
		PHINode* NewP = PHINode::Create(P->getType(), P->getNumIncomingValues(), {P->getName(),".dispatch"}, Dispatcher->getFirstInsertionPt());
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
				if (Meta.isSuccessor(*Pred))
					NewP->addIncoming(P, *Pred);
				else if (comingFromLoop(*Pred))
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
			BasicBlock* B = P->getIncomingBlock(i);
			if (!Meta.contains(B))
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
			// Save the PHIs for the last stage of use replacement, when all the
			// PHIs are in their final position
			DelayedFixes.emplace(P, NewP);
		}
		DispatchPHIs.emplace(NewP);
	}
}

void FixIrreducibleControlFlow::SCCVisitor::fixUse(Use& U)
{
	Instruction* Def = cast<Instruction>(U.get());

	auto pit = DomPHIs.find(Def);
	if (pit == DomPHIs.end())
	{
		PHINode* p = nullptr;
		IRBuilder<> Builder(Dispatcher->getFirstInsertionPt());
		const MetaBlock* DefMeta = getParentMetaBlock(Def->getParent());
		if (DefMeta)
		{
			p = Builder.CreatePHI(Def->getType(), 2, {Def->getName(),".indomphi"});
			for (auto Pred = pred_begin(Dispatcher), PredE = pred_end(Dispatcher); Pred != PredE; Pred++)
			{
				if (DefMeta->isSuccessor(*Pred))
				{
					if (DT.dominates(Def, *Pred))
						p->addIncoming(Def, *Pred);
					else
						p->addIncoming(p, *Pred);
				}
				else
				{
					if (comingFromLoop(*Pred))
						p->addIncoming(p, *Pred);
					else
						p->addIncoming(UndefValue::get(p->getType()), *Pred);
				}
			}
		}
		else
		{
			p = Builder.CreatePHI(Def->getType(), 2, {Def->getName(),".outdomphi"});
			for (auto Pred = pred_begin(Dispatcher), PredE = pred_end(Dispatcher); Pred != PredE; Pred++)
			{
				if (comingFromLoop(*Pred))
				{
					p->addIncoming(p, *Pred);
				}
				else
				{
					if (DT.dominates(Def, *Pred))
						p->addIncoming(Def, *Pred);
					else
						p->addIncoming(UndefValue::get(p->getType()), *Pred);
				}
			}
		}
		U.set(p);
		DomPHIs.emplace(Def, p);
	}
	else
	{
		U.set(pit->second);
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
			MetaBlock* PredMeta = getParentMetaBlock(Pred);
			fixPredecessor(Meta, Pred, PredMeta);
		}
	}

	// CFG is fixed from now on. Get the updated domination tree
	DT.recalculate(F);

	// Create all the DispatchPHIs, without fixing uses
	for (const auto& Meta: MetaBlocks)
	{
		makeDispatchPHIs(Meta);
	}
	// Update the uses in the original PHIs that generated the DispatchPHIs
	for (auto& D: DelayedFixes)
	{
		auto P = D.first;
		auto NewP = D.second;
		auto& Meta = *getParentMetaBlock(P->getParent());
		// Replace all uses outside of the metablock and the dispatch with NewP
		auto UI = P->use_begin(), E = P->use_end();
		for (; UI != E;) {
			Use &U = *UI;
			++UI;
			auto *Usr = cast<Instruction>(U.getUser());
			if (Meta.contains(Usr->getParent()))
				continue;
			if (Usr->getParent() == Dispatcher)
				continue;
			U.set(NewP);
		}
	}
	// Create the DomPHIs for the uses in the loop blocks
	for (const auto& Meta: MetaBlocks)
	{
		for (BasicBlock* BB: Meta.getBlocks())
		{
			IRBuilder<> Builder(Dispatcher->getFirstInsertionPt());
			for (BasicBlock::iterator I = BB->begin(), IE = BB->end(); I != IE; ++I)
			{
				for (Use& U: I->operands())
				{
					Instruction* Def = dyn_cast<Instruction>(U.get());
					if (Def == nullptr)
						continue;
					if (Meta.contains(Def->getParent()))
						continue;
					if (Def->getParent() == Dispatcher)
						continue;
					fixUse(U);
				}
			}
		}
	}
	// Create the DomPHIs for the uses in the DispatchPHIs
	for (auto& DP: DispatchPHIs)
	{
		for (Use& U: DP->operands())
		{
			Instruction* Def = dyn_cast<Instruction>(U.get());
			if (Def == nullptr)
				continue;
			if (Def == DP)
				continue;
			BasicBlock* Pred = DP->getIncomingBlock(U);
			const MetaBlock* DefMeta = getParentMetaBlock(Def->getParent());
			if (!DefMeta && DT.dominates(Def, U))
				continue;
			if (DefMeta && DefMeta->isSuccessor(Pred))
				continue;
			fixUse(U);
		}
	}
}

FixIrreducibleControlFlow::GraphNode::GraphNode(BasicBlock* BB, SubGraph& Graph): Header(BB), Graph(Graph)
{
	for (auto Succ: make_range(succ_begin(Header), succ_end(Header)))
	{
		if (!Graph.Blocks.count(Succ) || Succ == Graph.Entry)
			continue;
		Succs.emplace_back(Succ);
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
			Irreducible = true;
			SCCVisitor V(F, SCC);
			V.run(Queue);
		}
	}
	return Irreducible;
}

void FixIrreducibleControlFlow::SCCVisitor::run(std::queue<SubGraph>& Queue)
{
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
			MetaBlocks.emplace_back(Node, Group);
		}
	}
	if (MetaBlocks.size() != 1)
		processBlocks();
	for (MetaBlock& Meta: MetaBlocks)
	{
		std::unordered_set<BasicBlock*> BBs(Meta.getBlocks().begin(), Meta.getBlocks().end());
		SubGraph SG(Meta.getEntry(), std::move(BBs));
		Queue.push(std::move(SG));
	}
}

bool FixIrreducibleControlFlow::runOnFunction(Function& F)
{
	bool Changed = false;

	std::unordered_set<BasicBlock*> BBs;
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

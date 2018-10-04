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
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Verifier.h"

namespace llvm {

bool FixIrreducibleControlFlow::LoopVisitor::comingFromLoop(BasicBlock* B)
{
	for (auto& M: MetaBlocks)
	{
		if (M.isSuccessor(B))
			return true;
	}
	return false;
}
FixIrreducibleControlFlow::MetaBlock* FixIrreducibleControlFlow::LoopVisitor::getParentMetaBlock(BasicBlock* BB)
{
	for (auto& M: MetaBlocks)
	{
		if (M.contains(BB))
			return &M;
	}
	return nullptr;
}
void FixIrreducibleControlFlow::LoopVisitor::fixPredecessor(MetaBlock& Meta, BasicBlock* Pred, MetaBlock* PredMeta)
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
void FixIrreducibleControlFlow::LoopVisitor::makeDispatchPHIs(const MetaBlock& Meta)
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
		if (BB->getSinglePredecessor())
		{
			// The only predecessor is the dispatcher, completely remove
			// the PHI
			BasicBlock::iterator ii(P);
			ReplaceInstWithValue(P->getParent()->getInstList(), ii, NewP);
		}
		else
		{
			// Update the predecessors, removing the ones not part of the
			// metablock
			for (size_t i = 0; i < P->getNumIncomingValues();)
			{
				BasicBlock* B = P->getIncomingBlock(i);
				if (!Meta.contains(B))
				{
					P->removeIncomingValue(i);
				}
				else
				{
					i++;
				}
			}
			// Add the dispatcher as predecessor
			P->addIncoming(NewP, Dispatcher);
			// Replace all uses outside of the metablock with NewP
			auto UI = P->use_begin(), E = P->use_end();
			for (; UI != E;) {
				Use &U = *UI;
				++UI;
				auto *Usr = dyn_cast<Instruction>(U.getUser());
				if (Usr && Meta.contains(Usr->getParent()))
					continue;
				U.set(NewP);
			}
		}
		DispatchPHIs.emplace(NewP);
	}
}

void FixIrreducibleControlFlow::LoopVisitor::fixUse(Use& U)
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
					p->addIncoming(Def, *Pred);
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

void FixIrreducibleControlFlow::LoopVisitor::processBlocks(SetVector<BasicBlock*>& Heads)
{
	auto& Context = F.getParent()->getContext();
	// Ok. We have irreducible control flow! Create a dispatch block which will
	// contains a jump table to any block in the problematic set of blocks.
	Dispatcher = BasicBlock::Create(Context, "dispatcher", &F);
	LI.changeLoopFor(Dispatcher, L);

	// Add the jump table.
	IntegerType* Int32Ty = IntegerType::getInt32Ty(Context);
	IRBuilder<> Builder(Dispatcher);
	Label = Builder.CreatePHI(Int32Ty, 2, "label");
	SwitchInst* Switch = Builder.CreateSwitch(Label, Dispatcher);

	// Collect all the blocks inside the loop
	SmallVector<BasicBlock *, 4> SuccWorklist(Heads.begin(), Heads.end());
	while (!SuccWorklist.empty())
	{
		BasicBlock *BB = SuccWorklist.pop_back_val();
		unsigned Index = Switch->getNumCases();
		auto idx = Indices.insert(std::make_pair(BB, Index));
		if (!idx.second)
			continue;
		Loop* L = LI.getLoopFor(BB);
		MetaBlock Meta = (L && L->getHeader()==BB) ? MetaBlock(L) : MetaBlock(BB);
		for (auto *Succ : Meta.successors())
		{
			SuccWorklist.push_back(Succ);
		}
		MetaBlocks.push_back(std::move(Meta));
		Switch->addCase(ConstantInt::get(Int32Ty, Index), BB);
	}
	// Create the forward blocks
	for (auto& Meta: MetaBlocks)
	{
		for (auto *Pred: Meta.predecessors())
		{
			MetaBlock* PredMeta = getParentMetaBlock(Pred);
			fixPredecessor(Meta, Pred, PredMeta);
		}
	}
	Switch->setDefaultDest(Switch->getSuccessor(Switch->getNumSuccessors()-1));

	// CFG is fixed from now on. Get the domination tree
	DT.recalculate(F);

	// Create the DispatchPHIs
	for (const auto& Meta: MetaBlocks)
	{
		makeDispatchPHIs(Meta);
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
bool FixIrreducibleControlFlow::LoopVisitor::VisitLoop()
{
	/// Utility class used to walk through tha cfg skipping inner loops
	class SuccessorList final : public MetaBlock {
		size_t Index;
		size_t Num;

	public:
		explicit SuccessorList(BasicBlock *BB)
			: MetaBlock(BB), Index(0), Num(successors().size()) {}

		explicit SuccessorList(Loop *L)
			: MetaBlock(L), Index(0), Num(successors().size()) {}

		bool HasNext() const { return Index != Num; }

		BasicBlock *Next() {
			assert(HasNext());
			auto it = successors().begin();
			std::advance(it,Index++);
			return *it;
		}
	};

	BasicBlock *Header = L ? L->getHeader() : &*F.begin();
	SetVector<BasicBlock *> Heads;

	// DFS through L's body, looking for irreducible control flow. L is
	// natural, and we stay in its body, and we treat any nested loops
	// monolithically, so any cycles we encounter indicate irreducibility.
	SmallPtrSet<BasicBlock *, 8> OnStack;
	SmallVector<SuccessorList, 4> LoopWorklist;

	SmallPtrSet<BasicBlock *, 8> Visited;
	LoopWorklist.push_back(SuccessorList(Header));
	OnStack.insert(Header);
	Visited.insert(Header);
	while (!LoopWorklist.empty())
	{
		SuccessorList &Top = LoopWorklist.back();
		if (Top.HasNext())
		{
			BasicBlock *Next = Top.Next();
			// If we are back at the beginning of the llvm loop, or out of it,
			// skip
			if (Next == Header || (L && !L->contains(Next)))
			{
				continue;
			}
			if (LLVM_LIKELY(OnStack.insert(Next).second))
			{
				if (!Visited.insert(Next).second)
				{
					OnStack.erase(Next);
					continue;
				}
				Loop *InnerLoop = LI.getLoopFor(Next);
				if (InnerLoop != L)
					LoopWorklist.push_back(SuccessorList(InnerLoop));
				else
					LoopWorklist.push_back(SuccessorList(Next));
      		}
			else
			{
				Heads.insert(Next);
			}
			continue;
		}
		OnStack.erase(Top.getEntry());
		LoopWorklist.pop_back();
	}

	// Most likely, we didn't find any irreducible control flow.
	if (LLVM_LIKELY(Heads.empty()))
		return false;

	processBlocks(Heads);
	return true;
}

bool FixIrreducibleControlFlow::runOnFunction(Function& F)
{
	bool Changed = false;
	auto &LI = getAnalysis<LoopInfo>();

	{
		LoopVisitor LV(F, LI, nullptr);
		// Visit the function body, which is identified as a null loop.
		Changed |= LV.VisitLoop();
	}

	// Visit all the loops.
	SmallVector<Loop *, 8> Worklist(LI.begin(), LI.end());
	while (!Worklist.empty())
	{
		Loop *CurLoop = Worklist.pop_back_val();
		Worklist.append(CurLoop->begin(), CurLoop->end());
		LoopVisitor LV(F, LI, CurLoop);
		Changed |= LV.VisitLoop();
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
	AU.addRequired<LoopInfo>();
	AU.addPreserved<LoopInfo>();
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

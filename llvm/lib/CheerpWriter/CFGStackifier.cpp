//===-- CFGStackifier.cpp - Cheerp rendering helper --------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "CFGStackifier.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/IR/CFG.h"

#include <unordered_set>
#include <list>

using namespace llvm;

static BasicBlock* getUniqueForwardPredecessor(BasicBlock* BB, LoopInfo& LI)
{
	Loop* L = LI.isLoopHeader(BB) ? LI.getLoopFor(BB) : nullptr;
	BasicBlock* UniquePred = nullptr;
	for (auto Pred: make_range(pred_begin(BB), pred_end(BB)))
	{
		if (L && L->contains(Pred))
			continue;
		if (UniquePred)
			return nullptr;
		UniquePred = Pred;
	}
	return UniquePred;
}

static int getDefaultBranchIdx(BasicBlock* From)
{
	TerminatorInst* term = From->getTerminator();
	if (BranchInst* br = dyn_cast<BranchInst>(term))
	{
		return br->isConditional() ? 1 : 0;
	}
	else if (isa<SwitchInst>(term))
	{
		return 0;
	}
	return -1;
}

static bool isBackedge(BasicBlock* From, BasicBlock* To, LoopInfo& LI)
{
	return LI.isLoopHeader(To) && LI.getLoopFor(To)->contains(From);
}

static int getNumForwardPreds(BasicBlock* BB, LoopInfo& LI)
{
	int ForwardPreds = 0;
	for (auto P: make_range(pred_begin(BB), pred_end(BB)))
	{
		if (isBackedge(P, BB, LI))
			continue;
		ForwardPreds++;
	}
	return ForwardPreds;
}

static int getBranchIdx(BasicBlock* From, BasicBlock* To)
{
	int DefaultIdx = getDefaultBranchIdx(From);
	assert(DefaultIdx != -1 && "Not a conditional branch");
	int BrIdx = 0;
	for (auto S = succ_begin(From), SE = succ_end(From); S != SE; ++S, ++BrIdx)
	{
		if (*S == To)
		{
			return BrIdx == DefaultIdx ? -1 : BrIdx;
		}
	}
	report_fatal_error("No switch or branch between blocks From and To");
}


namespace cheerp {

#ifdef DEBUG_CFGSTACKIFIER
void CFGStackifier::Block::dump() const
{
	llvm::errs()<<id;
	llvm::errs()<<":\n";
	for (auto s: scopes)
	{
		switch(s.kind)
		{
			case LOOP:
				llvm::errs()<<"LOOP ";
				break;
			case LOOP_END:
				llvm::errs()<<"LOOP_END ";
				break;
			case BLOCK:
				llvm::errs()<<"BLOCK ";
				break;
			case BLOCK_END:
				llvm::errs()<<"BLOCK_END ";
				break;
			case BRANCH:
				llvm::errs()<<"BRANCH ";
				break;
			case BRANCH_END:
				llvm::errs()<<"BRANCH_END ";
				break;
		}
		llvm::errs()<< s.start << " " << s.end << "\n";
	}
	for (auto p: naturalPreds)
	{
		llvm::errs()<<"NATURAL PRED " << p << "\n";
	}
	if (BB)
	{
		llvm::errs()<<"BB ";
		BB->printAsOperand(llvm::errs(), false);
		llvm::errs()<<"\n";
		for (auto Succ: make_range(succ_begin(BB), succ_end(BB)))
		{
			llvm::errs() << "\t-> ";
			Succ->printAsOperand(llvm::errs(), false);
			llvm::errs()<<"\n";
		}
	}
	else
	{
		llvm::errs()<<"END\n";
	}
}
#endif

CFGStackifier::Block::Scope* CFGStackifier::Block::insertScope(CFGStackifier::Block::Scope s)
{
	switch (s.kind)
	{
		case BLOCK_END:
		case LOOP_END:
		case BRANCH_END:
		{
			auto insertPoint = std::find_if(scopes.begin(), scopes.end(), [&s](Scope& ss) {
				if (ss.kind == LOOP || ss.kind == BLOCK || ss.kind == BRANCH)
					return true;
				if (ss.start < s.start)
					return true;
				if (ss.end == s.end && ss.kind == BRANCH)
					return true;
				return false;
			});
			return &*scopes.insert(insertPoint, s);
		}
		case BLOCK:
		case LOOP:
		case BRANCH:
		{
			auto insertPoint = std::find_if(scopes.begin(), scopes.end(), [&s](Scope& ss) {
				if (ss.kind == LOOP_END || ss.kind == BLOCK_END || ss.kind == BRANCH_END)
					return false;
				if (ss.end > s.end)
					return false;
				if (ss.end == s.end && ss.kind == BRANCH)
					return false;
				return true;
			});
			return &*scopes.insert(insertPoint, s);
		}
	}
}
class BlockListBuilder {
public:
	using Block = CFGStackifier::Block;
	struct StackElem {
		enum Kind {
			BLOCK,
			DOM
		};
		union {
			llvm::BasicBlock* BB;
			llvm::DomTreeNode* DN;
		};
		Kind kind;
		StackElem(llvm::BasicBlock* BB): BB(BB), kind(Kind::BLOCK) {}
		StackElem(llvm::DomTreeNode* DN): DN(DN), kind(Kind::DOM) {}
	};
	struct BranchChain {
		Block& Root;
		bool Finished;
		std::vector<Block::Scope*> Branches;
		std::vector<int> Exits;
		BranchChain(Block& Root): Root(Root), Finished(false) {}
		Block& getRoot()
		{
			return Root;
		}
		void addBranch(Block& B)
		{
			auto S = B.insertScope(Block::Scope{Block::BRANCH, Root.getId(), -1});
			Branches.push_back(S);
		}
		void addExit(Block& B)
		{
			Exits.push_back(B.getId());
		}
		void setEnd(Block& B)
		{
			assert(Finished);
			for (auto S: Branches)
			{
				S->end = B.getId();
			}
			B.insertScope(Block::Scope{Block::BRANCH_END, Root.getId(), B.getId()});
			for (int E: Exits)
			{
				B.addNaturalPred(E);
			}
		}
		void merge(BranchChain& Other)
		{
			Exits.insert(Exits.end(), Other.Exits.begin(), Other.Exits.end());
		}
		void finish()
		{
			Finished = true;
		}
		bool isFinished()
		{
			return Finished;
		}
	};

	BlockListBuilder(const Function &F,
		std::vector<Block>& BlockList,
		std::unordered_map<BasicBlock*, int>& BlockIdMap,
		LoopInfo& LI, DominatorTree& DT)
		: F(const_cast<Function&>(F)), BlockList(BlockList), BlockIdMap(BlockIdMap), LI(LI), DT(DT)
	{
		build();
	}
private:
	void endBranchScopes(Block& B);
	bool enqueueSucc(BasicBlock* CurBB, BasicBlock* Succ);
	void processBlock(BasicBlock* CurBB, bool Delayed);
	void addLoopMarkers(Block& B);
	void addBlockMarkers(Block& B);
	int findLoopEnd(llvm::Loop* L, const Block& B);
	void build();

	Function& F;
	std::vector<Block>& BlockList;
	std::unordered_map<BasicBlock*, int>& BlockIdMap;
	LoopInfo& LI;
	DominatorTree& DT;
	DenseMap<BasicBlock*, int> Visited;
	std::unordered_map<DomTreeNode*, std::vector<BasicBlock*>> Queues;
	std::unordered_map<DomTreeNode*, BranchChain> Branches;
	std::vector<BranchChain*> BranchChainScopes;
	std::vector<StackElem> VisitStack;
};

void BlockListBuilder::endBranchScopes(Block& B)
{
	while (!BranchChainScopes.empty())
	{
		BranchChain& BC = *BranchChainScopes.back();
		if (!DT.dominates(BC.getRoot().getBB(), B.getBB()))
			BC.finish();
		if (!BC.isFinished())
		{
			break;
		}

		BC.addExit(BlockList[B.getId()-1]);
		BC.setEnd(BlockList.back());
		BranchChainScopes.pop_back();
	}
}

bool BlockListBuilder::enqueueSucc(BasicBlock* CurBB, BasicBlock* Succ)
{
	// Ignore backedges
	if (isBackedge(CurBB, Succ, LI))
		return false;
	// If Succ is a branch, add it to the visit stack
	if (getUniqueForwardPredecessor(Succ, LI))
	{
		VisitStack.push_back(Succ);
		return true;
	}
	// Otherwise, check if it is visitable
	int SuccForwardPreds = getNumForwardPreds(Succ, LI);
	if (++Visited[Succ] != SuccForwardPreds)
		return false;
	// Add it to to the delayed list
	DomTreeNode* SuccDN = DT.getNode(Succ)->getIDom();
	Queues[SuccDN].insert(Queues[SuccDN].end(), Succ);
	return false;
}

void BlockListBuilder::processBlock(BasicBlock* CurBB, bool Delayed)
{
	// Immediately add the block to its final position
	int Id = BlockList.size();
	BlockList.emplace_back(CurBB, Id);
	BlockIdMap.emplace(CurBB, Id);
	Block& CurB = BlockList.back();

	// Place BRANCH and BRANCH_END marks
	endBranchScopes(BlockList.back());
	BasicBlock* P = getUniqueForwardPredecessor(CurBB, LI);
	if (P && P->getTerminator()->getNumSuccessors() > 1)
	{
		assert(!BranchChainScopes.empty());
		BranchChainScopes.back()->addBranch(CurB);
		BranchChainScopes.back()->addExit(BlockList[CurB.getId()-1]);
	}
	if (P)
	{
		CurB.addNaturalPred(BlockIdMap.at(P));
	}

	VisitStack.emplace_back(DT.getNode(CurBB));

	bool HasNestedSuccs = false;
	bool DefaultIsNested = false;
	int DefaultIdx = getDefaultBranchIdx(CurBB);
	// enqueue the default case first (so it will be handled last)
	if (DefaultIdx != -1)
	{
		DefaultIsNested = enqueueSucc(CurBB, CurBB->getTerminator()->getSuccessor(DefaultIdx));
		HasNestedSuccs |= DefaultIsNested;
	}
	int Idx = 0;
	for (auto Succ: make_range(succ_begin(CurBB), succ_end(CurBB)))
	{
		if (DefaultIdx == Idx++)
			continue;
		HasNestedSuccs |= enqueueSucc(CurBB, Succ);
	}
	bool IsBranchRoot = Idx > 1 && HasNestedSuccs;
	if (IsBranchRoot)
	{
		if (DefaultIsNested)
		{
			CurB.setBranchState(Block::BranchState::RenderBranchCase::DEFAULT_NESTED);
		}
		else
		{
			CurB.setBranchState(Block::BranchState::RenderBranchCase::DEFAULT_FORWARD);
		}
		DomTreeNode* CurDN = DT.getNode(CurBB);
		auto it = Branches.emplace(CurDN, BranchChain(BlockList.back())).first;
		BranchChainScopes.push_back(&it->second);
	}

}

void BlockListBuilder::build()
{
	BlockList.reserve(F.size()+1);

	Visited[&F.getEntryBlock()] = 0;
	VisitStack.push_back(DT.getNode(&F.getEntryBlock()));
	VisitStack.push_back(&F.getEntryBlock());
	while(!VisitStack.empty())
	{
		StackElem CurE = VisitStack.back();
		if (CurE.kind == StackElem::DOM) {
			auto it = Branches.find(CurE.DN);
			if (it != Branches.end())
				it->second.finish();
			std::vector<BasicBlock*>& Delayed = Queues[CurE.DN];
			if (Delayed.empty())
			{
				VisitStack.pop_back();
				continue;
			}
			BasicBlock* DB = Delayed.back();
			Delayed.pop_back();
			processBlock(DB, true);
			continue;
		}
		VisitStack.pop_back();
		processBlock(CurE.BB, false);
	}

	// Fake block to collect the end markers
	BlockList.emplace_back(nullptr, F.size());
	BlockIdMap.emplace(nullptr, -1);
	endBranchScopes(BlockList.back());


	for (Block& B: BlockList)
	{
		// The fake block is the last one
		if (B.getBB() == nullptr)
			break;
		addLoopMarkers(B);
		addBlockMarkers(B);
	}
}

int BlockListBuilder::findLoopEnd(Loop* L, const Block& B)
{
	int EndId = B.getId() + 1;
	for (; EndId < (int)BlockList.size()-1; ++EndId)
	{
		if (!DT.dominates(L->getHeader(), BlockList[EndId].getBB()))
			break;
	}
	return EndId;
}

void BlockListBuilder::addLoopMarkers(Block& B)
{
	Loop *L = LI.getLoopFor(B.getBB());
	if (!L || L->getHeader() != B.getBB())
		return;
	int endId = findLoopEnd(L, B);
	if (endId == -1)
	{
		B.getBB()->getParent()->viewCFGOnly();
		report_fatal_error("noo");
	}
	B.insertScope(Block::Scope{Block::LOOP, B.getId(), endId});
	BlockList[endId].insertScope(Block::Scope{Block::LOOP_END, B.getId(), endId});
}

void BlockListBuilder::addBlockMarkers(Block& B)
{
	BasicBlock* BB = B.getBB();
	// First compute the nearest common dominator of all forward non-fallthrough
	// predecessors so that we minimize the time that the BLOCK is on the stack,
	// which reduces overall stack height.
	BasicBlock* Header = nullptr;
	bool Natural = true;
	for (pred_iterator pit = pred_begin(BB), pet = pred_end(BB); pit != pet; ++pit)
	{
		if (BlockIdMap.at(*pit) < B.getId())
		{
			Header = Header ? DT.findNearestCommonDominator(Header, *pit) : *pit;
			if (!B.isNaturalPred(BlockIdMap[*pit]))
			{
				Natural = false;
			}
		}
	}
	// No forward predecessors
	if (Header == nullptr)
		return;

	// Natural predecessors
	if (Natural)
		return;

	int PredId = BlockIdMap.at(Header);
	assert(PredId < B.getId());

	// If the nearest common dominator is inside a more deeply nested context,
	// walk out to the nearest scope which isn't more deeply nested.
	for (auto it = BlockList.begin()+B.getId()-1, e = BlockList.begin()+PredId; it != e; --it)
	{
		const Block::Scope* s = it->getTopEndScope();
		// walk out of this scope
		if (s && s->start < PredId)
		{
			PredId = s->start;
		}
	}
	BlockList[PredId].insertScope(Block::Scope{Block::BLOCK, PredId, B.getId()});
	B.insertScope(Block::Scope{Block::BLOCK_END, PredId, B.getId()});
}

CFGStackifier::CFGStackifier(const Function& F, LoopInfo& LI, DominatorTree& DT)
{
	BlockListBuilder BL(F, BlockList, BlockIdMap, LI, DT);

#if DEBUG_CFGSTACKIFIER
	llvm::errs()<<"CFG Stack of function "<<F.getName()<<":\n";
	for (const auto& B: BlockList)
	{
		B.dump();
	}
#endif
}

void CFGStackifier::render(RenderInterface& ri, bool asmjs)
{
	using Scope = Block::Scope;
	std::vector<const Scope*> ScopeStack;
	std::unordered_map<const Block*, std::vector<const Scope*>> ScopeMap;
	std::unordered_map<const Scope*, size_t> labels;
	size_t next_label = 1;
	auto renderForwardBranch = [&](const Block& From, const Block& To, int BrId, bool First)
	{
		bool unconditional = BrId == -1 && First;
		if (!unconditional)
		{
			if (BrId == -1)
				ri.renderElseBlockBegin();
			else
				ri.renderIfBlockBegin(From.getBB(), BrId, First);
		}
		ri.renderBlockPrologue(To.getBB(), From.getBB());

		if (!To.isNaturalPred(From.getId()))
			[&]() {
				size_t depth = 0;
				const std::vector<const Scope*>& Scopes = ScopeMap.at(&From);
				for (auto s=Scopes.rbegin(), se=Scopes.rend(); s!=se; ++s, ++depth)
				{
					if ((*s)->kind == Block::BLOCK && To.getId() == (*s)->end)
					{
						ri.renderBreak(labels.at(*s));
						return;
					}
					else if ((*s)->kind == Block::LOOP && To.getId() == (*s)->start)
					{
						ri.renderContinue(labels.at(*s));
						return;
					}
				}
				report_fatal_error("No scope for branching");
			}();
		if (BrId == -1 && !First)
		{
			ri.renderBlockEnd();
		}
	};
	auto renderForwardBranches = [&](const Block& B)
	{
		int DefaultId = getDefaultBranchIdx(B.getBB());
		BasicBlock* Default = DefaultId != -1 ? B.getBB()->getTerminator()->getSuccessor(DefaultId) : nullptr;
		Block::Scope* BS = BlockList[B.getId()+1].getBranchStart();
		bool Delayed = BS && BS->start == B.getId();
		bool First = !Delayed || BlockList[B.getId()+1].getBB() == Default;
		int BrId = 0;
		for (auto S = succ_begin(B.getBB()), SE = succ_end(B.getBB()); S != SE; ++S, ++BrId)
		{
			if (BrId==DefaultId)
			{
				continue;
			}
			const Block& To = BlockList[BlockIdMap[*S]];
			const Scope* Branch = To.getBranchStart();
			if (!Branch || Branch->start != B.getId())
			{
				renderForwardBranch(B, To, BrId, First);
			}
			First = false;
		}
		if (Default)
		{
			const Block& DefaultB = BlockList[BlockIdMap[Default]];
			const Block::Scope* DBS = DefaultB.getBranchStart();
			if (!DBS || DBS->start != B.getId())
			{
				renderForwardBranch(B, DefaultB, -1, First);
			}
		}
	};
	for (int id = 0; id <(int) BlockList.size(); ++id)
	{
		const Block& B = BlockList[id];
		BasicBlock* BB = B.getBB();
		for (const auto& scope: B.getScopes())
		switch (scope.kind)
		{
			case Block::LOOP:
				ri.renderWhileBlockBegin(next_label);
				ScopeStack.push_back(&scope);
				labels.emplace(&scope, next_label++);
				break;
			case Block::BLOCK:
				ri.renderDoBlockBegin(next_label);
				ScopeStack.push_back(&scope);
				labels.emplace(&scope, next_label++);
				break;
			case Block::LOOP_END:
				ri.renderBreak();
				ri.renderBlockEnd();
				assert(ScopeStack.back()->kind == Block::LOOP);
				ScopeStack.pop_back();
				break;
			case Block::BLOCK_END:
				ri.renderDoBlockEnd();
				assert(ScopeStack.back()->kind == Block::BLOCK);
				ScopeStack.pop_back();
				break;
			case Block::BRANCH:
			{
				int FromId = scope.start;
				const Block& From = BlockList[FromId];
				int BrId = getBranchIdx(From.getBB(), BB);
				bool First = FromId == id - 1;

				if (BrId == -1)
				{
					renderForwardBranches(From);
					ri.renderElseBlockBegin();
				}
				else
				{
					ri.renderIfBlockBegin(From.getBB(), BrId, First);
				}
				ri.renderBlockPrologue(BB, From.getBB());
				break;
			}
			case Block::BRANCH_END:
			{
				int FromId = scope.start;
				const Block& From = BlockList[FromId];
				if (From.getBranchCase() == Block::BranchState::RenderBranchCase::DEFAULT_FORWARD)
				{
					renderForwardBranches(From);
				}
				else
				{
					ri.renderBlockEnd();
				}
				break;
			}
		}
		ScopeMap[&B] = ScopeStack;
		// Fake block is at the end
		if (!BB)
			break;
		ri.renderBlock(BB);

		if (B.getBranchCase() == Block::BranchState::RenderBranchCase::ONLY_FORWARDS)
		{
			renderForwardBranches(B);
		}
	}
}

}

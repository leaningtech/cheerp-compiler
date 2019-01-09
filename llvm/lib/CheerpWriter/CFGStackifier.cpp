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

#include "llvm/Cheerp/Writer.h"

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

CFGStackifier::Block::ScopeIter CFGStackifier::Block::insertScope(CFGStackifier::Block::Scope s)
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
			return scopes.insert(insertPoint, s);
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
			return scopes.insert(insertPoint, s);
		}
	}
}
void CFGStackifier::Block::removeScope(CFGStackifier::Block::ScopeIter s)
{
	scopes.erase(s);
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
		std::vector<std::pair<Block*, Block::ScopeIter>> Branches;
		std::vector<int> Exits;
		BranchChain(Block& Root): Root(Root), Finished(false) {}
		Block& getRoot()
		{
			return Root;
		}
		void addBranch(Block& B)
		{
			auto S = B.insertScope(Block::Scope{Block::BRANCH, Root.getId(), -1});
			Branches.push_back(std::make_pair(&B, S));
		}
		void addExit(Block& B)
		{
			Exits.push_back(B.getId());
		}
		void setEnd(Block& B)
		{
			assert(Finished);
			if (Branches.empty())
				return;
			for (auto S: Branches)
			{
				S.second->end = B.getId();
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
		: F(const_cast<Function&>(F)), BlockList(BlockList), BlockIdMap(BlockIdMap)
		, LI(LI), DT(DT)
	{
		build();
	}
private:
	void endBranchScopes(Block& B);
	// If no branch is jumping ahead of the last nested one, it is possible to
	// unnest it without causing the need for an extra BLOCK scope
	bool removeLastNested(BranchChain& BC);
	bool enqueueSucc(BasicBlock* CurBB, BasicBlock* Succ);
	void processBlock(BasicBlock* CurBB, bool Delayed);
	void addLoopMarkers(Block& B);
	void addBlockMarkers(Block& B);
	int findLoopEnd(Loop* L, const Block& B);
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

		if (!removeLastNested(BC))
		{
			BC.addExit(BlockList[B.getId()-1]);
			BC.setEnd(BlockList.back());
		}
		BranchChainScopes.pop_back();
	}
}

bool BlockListBuilder::removeLastNested(BranchChain& BC)
{
	auto si = BC.Branches.back();
	for (BasicBlock* Succ: make_range(succ_begin(BC.Root.getBB()), succ_end(BC.Root.getBB())))
	{
		if (Succ == si.first->getBB())
			continue;
		auto it = BlockIdMap.find(Succ);
		if (it == BlockIdMap.end())
			return false;
		Block& SuccB = BlockList[it->second];
		auto* Br = SuccB.getBranchStart();
		if (Br && Br->start == BC.Root.getId())
		{
			for (int E: BC.Exits)
			{
				Block& EB = BlockList[E];
				if (EB.getId() == BC.Root.getId())
					continue;
				for (BasicBlock* ExitSucc: make_range(succ_begin(EB.getBB()), succ_end(EB.getBB())))
				{
					auto it = BlockIdMap.find(ExitSucc);
					if (it == BlockIdMap.end() || it->second > si.first->getId())
						return false;
				}

			}
		}
		else if (SuccB.getId() > BC.Root.getId())
			return false;
	}
	si.first->removeScope(si.second);
	BC.Branches.pop_back();
	BC.setEnd(*si.first);
	return true;
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
		BasicBlock* Default = CurBB->getTerminator()->getSuccessor(DefaultIdx);
		DefaultIsNested = enqueueSucc(CurBB, Default);
		HasNestedSuccs |= DefaultIsNested;
	}
	int Idx = 0;
	for (auto Succ: make_range(succ_begin(CurBB), succ_end(CurBB)))
	{
		if (DefaultIdx == Idx++)
			continue;
		bool Nested = enqueueSucc(CurBB, Succ);
		HasNestedSuccs |= Nested;
	}
	bool IsBranchRoot = Idx > 1 && HasNestedSuccs;
	if (IsBranchRoot)
	{
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
	size_t N = L->getNumBlocks();
	int EndId = B.getId();
	Block::Scope* LastScope = nullptr;
	for (; N > 0; ++EndId)
	{
		if (EndId == (int)BlockList.size())
			return EndId;
		if (L->contains(BlockList[EndId].getBB()))
			N--;
		Block::Scope* CurScope = BlockList[EndId+1].getBranchStart();
		if (CurScope && CurScope->start == EndId)
		{
			if (!LastScope || CurScope->end > LastScope->end)
				LastScope = CurScope;
		}
	}
	if (LastScope && LastScope->end > EndId)
	{
		return  LastScope->end;
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
			if (!B.isNaturalPred(BlockIdMap.at(*pit)))
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

class BlockListRenderer {
	using Block = CFGStackifier::Block;
	using Scope = Block::Scope;

public:
	BlockListRenderer(const std::vector<Block>& BlockList, const std::unordered_map<BasicBlock*, int> BlockIdMap, RenderInterface& ri, const Registerize& R, const PointerAnalyzer& PA, bool asmjs)
		: BlockList(BlockList), BlockIdMap(BlockIdMap), ri(ri), R(R), PA(PA), asmjs(asmjs)
	{
		render();
	}
private:
	enum RenderBranchCase {
		JUMP,
		NESTED,
		EMPTY,
		DIRECT,
	};
	struct BranchesState {
		bool IsBranchRoot{false};
		std::unordered_map<llvm::BasicBlock*, RenderBranchCase> Cases;
		RenderBranchCase DefaultCase{JUMP};
	};

	void renderJump(const Block& From, const Block& To);
	void renderJumpBranch(const Block& From, const Block& To, int BrId, bool First);
	void renderDefaultJumpBranch(const Block& From, const Block& To, const std::vector<int>& EmptyIds, bool First);
	void renderDirectBranch(const Block& From, const Block& To);
	std::vector<int> renderJumpBranches(const Block& B);
	void render();

	bool isEmptyPrologue(BasicBlock* From, BasicBlock* To);

	const Block& getBlock(int id) const
	{
		assert(id >= 0 && id < (int)BlockList.size());
		return BlockList[id];
	}
	const Block& getBlock(llvm::BasicBlock* BB) const
	{
		auto it = BlockIdMap.find(BB);
		assert(it != BlockIdMap.end());
		const Block& B =  getBlock(it->second);
		assert(B.getBB() == BB);
		return B;
	}

	std::vector<const Scope*> ScopeStack;
	std::unordered_map<const Block*, std::vector<const Scope*>> ScopeMap;
	std::unordered_map<const Scope*, size_t> labels;
	std::unordered_map<const BasicBlock*, BranchesState> BranchesStates;
	size_t next_label = 1;

	const std::vector<Block> BlockList;
	const std::unordered_map<BasicBlock*, int> BlockIdMap;

	RenderInterface& ri;
	const Registerize& R;
	const PointerAnalyzer& PA;
	bool asmjs;
};

bool BlockListRenderer::isEmptyPrologue(BasicBlock* From, BasicBlock* To)
{
	bool hasPrologue = To->getFirstNonPHI()!=&To->front();
	if (hasPrologue)
	{
		// We can avoid assignment from the same register if no pointer kind
		// conversion is required
		hasPrologue = CheerpWriter::needsPointerKindConversionForBlocks(To, From, PA, R);
	}
	return !hasPrologue;
}
void BlockListRenderer::renderJump(const Block& From, const Block& To)
{
	size_t depth = 0;
	const std::vector<const Scope*>& Scopes = ScopeMap.at(&From);
	for (auto s=Scopes.rbegin(), se=Scopes.rend(); s!=se; ++s, ++depth)
	{
		if ((*s)->kind == Block::BLOCK && To.getId() == (*s)->end)
		{
			if (s == Scopes.rbegin())
				ri.renderBreak();
			else
				ri.renderBreak(labels.at(*s));
			return;
		}
		else if ((*s)->kind == Block::LOOP && To.getId() == (*s)->start)
		{
			if (s == Scopes.rbegin())
				ri.renderContinue();
			else
				ri.renderContinue(labels.at(*s));
			return;
		}
	}
	report_fatal_error("No scope for branching");
}
void BlockListRenderer::renderJumpBranch(const Block& From, const Block& To, int BrId, bool First)
{
	ri.renderIfBlockBegin(From.getBB(), BrId, First);
	ri.renderBlockPrologue(To.getBB(), From.getBB());
	if (!To.isNaturalPred(From.getId()))
		renderJump(From, To);
}
void BlockListRenderer::renderDefaultJumpBranch(const Block& From, const Block& To, const std::vector<int>& EmptyIds, bool First)
{
	if (EmptyIds.size() == 0)
		ri.renderElseBlockBegin();
	else
		ri.renderIfBlockBegin(From.getBB(), EmptyIds, First);
	ri.renderBlockPrologue(To.getBB(), From.getBB());
	if (!To.isNaturalPred(From.getId()))
		renderJump(From, To);
	ri.renderBlockEnd();
}
void BlockListRenderer::renderDirectBranch(const Block& From, const Block& To)
{
	ri.renderBlockPrologue(To.getBB(), From.getBB());
	if (!To.isNaturalPred(From.getId()))
		renderJump(From, To);
}
std::vector<int> BlockListRenderer::renderJumpBranches(const Block& B)
{
	int DefaultIdx = getDefaultBranchIdx(B.getBB());
	BasicBlock* Default = DefaultIdx != -1 ? B.getBB()->getTerminator()->getSuccessor(DefaultIdx) : nullptr;
	const Block::Scope* BS = getBlock(B.getId()+1).getBranchStart();
	bool Delayed = BS && BS->start == B.getId();
	bool First = !Delayed || getBlock(B.getId()+1).getBB() == Default;
	int BrIdx = 0;
	std::vector<int> EmptyIds;
	for (auto S = succ_begin(B.getBB()), SE = succ_end(B.getBB()); S != SE; ++S, ++BrIdx)
	{
		if (BrIdx==DefaultIdx)
		{
			continue;
		}
		const Block& To = getBlock(*S);
		RenderBranchCase BC = BranchesStates.at(B.getBB()).Cases.at(*S);
		switch (BC)
		{
			case RenderBranchCase::JUMP:
				renderJumpBranch(B, To, BrIdx, First);
				break;
			case RenderBranchCase::DIRECT:
				report_fatal_error("A direct branch must also be the default. This is a bug");
				break;
			case RenderBranchCase::NESTED:
				break;
			case RenderBranchCase::EMPTY:
				EmptyIds.push_back(BrIdx);
				break;
		}
		if (BC != RenderBranchCase::EMPTY)
			First = false;
	}
	if (Default)
	{
		const Block& DefaultB = getBlock(Default);
		switch (BranchesStates.at(B.getBB()).DefaultCase)
		{
			case RenderBranchCase::JUMP:
				renderDefaultJumpBranch(B, DefaultB, EmptyIds, First);
				break;
			case RenderBranchCase::DIRECT:
				renderDirectBranch(B, DefaultB);
				break;
			case RenderBranchCase::NESTED:
				break;
			case RenderBranchCase::EMPTY:
				ri.renderBlockEnd();
				break;
		}
	}
	return EmptyIds;
}
void BlockListRenderer::render()
{
	for (int id = 0; id <(int) BlockList.size(); ++id)
	{
		const Block& B = getBlock(id);
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
				const Block& From = getBlock(FromId);
				int BrId = getBranchIdx(From.getBB(), BB);
				bool First = FromId == id - 1;

				if (BrId == -1)
				{
					auto EmptyIds = renderJumpBranches(From);
					if (EmptyIds.size() == 0)
						ri.renderElseBlockBegin();
					else
						ri.renderIfBlockBegin(From.getBB(), EmptyIds, EmptyIds.size() == From.getBB()->getTerminator()->getNumSuccessors()-1);
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
				const Block& From = getBlock(FromId);
				auto& BS = BranchesStates.at(From.getBB());
				if (BS.IsBranchRoot && BS.DefaultCase != RenderBranchCase::NESTED)
				{
					renderJumpBranches(From);
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

		auto* Scope = BlockList[B.getId()+1].getBranchStart();
		auto& BS = BranchesStates[BB];
		BS.IsBranchRoot = Scope && Scope->start == B.getId();

		int DefaultIdx = getDefaultBranchIdx(BB);
		int Idx = 0;
		for (auto Succ: make_range(succ_begin(BB), succ_end(BB)))
		{
			if (DefaultIdx == Idx++)
				continue;
			const auto& SuccB = getBlock(Succ);
			auto* SuccScope = SuccB.getBranchStart();
			if (SuccScope && SuccScope->start == B.getId())
				BS.Cases.emplace(Succ, RenderBranchCase::NESTED);
			else if (isEmptyPrologue(BB, Succ) && SuccB.isNaturalPred(B.getId()))
				BS.Cases.emplace(Succ, RenderBranchCase::EMPTY);
			else
				BS.Cases.emplace(Succ, RenderBranchCase::JUMP);
		}
		if (DefaultIdx != -1)
		{
			const auto& DefaultB = getBlock(BB->getTerminator()->getSuccessor(DefaultIdx));
			auto* SuccScope = DefaultB.getBranchStart();
			if (Idx == 1)
				BS.DefaultCase = RenderBranchCase::DIRECT;
			else if (SuccScope && SuccScope->start == B.getId())
				BS.DefaultCase = RenderBranchCase::NESTED;
			else if (isEmptyPrologue(BB, DefaultB.getBB()) && DefaultB.isNaturalPred(B.getId()))
				BS.DefaultCase = RenderBranchCase::EMPTY;
			else
				BS.DefaultCase = RenderBranchCase::JUMP;
		}

		if (!BS.IsBranchRoot)
		{
			renderJumpBranches(B);
		}
	}
}
void CFGStackifier::render(RenderInterface& ri, const Registerize& R, const PointerAnalyzer& PA, bool asmjs)
{
	BlockListRenderer BR(BlockList, BlockIdMap, ri, R, PA, asmjs);
}

}

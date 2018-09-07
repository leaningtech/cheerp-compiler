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

#include <unordered_set>
#include <list>

namespace cheerp {

using namespace llvm;

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
				llvm::errs()<<"LOOP "<<s.end;
				break;
			case LOOP_END:
				llvm::errs()<<"LOOP_END "<<s.start;
				break;
			case BLOCK:
				llvm::errs()<<"BLOCK "<<s.end;
				break;
			case BLOCK_END:
				llvm::errs()<<"BLOCK_END "<<s.start;
				break;
		}
		llvm::errs()<<"\n";
	}
	if (BB)
	{
		llvm::errs()<<"BB "<<BB->getName()<<"\n";
		for (auto Succ: make_range(succ_begin(BB), succ_end(BB)))
		{
			llvm::errs() << "\t-> "<<Succ->getName()<<"\n";
		}
	}
	else
	{
		llvm::errs()<<"END\n";
	}
}
#endif

void CFGStackifier::Block::insertScope(CFGStackifier::Block::Scope s)
{
	switch (s.kind)
	{
		case BLOCK_END:
		case LOOP_END:
		{
			auto insertPoint = std::find_if(scopes.begin(), scopes.end(), [&s](Scope& ss) {
				if (ss.kind == LOOP || ss.kind == BLOCK)
					return true;
				if (ss.start < s.start)
					return true;
				return false;
			});
			scopes.insert(insertPoint, s);
			break;
		}
		case BLOCK:
		case LOOP:
		{
			auto insertPoint = std::find_if(scopes.begin(), scopes.end(), [&s](Scope& ss) {
				if (ss.kind == LOOP_END || ss.kind == BLOCK_END)
					return false;
				if (ss.end > s.end)
					return false;
				return true;
			});
			scopes.insert(insertPoint, s);
			break;
		}
	}
}

void CFGStackifier::orderBlocks(Function& F, LoopInfo& LI, DominatorTree& DT)
{

	// Prepare for a topological sort: Record the number of predecessors each
	// block has, ignoring loop backedges.
	std::unordered_map<BasicBlock*, size_t> NumPredsLeft;
	for (auto& BB: F)
	{
		size_t N = 0;
		Loop* L = LI.getLoopFor(&BB);
		for (pred_iterator pit = pred_begin(&BB), pet = pred_end(&BB); pit != pet; ++pit)
		{
			if (!L || L->getHeader() != &BB ||! L->contains(*pit))
				++N;
		}
		NumPredsLeft.emplace(&BB, N);
	}
	// Topological sort the CFG, with the additional constraint that between
	// a loop header and the last block in the loop, there can be
	// no blocks not dominated by the loop header.
	std::vector<BasicBlock*> Ready;
	struct Entry {
		Loop* L;
		size_t NumBlocksLeft;
		// List of blocks not dominated by Loop's header that are deferred until
		// after all of Loop's blocks have been seen.
		std::vector<BasicBlock*> Deferred;

		explicit Entry(Loop* L): L(L), NumBlocksLeft(L->getNumBlocks())
		{}
	};
	SmallVector<Entry, 4> Loops;
	Ready.push_back(&F.getEntryBlock());
	while(!Ready.empty())
	{
		BasicBlock* BB = Ready.back();
		Ready.pop_back();

		// If BB isn't dominated by the top active loop header, defer it until
		// that loop is done.
		if (!Loops.empty() && !DT.dominates(Loops.back().L->getHeader(), BB)) {
			Loops.back().Deferred.push_back(BB);
			continue;
		}

		Loop* L = LI.getLoopFor(BB);
		if (L)
		{
			// If BB is a loop header, add it to the active loop list. We can't put
			// any blocks that it doesn't dominate until we see the end of the loop.
			if (L->getHeader() == BB)
				Loops.push_back(Entry(L));

			// For each active loop the block is in, decrement the count. If MBB is
			// the last block in an active loop, take it off the list and pick up any
			// blocks deferred because the header didn't dominate them.
			for (Entry &E : Loops)
			{
				if (E.L->contains(BB) && --E.NumBlocksLeft == 0)
				{
					Ready.insert(Ready.end(), E.Deferred.begin(), E.Deferred.end());
				}
			}
			while (!Loops.empty() && Loops.back().NumBlocksLeft == 0)
				Loops.pop_back();
		}
		// The main topological sort logic.
		for (succ_iterator sit = succ_begin(BB), set = succ_end(BB); sit != set; ++sit)
		{
			BasicBlock* Succ = *sit;
			// Ignore backedges.
			Loop *SuccL = LI.getLoopFor(Succ);
			if (SuccL && SuccL->getHeader() == Succ && SuccL->contains(BB))
				continue;
			// Decrement the predecessor count. If it's now zero, it's ready.
			if (--NumPredsLeft[Succ] == 0)
				Ready.push_back(Succ);
		}
		// Otherwise, emit BB into the final order
		size_t id = BlockList.size();
		BlockIdMap.emplace(BB, id);
		BlockList.emplace_back(BB, id);
	}
	// Fake block to collect the end markers
	BlockList.emplace_back(nullptr, BlockList.size());
}

size_t CFGStackifier::findLoopEnd(Loop* L, size_t start)
{
	size_t N = L->getNumBlocks();
	size_t id = start;
	for (; id < BlockList.size() && N > 0; ++id)
	{
		if (L->contains(BlockList[id].getBB()))
			--N;
	}
	assert(N==0 && "No more blocks but loop is still open");
	return id;
}
void CFGStackifier::addLoopMarkers(Block& B, LoopInfo& LI)
{
	Loop *L = LI.getLoopFor(B.getBB());
	if (!L || L->getHeader() != B.getBB())
		return;
	size_t endId = findLoopEnd(L, B.getId());

	B.insertScope(Block::Scope{Block::LOOP, B.getId(), endId});
	BlockList[endId].insertScope(Block::Scope{Block::LOOP_END, B.getId(), endId});
}
void CFGStackifier::addBlockMarkers(Block& B, DominatorTree& DT)
{
	BasicBlock* BB = B.getBB();
	// First compute the nearest common dominator of all forward non-fallthrough
	// predecessors so that we minimize the time that the BLOCK is on the stack,
	// which reduces overall stack height.
	BasicBlock* Header = nullptr;
	for (pred_iterator pit = pred_begin(BB), pet = pred_end(BB); pit != pet; ++pit)
	{
		if (BlockIdMap.at(*pit) < B.getId())
			Header = Header ? DT.findNearestCommonDominator(Header, *pit) : *pit;
	}
	// No forward predecessors
	if (Header == nullptr)
		return;
	// Natural predecessor
	if (BlockList[B.getId()-1] == Header)
		return;
	size_t PredId = BlockIdMap.at(Header);
	assert(PredId < B.getId());

	// If the nearest common dominator is inside a more deeply nested context,
	// walk out to the nearest scope which isn't more deeply nested.
	for (auto it = BlockList.begin()+B.getId(), e = BlockList.begin()+PredId; it != e; --it)
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
	orderBlocks(const_cast<Function&>(F), LI, DT);

	for (Block& B: BlockList)
	{
		// The fake block is the last one
		if (B.getBB() == nullptr)
			break;
		addLoopMarkers(B, LI);
		addBlockMarkers(B, DT);
	}
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
	std::unordered_map<const Scope*, size_t> labels;
	size_t next_label = 1;
	for (const auto& B: BlockList)
	{
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
		}
		BasicBlock* BB = B.getBB();
		// Fake block is at the end
		if (!BB)
			break;
		ri.renderBlock(BB);

		auto compileBranch = [&](BasicBlock* From, BasicBlock* To, bool first, bool last, bool conditional, int id)
		{
			if (conditional)
			{
				if (last)
					ri.renderElseBlockBegin();
				else
					ri.renderIfBlockBegin(From, id, first);
			}
			ri.renderBlockPrologue(To, From);

			size_t fromId = BlockIdMap.at(From);
			size_t toId = BlockIdMap.at(To);
			// Natural control flow
			if (fromId == toId-1)
				return;

			size_t depth = 0;
			for (auto s=ScopeStack.rbegin(), se=ScopeStack.rend(); s!=se; ++s, ++depth)
			{
				if ((*s)->kind == Block::BLOCK && toId == (*s)->end)
				{
					ri.renderBreak(labels.at(*s));
					return;
				}
				else if ((*s)->kind == Block::LOOP && toId == (*s)->start)
				{
					ri.renderContinue(labels.at(*s));
					return;
				}
			}
			report_fatal_error("No scope for branching");
		};
		TerminatorInst* term = BB->getTerminator();
		bool conditional = true;
		int defaultId = -1;
		if (BranchInst* br = dyn_cast<BranchInst>(term))
		{
			conditional = br->isConditional();
			defaultId = conditional ? 1 : 0;
		}
		else if (SwitchInst* sw = dyn_cast<SwitchInst>(term))
		{
			defaultId = 0;
		}
		else if (isa<ReturnInst>(term) || isa<UnreachableInst>(term))
		{
			continue;
		}
		else
		{
			term->dump();
			report_fatal_error("not implemented yet");
		}
		bool first = true;
		int id = 0;
		BasicBlock* defaultBranch = nullptr;
		for (auto S = succ_begin(BB), SE = succ_end(BB); S != SE; ++S, ++id)
		{
			if (id==defaultId)
			{
				defaultBranch = *S;
				continue;
			}
			compileBranch(BB, *S, first, false, conditional, id);
			first = false;
		}
		assert(defaultBranch != nullptr);
		compileBranch(BB, defaultBranch, first, true, conditional, -1);
		if (conditional)
			ri.renderBlockEnd();
	}
}

}

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
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/Cheerp/Utility.h"

using namespace llvm;
using namespace cheerp;

class TokenListBuilder {
public:
	struct StackElem {
		enum Kind {
			BLOCK,
			DOM
		};
		union {
			const llvm::BasicBlock* BB;
			const llvm::DomTreeNode* DN;
		};
		Kind kind;
		StackElem(const llvm::BasicBlock* BB): BB(BB), kind(Kind::BLOCK) {}
		StackElem(const llvm::DomTreeNode* DN): DN(DN), kind(Kind::DOM) {}
	};
	TokenListBuilder(const Function &F,
		TokenList& Tokens,
		const LoopInfo& LI, const DominatorTree& DT,
		bool NestSwitches)
		: F(const_cast<Function&>(F)), Tokens(Tokens)
		, InsertPt(Tokens.begin())
		, LI(LI), DT(DT)
		, NestSwitches(NestSwitches)
	{
		build();
	}
private:
	struct Scope {
		enum Kind {
			Loop,
			If,
			Case,
			Direct,
		};
		Kind Kind;
		const DomTreeNode* Dom;
		TokenList::iterator EndPt;
		bool Nested;
	};

	bool enqueueSucc(const BasicBlock* CurBB, const BasicBlock* Succ);
	void processBlock(const BasicBlock* CurBB, bool Delayed);
	void processBlockTerminator(Token* BBT, const DomTreeNode* CurNode);
	void processLoopScopes(const BasicBlock* CurBB);
	void processBlockScopes(const std::vector<Token*>& Branches);
	void popScopes(const DomTreeNode* CurNode);
	TokenList::iterator findBlockBegin(TokenList::iterator Target,
		TokenList::iterator Candidate);
	void build();

	const Function& F;
	TokenList& Tokens;
	TokenList::iterator InsertPt;
	const LoopInfo& LI;
	const DominatorTree& DT;
	bool NestSwitches;

	DenseMap<const BasicBlock*, int> Visited;
	DenseMap<const DomTreeNode*, std::vector<const BasicBlock*>> Queues;
	DenseMap<const BasicBlock*, Token*> BlockTokenMap;
	DenseMap<const Loop*, int> LoopCounts;
	DenseMap<const BasicBlock*, Token*> LoopHeaders;
	DenseMap<const BasicBlock*, std::vector<Token*>> BlockScopes;
	std::vector<Scope> Scopes;
	std::vector<StackElem> VisitStack;
};

class TokenListVerifier {
public:
	TokenListVerifier(const TokenList& Tokens): Tokens(Tokens) {}
	bool verify();
private:
	const TokenList& Tokens;
};

bool TokenListVerifier::verify()
{
	std::vector<const Token*> ScopeStack;
	DenseSet<const Token*> ActiveScopes;
	for (const Token& T: Tokens)
	{
		switch (T.getKind())
		{
			case Token::TK_BasicBlock:
				break;
			case Token::TK_Loop:
			case Token::TK_Block:
			case Token::TK_If:
			case Token::TK_IfNot:
			case Token::TK_Switch:
				ScopeStack.push_back(&T);
				ActiveScopes.insert(&T);
				break;
			case Token::TK_Else:
			{
				if (ScopeStack.empty())
				{
					llvm::errs() << "Error: Scope stack empty but ELSE Token found\n";
					return false;
				}
				if (T.getMatch()->getKind() != Token::TK_End)
				{
					llvm::errs() << "Error: Match for ELSE Token is not a END Token\n";
					return false;
				}
				if (T.getMatch()->getMatch()->getKind() != Token::TK_If)
				{
					llvm::errs() << "Error: Match for END after ELSE Token is not a IF Token\n";
					return false;
				}
				const Token* Match = ScopeStack.back();
				if (Match->getMatch() != &T)
				{
					llvm::errs() << "Error: ELSE Token is not the match of the current Token in the stack\n";
					return false;
				}
				break;
			}
			case Token::TK_End:
			{
				if (ScopeStack.empty())
				{
					llvm::errs() << "Error: Scope stack empty but END Token found\n";
					return false;
				}
				const Token* Match = ScopeStack.back();
				if (Match != T.getMatch())
				{
					llvm::errs() << "Error: Top Token in the stack is not the match for current END Token:\n";
					llvm::errs() << "Current: ";T.dump();
					llvm::errs() << "Top: ";Match->dump();
					return false;
				}
				ScopeStack.pop_back();
				ActiveScopes.erase(Match);
				break;
			}
			case Token::TK_Branch:
			case Token::TK_BrIf:
			case Token::TK_BrIfNot:
			{
				const Token* Match = T.getMatch()->getKind() == Token::TK_End
					? T.getMatch()->getMatch()
					: T.getMatch();
				if (!ActiveScopes.count(Match))
				{
					auto TokenStr = T.getKind() == Token::TK_Branch ? "BRANCH" : "BR_IF";
					llvm::errs() << "Error: " << TokenStr << " Token is jumping to a non-active scope\n";
					return false;
				}
				break;
			}
			case Token::TK_Prologue:
			case Token::TK_Condition:
			case Token::TK_Case:
				break;
			case Token::TK_Invalid:
				llvm::errs()<<"Error: INVALID Token found\n";
				return false;
				break;
		}
	}
	return true;
}

static uint32_t blockLikeTokens(const CFGStackifier::Mode& mode)
{
	uint32_t BlockLikeTokens = Token::TK_Block;
	switch (mode)
	{
		case CFGStackifier::GenericJS:
			BlockLikeTokens |= Token::TK_If|Token::TK_IfNot|Token::TK_Switch|Token::TK_Loop;
			break;
		case CFGStackifier::AsmJS:
			// TODO: there should be also TK_If|TK_IfNot and TK_Loop here,
			// but there are 2 bugs in V8 that prevent it.
			BlockLikeTokens |= Token::TK_Switch;
			break;
		case CFGStackifier::Wasm:
			BlockLikeTokens |= Token::TK_If|Token::TK_IfNot;
			break;
	}
	return BlockLikeTokens;
}

static const BasicBlock* getUniqueForwardPredecessor(const BasicBlock* BB, const LoopInfo& LI)
{
	Loop* L = LI.isLoopHeader(const_cast<BasicBlock*>(BB)) ? LI.getLoopFor(BB) : nullptr;
	const BasicBlock* UniquePred = nullptr;
	for (auto Pred: make_range(pred_begin(BB), pred_end(BB)))
	{
		if (L && L->contains(Pred))
			continue;
		if (UniquePred && UniquePred != Pred)
			return nullptr;
		UniquePred = Pred;
	}
	return UniquePred;
}

static bool isBackedge(const BasicBlock* From, const BasicBlock* To, const LoopInfo& LI)
{
	auto CTo = const_cast<BasicBlock*>(To);
	return LI.isLoopHeader(CTo) && LI.getLoopFor(CTo)->contains(From);
}

static int getNumForwardPreds(const BasicBlock* BB, const LoopInfo& LI)
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

bool TokenListBuilder::enqueueSucc(const BasicBlock* CurBB, const BasicBlock* Succ)
{
	// Ignore backedges
	if (isBackedge(CurBB, Succ, LI))
		return false;
	// Check if it is ready to visit
	int SuccForwardPreds = getNumForwardPreds(Succ, LI);
	if (++Visited[Succ] != SuccForwardPreds)
		return false;
	// If Succ is a branch, add it to the visit stack
	if (getUniqueForwardPredecessor(Succ, LI))
	{
		VisitStack.push_back(Succ);
		return true;
	}
	// Otherwise, add it to to the delayed list
	const DomTreeNode* SuccDN = DT.getNode(const_cast<BasicBlock*>(Succ))->getIDom();
	Queues[SuccDN].insert(Queues[SuccDN].end(), Succ);
	return false;
}

// Iterate all successors and call the given closure. The default case is iterated
// last
template<typename F>
static void for_each_succ(const BasicBlock* BB, F f)
{
	const Instruction* Term = BB->getTerminator();
	size_t DefaultIdx = isa<SwitchInst>(Term) ? 0 : Term->getNumSuccessors()-1;
	DenseMap<const BasicBlock*, SmallVector<int, 2>> Destinations;
	for (size_t i = 0; i < Term->getNumSuccessors(); ++i)
	{
		Destinations[Term->getSuccessor(i)].push_back(i);
	}
	for (size_t i = 0; i < Term->getNumSuccessors(); ++i)
	{
		if (Term->getSuccessor(i) == Term->getSuccessor(DefaultIdx))
			continue;
		auto it = Destinations.find(Term->getSuccessor(i));
		if (it == Destinations.end())
			continue;
		f(it->first, it->second);
		Destinations.erase(it);
	}
	auto it = Destinations.find(Term->getSuccessor(DefaultIdx));
	assert(it != Destinations.end());

	//The default destination might be nullptr
	if (it->first)
		f(it->first, it->second);
}

void TokenListBuilder::processBlockTerminator(Token* BBT, const DomTreeNode* CurNode)
{
	assert(BBT->getKind() == Token::TK_BasicBlock);
	const Instruction* Term = BBT->getBB()->getTerminator();
	if (const BranchInst* BrInst = dyn_cast<BranchInst>(Term))
	{
		if (BrInst->isUnconditional())
		{
			Token* Prologue = Token::createPrologue(BBT->getBB(), 0);
			InsertPt = Tokens.insertAfter(InsertPt, Prologue);
			const DomTreeNode* Dom = DT.getNode(BrInst->getSuccessor(0));
			bool Nested = CurNode->getBlock() == getUniqueForwardPredecessor(Dom->getBlock(), LI);
			Scope DirectScope { Scope::Direct, Dom, Tokens.end(), Nested};
			Scopes.push_back(DirectScope);
		}
		else
		{
			Token* If = Token::createIf(BBT->getBB());
			auto IfPt = Tokens.insertAfter(InsertPt, If);
			Token* IfPrologue = Token::createPrologue(BBT->getBB(), 0);
			IfPt = Tokens.insertAfter(IfPt, IfPrologue);

			Token* Else = Token::createElse(If);
			auto ElsePt = Tokens.insertAfter(IfPt, Else);
			Token* ElsePrologue = Token::createPrologue(BBT->getBB(), 1);
			ElsePt = Tokens.insertAfter(ElsePt, ElsePrologue);

			Token* End = Token::createIfEnd(If, Else);
			auto EndPt = Tokens.insertAfter(ElsePt, End);

			const DomTreeNode* IfDom = DT.getNode(BrInst->getSuccessor(0));
			const DomTreeNode* ElseDom = DT.getNode(BrInst->getSuccessor(1));
			bool IfNested = CurNode->getBlock() == getUniqueForwardPredecessor(IfDom->getBlock(), LI);
			bool ElseNested = CurNode->getBlock() == getUniqueForwardPredecessor(ElseDom->getBlock(), LI);
			InsertPt = IfPt;
			Scope IfScope { Scope::If, IfDom, ElsePt, IfNested };
			Scope ElseScope { Scope::If, ElseDom, EndPt, ElseNested };
			Scopes.emplace_back(ElseScope);
			Scopes.emplace_back(IfScope);
		}
	}
	else if (isa<SwitchInst>(Term) && !NestSwitches)
	{
		const SwitchInst* SwInst = cast<SwitchInst>(Term);
		Token* Switch = Token::createSwitch(BBT->getBB());
		InsertPt = Tokens.insertAfter(InsertPt, Switch);
		Token* Prev = Switch;
		std::vector<Token*> Branches;
		for_each_succ(BBT->getBB(), [&](const BasicBlock* Succ, const SmallVectorImpl<int>& Indexes)
		{
			for (int idx: Indexes)
			{
				Token* Case = Token::createCase(BBT->getBB(), idx, Prev);
				Prev = Case;
				InsertPt = Tokens.insertAfter(InsertPt, Case);
			}
			Token* Br = Token::createBranch(nullptr);
			InsertPt = Tokens.insertAfter(InsertPt, Br);
			Branches.push_back(Br);
		});
		Token* End = Token::createSwitchEnd(Switch, Prev);
		InsertPt = Tokens.insertAfter(InsertPt, End);
		std::vector<Scope> SwitchScopes;
		Scopes.reserve(SwInst->getNumSuccessors());
		int i = 0;
		auto FirstPt = Tokens.end();
		for_each_succ(BBT->getBB(), [&](const BasicBlock* Succ, const SmallVectorImpl<int>& Ids)
		{
			processBlockScopes({Branches[i]});
			Token* Prologue = Token::createPrologue(BBT->getBB(), Ids.front());
			InsertPt = Tokens.insertAfter(InsertPt, Prologue);

			const DomTreeNode* Dom = DT.getNode(const_cast<BasicBlock*>(Succ));
			bool Nested = CurNode->getBlock() == getUniqueForwardPredecessor(Succ, LI);
			if (SwitchScopes.empty())
			{
				FirstPt = InsertPt;
			}
			else
			{
				SwitchScopes.back().EndPt = InsertPt;
			}
			Scope S { Scope::Case, Dom, Tokens.end(), Nested};
			SwitchScopes.push_back(S);
			i++;
		});
		InsertPt = FirstPt;
		Scopes.insert(Scopes.end(), SwitchScopes.rbegin(), SwitchScopes.rend());

	}
	else if (isa<SwitchInst>(Term) && NestSwitches)
	{
		const SwitchInst* SwInst = cast<SwitchInst>(Term);
		Token* Switch = Token::createSwitch(BBT->getBB());
		InsertPt = Tokens.insertAfter(InsertPt, Switch);
		Token* Prev = Switch;
		TokenList::iterator FirstPt = Tokens.end();
		std::vector<Scope> SwitchScopes;
		Scopes.reserve(SwInst->getNumSuccessors());
		for_each_succ(BBT->getBB(), [&](const BasicBlock* Succ, const SmallVectorImpl<int>& Indexes)
		{
			for (int idx: Indexes)
			{
				Token* Case = Token::createCase(BBT->getBB(), idx, Prev);
				Prev = Case;
				InsertPt = Tokens.insertAfter(InsertPt, Case);
			}
			Token* Prologue = Token::createPrologue(BBT->getBB(), Indexes.front());
			InsertPt = Tokens.insertAfter(InsertPt, Prologue);
			if (!SwitchScopes.empty())
			{
				SwitchScopes.back().EndPt = InsertPt;
			}
			else
			{
				FirstPt = InsertPt;
			}
			const DomTreeNode* Dom = DT.getNode(const_cast<BasicBlock*>(Succ));
			bool Nested = CurNode->getBlock() == getUniqueForwardPredecessor(Succ, LI);
			Scope S { Scope::Case, Dom, Tokens.end(), Nested};
			SwitchScopes.push_back(S);
		});
		Token* End = Token::createSwitchEnd(Switch, Prev);
		InsertPt = Tokens.insertAfter(InsertPt, End);
		SwitchScopes.back().EndPt = InsertPt; 
		Scopes.insert(Scopes.end(), SwitchScopes.rbegin(), SwitchScopes.rend());
		InsertPt = FirstPt;
	}
	else if (isa<ReturnInst>(BBT->getBB()->getTerminator()))
	{
		// Nothing to do
	}
	else if (isa<UnreachableInst>(BBT->getBB()->getTerminator()))
	{
		// Nothing to do
	}
	else
	{
#ifndef NDEBUG
		BBT->getBB()->getTerminator()->dump();
#endif
		report_fatal_error("Unsupported terminator");
	}
}
void TokenListBuilder::popScopes(const DomTreeNode* CurNode)
{
	// Check if we need to close some scopes
	while (!Scopes.empty())
	{
		auto& CurScope = Scopes.back();
		switch (CurScope.Kind)
		{
			case Scope::Case:
			case Scope::If:
			case Scope::Direct:
			{
				if (CurScope.Nested && DT.dominates(CurScope.Dom, CurNode))
					return;
				// If the scope is not nested, add the branch token
				if (!CurScope.Nested)
				{
					auto LoopHeaderIt = LoopHeaders.find(CurScope.Dom->getBlock());
					Token* Br = LoopHeaderIt == LoopHeaders.end()
						? Token::createBranch(nullptr)
						: Token::createBranch(LoopHeaderIt->getSecond());
					BlockScopes[CurScope.Dom->getBlock()].push_back(Br);
					InsertPt = Tokens.insertAfter(InsertPt, Br);
				}
				break;
			}
			case Scope::Loop:
			{
				const Loop* CurL = LI.getLoopFor(CurScope.Dom->getBlock());
				auto LoopIt = LoopCounts.find(CurL);
				assert(LoopIt != LoopCounts.end());
				int& Count = LoopIt->getSecond();
				if (Count > 0)
					return;
				break;
			}
		}
		if (CurScope.EndPt != Tokens.end())
			InsertPt = CurScope.EndPt;
		Scopes.pop_back();
	}
}
void TokenListBuilder::processLoopScopes(const BasicBlock* CurBB)
{
	const Loop* CurL = LI.getLoopFor(CurBB);
	if (!CurL)
		return;
	// Open a new loop
	if (CurL->getHeader() == CurBB)
	{
		Token* Loop = Token::createLoop();
		Token* End = Token::createLoopEnd(Loop);

		auto LoopPt = Tokens.insertAfter(InsertPt, Loop);
		auto EndPt = Tokens.insertAfter(LoopPt, End);
		InsertPt = LoopPt;

		Scope LoopScope { Scope::Loop, DT.getNode(const_cast<BasicBlock*>(CurBB)), EndPt, true };
		Scopes.push_back(LoopScope);

		LoopCounts.insert(std::make_pair(CurL, CurL->getNumBlocks()));

		LoopHeaders.insert(std::make_pair(CurBB, &*LoopPt));
	}
	// Decrement the loop count, and if necessary update outer loops too
	auto LoopCountIt = LoopCounts.find(CurL);
	assert(LoopCountIt != LoopCounts.end());
	LoopCountIt->getSecond()--;
	while(LoopCountIt->getSecond() == 0)
	{
		int N = LoopCountIt->getFirst()->getNumBlocks();
		LoopCountIt = LoopCounts.find(LoopCountIt->getFirst()->getParentLoop());
		if (LoopCountIt == LoopCounts.end())
			break;
		LoopCountIt->getSecond() -= N;
	}
}

void TokenListBuilder::processBlockScopes(const std::vector<Token*>& Branches)
{
	// We will insert all the ends at this point
	auto EndPt = InsertPt;
	assert(EndPt != Tokens.end());
	// We will resume the normal insertion after all the end blocks
	InsertPt++;
	for (Token* Branch: Branches)
	{
		assert(Branch->getKind() == Token::TK_Branch);

		Token* Block = Token::createBlock();
		Token* End = Token::createBlockEnd(Block);
		Branch->setMatch(End);

		auto TargetPt = Tokens.insertAfter(EndPt, End);
		auto BlockPt = findBlockBegin(TargetPt, Branch->getIter());
		Tokens.insertAfter(BlockPt, Block);
	}
	InsertPt--;
}

void TokenListBuilder::processBlock(const BasicBlock* CurBB, bool Delayed)
{
	const DomTreeNode* CurNode = DT.getNode(const_cast<BasicBlock*>(CurBB));

	// Check if we need to close some loop and/or if/else scopes
	popScopes(CurNode);

	// If CurBB is the target of one or more breaks, instantiate the blocks now
	auto BlockScopeIt = BlockScopes.find(CurBB);
	if (BlockScopeIt != BlockScopes.end())
	{
		const auto& Branches = BlockScopeIt->getSecond();
		processBlockScopes(Branches);
		BlockScopes.erase(CurBB);
	}

	// Update the loop information and start a new one if needed
	processLoopScopes(CurBB);

	// Create the token for this basic block and add it to the list
	Token* BBT = Token::createBasicBlock(CurBB);
	InsertPt = Tokens.insertAfter(InsertPt, BBT);
	BlockTokenMap.insert(std::make_pair(CurBB, BBT));

	// Process the successors of this block
	// First, add the dom node to the visit stack
	VisitStack.emplace_back(CurNode);
	// Then, create the successor tokens for the block (direct branch, an if/else/end chain,
	// or TODO a switch)
	processBlockTerminator(BBT, CurNode);
	// Enqueue successors on the visit stack
	// (in reverse order so they are popped in the correct order)
	const Instruction* Term = CurBB->getTerminator();
	int ie = 0;
	if (isa<SwitchInst>(Term))
	{
		enqueueSucc(CurBB, Term->getSuccessor(0));
		ie = 1;
	}
	for (int i = Term->getNumSuccessors()-1; i >= ie; --i)
	{
		enqueueSucc(CurBB, Term->getSuccessor(i));
	}
}

void TokenListBuilder::build()
{
	Visited[&F.getEntryBlock()] = 0;
	const DomTreeNode* EntryDom = DT.getNode(const_cast<BasicBlock*>(&F.getEntryBlock()));
	VisitStack.push_back(EntryDom);
	VisitStack.push_back(&F.getEntryBlock());
	while(!VisitStack.empty())
	{
		StackElem CurE = VisitStack.back();
		if (CurE.kind == StackElem::DOM) {
			// Handle the delayed blocks
			std::vector<const BasicBlock*>& Delayed = Queues[CurE.DN];
			if (Delayed.empty())
			{
				VisitStack.pop_back();
				continue;
			}
			const BasicBlock* DB = Delayed.back();
			Delayed.pop_back();
			processBlock(DB, true);
			continue;
		}
		VisitStack.pop_back();
		processBlock(CurE.BB, false);
	}
	popScopes(EntryDom);
	assert(Scopes.empty());
}

TokenList::iterator TokenListBuilder::findBlockBegin(TokenList::iterator Target,
	TokenList::iterator Candidate)
{
	// First, walk from the Candidate to the Target, and keep
	// track of the last closed scope we don't see opened
	Token* LastUnmatchedScope = nullptr;
	auto it = Candidate;
	for (; it != Target; ++it)
	{
		assert(it!=Tokens.end());
		switch (it->getKind())
		{
			case Token::TK_End:
				LastUnmatchedScope = it->getMatch();
				break;
			case Token::TK_Else:
				LastUnmatchedScope = it->getMatch()->getMatch();
				it = it->getMatch()->getIter();
				break;
			case Token::TK_If:
			case Token::TK_Loop:
			case Token::TK_Block:
			case Token::TK_Switch:
				while(it->getKind() != Token::TK_End)
					it = it->getMatch()->getIter();
				break;
			default:
				break;
		}
	}
	// If we have an unopened scope, move the candidate to it
	if (LastUnmatchedScope)
		Candidate = LastUnmatchedScope->getIter();
	// Return the node before the final candidate. We will insert after it
	return Candidate->getPrevNode()->getIter();
}

class TokenListOptimizer {
	TokenList& Tokens;
	const Registerize& R;
	const PointerAnalyzer& PA;
	CFGStackifier::Mode Mode;
public:
	TokenListOptimizer(TokenList& Tokens, const Registerize& R, const PointerAnalyzer& PA,
		CFGStackifier::Mode Mode)
		: Tokens(Tokens), R(R), PA(PA), Mode(Mode) {}
	void runAll();
	void removeRedundantBranches(const bool removeAlsoBlockAndEnd = false);
	void removeEmptyBasicBlocks();
	void removeEmptyPrologues();
	void removeRedundantLoops();
	void removeEmptyIfs();
	void createBrIfs();
	void createBrIfsFromIfs();
	void mergeBlocks();
	void removeUnnededNesting();
	void adjustLoopEnds();
	void adjustBranchTarget();
	void removeRedundantBlocks();
private:
	// Helper function for iterating on one or more kinds of tokens
	// The closure `f` must use the `erase` function defined below if it needs
	// to remove Tokens from the list
	template<uint32_t K, typename F>
	void for_each_kind(F f)
	{
		for_each_kind<K>(Tokens.begin(), Tokens.end(), f);
	}
	template<uint32_t K, typename F>
	void for_each_kind(TokenList::iterator begin, TokenList::iterator end, F f)
	{
		uint32_t depth = ItPts.size();
		ItPts.push_back(begin);
		RemovedItPts.push_back(false);

		TokenList::iterator& ItPt = ItPts[depth];
		int& RemovedItPt = RemovedItPts[depth];
		while(ItPt != end)
		{
			if ((ItPt->getKind() & K) == 0)
			{
				ItPt++;
				continue;
			}
			f(&*ItPt);
			if (RemovedItPt)
			{
				RemovedItPt = false;
			}
			else
			{
				ItPt++;
			}
		}

		ItPts.pop_back();
		RemovedItPts.pop_back();
	}
	// Global state used to track if the closure removed the current iteration
	// element
	std::deque<TokenList::iterator> ItPts;
	std::deque<int> RemovedItPts;
	// Helper function for removing Tokens from the list without invalidating
	// the iteration during `for_each_kind`
	void erase(Token* ToRemove)
	{
		for (uint32_t i=0; i<ItPts.size(); i++)
		{
			if (ToRemove->getIter() == ItPts[i])
			{
				RemovedItPts[i] = true;
				ItPts[i]++;
			}
		}
		Tokens.erase(ToRemove);
	}
	void passStart()
	{
#ifdef TOKEN_OPT_DUMP
		Tokens.CFGdump();
#endif
	}
};
void TokenListOptimizer::runAll()
{
	removeRedundantBranches(/*removeAlsoBlockAndEnd*/true);
	removeEmptyPrologues();
	removeEmptyBasicBlocks();
	removeRedundantLoops();
	removeEmptyIfs();
	mergeBlocks();
	removeUnnededNesting();
	adjustLoopEnds();
	removeRedundantBlocks();
	adjustBranchTarget();
	if (Mode == CFGStackifier::Wasm)
	{
		createBrIfs();
		adjustBranchTarget();
		createBrIfsFromIfs();
	}
	removeRedundantBranches(/*removeAlsoBlockAndEnd*/false);
}

static bool isNaturalFlow(TokenList::iterator From, TokenList::iterator To, const bool allowBranches)
{
	if (From == To)
		return true;
	for (auto it = ++From; it != To; it++)
	{
		switch (it->getKind())
		{
			case Token::TK_Else:
				it = it->getMatch()->getIter();
				if (it == To)
					return true;
				break;
			case Token::TK_End:
			case Token::TK_Loop:
			case Token::TK_Block:
				break;
			case Token::TK_Branch:
			{
				if (allowBranches)
				{
					it = it->getMatch()->getIter();
					if (it == To)
						return true;
					break;
				}
				[[clang::fallthrough]];
			}
			default:
				return false;
		}
	}
	return true;
}
void TokenListOptimizer::removeRedundantBranches(const bool removeAlsoBlockAndEnd)
{
	passStart();
	const uint32_t BlockLikeTokens = removeAlsoBlockAndEnd ? blockLikeTokens(Mode) : Token::TK_Block;
	for_each_kind<Token::TK_Branch>([&](Token* Branch)
	{
		Token* End = Branch->getMatch();
		if (End->getKind() == Token::TK_Loop)
			return;

		assert(End->getKind() == Token::TK_End);


		Token* Block = End->getMatch();
		if ((Block->getKind() & BlockLikeTokens) == 0)
		{
			return;
		}

		assert(Branch->getNextNode() != nullptr);
		if (isNaturalFlow(Branch->getIter(), End->getIter(), /*allowBranches*/true))
		{
			erase(Branch);

			if (removeAlsoBlockAndEnd)
			{
				erase(Block);
				erase(End);
			}
		}
	});
}

void TokenListOptimizer::removeRedundantLoops()
{
	passStart();
	for_each_kind<Token::TK_Loop>([&](Token* Loop)
	{
		DenseSet<Token*> ExtraLoops;
		auto ItPt = Loop->getIter();
		while((++ItPt)->getKind() == Token::TK_Loop)
		{
			ExtraLoops.insert(&*ItPt);
		}
		if (ExtraLoops.empty())
			return;
		TokenList::iterator SearchEndPt = Loop->getNextNode()->getMatch()->getIter();
		assert(SearchEndPt->getKind() == Token::TK_End);
		for_each_kind<Token::TK_Branch>(ItPt, SearchEndPt, [&](Token* Br)
		{
			if (ExtraLoops.count(Br->getMatch()))
			{
				Br->setMatch(Loop);
			}
		});
		for (Token* EL: ExtraLoops)
		{
			Token* End = EL->getMatch();
			erase(End);
			erase(EL);
		}
	});
}

static bool isEmptyPrologue(const BasicBlock* From, const BasicBlock* To,
	const Registerize& R, const PointerAnalyzer& PA)
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
void TokenListOptimizer::removeEmptyPrologues()
{
	passStart();
	for_each_kind<Token::TK_Prologue>([&](Token* Prologue)
	{
		if (isEmptyPrologue(Prologue->getBB(),
			Prologue->getBB()->getTerminator()->getSuccessor(Prologue->getId()), R, PA))
		{
			erase(Prologue);
		}
	});
}

void TokenListOptimizer::removeEmptyBasicBlocks()
{
	passStart();
	for_each_kind<Token::TK_BasicBlock>([&](Token* BBT)
	{
		if (isNumStatementsLessThan<1>(BBT->getBB(), PA, R)
			&& !isa<UnreachableInst>(BBT->getBB()->getTerminator()))
			erase(BBT);
	});
}

void TokenListOptimizer::removeEmptyIfs()
{
	passStart();
	for_each_kind<Token::TK_End | Token::TK_Else>([&](Token* T)
	{
		Token* Prev = T->getPrevNode();
		if (Prev->getKind() & (Token::TK_If | Token::TK_Else | Token::TK_IfNot)
			&& Prev->getMatch() == T)
		{
			if (Prev->getKind() & (Token::TK_If | Token::TK_IfNot))
			{
				Token* EmptyIf = Prev;
				if (T->getKind() == Token::TK_Else)
				{
					Token* Else = T;
					Token* End = Else->getMatch();
					Token* IfNot = Token::createIfNot(EmptyIf->getBB());
					IfNot->setMatch(End);
					End->setMatch(IfNot);
					Tokens.insertAfter(EmptyIf->getIter(), IfNot);
					erase(EmptyIf);
					erase(Else);
				}
				else
				{
					const Instruction* Term = EmptyIf->getBB()->getTerminator();
					assert(isa<BranchInst>(Term));
					const Value* Cond = cast<BranchInst>(Term)->getCondition();
					if (mayContainSideEffects(Cond, PA))
					{
						Token* CondT = Token::createCondition(EmptyIf->getBB());
						Tokens.insert(EmptyIf->getIter(), CondT);
					}
					erase(EmptyIf);
					erase(T);
				}
			}
			else
			{
				assert(Prev->getKind() == Token::TK_Else);
				Token* EmptyElse = Prev;
				Token* End = T;
				Token* If = End->getMatch();
				If->setMatch(End);
				erase(EmptyElse);
			}
		}
	});
}

void TokenListOptimizer::createBrIfs()
{
	passStart();
	for_each_kind<Token::TK_If | Token::TK_IfNot>([&](Token* T)
	{
		Token* End = T->getMatch();
		if (End->getKind() != Token::TK_End)
			return;
		Token* Branch = T->getNextNode();
		if (Branch->getKind() != Token::TK_Branch || Branch->getNextNode() != End)
			return;
		bool IsIfNot = T->getKind() == Token::TK_IfNot;
		Token* BrIf = IsIfNot
			? Token::createBrIfNot(T->getBB(), Branch->getMatch())
			: Token::createBrIf(T->getBB(), Branch->getMatch());
		Tokens.insert(T->getIter(), BrIf);
		erase(T);
		erase(Branch);
		erase(End);
	});
}

void TokenListOptimizer::createBrIfsFromIfs()
{
	passStart();
	for_each_kind<Token::TK_If | Token::TK_IfNot>([&](Token* T)
	{
		Token* End = T->getMatch();
		if (End->getKind() != Token::TK_End)
			return;
		Token* End2 = End->getNextNode();

		while (End2->getKind() == Token::TK_End && End2->getMatch()->getKind() == Token::TK_Loop)
			End2 = End2->getNextNode();

		if (End2->getKind() != Token::TK_End)
			return;

		if ((End2->getMatch()->getKind() & (Token::TK_Block | Token::TK_IfNot | Token::TK_If)) == 0)
			return;

		for_each_kind<Token::TK_Branch|Token::TK_BrIf|Token::TK_BrIfNot>(T->getIter(), End2->getIter(), [&](Token* Branch)
		{
			if (Branch->getMatch() == End)
				Branch->setMatch(End2);
		});

		bool IsIfNot = T->getKind() == Token::TK_IfNot;
		Token* BrIf = IsIfNot
			? Token::createBrIf(T->getBB(), End2)
			: Token::createBrIfNot(T->getBB(), End2);
		Tokens.insert(T->getIter(), BrIf);
		erase(T);
		erase(End);
	});
}

void TokenListOptimizer::mergeBlocks()
{
	passStart();
	for_each_kind<Token::TK_Branch>([&](Token* Br)
	{
		// Look for branches that target Block Tokens
		Token* End = Br->getMatch();
		if (End->getKind() != Token::TK_End)
			return;
		// If we have an outer Block Token that ends here, we can branch to that,
		// and remove the current one
		Token* NewEnd = End;
		while(NewEnd->getNextNode()->getKind() == Token::TK_End
			&& NewEnd->getNextNode()->getMatch()->getKind() == Token::TK_Block)
		{
			NewEnd = NewEnd->getNextNode();
		}
		if (NewEnd == End)
			return;
		Token* Begin = End->getMatch();
		Br->setMatch(NewEnd);
		erase(Begin);
		erase(End);
	});
}

static bool canFallThrough(Token* T)
{
	assert(T->getKind() & (Token::TK_If|Token::TK_Else));
	Token* End = T->getMatch();
	Token* Last = End->getPrevNode();
	// If the if is empty, it does not matter that it technically falls through
	if (Last == T)
		return false;
	switch(Last->getKind())
	{
		case Token::TK_BasicBlock:
			return Last->getBB()->getTerminator()->getNumSuccessors() != 0;
		case Token::TK_Branch:
			return false;
		case Token::TK_Prologue:
		case Token::TK_End:
		case Token::TK_Condition:
		case Token::TK_BrIf:
		case Token::TK_BrIfNot:
			return true;
		case Token::TK_If:
		case Token::TK_IfNot:
		case Token::TK_Else:
		case Token::TK_Switch:
		case Token::TK_Case:
		case Token::TK_Loop:
		case Token::TK_Block:
			report_fatal_error("Unexpected token");
		case Token::TK_Invalid:
			llvm_unreachable("Invalid token");
	}
}
void TokenListOptimizer::removeUnnededNesting()
{
	passStart();
	// Iterate the End Tokens, so we handle the innermost Ifs first
	for_each_kind<Token::TK_End>([&](Token* End)
	{
		Token* If = End->getMatch();
		if (If->getKind() != Token::TK_If)
			return;
		Token* Else = If->getMatch();
		if (Else->getKind() != Token::TK_Else)
			return;
		auto removeElse = [&]()
		{
			Token* NewEnd = Token::createIfEnd(If, nullptr);
			Tokens.insert(Else->getIter(), NewEnd);
			erase(Else);
			erase(End);
		};
		auto removeIf = [&]()
		{
			Tokens.moveAfter(End->getIter(), std::next(If->getIter()), Else->getIter());
			Token* IfNot = Token::createIfNot(If->getBB());
			IfNot->setMatch(End);
			End->setMatch(IfNot);
			Tokens.insert(Else->getIter(), IfNot);
			erase(If);
			erase(Else);
		};
		auto IsBrIf = [](Token* T)
		{
			Token* Match = T->getMatch();
			Token* Inner = T->getNextNode();
			return Inner->getKind() == Token::TK_Branch
				&& Inner->getNextNode() == Match;
		};
		// Candidates for removing
		bool ElseIsCandidate = !canFallThrough(If);
		bool IfIsCandidate = !canFallThrough(Else);
		if (IfIsCandidate && ElseIsCandidate)
		{
			// TODO: if both the If and the Else cannot fall through, decide which 
			// one is s better to remove. Right now, we only look if one of the two
			// is a BrIf candidate, and remove the other one
			if (IsBrIf(If))
				removeElse();
			else if(IsBrIf(Else))
				removeIf();
			else
				removeElse();
		}
		else if (IfIsCandidate)
		{
			removeIf();
		}
		else if (ElseIsCandidate)
		{
			removeElse();
		}
	});
}

void TokenListOptimizer::adjustLoopEnds()
{
	passStart();
	for_each_kind<Token::TK_Loop>([&](Token* Loop)
	{
		Token* End = Loop->getMatch();
		Token* Cur = End->getPrevNode();
		Token* LastDepth0 = nullptr;
		int Depth = 0;
		while (Cur != Loop)
		{
			if (Cur->getKind() & (Token::TK_If|Token::TK_IfNot|Token::TK_Loop|Token::TK_Block))
			{
				Depth--;
			}
			else if (Cur->getKind() == Token::TK_End)
			{
				if (Depth == 0)
					LastDepth0 = Cur;
				Depth++;
			}
			else if (Cur->getKind() & (Token::TK_Branch|Token::TK_BrIf|Token::TK_BrIfNot)
				&& Cur->getMatch() == Loop)
			{
				break;
			}
			Cur = Cur->getPrevNode();
		}
		if (LastDepth0 != nullptr && LastDepth0 != End->getPrevNode())
		{
			Tokens.moveAfter(LastDepth0->getIter(), End->getIter(), std::next(End->getIter()));
		}
	});
}

void TokenListOptimizer::adjustBranchTarget()
{
	passStart();
	// We can branch out of some tokens like they were Block tokens.
	// Which ones depends on the actual target
	const uint32_t BlockLikeTokens = blockLikeTokens(Mode);

	for_each_kind<Token::TK_Branch | Token::TK_BrIf | Token::TK_BrIfNot>([&](Token* BranchLike)
	{
		Token* CurrTarget = BranchLike->getMatch();

		Token* lastGood = nullptr;
		Token* Next = CurrTarget->getNextNode();

		while (Next->getKind() & (Token::TK_Branch | Token::TK_End))
		{
			if (Next->getKind() == Token::TK_Branch)
			{
				Next = Next->getMatch();
				lastGood = Next;
			}
			else if (Next->getMatch()->getKind() & BlockLikeTokens)
			{
				lastGood = Next;
				Next = Next->getNextNode();
			}
			else
			{
				break;
			}
		}

		if (lastGood)
			BranchLike->setMatch(lastGood);
	});
}

void TokenListOptimizer::removeRedundantBlocks()
{
	passStart();
	// We can branch out of some tokens like they were Block tokens.
	// Which ones depends on the actual target
	const uint32_t BlockLikeTokens = blockLikeTokens(Mode);

	for_each_kind<Token::TK_End>([&](Token* End)
	{
		Token* Block = End->getMatch();
		if (Block->getKind() != Token::TK_Block)
			return;
		Token* Inner = Block->getNextNode();
		assert(Inner->getKind() != Token::TK_End);
		if (!(Inner->getKind() & BlockLikeTokens))
			return;
		Token* InnerEnd = Inner->getMatch();
		while(InnerEnd->getKind() != Token::TK_End)
			InnerEnd = InnerEnd->getMatch();
		if (InnerEnd->getNextNode() != End)
			return;

		for_each_kind<Token::TK_Branch|Token::TK_BrIf|Token::TK_BrIfNot>(Block->getIter(), End->getIter(), [&](Token* Branch)
		{
			if (Branch->getMatch() == End)
				Branch->setMatch(InnerEnd);
		});
		erase(Block);
		erase(End);
	});
}

CFGStackifier::CFGStackifier(const llvm::Function &F, const llvm::LoopInfo& LI,
	const llvm::DominatorTree& DT, const Registerize& R, const PointerAnalyzer& PA,
	Mode M)
{
	TokenListBuilder Builder(F, Tokens, LI, DT, M != Mode::Wasm);
#ifndef NDEBUG
	{
		TokenListVerifier Verifier(Tokens);
		assert(Verifier.verify());
	}
#endif
#ifdef TOKEN_OPT_DUMP
	llvm::errs() << F.getName() << "\n";
#endif
	TokenListOptimizer Opt(Tokens, R, PA, M);
	Opt.runAll();
#ifndef NDEBUG
	{
		TokenListVerifier Verifier(Tokens);
		assert(Verifier.verify());
	}
#endif
}

std::vector<const llvm::BasicBlock*> CFGStackifier::selectBasicBlocksWithPossibleIncomingResult() const
{
	std::vector<const llvm::BasicBlock*> blocksWithPossibleResult;

	std::unordered_map<const BasicBlock*, std::unordered_set<const Token*>> BBtoRenderedPrologue;

	for (const auto& token : Tokens)
	{
		if (token.getKind() != Token::TK_Prologue)
			continue;

		const llvm::BasicBlock* fromBB = token.getBB();
		const llvm::BasicBlock* toBB = fromBB->getTerminator()->getSuccessor(token.getId());

		BBtoRenderedPrologue[toBB].insert(&token);
	}

	for (const auto& token : Tokens)
	{
		if (token.getKind() != Token::TK_BasicBlock)
			continue;

		const llvm::BasicBlock* BB = token.getBB();

		const uint32_t numIncomingPrologues = BBtoRenderedPrologue[BB].size();

		//No prologues -> nothing to optimize
		if (numIncomingPrologues == 0)
			continue;

		std::unordered_set<const llvm::BasicBlock*> pred;

		for (const llvm::BasicBlock* bb : predecessors(BB))
			pred.insert(bb);

		//We bail out if the number of prologues is different that the number of predecessors for 2 reasons
		//1. we lost track of the information about where the missing prologues would have been located
		//2. if the whole prologue is a no-op, it's probably optimal not to render it
		//TODO: find whether this could be relaxed
		if (numIncomingPrologues != pred.size())
			continue;

		std::unordered_map<const Token*, uint32_t> numberOfVisits;

		for (const Token* T : BBtoRenderedPrologue.at(BB))
		{
			for (auto it = T->getIter(); it != token.getIter();)
			{
				if (it == T->getIter())
				{
					assert(it->getKind() == Token::TK_Prologue);
					numberOfVisits[&*it]++;
					it++;
					continue;
				}
				if (it->getKind() == Token::TK_Block || it->getKind() == Token::TK_Loop)
				{
					//Currently any Block or Loop cause to bail out, this is generalizable by adding a token kind "prologue assigment"
					break;
				}

				//isNaturalFlow like iteration
				switch (it->getKind())
				{
					case Token::TK_Else:
					case Token::TK_Branch:
						it = it->getMatch()->getIter();
						break;
					case Token::TK_End:
						it++;
						break;
					default:
						llvm_unreachable("Unexpected token encountered");
						break;
				}

				numberOfVisits[&*it]++;
			}
			//Increment at the exit of the loop on the destination
		}

		if (numberOfVisits[&token] != numIncomingPrologues)
			continue;

		blocksWithPossibleResult.push_back(BB);
	}

	return blocksWithPossibleResult;
}

static Token::TokenResultType getTokenResultType(const llvm::Instruction* I, const Registerize& registerize)
{
	switch (registerize.getRegKindFromType(I->getType(), /*asmjs*/true))
	{
		case Registerize::INTEGER:
			return Token::TokenResultType::I32;
		case Registerize::INTEGER64:
			return Token::TokenResultType::I64;
		case Registerize::FLOAT:
			return Token::TokenResultType::F32;
		case Registerize::DOUBLE:
			return Token::TokenResultType::F64;
		default:
			break;
	}
	llvm_unreachable("Unsupported getTokenResultType");
}


void CFGStackifier::addResultToTokens(const std::map<const llvm::BasicBlock*, const llvm::PHINode*>& specialPHINodes, const Registerize& registerize)
{
	//Every block / if / loop gets initialized with NONE as result type
	for (auto& token : Tokens)
	{
		if (token.needsResultType())
		{
			token.setResultType(Token::TokenResultType::NONE);
		}
	}

	//Initialize map to speed up processing
	llvm::DenseMap<const BasicBlock*, Token*> mapBBtoToken;
	for (auto& token : Tokens)
	{
		if (token.getKind() == Token::TK_BasicBlock)
		{
			mapBBtoToken[token.getBB()] = &token;
		}
	}

	for (auto& token : Tokens)
	{
		if (token.getKind() != Token::TK_Prologue)
			continue;

		const BasicBlock* fromBB = token.getBB();
		const BasicBlock* toBB = fromBB->getTerminator()->getSuccessor(token.getId());

		auto mapIterator = specialPHINodes.find(toBB);
		if (mapIterator == specialPHINodes.end())
			continue;

		const PHINode* phi = mapIterator->second;
		assert(phi->getParent() == toBB);

		//TK_Prologue keeps note of which phi it will have to skip
		token.setPhiHandledAsResult(phi);

		const auto tokenResultType = getTokenResultType(phi, registerize);

		auto tokenIterator = token.getIter();
		const auto tokenDestinationIterator = mapBBtoToken[toBB]->getIter();

		//We start in a prologue, so we can directly go to the following
		assert(tokenIterator->getKind() == Token::TK_Prologue);
		tokenIterator++;

		for (; tokenIterator != tokenDestinationIterator; )
		{
			//isNaturalFlow like iteration
			switch (tokenIterator->getKind())
			{
				case Token::TK_Else:
				case Token::TK_Branch:
					tokenIterator = tokenIterator->getMatch()->getIter();
					break;
				case Token::TK_End:
					tokenIterator->getMatch()->setResultType(tokenResultType);
					tokenIterator++;
					break;
				default:
					llvm_unreachable("Unexpected token kind");
			}
		}

		//We end up in a BasicBlock
		assert(tokenIterator->getKind() == Token::TK_BasicBlock);

		//TK_BasicBlock keeps note of which phi it will have to assign
		tokenIterator->setPhiHandledAsResult(phi);
	}

	//Here we handle a stuble corner case: in the case of multiple consecutive token End followed by possibly a token Else,
	//where the relevant matching blocks should all have the same result of the last End/Else

	//The idea is this:
	//We walk backwards.
	//Whenever we encounter something not End not Else we set previousWasEnd to false;
	//Whenever we encounter a End or a Else, AND previousWasEnd is false, we switch previousWasEnd and set lastEndType to the right type
	//Whenever we encounter a End or a Else, AND previousWasEnd is true, we set its result type to match the lastEndType
	auto iterator = Tokens.rbegin();
	const auto rendIterator = Tokens.rend();

	bool previousWasEnd = false;
	Token::TokenResultType lastEndType;

	while (iterator != rendIterator)
	{
		if ((iterator->getKind() == Token::TK_End) || (iterator->getKind() == Token::TK_Else))	// previous ?, current !END
		{
			Token* matched = iterator->getMatch();

			if (matched->getKind() == Token::TK_Switch)
			{
				//In Wasm TK_Switch is followed by some TK_Case and then a TK_End that would not be rendered, so it's not relevant the eventual result
				iterator++;
				continue;
			}

			if (matched->getKind() == Token::TK_End)
				matched = matched->getMatch();

			if (previousWasEnd)	// -> previous END, current END
			{
				matched->setResultType(lastEndType);
			}
			else	// -> previous ! END, current END
			{
				lastEndType = matched->getResultType();
				previousWasEnd = true;
			}
		}
		else
		{
			previousWasEnd = false;
		}

		//We are going to the previous token, since the iterator is a reverse one
		iterator++;
	}
}

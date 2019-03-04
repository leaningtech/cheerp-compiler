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

#include "TokenList.h"
#include "CFGStackifier.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Cheerp/Writer.h"

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
		const LoopInfo& LI, const DominatorTree& DT)
		: F(const_cast<Function&>(F)), Tokens(Tokens)
		, InsertPt(Tokens.begin())
		, NextId(0)
		, LI(LI), DT(DT)
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
	int NextId;
	const LoopInfo& LI;
	const DominatorTree& DT;

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
			{
				const Token* Match = T.getMatch()->getKind() == Token::TK_End
					? T.getMatch()->getMatch()
					: T.getMatch();
				if (!ActiveScopes.count(Match))
				{
					llvm::errs() << "Error: BRANCH Token is jumping to a non-active scope\n";
					return false;
				}
				break;
			}
			case Token::TK_Prologue:
				break;
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
	const TerminatorInst* Term = BB->getTerminator();
	size_t DefaultIdx = isa<SwitchInst>(Term) ? 0 : Term->getNumSuccessors()-1;
	DenseMap<const BasicBlock*, SmallVector<int, 2>> Destinations;
	for (size_t i = 0; i < Term->getNumSuccessors(); ++i)
	{
		Destinations[Term->getSuccessor(i)].push_back(i);
	}
	for (size_t i = 0; i < Term->getNumSuccessors(); ++i)
	{
		if (i == DefaultIdx)
			continue;
		auto it = Destinations.find(Term->getSuccessor(i));
		if (it == Destinations.end())
			continue;
		f(it->first, it->second);
		Destinations.erase(it);
	}
	auto it = Destinations.find(Term->getSuccessor(DefaultIdx));
	assert(it != Destinations.end());
	f(it->first, it->second);
}

void TokenListBuilder::processBlockTerminator(Token* BBT, const DomTreeNode* CurNode)
{
	assert(BBT->getKind() == Token::TK_BasicBlock);
	if (const BranchInst* BrInst = dyn_cast<BranchInst>(BBT->getBB()->getTerminator()))
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
	else if (const SwitchInst* SwInst = dyn_cast<SwitchInst>(BBT->getBB()->getTerminator()))
	{
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
		BBT->getBB()->getTerminator()->dump();
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

		LoopHeaders.insert(std::make_pair(CurBB, LoopPt));
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
		auto BlockPt = findBlockBegin(TargetPt, Branch);
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
	Token* BBT = Token::createBasicBlock(CurBB, NextId++);
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
	const TerminatorInst* Term = CurBB->getTerminator();
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
				it = it->getMatch();
				break;
			case Token::TK_If:
				it = it->getMatch()->getMatch();
				break;
			case Token::TK_Loop:
			case Token::TK_Block:
			case Token::TK_Switch:
				it = it->getMatch();
				break;
			case Token::TK_Case:
				it = it->getMatch();
				it--;
				break;
			default:
				break;
		}
	}
	// If we have an unopened scope, move the candidate to it
	if (LastUnmatchedScope)
		Candidate = LastUnmatchedScope;
	// Return the node before the final candidate. We will insert after it
	return Candidate->getPrevNode();
}

class TokenListOptimizer {
	TokenList& Tokens;
	const Registerize& R;
	const PointerAnalyzer& PA;
public:
	TokenListOptimizer(TokenList& Tokens, const Registerize& R, const PointerAnalyzer& PA)
		: Tokens(Tokens), R(R), PA(PA) {}
	void runAll();
	void removeRedundantBlocks();
	void removeEmptyBasicBlocks();
	void removeEmptyPrologues();
	void removeRedundantLoops();
	void removeEmptyIfs();
	void mergeBlocks();
	void removeUnnededNesting();
private:
	// Helper function for iterating on one or more kinds of tokens
	// The second parameter passed to `f` is a reference to the next iterator to
	// process. Implementations of `f` may to mutate it to point to the next
	// valid node to iterate on.
	template<uint32_t K, typename F>
	void for_each_kind(F f)
	{
		for_each_kind<K>(Tokens.begin(), Tokens.end(), f);
	}
	template<uint32_t K, typename F>
	void for_each_kind(TokenList::iterator begin, TokenList::iterator end, F f)
	{
		auto ItPt = begin;
		while(ItPt != end)
		{
			if ((ItPt->getKind() & K) == 0)
			{
				ItPt++;
				continue;
			}
			auto NextPt = std::next(ItPt);
			f(ItPt, NextPt);
			ItPt = NextPt;
		}
	}
};
void TokenListOptimizer::runAll()
{
	removeRedundantBlocks();
	removeEmptyPrologues();
	removeEmptyBasicBlocks();
	removeRedundantLoops();
	removeEmptyIfs();
	mergeBlocks();
	removeUnnededNesting();
}

static bool isNaturalFlow(TokenList::iterator From, TokenList::iterator To)
{
	if (From == To)
		return true;
	for (auto it = ++From; it != To; it++)
	{
		switch (it->getKind())
		{
			case Token::TK_Else:
				it = it->getMatch();
				break;
			case Token::TK_End:
			case Token::TK_Loop:
			case Token::TK_Block:
				break;
			default:
				return false;
		}
	}
	return true;
}
void TokenListOptimizer::removeRedundantBlocks()
{
	for_each_kind<Token::TK_Branch>([&](TokenList::iterator Branch, TokenList::iterator& Next)
	{
		TokenList::iterator End = Branch->getMatch();
		if (End->getKind() == Token::TK_Loop)
			return;
		assert(End->getKind() == Token::TK_End);
		TokenList::iterator Block = End->getMatch();
		assert(Block->getKind() == Token::TK_Block);
		assert(Branch->getNextNode() != nullptr);
		if (isNaturalFlow(Branch, End))
		{
			if (Next == End)
				Next = std::next(End);
			Tokens.erase(Branch);
			Tokens.erase(Block);
			Tokens.erase(End);
		}
	});
}

void TokenListOptimizer::removeRedundantLoops()
{
	for_each_kind<Token::TK_Loop>([&](TokenList::iterator Loop, TokenList::iterator& Next)
	{
		DenseSet<Token*> ExtraLoops;
		auto ItPt = Loop;
		while((++ItPt)->getKind() == Token::TK_Loop)
		{
			ExtraLoops.insert(ItPt);
		}
		if (ExtraLoops.empty())
			return;
		TokenList::iterator SearchEndPt = Loop->getNextNode()->getMatch();
		assert(SearchEndPt->getKind() == Token::TK_End);
		for_each_kind<Token::TK_Branch>(ItPt, SearchEndPt, [&](Token* Br, Token*)
		{
			if (ExtraLoops.count(Br->getMatch()))
			{
				Br->setMatch(Loop);
			}
		});
		for (Token* EL: ExtraLoops)
		{
			TokenList::iterator End = EL->getMatch();
			if (Next == End)
				Next = std::next(End);
			Tokens.erase(End);
			Tokens.erase(EL);
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
	for_each_kind<Token::TK_Prologue>([&](Token* Prologue, Token*)
	{
		if (isEmptyPrologue(Prologue->getBB(),
			Prologue->getBB()->getTerminator()->getSuccessor(Prologue->getId()), R, PA))
		{
			Tokens.erase(Prologue);
		}
	});
}

void TokenListOptimizer::removeEmptyBasicBlocks()
{
	for_each_kind<Token::TK_BasicBlock>([&](Token* BBT, Token*)
	{
		// If there are only PHIs and a branch or switch as terminator, the block
		// is empty
		BasicBlock::const_iterator It = BBT->getBB()->begin();
		while(const PHINode* PHI = dyn_cast<PHINode>(It))
		{
			// Delayed PHIs are rendered in their parent block
			if (CheerpWriter::canDelayPHI(PHI, PA, R))
				return;
			It++;
		}
		if (isa<BranchInst>(It) || isa<SwitchInst>(It))
		{
			Tokens.erase(BBT);
		}
	});
}

void TokenListOptimizer::removeEmptyIfs()
{
	for_each_kind<Token::TK_If | Token::TK_Else>([&](Token* T, TokenList::iterator& Next)
	{
		if (T->getMatch() == T->getNextNode())
		{
			if (T->getKind() == Token::TK_If)
			{
				Token* EmptyIf = T;
				Token* Else = EmptyIf->getMatch();
				Token* End = Else->getMatch();
				Token* IfNot = Token::createIfNot(EmptyIf->getBB());
				IfNot->setMatch(End);
				End->setMatch(IfNot);
				Tokens.insertAfter(EmptyIf, IfNot);
				Next = std::next(IfNot->getIter());
				Tokens.erase(EmptyIf);
				Tokens.erase(Else);
			}
			else
			{
				Token* EmptyElse = T;
				Token* End = EmptyElse->getMatch();
				Token* If = End->getMatch();
				If->setMatch(End);
				Tokens.erase(EmptyElse);
			}
		}
	});
}

void TokenListOptimizer::mergeBlocks()
{
	for_each_kind<Token::TK_Branch>([&](Token* Br, TokenList::iterator& Next)
	{
		// Look for branches that target Block Tokens
		TokenList::iterator End = Br->getMatch();
		if (End->getKind() != Token::TK_End)
			return;
		// If we have an outer Block Token that ends here, we can branch to that,
		// and remove the current one
		TokenList::iterator NewEnd = End;
		while(std::next(NewEnd)->getKind() == Token::TK_End
			&& std::next(NewEnd)->getMatch()->getKind() == Token::TK_Block)
		{
			NewEnd++;
		}
		if (NewEnd == End)
			return;
		Token* Begin = End->getMatch();
		Br->setMatch(NewEnd);
		if (Next == End)
			Next = std::next(End);
		Tokens.erase(Begin);
		Tokens.erase(End);
	});
}

static bool isReachable(TokenList::iterator FromIt, TokenList::iterator ToIt)
{
	for (auto It = FromIt; It != ToIt; ++It)
	{
		switch(It->getKind())
		{
			case Token::TK_BasicBlock:
				if (It->getBB()->getTerminator()->getNumSuccessors() == 0)
					return false;
				break;
			case Token::TK_Branch:
				return false;
				break;
			case Token::TK_Prologue:
			case Token::TK_Block:
			case Token::TK_End:
				break;
			case Token::TK_If:
			case Token::TK_IfNot:
			case Token::TK_Else:
			case Token::TK_Switch:
			case Token::TK_Case:
			case Token::TK_Loop:
				return true;
			case Token::TK_Invalid:
				llvm_unreachable("Invalid token");
		}
	}
	return true;
}
void TokenListOptimizer::removeUnnededNesting()
{
	for_each_kind<Token::TK_If>([&](TokenList::iterator If, TokenList::iterator& Next)
	{
		if (If->getMatch()->getKind() != Token::TK_Else)
			return;
		TokenList::iterator Else = If->getMatch();
		TokenList::iterator End = Else->getMatch();
		if (!isReachable(std::next(If), std::next(End)))
		{
			Token* NewEnd = Token::createIfEnd(If, nullptr);
			Tokens.insert(Else, NewEnd);
			Tokens.erase(Else);
			Tokens.erase(End);
		}
		else if (!isReachable(std::next(Else), std::next(End)))
		{
			Tokens.moveAfter(End, std::next(If), Else);
			Token* IfNot = Token::createIfNot(If->getBB());
			IfNot->setMatch(End);
			End->setMatch(IfNot);
			Tokens.insert(Else, IfNot);
			Tokens.erase(If);
			Tokens.erase(Else);
			Next = std::next(IfNot->getIter());
		}
	});
}

class TokenListRenderer {
	const TokenList& Tokens;
	RenderInterface& ri;
	DenseSet<const Token*> LabeledTokens;
public:
	TokenListRenderer(const TokenList& Tokens, RenderInterface& ri)
		: Tokens(Tokens), ri(ri)
	{
		setLabels();
		render();
	}
private:
	void setLabels();
	void render();
};

void TokenListRenderer::setLabels()
{
	std::vector<const Token*> ScopeStack;
	for (const Token& T: Tokens)
	{
		switch (T.getKind())
		{
			case Token::TK_BasicBlock:
			case Token::TK_Case:
			case Token::TK_If:
			case Token::TK_IfNot:
			case Token::TK_Else:
			case Token::TK_Prologue:
				break;
			case Token::TK_Loop:
			case Token::TK_Block:
			case Token::TK_Switch:
				ScopeStack.push_back(&T);
				break;
			case Token::TK_Branch:
			{
				const Token* Target = T.getMatch();
				if (Target->getKind() == Token::TK_End)
					Target = Target->getMatch();
				assert(!ScopeStack.empty());
				if (Target->getKind() == Token::TK_Block || Target != ScopeStack.back())
					LabeledTokens.insert(Target);
				break;
			}
			case Token::TK_End:
				if (T.getMatch()->getKind() == Token::TK_Block
					|| T.getMatch()->getKind() == Token::TK_Loop
					|| T.getMatch()->getKind() == Token::TK_Switch)
				{
					ScopeStack.pop_back();
				}
				break;
			case Token::TK_Invalid:
				report_fatal_error("Invalid token found");
				break;
		}
	}
}

void TokenListRenderer::render()
{
	DenseMap<const Token*, int> Labels;
	int NextLabel = 1;
	for (auto it = Tokens.begin(), ie = Tokens.end(); it != ie; ++it)
	{
		const Token& T = *it;
		switch (T.getKind())
		{
			case Token::TK_BasicBlock:
				ri.renderBlock(T.getBB());
				break;
			case Token::TK_Loop:
			{
				if (LabeledTokens.count(&T))
				{
					Labels.insert(std::make_pair(&T, NextLabel));
					ri.renderLoopBlockBegin(NextLabel++);
				}
				else
					ri.renderLoopBlockBegin(0);
				break;
			}
			case Token::TK_Block:
			{
				if (LabeledTokens.count(&T))
				{
					Labels.insert(std::make_pair(&T, NextLabel));
					ri.renderBlockBegin(NextLabel++);
				}
				else
					ri.renderBlockBegin(0);
				break;
			}
			case Token::TK_If:
				ri.renderIfBlockBegin(T.getBB(), 0, true);
				break;
			case Token::TK_IfNot:
				ri.renderIfBlockBegin(T.getBB(), std::vector<int>{0}, true);
				break;
			case Token::TK_Else:
				ri.renderElseBlockBegin();
				break;
			case Token::TK_Branch:
				if (T.getMatch()->getKind() == Token::TK_Loop)
				{
					auto LabelIt = Labels.find(T.getMatch());
					if (LabelIt == Labels.end())
						ri.renderContinue();
					else
						ri.renderContinue(LabelIt->getSecond());
				}
				else
				{
					assert(T.getMatch()->getKind() == Token::TK_End);
					const Token* Block = T.getMatch()->getMatch();
					assert(Block->getKind() == Token::TK_Block);
					auto LabelIt = Labels.find(Block);
					if (LabelIt == Labels.end())
						ri.renderBreak();
					else
						ri.renderBreak(LabelIt->getSecond());
				}
				break;
			case Token::TK_End:
				if (Labels.count(T.getMatch()))
					NextLabel--;
				if(T.getMatch()->getKind() == Token::TK_Loop)
				{
					ri.renderLoopBlockEnd();
				}
				else if(T.getMatch()->getKind() == Token::TK_Switch)
				{
					report_fatal_error("The end of a switch token should be handled with the switch itself");
				}
				else
				{
					ri.renderBlockEnd();
				}
				break;
			case Token::TK_Prologue:
				ri.renderBlockPrologue(T.getBB()->getTerminator()->getSuccessor(T.getId()), T.getBB());
				break;
			case Token::TK_Switch:
			{
				std::vector<std::pair<int, int>> Cases;
				const SwitchInst* si = cast<SwitchInst>(T.getBB()->getTerminator());
				it++;
				while(it->getKind() != Token::TK_End)
				{
					assert(it->getKind()==Token::TK_Case);
					int label = -1;
					std::vector<int> ids;
					while(it->getKind() == Token::TK_Case)
					{
						ids.push_back(it->getId());
						it++;
					}
					assert(it->getKind() == Token::TK_Branch);
					if (it->getMatch()->getKind() == Token::TK_Loop)
					{
						auto LabelIt = Labels.find(it->getMatch());
						assert(LabelIt != Labels.end());
						label = LabelIt->getSecond();
					}
					else
					{
						assert(it->getMatch()->getKind() == Token::TK_End);
						const Token* Block = it->getMatch()->getMatch();
						assert(Block->getKind() == Token::TK_Block);
						auto LabelIt = Labels.find(Block);
						assert(LabelIt != Labels.end());
						label = LabelIt->getSecond();
					}
					for (int id: ids)
						Cases.push_back(std::make_pair(id, label));
					it++;
				}
				ri.renderBrTable(si, Cases);
				break;
			}
			case Token::TK_Case:
				report_fatal_error("Case token found outside of switch block");
				break;
			case Token::TK_Invalid:
				report_fatal_error("Invalid token found");
				break;
		}
	}
}

CFGStackifier::CFGStackifier(const llvm::Function &F, const llvm::LoopInfo& LI,
	const llvm::DominatorTree& DT, const Registerize& R, const PointerAnalyzer& PA)
{
	TokenListBuilder Builder(F, Tokens, LI, DT);
#ifndef NDEBUG
	{
		TokenListVerifier Verifier(Tokens);
		assert(Verifier.verify());
	}
#endif
	TokenListOptimizer Opt(Tokens, R, PA);
	Opt.runAll();
#ifndef NDEBUG
	{
		TokenListVerifier Verifier(Tokens);
		assert(Verifier.verify());
	}
#endif
}
void CFGStackifier::render(RenderInterface& ri)
{
	TokenListRenderer Renderer(Tokens, ri);
}

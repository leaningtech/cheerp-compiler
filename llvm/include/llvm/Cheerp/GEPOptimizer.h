//===-- Cheerp/GEPOptimizer.h - Cheerp utility code -----------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_GEP_OPTIMIZER_H
#define _CHEERP_GEP_OPTIMIZER_H

#include "llvm/Pass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Cheerp/CommandLine.h"

#include <unordered_map>

namespace llvm
{

static BasicBlock* immediateDominator(BasicBlock* BB, const DominatorTree* DT)
{
	auto X = DT->getNode(BB)->getIDom();
	if (!X)
		return NULL;
	else
		return X->getBlock();
}

/**
 * This class represent a block collection, with the property that if a node is in, all nodes postdominated by him are in too
 * Since in the dominator tree, this represent a (sub)-tree, and this is a collection of trees, this gets called a forest
 */
class BasicBlockForest
{
	typedef std::set<BasicBlock*> BlockSet;
public:
	BasicBlockForest() : DT(NULL), roots()
	{
	}
	BasicBlockForest(const BasicBlockForest& other) : DT(other.DT), roots(other.roots)
	{
	}
	static BasicBlockForest expandToTheWholeFunction(const BasicBlockForest& other)
	{
		BasicBlockForest ret(other);
		if (ret.getRoots().empty())
			return ret;
		BasicBlock* BB = *(ret.getRoots().begin());
		ret.clear();
		ret.insert(&BB->getParent()->getEntryBlock());
		return ret;
	}
	bool properlyDominated(BasicBlock* block) const
	{
		if (roots.count(block))
			return false;
		if (count(block))
			return true;
		return false;
	}
	uint32_t count(BasicBlock* block) const
	{
		assert(DT);
		assert(block);

		//TODO: Now it is O(number of roots), it could become O(log(number of roots)) + precomputation, by storing the range as interval of in-order/post-order visit

		if (getRoots().count(block))
			return 1;
		for (auto BB : getRoots())
		{
			if (DT->dominates(BB, block))
				return 1;
		}
		return 0;
	}
	void insert(BasicBlock* block)
	{
		roots.insert(block);
	}
	void clear()
	{
		roots.clear();
	}
	void keepOnlyDominated(const BasicBlock* block)
	{
		assert(DT);

		BlockSet next;
		for (auto BB : roots)
		{
			//DT is a tree, so there are only 3 possibility, either:
			// A dominates B -> the intersection is B
			// B dominates A -> the intersection is A
			// neither dominates -> no intersection
			if (block == BB || DT->dominates(block, BB))
				next.insert(BB);
			else if (DT->dominates(BB, block))
				next.insert(const_cast<BasicBlock*>(block));
		}
		swap (roots, next);
	}
	BasicBlockForest keepOnlyDominated(const BasicBlockForest& dominant) const
	{
		assert(DT);
		assert(DT == dominant.DT);

		BasicBlockForest V;
		V.setDominatorTree(DT);

		std::vector<BasicBlock*> toBeProcessed(getRoots().begin(), getRoots().end());

		while (!toBeProcessed.empty())
		{
			auto BB = toBeProcessed.back();
			toBeProcessed.pop_back();

			//If a node is dominated, keep it
			if (dominant.count(BB))
				V.insert(BB);
			//Otherwise skip it and process the children (in the DT)
			else
			{
				auto NN = DT->getNode(BB);

				for (auto X : make_range(NN->begin(), NN->end()))
				{
					toBeProcessed.push_back(X->getBlock());
				}
			}
		}
		V.simplify();
		return V;
	}
	void keepOnlyDominatedByOperand(const Value* value);
	void setDominatorTree(const DominatorTree* DT_)
	{
		DT = DT_;
	}
	const BlockSet& getRoots() const
	{
		return roots;
	}
	void expandUpwards()
	{
		std::vector<BasicBlock*> V(roots.begin(), roots.end());
		std::set<BasicBlock*> S;
		while (!V.empty() || !S.empty())
		{
			if (!V.empty())
			{
				auto K = V.back();
				V.pop_back();
				for (auto X : make_range(pred_begin(K), pred_end(K)))
				{
					assert(X);
					S.insert(X);
				}
			}
			else
			{
				auto X = *(S.begin());
				S.erase(X);
				if (!roots.count(X) && areAllChildrenValid(X))
				{
					V.push_back(X);
					roots.insert(X);
				}
			}
		}
		simplify();
	}
	void simplify()
	{
		assert(DT);
		//Loop over the roots, nodes that already have their immediate dominator in roots will get skipped
		//It is necessary to have the next, since we may otherwise delete someone else immediate dominator
		//think of mom, child, granma, if you process them in this order mom will get out of the set
		//(it has her own mother), and child will remain in the structure
		BlockSet next;
		for (auto x : roots)
		{
			auto X = immediateDominator(x, DT);
			if (X && count(X))
				continue;
			next.insert(x);
		}

		std::swap(next, roots);
	}
	void dump() const
	{
		llvm::errs()<< "{ ";
		bool is_first = true;
		for (auto x : getRoots())
		{
			if (!is_first)
				llvm::errs() << ",\t";
			is_first = false;
			x->printAsOperand(llvm::errs(), false);
		}
		llvm::errs() << " }\n";
	}
	const DominatorTree* getDominatorTree() const
	{
		return DT;
	}
private:
	bool areAllChildrenValid(BasicBlock* BB) const
	{
		bool hasProperSuccessors = false;
		for (auto it = succ_begin(BB), end = succ_end(BB); it != end; ++it)
		{
			if (*it == BB)
				continue;
			hasProperSuccessors = true;
			if (!roots.count(*it))
				return false;
		}
		assert(hasProperSuccessors);
		return true;
	}
	const DominatorTree* DT;
	BlockSet roots;
	friend std::vector<BasicBlock*> findRepresentingBasicBlock(const DominatorTree* DT, const std::vector<BasicBlock*> blocks);
};

std::vector<BasicBlock*> findRepresentingBasicBlock(const DominatorTree* DT, const std::vector<BasicBlock*> blocks);

//	This class takes two BasicBlockForest in input, one comprising the basic blocks known to be valid and one representing all BB to be classified.
//	The BB to be classified will be classified according to the answer to this question:
//	-starting from a certaint BB, any executuion would lead inevitably to visit one of the known to be valid blocks (or a node post-dominated by him)?
//
//	To do so, a modified CFG is build, in which there are two special sink nodes:
//	"Good" -> successor of nodes known to be valid
//	and "Bad" -> successor of nodes known to be invalid
//	and all other BasicBlock gets associated to a node with the appropriate connections.
//	Building the PostDominatorTree in this CFG allows all nodes to be categorized.
//	In particular, all nodes that are both:
//	-predecessors of "Good" AND part of toBeClassified gets classified as valid and the rest implicitly as invalid
//
//	Example of use of this class is calculating valid location where to place GEPs.
class ValidBasicBlockForestGraph{
public:
	explicit ValidBasicBlockForestGraph(const BasicBlockForest& knownPostdominated, const BasicBlockForest& toBeClassified)
		: knownValid(knownPostdominated.keepOnlyDominated(toBeClassified)), toBeClassified(toBeClassified)
	{
		const DominatorTree* DT = getDominatorTree();
		//Blocks in the CFG could be of three kinds;
		// -knownValid: blocks already classified as valid (it's known they lead to visit a valid BasicBlock and are a subset of toBeClassified-forest)
		// -toBeClassified: blocks we are interested in classifying
		// -invalid: all nodes known to be invalid

		//The objective of this class is categorize all nodes in toBeClassified

		//TODO: Create the graph just once, and only update values in it (possibly using the trick of In-order Post-order numbering on the dominator tree)
		std::vector<BasicBlock*> toBeProcessed(toBeClassified.getRoots().begin(), toBeClassified.getRoots().end());
		while (!toBeProcessed.empty())
		{
			auto BB = toBeProcessed.back();
			toBeProcessed.pop_back();

			//Node that are already known to be valid could avoid to be processed
			if (knownValid.count(BB))
				continue;

			Node* N = getOrCreate(BB, true);
			if (N->kind == Kind::ConnectedToGood)
				GoodPred.push_back(N);
			else if (N->kind == Kind::ConnectedToBad)
				BadPred.push_back(N);
			//A node could be either connected to Good or Bad, since node connected to both
			//are only connected to Bad (since they will never be postdominated by Good, we could avoid the processing)

			auto NN = DT->getNode(BB);

			for (auto X : make_range(NN->begin(), NN->end()))
			{
				toBeProcessed.push_back(X->getBlock());
			}
		}
		getOrCreate(Kind::Good, true);
		getOrCreate(Kind::Bad, true);
	}
	explicit ValidBasicBlockForestGraph(const BasicBlockForest& knownPostdominated)
		: ValidBasicBlockForestGraph(knownPostdominated, BasicBlockForest::expandToTheWholeFunction(knownPostdominated))
	{
	}
	const DominatorTree* getDominatorTree() const
	{
		assert(knownValid.getDominatorTree() == toBeClassified.getDominatorTree());
		assert(knownValid.getDominatorTree());
		return knownValid.getDominatorTree();
	}
	void dump() const
	{
		for (auto x : Nodes)
		{
			x.second.printAsOperand(llvm::errs(), false);
			llvm::errs() << "\t";
		}
		llvm::errs() << "\n";
	}
	enum class Kind
	{
		//There are 5 kinds of nodes:
		//2 virtual sink nodes (Good and Bad)
		//2 set of predecessors of Good (ConnectedToGood) or Bad (ConnectedToBad)
		//and all other nodes (Regular)

		//Good and Bad have no successors, and as predecessors only ConnectedToGood or ConnectedToBad nodes
		//Regular nodes have all they normal predecessor and successors, as long as they exist in the subgraph
		//ConnectedToGood and ConnectedToBad have all they regular predecessors and successors + Good/Bad as additional successor
		Good, Bad, ConnectedToGood, ConnectedToBad, Regular
	};
	Kind determineKind(BasicBlock* BB) const
	{
		assert(BB);
		if (!toBeClassified.count(BB))
		{
			//If they are not part of toBeClassified, they should be marked as bad
			return Kind::ConnectedToBad;
		}
		if (knownValid.count(BB))
		{
			//If they are part of knowValid, they are connected to "Good" (a special sink node)
			return Kind::ConnectedToGood;
		}
		//Otherwise they are just regular nodes
		return Kind::Regular;
	}
	struct Node {
		BasicBlock* BB;
		Kind kind;
		ValidBasicBlockForestGraph& G;
		explicit Node(BasicBlock* BB, ValidBasicBlockForestGraph& G): BB(BB), kind(G.determineKind(BB)), G(G)
		{
			assert(BB);
		}
		explicit Node(Kind kind, ValidBasicBlockForestGraph& G): BB(nullptr), kind(kind), G(G)
		{}
		void printAsOperand(llvm::raw_ostream& o, bool b) const
		{
			if (BB)
				BB->printAsOperand(o, b);
			llvm::errs() << " ";
			if (kind == Kind::Good)
				llvm::errs() << "Good";
			else if (kind == Kind::ConnectedToGood)
				llvm::errs() << "ConnectedToGood";
			else if (kind == Kind::Regular)
				llvm::errs() << "Regular";
			else if (kind == Kind::ConnectedToBad)
				llvm::errs() << "ConnectedToBad";
			else
				llvm::errs() << "Bad";
			llvm::errs() << " ; ";
		}
		void dump() const
		{
			printAsOperand(llvm::errs(), false);
			llvm::errs() << "\n";
		}
	};
	Node* getOrCreate(BasicBlock* BB, bool create = false)
	{
		// Non-virtual nodes are stored as Regular (as to avoid computing the kind here)
		auto it = Nodes.find(std::make_pair(BB, Kind::Regular));
		if (it == Nodes.end())
		{
			assert(create);
			it = Nodes.emplace(std::make_pair(BB, Kind::Regular), Node(BB, *this)).first;
		}
		return &it->second;
	}
	Node* getOrCreate(Kind kind, bool create = false)
	{
		assert(kind != Kind::Regular);
		assert(kind != Kind::ConnectedToBad);
		assert(kind != Kind::ConnectedToGood);
		BasicBlock* BB = nullptr;
		auto it = Nodes.find(std::make_pair(BB, kind));
		if (it == Nodes.end())
		{
			assert(create);
			it = Nodes.emplace(std::make_pair(BB, kind), Node(kind, *this)).first;
		}
		return &it->second;
	}
	bool nodeExist(BasicBlock* BB) const
	{
		assert(BB);
		auto it = Nodes.find(std::make_pair(BB, Kind::Regular));
		return (it != Nodes.end());
	}
	Node* getEntryNode()
	{
		//This function has to be defined, and "Good" is guaranteed to exist
		//But this graph hasn't really a entryNode (as in a function), so if this function is called probably something is wrong
		assert(false);
		return getOrCreate(Kind::Good);
	}
	BasicBlockForest getValidBlocks();
	class SuccIterator
		: public iterator_facade_base<SuccIterator, std::forward_iterator_tag, Node, int, Node, Node> {
		void skipNonExistentSuccessors()
		{
			assert(N->BB);
			while (Idx < (int)N->BB->getTerminator()->getNumSuccessors() &&
					N->G.nodeExist(N->BB->getTerminator()->getSuccessor(Idx)) == false)
			{
				++Idx;
			}
		}
	public:
		explicit SuccIterator(Node* N): N(N)
		{
			if (N->BB == nullptr)
			{
				Idx = -3;
			}
			else if (N->kind == Kind::ConnectedToBad)
				Idx = -2;
			else if (N->kind == Kind::ConnectedToGood)
				Idx = -1;
			else
			{
				Idx = 0;
				skipNonExistentSuccessors();
			}
		}
		explicit SuccIterator(Node* N, bool): N(N)
		{
			if (N->BB == nullptr)
			{
				Idx = -3;
			}
			else
				Idx = N->BB->getTerminator()->getNumSuccessors();
		}
		Node* operator*() const
		{
			assert(Idx > -3);
			if (Idx == -2)
				return N->G.getOrCreate(Kind::Bad);
			if (Idx == -1)
				return N->G.getOrCreate(Kind::Good);
			assert(Idx >= 0);
			BasicBlock* BB = N->BB->getTerminator()->getSuccessor(Idx);
			return N->G.getOrCreate(BB);
		}
		SuccIterator& operator++()
		{
			if (Idx == -2)
				Idx = -1;
			++Idx;
			assert(Idx >= 0);
			skipNonExistentSuccessors();
			return *this;
		}
		SuccIterator operator++(int)
		{
			SuccIterator tmp = *this; ++*this; return tmp;
		}
		bool operator==(const SuccIterator& I) const
		{
			return std::tie(N, Idx) == std::tie(I.N, I.Idx);
		}
	private:
		const Node* N;
		int Idx;

	};
	class PredIterator : public std::iterator<std::forward_iterator_tag, Node, ptrdiff_t, Node*, Node*> {
		typedef std::iterator<std::forward_iterator_tag, Node, ptrdiff_t, Node*, Node*> super;
		typedef PredIterator Self;

		pred_iterator It;
		std::vector<Node*>::iterator VirtIt;
		bool VirtualNode;
		const Node* N;

		void skipNonExistantPredecessors()
		{
			assert(N->BB);
			while(It != pred_end(N->BB) && N->G.nodeExist(*It) == false)
			{
				++It;
			}
		}
	public:
		typedef typename super::pointer pointer;
		typedef typename super::reference reference;

		PredIterator() {}
		explicit PredIterator(Node* N): N(N)
		{
			if (N->BB)
			{
				VirtualNode = false;
				It = pred_begin(N->BB);
				skipNonExistantPredecessors();
			}
			else
			{
				VirtualNode = true;
				if (N->kind == Kind::Good)
					VirtIt = N->G.GoodPred.begin();
				else
					VirtIt = N->G.BadPred.begin();
			}
		}
		PredIterator(Node* N, bool): N(N)
		{
			if (N->BB)
			{
				VirtualNode = false;
				It = pred_end(N->BB);
			}
			else
			{
				VirtualNode = true;
				if (N->kind == Kind::Good)
					VirtIt = N->G.GoodPred.end();
				else
					VirtIt = N->G.BadPred.end();
			}
		}
		bool operator==(const Self& x) const
		{
			if (N->kind != x.N->kind)
				return false;
			if (VirtualNode)
				return VirtIt == x.VirtIt;
			return It == x.It;
		}
		bool operator!=(const Self& x) const
		{
			return !(this->operator==(x));
		}
		reference operator*() const
		{
			if (VirtualNode)
			{
				return *VirtIt;
			}
			else
			{
				return N->G.getOrCreate(*It);
			}
		}
		Self& operator++()
		{
			if (VirtualNode)
			{
				++VirtIt;
			}
			else
			{
				++It;
				skipNonExistantPredecessors();
			}
			return *this;
		}
		Self operator++(int)
		{
			Self tmp = *this; ++*this; return tmp;
		}
	};
	struct NodeHasher
	{
		inline size_t operator()(const std::pair<BasicBlock*, Kind>& p) const
		{
			size_t seed = 0x9e3779b9;
			if (p.second == Kind::Good) seed = 0x2e6739b1;
			seed ^=  reinterpret_cast<size_t>(p.first) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
			return seed;
		}
	};

	typedef std::unordered_map<std::pair<BasicBlock*, Kind>, Node, NodeHasher> NodeMap;
	friend struct GraphTraits<ValidBasicBlockForestGraph*>;
private:
	const BasicBlockForest knownValid;
	const BasicBlockForest& toBeClassified;
	NodeMap Nodes;
	std::vector<Node*> GoodPred;
	std::vector<Node*> BadPred;
};

/**
 * This pass rewrite GEPs in a function to remove redundant object accesses
 */
class GEPOptimizer: public FunctionPass
{
	//TODO: move this to std::set<const BasicBlock*> when getNode(const BB) will be supported by llvm
	typedef std::set<BasicBlock*> BlockSet;
	typedef BasicBlockForest ValidGEPLocations;
public:
	/// This struct represents a subset of a GEP, from operand 0 to operand
	/// `size - 1
	struct GEPRange {
		const GetElementPtrInst* GEP;
		const size_t size;
	private:
		GEPRange(const GetElementPtrInst* GEP, const size_t size_): GEP(GEP), size(size_)
		{}
	public:
		static GEPRange createGEPRange(const GetElementPtrInst* GEP, size_t size)
		{
			if (AggressiveGepOptimizer)
			{
				while (size > 2 && isa<llvm::ConstantInt>(GEP->getOperand(size-1)))
				{
					--size;
				}
			}
			return GEPRange(GEP, size);
		}
		static GEPRange createGEPRange(const GetElementPtrInst* GEP)
		{
			return createGEPRange(GEP, GEP->getNumOperands());
		}
		bool operator==(const GEPRange& Other) const
		{
			if (size != Other.size)
				return false;
			for (size_t i = 0; i < size; ++i)
			{
				if (GEP->getOperand(i) != Other.GEP->getOperand(i))
				{
					return false;
				}
			}
			return true;
		}
		bool subsetOf(const GEPRange& Other) const
		{
			if (size > Other.size)
				return false;
			return *this == GEPRange(Other.GEP, size);
		}
		void dump() const
		{
			llvm::errs()<<"GEPRange [ ";
			GEP->getOperand(0)->printAsOperand(llvm::errs(), false);
			for (size_t i = 1; i < size; ++i)
			{
				llvm::errs()<<", ";
				GEP->getOperand(i)->printAsOperand(llvm::errs(), false);
			}
			llvm::errs()<<" ]\n";
		}
	};
private:
	struct GEPRangeHasher
	{
		inline size_t operator()(const GEPRange& g) const
		{
			size_t seed = 0;
			for (size_t i = 0; i<g.size; i++)
			{
				seed ^=  reinterpret_cast<size_t>(g.GEP->getOperand(i)) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
			}
			return seed;
		}
	};
	typedef std::map<Value*, size_t> OrderOfAppearence;
	struct OrderByOperands
	{
		//We build at the same time the multiset of GEPs and the map(Value* -> index)
		//and we pass this structure to the multiset to determine the order it should have
		//The order is passed by pointer since we want to be able to call multiset::swap
		OrderByOperands(const OrderOfAppearence* ord) : orderOfAppearence(ord)
		{
		}
		bool operator()(const llvm::Instruction* r, const llvm::Instruction* l) const
		{
			//We are ordering Values by order of appearence (somehow arbitrary, but fixed for a given function)
			//so it could be that constant value 3 is smaller than constant value 0
			for(uint32_t i=0;;i++)
			{
				llvm::Value* rVal = NULL;
				llvm::Value* lVal = NULL;
				if(i < r->getNumOperands())
					rVal = r->getOperand(i);
				if(i < l->getNumOperands())
					lVal = l->getOperand(i);
				if (lVal == NULL)
				{
					//Either they are both ended, so they are equal -> the first is not smaller
					//Or the second is done, so it is the "smaller"
					return false;
				}
				if (rVal == lVal)
				{
					//They are not ended (otherwise the case before would have been triggered),
					//thus go to the next pair of values
					continue;
				}
				return orderOfAppearence->at(rVal) < orderOfAppearence->at(lVal);
			}
		}
		const OrderOfAppearence* orderOfAppearence;
	};
	typedef std::multiset<GetElementPtrInst*, OrderByOperands> OrderedGEPs;
	typedef std::unordered_map<GEPRange, ValidGEPLocations, GEPRangeHasher> ValidGEPMap;
	DominatorTree* DT;
	ValidGEPMap validGEPMap;
	ValidGEPMap subsetGEPMap;
	Instruction* hoistGEP(Instruction* I, const GEPRange& R) const;

	static bool isConstantZero(const Value* value)
	{
		if (const ConstantInt* x = dyn_cast<const ConstantInt> (value))
			if (x->isZero())
				return true;
		return false;
	}

	enum class ShortGEPPolicy { ALLOWED, NOT_ALLOWED };

	class GEPRecursionData
	{
	public:
		GEPRecursionData(Function& F, GEPOptimizer* data);
		void startRecursion();
		void applyOptGEP();
		static void mergeGEPs(GetElementPtrInst* a, GetElementPtrInst* b);
		void compressGEPTree(const ShortGEPPolicy shortGEPPolicy);
		bool anyChange() const
		{
			return !erasedInst.empty();
		}
	private:
		static Value* getValueNthOperator(const OrderedGEPs::iterator it, const uint32_t index);
		void optimizeGEPsRecursive(OrderedGEPs::iterator begin, const OrderedGEPs::iterator end,
			llvm::Value* base, const uint32_t startIndex);
		Instruction* findInsertionPoint(const OrderedGEPs::iterator begin, const OrderedGEPs::iterator end, const uint32_t endIndex);
		OrderOfAppearence order;
		OrderedGEPs orderedGeps;
		OrderedGEPs skippedGeps;

		GEPOptimizer* passData;

		std::map<GetElementPtrInst*, std::vector<GetElementPtrInst*>> erasedInst;
		std::vector<GetElementPtrInst*> nonTerminalGeps;
	};
public:
	static char ID;
	explicit GEPOptimizer() : FunctionPass(ID), DT(NULL) { }
	bool runOnFunction(Function &F) override;
	const char *getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// GEPOptimizer
//
FunctionPass *createGEPOptimizerPass();

}

#endif

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
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/DeterministicUnorderedMap.h"

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
	typedef llvm::DenseSet<BasicBlock*> BlockSet;
public:
	BasicBlockForest(const DominatorTree* DT) : DT(DT), roots()
	{
	}
	BasicBlockForest(const BasicBlockForest& other) : DT(other.DT), roots(other.roots)
	{
	}
	BasicBlockForest(BasicBlockForest&& other) : BasicBlockForest(other.DT)
	{
		roots = std::move(other.roots);
	}
	BasicBlockForest& operator=(const BasicBlockForest& other)
	{
		DT = other.DT;
		roots = other.roots;
		return *this;
	}
	BasicBlockForest& operator=(BasicBlockForest&& other)
	{
		DT = other.DT;
		roots = std::move(other.roots);
		return *this;
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
	BasicBlock* getRepresentingRoot(BasicBlock* block) const
	{
		assert(DT);
		assert(block);

		//TODO: Now it is O(number of roots), it could become O(log(number of roots)) + precomputation, by storing the range as interval of in-order/post-order visit

		for (auto BB : getRoots())
		{
			if (BB == block || DT->dominates(BB, block))
				return BB;
		}
		return NULL;
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
		std::swap (roots, next);
	}
	BasicBlockForest keepOnlyDominated(const BasicBlockForest& dominant) const
	{
		assert(DT);
		assert(DT == dominant.DT);

		BasicBlockForest V(DT);

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
	const BlockSet& getRoots() const
	{
		return roots;
	}
	void expandUpwards()
	{
		std::vector<BasicBlock*> V(roots.begin(), roots.end());
		BlockSet S;
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
	bool properlyDominated(BasicBlock* block) const
	{
		if (roots.count(block))
			return false;
		if (count(block))
			return true;
		return false;
	}
	bool areAllChildrenValid(BasicBlock* BB) const
	{
#ifndef NDEBUG
		bool hasProperSuccessors = false;
#endif
		for (auto it = succ_begin(BB), end = succ_end(BB); it != end; ++it)
		{
			if (*it == BB)
				continue;
#ifndef NDEBUG
			hasProperSuccessors = true;
#endif
			if (!roots.count(*it))
				return false;
		}
		assert(hasProperSuccessors);
		return true;
	}
	const DominatorTree* DT;
	BlockSet roots;
};

std::vector<BasicBlock*> findRepresentingBasicBlock(const DominatorTree* DT, const std::vector<BasicBlock*>& blocks);

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
		assert(current_VBBFGraph == NULL);
		current_VBBFGraph = this;
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
			if (N->kind() == Kind::ConnectedToGood)
				GoodPred.push_back(N);
			else if (N->kind() == Kind::ConnectedToBad)
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
	~ValidBasicBlockForestGraph() {
		current_VBBFGraph = NULL;
	}
	ValidBasicBlockForestGraph(const ValidBasicBlockForestGraph&) = delete;
	ValidBasicBlockForestGraph(ValidBasicBlockForestGraph&&) = delete;
	ValidBasicBlockForestGraph& operator= (const ValidBasicBlockForestGraph&) = delete;
	ValidBasicBlockForestGraph& operator= (ValidBasicBlockForestGraph&&) = delete;

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
	BasicBlockForest getValidBlocks();
private:
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
	static Kind compressKind(const Kind& k)
	{
		//There are 5 kinds, but to compress them in 2 bits Good and Regular share the same value (and are kept apart by BB() being non null)
		return Kind((int)k%4);
	}
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
	static ValidBasicBlockForestGraph* current_VBBFGraph;
	typedef llvm::PointerIntPair<llvm::BasicBlock*, 2, Kind> BasicBlockKindPair;
	struct Node {
		typedef Node* NodeRef;
		const llvm::PointerIntPair<llvm::BasicBlock*, 2, Kind> pair;
		// NOTE: This may be used only in assertions
		ValidBasicBlockForestGraph* parent;
		BasicBlock* BB() const
		{
			return pair.getPointer();
		}
		Kind kind() const
		{
			//There are 5 kinds, but to compress them in 2 bits Good and Regular share the same value (and are kept apart by BB() being non null)
			return (pair.getInt() == Kind::Good && BB()) ? Kind::Regular : pair.getInt();
		}
		ValidBasicBlockForestGraph& getGraph() const {return const_cast<ValidBasicBlockForestGraph&>(*current_VBBFGraph);}
		explicit Node(ValidBasicBlockForestGraph* p, BasicBlock* BB): pair(BB, compressKind(getGraph().determineKind(BB))), parent(p)
		{
			assert(BB);
		}
		explicit Node(ValidBasicBlockForestGraph* p, Kind kind): pair(nullptr, kind), parent(p)
		{
		}
		void printAsOperand(llvm::raw_ostream& o, bool b) const
		{
			if (BB())
				BB()->printAsOperand(o, b);
			llvm::errs() << " ";
			if (kind() == Kind::Good)
				llvm::errs() << "Good";
			else if (kind() == Kind::ConnectedToGood)
				llvm::errs() << "ConnectedToGood";
			else if (kind() == Kind::Regular)
				llvm::errs() << "Regular";
			else if (kind() == Kind::ConnectedToBad)
				llvm::errs() << "ConnectedToBad";
			else
				llvm::errs() << "Bad";
			llvm::errs() << " ; ";
		}
		ValidBasicBlockForestGraph* getParent() const
		{
			assert(false);
			return nullptr;
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
		auto it = Nodes.find(BasicBlockKindPair(BB, compressKind(Kind::Regular)));
		if (it == Nodes.end())
		{
			assert(create);
			it = Nodes.insert(std::make_pair(BasicBlockKindPair(BB, compressKind(Kind::Regular)), Node(this, BB))).first;
		}
		return &it->second;
	}
	Node* getOrCreate(Kind kind, bool create = false)
	{
		assert(kind != Kind::Regular);
		assert(kind != Kind::ConnectedToBad);
		assert(kind != Kind::ConnectedToGood);
		BasicBlock* BB = nullptr;
		auto it = Nodes.find(BasicBlockKindPair(BB, compressKind(kind)));
		if (it == Nodes.end())
		{
			assert(create);
			it = Nodes.insert(std::make_pair(BasicBlockKindPair(BB, compressKind(kind)), Node(this, kind))).first;
		}
		return &it->second;
	}
	bool nodeExist(BasicBlock* BB) const
	{
		assert(BB);
		auto it = Nodes.find(BasicBlockKindPair(BB, compressKind(Kind::Regular)));
		return (it != Nodes.end());
	}
	Node* getEntryNode()
	{
		//This function has to be defined, and "Good" is guaranteed to exist
		//But this graph hasn't really a entryNode (as in a function), so if this function is called probably something is wrong
		assert(false);
		return getOrCreate(Kind::Good);
	}
	class SuccIterator
		: public iterator_facade_base<SuccIterator, std::bidirectional_iterator_tag, Node, int, Node*, Node*> {
		void skipNonExistentSuccessorsForward()
		{
			assert(N->BB());
			assert(Idx >=0);
			while (Idx < (int)N->BB()->getTerminator()->getNumSuccessors() &&
					N->getGraph().nodeExist(N->BB()->getTerminator()->getSuccessor(Idx)) == false)
			{
				++Idx;
			}
		}
		void skipNonExistentSuccessorsBackward()
		{
			assert(N->BB());
			while(Idx>=0 && N->getGraph().nodeExist(N->BB()->getTerminator()->getSuccessor(Idx)) == false)
			{
				--Idx;
			}
			if (Idx == -1)
			{
				if (N->kind() == Kind::ConnectedToBad)
					Idx = -2;
				else
					assert(N->kind() == Kind::ConnectedToGood);
			}
		}
	public:
		explicit SuccIterator(Node* N): N(N)
		{
			if (N->BB() == nullptr)
			{
				Idx = -3;
			}
			else if (N->kind() == Kind::ConnectedToBad)
				Idx = -2;
			else if (N->kind() == Kind::ConnectedToGood)
				Idx = -1;
			else
			{
				Idx = 0;
				skipNonExistentSuccessorsForward();
			}
		}
		explicit SuccIterator(Node* N, bool): N(N)
		{
			if (N->BB() == nullptr)
			{
				Idx = -3;
			}
			else
				Idx = N->BB()->getTerminator()->getNumSuccessors();
		}
		Node* operator*() const
		{
			assert(Idx > -3);
			if (Idx == -2)
				return N->getGraph().getOrCreate(Kind::Bad);
			if (Idx == -1)
				return N->getGraph().getOrCreate(Kind::Good);
			assert(Idx >= 0);
			BasicBlock* BB = N->BB()->getTerminator()->getSuccessor(Idx);
			return N->getGraph().getOrCreate(BB);
		}
		SuccIterator& operator++()
		{
			if (Idx == -2)
				Idx = -1;
			++Idx;
			skipNonExistentSuccessorsForward();
			return *this;
		}
		SuccIterator operator++(int)
		{
			SuccIterator tmp = *this; ++*this; return tmp;
		}
		SuccIterator& operator--()
		{
			assert(Idx >= 0);
			Idx--;
			skipNonExistentSuccessorsBackward();
			return *this;
		}
		SuccIterator operator--(int)
		{
			SuccIterator tmp = *this; --*this; return tmp;
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
			assert(N->BB());
			while(It != pred_end(N->BB()) && N->getGraph().nodeExist(*It) == false)
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
			if (N->BB())
			{
				VirtualNode = false;
				It = pred_begin(N->BB());
				skipNonExistantPredecessors();
			}
			else
			{
				VirtualNode = true;
				if (N->kind() == Kind::Good)
					VirtIt = N->getGraph().GoodPred.begin();
				else
					VirtIt = N->getGraph().BadPred.begin();
			}
		}
		PredIterator(Node* N, bool): N(N)
		{
			if (N->BB())
			{
				VirtualNode = false;
				It = pred_end(N->BB());
			}
			else
			{
				VirtualNode = true;
				if (N->kind() == Kind::Good)
					VirtIt = N->getGraph().GoodPred.end();
				else
					VirtIt = N->getGraph().BadPred.end();
			}
		}
		bool operator==(const Self& x) const
		{
			if (N->kind() != x.N->kind())
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
				return N->getGraph().getOrCreate(*It);
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

	typedef llvm::DenseMap<BasicBlockKindPair, Node> NodeMap;
	friend struct GraphTraits<ValidBasicBlockForestGraph*>;
	friend struct GraphTraits<ValidBasicBlockForestGraph::Node*>;
	friend struct GraphTraits<Inverse<ValidBasicBlockForestGraph::Node*>>;
	const BasicBlockForest knownValid;
	const BasicBlockForest toBeClassified;
	NodeMap Nodes;
	std::vector<Node*> GoodPred;
	std::vector<Node*> BadPred;
};

/*
	This class takes care of giving a deterministic id to each pointer
*/
template <typename T>
class PointerToUniqueIdentifier
{
	//Every times it's queried, return a unique identifier for each pointer
	//(possibly assigning it in that moment)
	static_assert(std::is_pointer<T>::value, "The class is intended to be used only with pointers");
public:
	PointerToUniqueIdentifier()
		: firstUnused(0)
	{}
	uint32_t id(const T t)
	{
		insert(t);
		return map.at(t);
	}
	uint32_t at(const T t) const
	{
		return map.at(t);
	}
	void insert(const T t)
	{
		if (map.count(t))
			return;
		map.insert({t, firstUnused++});
	}
	uint32_t count(const T t) const
	{
		return map.count(t);
	}
private:
	std::unordered_map<T, uint32_t> map;
	uint32_t firstUnused;
};

/**
 * This pass rewrite GEPs in a function to remove redundant object accesses
 */
class GEPOptimizer: public FunctionPass
{
	//TODO: move this to llvm::DenseSet<const BasicBlock*> when getNode(const BasicBlock*) will be supported by llvm
	typedef llvm::DenseSet<BasicBlock*> BlockSet;
	typedef BasicBlockForest ValidGEPLocations;
public:
	/// This struct represents a subset of a GEP, from operand 0 to operand
	/// `size - 1
	struct GEPRange {
		const GetElementPtrInst* GEP;
		const size_t size;
	private:
		GEPRange(const GetElementPtrInst* GEP, const size_t size_): GEP(GEP), size(size_)
		{
			assert(GEP->getNumOperands() >= size);
		}
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
		bool properSubsetOf(const GEPRange& Other) const
		{
			if (size >= Other.size)
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
	typedef PointerToUniqueIdentifier<llvm::Value*> OrderOfAppearence;
	struct OrderByOperands
	{
		OrderByOperands(const OrderOfAppearence& orderOfAppearence)
			: orderOfAppearence(orderOfAppearence)
		{
		}
		bool operator()(const llvm::Instruction* r, const llvm::Instruction* l)
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
				return orderOfAppearence.at(rVal) < orderOfAppearence.at(lVal);
			}
		}
		const OrderOfAppearence& orderOfAppearence;
	};
	typedef std::vector<GetElementPtrInst*> OrderedGEPs;
	typedef cheerp::DeterministicUnorderedMap<GEPRange, ValidGEPLocations, cheerp::RestrictionsLifted::NoErasure | cheerp::RestrictionsLifted::NoDeterminism, GEPRangeHasher> ValidGEPMap;
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
		void sortGEPs();
		void buildGEPTree();
		void applyOptGEP();
		static void mergeGEPs(GetElementPtrInst* a, GetElementPtrInst* b);
		void compressGEPTree(const ShortGEPPolicy shortGEPPolicy);
		bool anyChange() const
		{
			return !erasedInst.empty();
		}
	private:
		struct PairRepresentativeOrderedGEPs
		{
			llvm::BasicBlock* representative;
			OrderedGEPs GEPs;
			PairRepresentativeOrderedGEPs(BasicBlock* rep)
				: representative(rep)
			{}
		};
		static Value* getValueNthOperator(const GetElementPtrInst* it, const uint32_t index);
		std::vector<PairRepresentativeOrderedGEPs> splitIntoSubproblems (const OrderedGEPs::iterator begin, const OrderedGEPs::iterator end, const uint32_t endIndex) const;
		void buildNodesOfGEPTree(OrderedGEPs::iterator begin, const OrderedGEPs::iterator end,
			llvm::Value* base, const uint32_t startIndex);
		bool checkInvariantsOnOrderedGEPs(const OrderedGEPs& orderedGEPs)
		{
			OrderByOperands orderByOperands(order);
			for (uint32_t i=1; i<orderedGEPs.size(); i++)
			{
				const GetElementPtrInst* A = orderedGEPs[i-1];
				const GetElementPtrInst* B = orderedGEPs[i];
				GEPRange rangeA = GEPRange::createGEPRange(A);
				GEPRange rangeB = GEPRange::createGEPRange(B);
				if (rangeB.properSubsetOf(rangeA))
					return false;
				if (orderByOperands(B, A))
					return false;

			}
			return true;
		}
		Instruction* findInsertionPoint(const OrderedGEPs& GEPs);
		OrderOfAppearence order;
		OrderedGEPs orderedGeps;

		GEPOptimizer* passData;

		std::map<GetElementPtrInst*, std::vector<GetElementPtrInst*>> erasedInst;
		std::vector<GetElementPtrInst*> nonTerminalGeps;
	};
public:
	static char ID;
	explicit GEPOptimizer() : FunctionPass(ID), DT(NULL) { }
	bool runOnFunction(Function &F) override;
	StringRef getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// GEPOptimizer
//
FunctionPass *createGEPOptimizerPass();

}

#endif

//===-- Cheerp/PointerPasses.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_POINTER_PASSES_H
#define _CHEERP_POINTER_PASSES_H

#include "llvm/Pass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Cheerp/CommandLine.h"

#include <functional>
#include <unordered_map>
#include <queue>

namespace llvm
{

/**
 * Collection of passes whose sole purpose is to help
 * the pointer analyzer generate better code
 */

// Replace an alloca of a single value with an alloca of an array of size 1 if the 
// generated pointer would be CO instead of regular
class AllocaArrays: public FunctionPass
{
	bool replaceAlloca( AllocaInst * ai );
public:
	static char ID;
	explicit AllocaArrays() : FunctionPass(ID) { }
	bool runOnFunction(Function &F) override;
	const char *getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// AllocaArrays
//
FunctionPass *createAllocaArraysPass();


/**
 * Construct a wrapper function for the function which are called indirectly.
 * This is used to allow the PA to pass the function parameters as CO when the function is called
 * directly, while the wrapper function is used only in indirect calls and performs the conversion from REGULAR to CO and
 * then forwards to the actual function.
 * 
 * For each function that:
 *  1) Can be called indirectly
 *  2) Takes non-REGULAR pointer arguments
 * 
 * Replace every instruction which takes the address of the function
 * with a new function, called __duettoindirect##functionname, which calls the original
 * one. This enables pointer kind optimizations for direct calls.
 */
  
class IndirectCallOptimizer: public ModulePass
{
public:
	static char ID;
	explicit IndirectCallOptimizer() : ModulePass(ID) { }
	bool runOnModule(Module &) override;
	const char *getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};


//===----------------------------------------------------------------------===//
//
// IndirectCallOptimizer 

//
ModulePass *createIndirectCallOptimizerPass();

/**
 * This pass will convert PHIs of pointers inside the same array to PHIs of the corresponding indexes
 * It is useful to avoid generating tons of small pointer objects in tight loops.
 */
class PointerArithmeticToArrayIndexing: public FunctionPass
{
public:
	static char ID;
	explicit PointerArithmeticToArrayIndexing() : FunctionPass(ID) { }
	bool runOnFunction(Function &F) override;
	const char *getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// PointerArithmeticToArrayIndexing
//
FunctionPass *createPointerArithmeticToArrayIndexingPass();

/**
 * This pass removes REGULAR pointers by duplicating small blocks which immediately return
 */
class PointerToImmutablePHIRemoval: public FunctionPass
{
private:
	void hoistBlock(BasicBlock* targetBlock);
public:
	static char ID;
	explicit PointerToImmutablePHIRemoval() : FunctionPass(ID) { }
	bool runOnFunction(Function &F) override;
	const char *getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// PointerToImmutablePHIRemoval
//
FunctionPass *createPointerToImmutablePHIRemovalPass();

/**
 * This pass removes all free/delete/delete[] calls as their are no-op in Cheerp
 */
class FreeAndDeleteRemoval: public FunctionPass
{
private:
	void deleteInstructionAndUnusedOperands(Instruction* I);
public:
	static char ID;
	explicit FreeAndDeleteRemoval() : FunctionPass(ID) { }
	bool runOnFunction(Function &F) override;
	const char *getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// FreeAndDeleteRemoval
//
FunctionPass *createFreeAndDeleteRemovalPass();

/**
 * This pass moves instructions as close as possible to the actual users
 */
class DelayInsts: public FunctionPass
{
private:
	struct InsertPoint
	{
		llvm::Instruction* insertInst;
		llvm::BasicBlock* source;
		llvm::BasicBlock* target;
		InsertPoint(llvm::Instruction* i, BasicBlock* s = nullptr, BasicBlock* t = nullptr):insertInst(i),source(s),target(t)
		{
		}
	};
	InsertPoint delayInst(Instruction* I, std::vector<std::pair<Instruction*, InsertPoint>>& movedAllocaMaps, LoopInfo* LI,
					DominatorTree* DT, std::unordered_map<Instruction*, InsertPoint>& visited, bool moveAllocas);
	static uint32_t countInputInstructions(Instruction* I);
public:
	static char ID;
	explicit DelayInsts() : FunctionPass(ID) { }
	bool runOnFunction(Function &F) override;
	const char *getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// DelayInsts
//
FunctionPass *createDelayInstsPass();

/**
 * This pass rewrite GEPs in a function to remove redundant object accesses
 */
class GEPOptimizer: public FunctionPass
{
	typedef std::set<BasicBlock*> BlockSet;
public:
	static BasicBlock* immediateDominator(BasicBlock* BB, DominatorTree* DT)
	{
		auto X = DT->getNode(BB)->getIDom();
		if (!X)
			return NULL;
		else
			return X->getBlock();
	}
	class ValidGEPLocations
	{
	public:
		ValidGEPLocations() : DT(NULL), roots()
		{
		}
		uint32_t count(BasicBlock* block) const
		{
			assert(DT);

			if (roots.count(block))
				return 1;
			for (auto BB : roots)
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
		void setDominatorTree(DominatorTree* DT_)
		{
			DT = DT_;
		}
		const BlockSet& getRoots() const
		{
			return roots;
		}
		void simplify()
		{
			//Loop over the roots, nodes that already have their immediate dominator in roots will get skipped
			//It is necessary to have the next, since we may otherwise delete someone else immediate dominator
			//think of mom, child, granma, if you process them in this order mom will get out of the set
			//(it has her own mother), and child will remain in the structure
			BlockSet next;
			for (auto x : roots)
			{
				if (roots.count(immediateDominator(x, DT)))
					continue;
				next.insert(x);
			}

			std::swap(next, roots);
		}
	private:
		DominatorTree* DT;
		BlockSet roots;
	};
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
	/// This class represents a modified CFG of the function, in which all blocks
	/// where a GEP whose Range is a subset is used have a special virtual node
	/// as the only successor.
	/// In combination with a PostDominatorTree, this is used to find all the blocks
	/// in which it is valid to materialize the GEP represented by Range
	class ValidGEPGraph {
		using GEPRange = GEPOptimizer::GEPRange;
	public:
		explicit ValidGEPGraph(Function* F, DominatorTree* DT, GEPRange Range, const ValidGEPLocations& Subset, const ValidGEPLocations& isRangeValid)
			: F(F), Range(Range), isRangeValid(isRangeValid)
		{
			for (auto BB : isRangeValid.getRoots())
			{
				ValidNodes.push_back(getOrCreate(BB));
			}
			// Here we create explicitly nodes that is it worth visiting
			// (skipping all the nodes dominated by a valid node, since they are already valid, and there is no need to double check them)
			// Nodes have to be created here, since the iterators currently require them to be already created
			for (auto DI = df_begin(DT), DE = df_end(DT); DI != DE;)
			{
				Node* N = getOrCreate(DI->getBlock());
				if (isRangeValid.getRoots().count(DI->getBlock()))
				{
					DI.skipChildren();
				}
				else
				{
					++DI;
				}
			}
			getOrCreate(nullptr);
		}
		struct Node {
			BasicBlock* BB;
			ValidGEPGraph& G;
			explicit Node(BasicBlock* BB, ValidGEPGraph& G): BB(BB), G(G)
			{}
			//General version
			bool isValidForGEP()
			{
				return G.isRangeValid.count(BB);
			}
			//Specialized version
			// If we are looking only at successors, it suffice to check that the BasicBlock is one
			// of the roots of the forest (in the DT tree). It is never possible to "skip" a root and
			// visit a successor of the root without visiting the root first
			bool isValidForGEPSuccessor()
			{
				return G.isRangeValid.getRoots().count(BB);
			}
			void printAsOperand(llvm::raw_ostream& o, bool b)
			{
				if (BB)
					BB->printAsOperand(o, b);
				else
					o<<"null";
			}
		};
		Node* getOrCreate(BasicBlock* BB)
		{
			auto it = Nodes.find(BB);
			if (it == Nodes.end())
			{
				it = Nodes.emplace(BB, Node(BB, *this)).first;
			}
			return &it->second;
		}
		Node* getEntryNode() { return getOrCreate(&F->getEntryBlock()); }
		void getValidBlocks(ValidGEPLocations& validGEPLocations);
		class SuccIterator
			: public iterator_facade_base<SuccIterator, std::forward_iterator_tag, Node, int, Node, Node> {
		public:
			explicit SuccIterator(Node* N): N(N)
			{
				if (N->BB == nullptr)
				{
					Idx = 0;
					return;
				}
				Idx = N->isValidForGEPSuccessor() ? -1 : 0;
			}
			explicit SuccIterator(Node* N, bool): N(N)
			{
				if (N->BB && !N->isValidForGEPSuccessor())
					Idx = N->BB->getTerminator()->getNumSuccessors();
				else
					Idx = 0;
			}
			Node* operator*() const
			{
				if (Idx == -1)
					return N->G.getOrCreate(nullptr);
				BasicBlock* BB = N->BB->getTerminator()->getSuccessor(Idx);
				return N->G.getOrCreate(BB);
			}
			SuccIterator& operator++()
			{
				++Idx;
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
			Node* N;
			int Idx;

		};
		class PredIterator : public std::iterator<std::forward_iterator_tag, Node, ptrdiff_t, Node*, Node*> {
			typedef std::iterator<std::forward_iterator_tag, Node, ptrdiff_t, Node*, Node*> super;
			typedef PredIterator Self;

			pred_iterator It;
			std::vector<Node*>::iterator VirtIt;
			bool VirtualNode;
			Node* N;

			void skipValidPredsAndOutOfSubset()
			{
				if (VirtualNode == false)
				{
					while(It != pred_end(N->BB))
					{
						Node* N = this->operator*();
						if (N->isValidForGEP())
						{
							It++;
						}
						else
							return;
					}
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
					skipValidPredsAndOutOfSubset();
				}
				else
				{
					VirtualNode = true;
					VirtIt = N->G.ValidNodes.begin();
				}
			}
			PredIterator(Node* N, bool): N(N)
			{
				if (N->BB)
				{
					VirtualNode = false;
					It = pred_end(N->BB);
					skipValidPredsAndOutOfSubset();
				}
				else
				{
					VirtualNode = true;
					VirtIt = N->G.ValidNodes.end();
				}
			}
			bool operator==(const Self& x) const
			{
				if (VirtualNode != x.VirtualNode)
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
					skipValidPredsAndOutOfSubset();
				}
				return *this;
			}
			Self operator++(int)
			{
				Self tmp = *this; ++*this; return tmp;
			}
		};

		typedef std::map<BasicBlock*, Node> NodeMap;
		friend struct GraphTraits<ValidGEPGraph*>;
	private:
		Function* F;
		GEPRange Range;
		const ValidGEPLocations& isRangeValid;
		NodeMap Nodes;
		std::vector<Node*> ValidNodes;
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
		bool operator()(llvm::Instruction* r, llvm::Instruction* l) const
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
	typedef std::multiset<Instruction*, OrderByOperands> OrderedGEPs;
	typedef std::unordered_map<GEPRange, ValidGEPLocations, GEPRangeHasher> ValidGEPMap;
	DominatorTree* DT;
	ValidGEPMap validGEPMap;
	ValidGEPMap subsetGEPMap;
	Instruction* hoistGEP(Instruction* I, const GEPRange& R);

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
		void mergeGEPs(GetElementPtrInst* a, GetElementPtrInst* b);
		void compressGEPTree(const ShortGEPPolicy shortGEPPolicy);
		bool anyChange() const
		{
			return !erasedInst.empty();
		}
	private:
		Value* getValueNthOperator(const OrderedGEPs::iterator it, const uint32_t index) const;
		void optimizeGEPsRecursive(OrderedGEPs::iterator begin, const OrderedGEPs::iterator end,
			llvm::Value* base, const uint32_t startIndex);
		Instruction* findInsertionPoint(const OrderedGEPs::iterator begin, const OrderedGEPs::iterator end, const uint32_t endIndex);
		OrderOfAppearence order;
		OrderedGEPs orderedGeps;
		OrderedGEPs skippedGeps;

		void keepOnlyDominated(ValidGEPLocations& blocks, const Value* value);

		GEPOptimizer* passData;

		std::map<GetElementPtrInst*, std::vector<Instruction*>> erasedInst;
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

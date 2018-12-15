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

#include <functional>
#include <unordered_map>

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
 * This pass moves allocas as close as possible to the actual users
 */
class DelayAllocas: public FunctionPass
{
private:
	llvm::Instruction* findCommonInsertionPoint(llvm::Instruction* I, llvm::DominatorTree* DT, llvm::Instruction* currentInsertionPoint, llvm::Instruction* user);
public:
	static char ID;
	explicit DelayAllocas() : FunctionPass(ID) { }
	bool runOnFunction(Function &F) override;
	const char *getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// DelayAllocas
//
FunctionPass *createDelayAllocasPass();

/**
 * This pass rewrite GEPs in a function to remove redundant object accesses
 */
class GEPOptimizer: public FunctionPass
{
	typedef SmallPtrSet<BasicBlock*, 4> BlockSet;
public:
	/// This struct represents a subset of a GEP, from operand 0 to operand
	/// `size - 1`
	struct GEPRange {
		GetElementPtrInst* GEP;
		size_t size;
		GEPRange(GetElementPtrInst* GEP, size_t size): GEP(GEP), size(size)
		{}
		GEPRange(GetElementPtrInst* GEP): GEP(GEP), size(GEP->getNumOperands())
		{}
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
		void dump()
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
	/// where a GEP whoose Range is a subset is used have a special virtual node
	/// as the only successor.
	/// In combination with a PostDominatorTree, this is used to find all the blocks
	/// in which it is valid to materialize the GEP represented by Range
	class ValidGEPGraph {
		using GEPRange = GEPOptimizer::GEPRange;
	public:
		explicit ValidGEPGraph(Function* F, DominatorTree* DT, GEPRange Range, const BlockSet& Subset)
			: F(F), Range(Range)
		{
			for (auto DI = df_begin(DT), DE = df_end(DT); DI != DE;)
			{
				Node* N = getOrCreate(DI->getBlock());
				if (N->isValidForGEP())
				{
					ValidNodes.push_back(N);
					DI.skipChildren();
				}
				else
				{
					++DI;
				}
			}
			for (auto* BB: Subset)
			{
				getOrCreate(BB);
			}
			getOrCreate(nullptr);
		}
		struct Node {
			BasicBlock* BB;
			ValidGEPGraph& G;
			explicit Node(BasicBlock* BB, ValidGEPGraph& G): BB(BB), G(G)
			{}
			bool isValidForGEP()
			{
				for (auto& I: *BB)
				{
					if (GetElementPtrInst* GEP = dyn_cast<GetElementPtrInst>(&I))
					{
						if (G.Range.subsetOf(GEP))
						{
							return true;
						}
					}
				}
				return false;
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
		void getValidBlocks(BlockSet& ValidBlocks);
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
				Idx = N->isValidForGEP() ? -1 : 0;
				skipOutOfSubset();
			}
			explicit SuccIterator(Node* N, bool): N(N)
			{
				if (N->BB && !N->isValidForGEP())
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
				skipOutOfSubset();
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
			void skipOutOfSubset()
			{
				if (Idx == -1)
					return;
				auto end = SuccIterator(N, true);
				while (*this != end)
				{
					if (!N->G.Nodes.count(N->BB->getTerminator()->getSuccessor(Idx)))
					{
						Idx++;
					}
					else
					{
						return;
					}
				}
			}
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
						if (N->isValidForGEP() || !N->G.Nodes.count(N->BB))
							It++;
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
				if (VirtualNode)
					return VirtIt == x.VirtIt;
				return It == x.It;
			}
			bool operator!=(const Self& x) const
			{
				if (VirtualNode)
					return VirtIt != x.VirtIt;
				return It != x.It;
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

		typedef std::unordered_map<BasicBlock*, Node> NodeMap;
		friend struct GraphTraits<ValidGEPGraph*>;
	private:
		Function* F;
		GEPRange Range;
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
	struct OrderByOperands
	{
		bool operator()(llvm::Instruction* r, llvm::Instruction* l) const
		{
			// We are interested in ordering values like symbols, so using pointers is ok
			for(uint32_t i=0;;i++)
			{
				llvm::Value* rVal = NULL;
				llvm::Value* lVal = NULL;
				if(i < r->getNumOperands())
					rVal = r->getOperand(i);
				if(i < l->getNumOperands())
					lVal = l->getOperand(i);
				if(rVal == NULL && lVal == NULL)
				{
					// Both are ended without any difference
					return false;
				}
				if(rVal < lVal)
					return true;
				if(rVal > lVal)
					return false;
			}
		}
	};
	typedef std::multiset<Instruction*, OrderByOperands> OrderedGEPs;
	typedef std::unordered_map<GEPRange, BlockSet, GEPRangeHasher> ValidGEPMap;
	DominatorTree* DT;
	class GEPRecursionData
	{
	public:
		GEPRecursionData(const OrderedGEPs& gepsFromBasePointer, const ValidGEPMap& validGEPMap, DominatorTree* DT)
			: orderedGeps(gepsFromBasePointer), validGEPMap(validGEPMap), DT(DT)
			{}
		void startRecursion();
		void applyOptGEP();
		bool anyChange() const
		{
			return !erasedInst.empty();
		}
	private:
		Value* getValueNthOperator(OrderedGEPs::iterator it, uint32_t index) const;
		void optimizeGEPsRecursive(OrderedGEPs::iterator begin, OrderedGEPs::iterator end,
			llvm::Value* base, uint32_t startIndex);
		Instruction* findInsertionPoint(OrderedGEPs::iterator begin, OrderedGEPs::iterator end, uint32_t endIndex);
		OrderedGEPs orderedGeps;
		OrderedGEPs skippedGeps;
		const ValidGEPMap& validGEPMap;
		DominatorTree* DT;
		std::set<std::pair<Instruction*, Instruction*>> erasedInst;
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

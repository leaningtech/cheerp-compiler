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
	llvm::Instruction* findCommonInsertionPoint(llvm::AllocaInst* AI, llvm::DominatorTree* DT, llvm::Instruction* currentInsertionPoint, llvm::Instruction* user);
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
private:
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
	typedef std::set<Instruction*, OrderByOperands> OrderedGEPs;
	typedef std::unordered_map<Value*, llvm::Instruction*> InsertPointLimit;
	void optimizeGEPsRecursive(std::set<Instruction*>& erasedInst, OrderedGEPs& orderedGeps, OrderedGEPs::iterator begin, OrderedGEPs::iterator end,
					llvm::Value* base, uint32_t startIndex, const InsertPointLimit* insertPointLimit);
	DominatorTree* DT;
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

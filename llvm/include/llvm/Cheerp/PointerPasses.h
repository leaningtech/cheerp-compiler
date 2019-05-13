//===-- Cheerp/PointerPasses.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_POINTER_PASSES_H
#define _CHEERP_POINTER_PASSES_H

#include "llvm/Pass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Utility.h"

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
	bool replaceAlloca( AllocaInst * ai, cheerp::GlobalDepsAnalyzer& gda );
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
		const llvm::Instruction* insertInst;
		const llvm::BasicBlock* source;
		const llvm::BasicBlock* target;
		InsertPoint(const llvm::Instruction* i, BasicBlock* s = nullptr, BasicBlock* t = nullptr):insertInst(i),source(s),target(t)
		{
		}
		bool operator==(const InsertPoint& other) const
		{
			return insertInst == other.insertInst && source == other.source && target == other.target;
		}
		bool fwdBlockDominatesInsertionPoint(const InsertPoint& other, const DominatorTree* DT, const DominatorTreeBase<llvm::BasicBlock>* PDT) const
		{
			//Return true whether the insertionPoint represent a forward block AND this forward block dominates the other insert point
			const llvm::BasicBlock* BB = other.insertInst->getParent();
			return (target &&
				source != target &&
				DT->dominates(source, target) &&
				PDT->dominates(source, target) &&
				(target == BB || DT->dominates(target, BB)));
		}
	};
	InsertPoint delayInst(const Instruction* I, const LoopInfo* LI, const DominatorTree* DT, const DominatorTreeBase<BasicBlock>* PDT, cheerp::InlineableCache& inlineableCache, bool moveAllocas);
	void calculatePlacementOfInstructions(const Function& F, const bool moveAllocas);
	std::unordered_map<const Instruction*, InsertPoint> visited;
	std::vector<std::pair<const Instruction*, InsertPoint>> movedAllocaMaps;

	/**
	 * Return the count of registers used, capped at 2 for speed
	 */
	uint32_t countInputRegisters(const Instruction* I, cheerp::InlineableCache& inlineableCache) const;
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

}
#endif

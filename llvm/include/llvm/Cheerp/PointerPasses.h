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
#include <stack>
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
	StringRef getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// AllocaArrays
//
FunctionPass *createAllocaArraysPass();

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
	StringRef getPassName() const override;

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
	StringRef getPassName() const override;

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
class FreeAndDeleteRemoval: public ModulePass
{
private:
	void deleteInstructionAndUnusedOperands(Instruction* I);
	bool isAllGenericJS;
public:
	static char ID;
	explicit FreeAndDeleteRemoval() : ModulePass(ID), isAllGenericJS(false) { }
	bool runOnModule(Module &M) override;
	StringRef getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// FreeAndDeleteRemoval
//
ModulePass *createFreeAndDeleteRemovalPass();

/**
 * This pass moves instructions as close as possible to the actual users
 */
class DelayInsts: public ModulePass
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
		bool fwdBlockDominatesInsertionPoint(const InsertPoint& other, const DominatorTree* DT, const PostDominatorTree* PDT) const
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
	InsertPoint delayInst(const Instruction* I, const LoopInfo* LI, const DominatorTree* DT, const PostDominatorTree* PDT, cheerp::InlineableCache& inlineableCache, bool moveAllocas);
	void instructionToBeMoved(const Instruction* I, const InsertPoint& insertionPoint);
	void calculatePlacementOfInstructions(const Function& F, cheerp::InlineableCache& inlineableCache);
	void calculatePlacementOfInstructions(const Module& M);

	typedef std::pair<const Instruction*, InsertPoint> PairInstructionLocation;
	typedef std::stack<PairInstructionLocation> StackInstructionsLocations;

	std::unordered_map<const Instruction*, InsertPoint> visited;
	std::vector<std::pair<const Function*, StackInstructionsLocations>> movedInstructionsPerFunction;
	std::unordered_set<const Function*> movedAllocaOnFunction;
	bool Changed;

	/**
	 * Return the count of registers used, capped at 2 for speed
	 */
	uint32_t countInputRegisters(const Instruction* I, cheerp::InlineableCache& inlineableCache) const;
	void moveOnFunction(Function &F, StackInstructionsLocations& stackInstructionsToBeMoved);
public:
	static char ID;
	explicit DelayInsts() : ModulePass(ID) { }
	bool runOnModule(Module &M) override;
	StringRef getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// DelayInsts
//
ModulePass *createDelayInstsPass();

}
#endif

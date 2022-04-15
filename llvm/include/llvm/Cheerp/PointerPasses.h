//===-- Cheerp/PointerPasses.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_POINTER_PASSES_H
#define _CHEERP_POINTER_PASSES_H

#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Utility.h"

#include <functional>
#include <stack>
#include <unordered_map>

namespace cheerp
{


//===----------------------------------------------------------------------===//
//
// AllocaArrays
//
class AllocaArraysPass : public llvm::PassInfoMixin<AllocaArraysPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

/**
 * This pass will convert PHIs of pointers inside the same array to PHIs of the corresponding indexes
 * It is useful to avoid generating tons of small pointer objects in tight loops.
 */
class PointerArithmeticToArrayIndexing
{
public:
	explicit PointerArithmeticToArrayIndexing() { }
	bool runOnFunction(llvm::Function &F);
};

//===----------------------------------------------------------------------===//
//
// PointerArithmeticToArrayIndexing
//
class PointerArithmeticToArrayIndexingPass : public llvm::PassInfoMixin<PointerArithmeticToArrayIndexingPass> {
public:
	llvm::PreservedAnalyses run(llvm::Function& M, llvm::FunctionAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

/**
 * This pass removes REGULAR pointers by duplicating small blocks which immediately return
 */
class PointerToImmutablePHIRemoval
{
private:
	void hoistBlock(llvm::BasicBlock* targetBlock);
public:
	explicit PointerToImmutablePHIRemoval() { }
	bool runOnFunction(llvm::Function &F);
};

//===----------------------------------------------------------------------===//
//
// PointerToImmutablePHIRemoval
//
class PointerToImmutablePHIRemovalPass : public llvm::PassInfoMixin<PointerToImmutablePHIRemovalPass> {
public:
	llvm::PreservedAnalyses run(llvm::Function& M, llvm::FunctionAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

/**
 * This pass removes all free/delete/delete[] calls as their are no-op in Cheerp
 */

//===----------------------------------------------------------------------===//
//
// FreeAndDeleteRemoval
//
class FreeAndDeleteRemovalPass : public llvm::PassInfoMixin<FreeAndDeleteRemovalPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};



/**
 * This pass moves instructions as close as possible to the actual users
 */
class DelayInsts
{
private:
	struct InsertPoint
	{
		const llvm::Instruction* insertInst;
		const llvm::BasicBlock* source;
		const llvm::BasicBlock* target;
		InsertPoint(const llvm::Instruction* i, llvm::BasicBlock* s = nullptr, llvm::BasicBlock* t = nullptr):insertInst(i),source(s),target(t)
		{
		}
		bool operator==(const InsertPoint& other) const
		{
			return insertInst == other.insertInst && source == other.source && target == other.target;
		}
		bool fwdBlockDominatesInsertionPoint(const InsertPoint& other, const llvm::DominatorTree* DT, const llvm::PostDominatorTree* PDT) const
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
	InsertPoint delayInst(const llvm::Instruction* I, const llvm::LoopInfo* LI, const llvm::DominatorTree* DT, const llvm::PostDominatorTree* PDT, cheerp::InlineableCache& inlineableCache, bool moveAllocas);
	void instructionToBeMoved(const llvm::Instruction* I, const InsertPoint& insertionPoint);
	void calculatePlacementOfInstructions(const llvm::Function& F, cheerp::InlineableCache& inlineableCache, const llvm::LoopInfo* LI, const llvm::DominatorTree* DT, const llvm::PostDominatorTree* PDT);
	void calculatePlacementOfInstructions(const llvm::Module& M, cheerp::PointerAnalyzer& PA, llvm::FunctionAnalysisManager& FAM);

	typedef std::pair<const llvm::Instruction*, InsertPoint> PairInstructionLocation;
	typedef std::stack<PairInstructionLocation> StackInstructionsLocations;

	std::unordered_map<const llvm::Instruction*, InsertPoint> visited;
	std::vector<std::pair<const llvm::Function*, StackInstructionsLocations>> movedInstructionsPerFunction;
	std::unordered_set<const llvm::Function*> movedAllocaOnFunction;
	bool Changed;

	/**
	 * Return the count of registers used, capped at 2 for speed
	 */
	uint32_t countInputRegisters(const llvm::Instruction* I, cheerp::InlineableCache& inlineableCache) const;
	void moveOnFunction(llvm::Function &F, StackInstructionsLocations& stackInstructionsToBeMoved);
public:
	explicit DelayInsts() { }
	bool runOnModule(llvm::Module &M, cheerp::Registerize& registerize, cheerp::PointerAnalyzer& PA, llvm::FunctionAnalysisManager& FAM);
};

//===----------------------------------------------------------------------===//
//
// DelayInsts
//
class DelayInstsPass : public llvm::PassInfoMixin<DelayInstsPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

}
#endif

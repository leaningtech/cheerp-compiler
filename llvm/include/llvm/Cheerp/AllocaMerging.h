//===-- Cheerp/AllocaMerging.h - Cheerp alloca elision code ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_ALLOCA_MERGING_H
#define _CHEERP_ALLOCA_MERGING_H

#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"
#include <map>
#include <set>

namespace llvm {

// This class is resposible for recycling allocas. We can use lifetime intrinsics to know
// about the lifetime of an alloca
class AllocaMerging: public FunctionPass
{
private:
	// Sort allocas based on type
	struct OrderAllocasByType
	{
		bool operator()(llvm::AllocaInst* LHS, llvm::AllocaInst* RHS)
		{
			if (LHS->getType() == RHS->getType())
				return LHS < RHS;
			else
				return LHS->getType() < RHS->getType();
		}
	};
	std::set<llvm::AllocaInst*, OrderAllocasByType> allocaInfos;
	
	void analyzeBlock(llvm::BasicBlock& BB);
	bool hasUseBelowInBlocks(const llvm::BasicBlock* BB, const std::set<const llvm::BasicBlock*>& curAllocaUsersBlocks,
								std::set<const llvm::BasicBlock*>& visitedSet);
	bool hasUseBelow(const llvm::Instruction* U, const std::set<const llvm::Instruction*>& curAllocaUsers,
						const std::set<const llvm::BasicBlock*>& curAllocaUsersBlocks);
	bool hasUseAboveInBlocks(const llvm::BasicBlock* BB, const std::set<const llvm::BasicBlock*>& curAllocaUsersBlocks,
								std::set<const llvm::BasicBlock*>& visitedSet);
	bool hasUseAbove(const llvm::Instruction* U, const std::set<const llvm::Instruction*>& curAllocaUsers,
						const std::set<const llvm::BasicBlock*>& curAllocaUsersBlocks);
	std::set<const llvm::Instruction*> gatherDerivedUses(const llvm::AllocaInst* rootI);
	bool checkUsesForArrayMerging(AllocaInst* alloca);
public:
	static char ID;
	explicit AllocaMerging() : FunctionPass(ID) { }
	bool runOnFunction(Function &F);
	const char *getPassName() const;
};

//===----------------------------------------------------------------------===//
//
// AllocaMerging - This pass merges allocas which are not used at the same time
//
FunctionPass *createAllocaMergingPass();
}

#endif //_CHEERP_ALLOCA_MERGING_H


//===-- AllocaMerging.cpp - The Duetto JavaScript generator ---------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <algorithm>
#include "llvm/Analysis/CaptureTracking.h"
#include "llvm/Duetto/AllocaMerging.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/IntrinsicInst.h"

using namespace llvm;

namespace llvm {

bool AllocaMerging::hasUseAboveInBlocks(const BasicBlock* BB, const std::set<const BasicBlock*>& curAllocaUsersBlocks,
								std::set<const BasicBlock*>& visitedSet)
{
	if(visitedSet.count(BB))
		return false;
	visitedSet.insert(BB);
	//Check if any of the predecessors contains any use of curAlloca
	for (const_pred_iterator PI = pred_begin(BB), E = pred_end(BB); PI != E; ++PI)
	{
		if (curAllocaUsersBlocks.count(*PI))
			return true;
		bool ret=hasUseAboveInBlocks(*PI, curAllocaUsersBlocks, visitedSet);
		if (ret)
			return true;
	}
	return false;
}

bool AllocaMerging::hasUseAbove(const Instruction* U, const std::set<const Instruction*>& curAllocaUsers,
						const std::set<const BasicBlock*>& curAllocaUsersBlocks)
{
	// Look for any use from the beginning of the block
	for(BasicBlock::const_iterator it=U->getParent()->begin(); &(*it)!=U; ++it)
	{
		// Check if instructions is a user of curAlloca
		if(curAllocaUsers.count(&(*it)))
			return true;
	}

	// Look in predecessors blocks
	std::set<const BasicBlock*> visitedBlocks;
	return hasUseAboveInBlocks(U->getParent(), curAllocaUsersBlocks, visitedBlocks);
}

bool AllocaMerging::hasUseBelowInBlocks(const BasicBlock* BB, const std::set<const BasicBlock*>& curAllocaUsersBlocks,
								std::set<const BasicBlock*>& visitedSet)
{
	if(visitedSet.count(BB))
		return false;
	visitedSet.insert(BB);
	//Check if any of the successors contains any use of curAlloca
	for (succ_const_iterator PI = succ_begin(BB), E = succ_end(BB); PI != E; ++PI)
	{
		if (curAllocaUsersBlocks.count(*PI))
			return true;
		bool ret=hasUseBelowInBlocks(*PI, curAllocaUsersBlocks, visitedSet);
		if (ret)
			return true;
	}
	return false;
}

bool AllocaMerging::hasUseBelow(const Instruction* U, const std::set<const Instruction*>& curAllocaUsers,
						const std::set<const BasicBlock*>& curAllocaUsersBlocks)
{
	// Look for any use from the end of the block to U
	for(BasicBlock::const_reverse_iterator it=U->getParent()->rbegin(); &(*it)!=U; ++it)
	{
		// Check if instructions is a user of curAlloca
		if(curAllocaUsers.count(&(*it)))
			return true;
	}

	// Look in successors blocks
	std::set<const BasicBlock*> visitedBlocks;
	return hasUseBelowInBlocks(U->getParent(), curAllocaUsersBlocks, visitedBlocks);
}

/*
	Returns true if I escapes our analysis, false otherwise.
	If true is returned the vector of uses will not be complete.
*/
std::set<const Instruction*> AllocaMerging::gatherDerivedUses(const Instruction* rootI)
{
	SmallVector<const Instruction*, 10> allUses;
	for(const User* U: rootI->users())
		allUses.push_back(cast<Instruction>(U));

	bool escapes = false;
	// We need to keep track of all our users, to discriminate StoreInst and similar
	std::set<const Instruction*> allUsers;
	allUsers.insert(rootI);
	// NOTE: allUses.size() will grow over time, that's fine
	for(uint32_t i=0;i<allUses.size();i++)
	{
		const Instruction* I = allUses[i];
		switch(I->getOpcode())
		{
			case Instruction::BitCast:
			case Instruction::GetElementPtr:
			{
				// Check derived uses
				for(const User* U: I->users())
					allUses.push_back(cast<Instruction>(U));
				allUsers.insert(I);
				break;
			}
			case Instruction::Store:
			{
				// If we are storing any user, it escapes
				const StoreInst* SI = cast<StoreInst>(I);
				if (allUsers.count(dyn_cast<Instruction>(SI->getValueOperand())))
					escapes = true;
				break;
			}
			case Instruction::Load:
			{
				// Loads are fine
				break;
			}
			default:
				// Be conservative
				escapes = true;
				break;
		}
		if (escapes)
		{
			allUses.clear();
			break;
		}
	}
	return std::set<const Instruction*>(allUses.begin(), allUses.end());
}

void AllocaMerging::analyzeBlock(BasicBlock& BB)
{
	for(Instruction& I: BB)
	{
		// We are interested in Allocas and lifetime intrinsics
		if(I.getOpcode() == Instruction::Alloca)
		{
			AllocaInst* AI = cast<AllocaInst>(&I);
			allocaInfos.insert(AI);
		}
	}
}

bool AllocaMerging::runOnFunction(Function& F)
{
	allocaInfos.clear();
	// Gather all the allocas
	for(BasicBlock& BB: F)
		analyzeBlock(BB);
	if (allocaInfos.size() < 2)
		return false;
	// Look if we can merge allocas of the same type
	auto targetCandidate=allocaInfos.begin();
	auto sourceCandidate=allocaInfos.begin();
	++sourceCandidate;
	std::set<const Instruction*> targetAllocaUsers;
	std::set<const BasicBlock*> targetAllocaUsersBlocks;
	bool Changed = false;
	while(sourceCandidate!=allocaInfos.end())
	{
		AllocaInst* targetAlloca = *targetCandidate;
		if (targetAllocaUsers.empty())
		{
			// Update the derived user list
			targetAllocaUsers = gatherDerivedUses(targetAlloca);
			for(const Instruction* U: targetAllocaUsers)
				targetAllocaUsersBlocks.insert(U->getParent());
		}
		AllocaInst* sourceAlloca = *sourceCandidate;
		// If still empty, we have an escaping use that we can't analyze
		if (targetAlloca->getType() != sourceAlloca->getType() || targetAllocaUsers.empty())
		{
			++targetCandidate;
			++sourceCandidate;
			targetAllocaUsers.clear();
			targetAllocaUsersBlocks.clear();
			continue;
		}

		bool useAbove = false;
		bool useBelow = false;
		std::set<const Instruction*> sourceAllocaUsers = gatherDerivedUses(sourceAlloca);
		// Empty uses means that the candidate source has escaping uses
		if (sourceAllocaUsers.empty())
		{
			++sourceCandidate;
			continue;
		}

		// Both allocas are of the same type and analyzable. Now we have to check, for derived use:
		// 1) If there is any use of the other alloca in the predecessors
		// 2) If there is any use of the other alloca in the successors
		// If all uses are only above, or only below, or in neither path side we can re-use the alloca
		for(const Instruction* I: sourceAllocaUsers)
		{
			if(hasUseAbove(I, targetAllocaUsers, targetAllocaUsersBlocks))
			{
				useAbove = true;
				if(useBelow)
					break;
			}
			if(hasUseBelow(I, targetAllocaUsers, targetAllocaUsersBlocks))
			{
				useBelow = true;
				if(useAbove)
					break;
			}
		}
		if (useAbove && useBelow)
		{
			++sourceCandidate;
			continue;
		}

		// We can merge the allocas
		// Merge the users of the source in the target
		for(const Instruction* U: sourceAllocaUsers)
		{
			targetAllocaUsersBlocks.insert(U->getParent());
			targetAllocaUsers.insert(U);
		}
		sourceAlloca->replaceAllUsesWith(targetAlloca);
		sourceAlloca->eraseFromParent();
		allocaInfos.erase(sourceCandidate++);
		Changed = true;
	}
	return Changed;
}

const char *AllocaMerging::getPassName() const {
	return "AllocaMerging";
}

char AllocaMerging::ID = 0;

FunctionPass *createAllocaMergingPass() { return new AllocaMerging(); }

}

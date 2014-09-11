//===-- Registerize.cpp - TTODO -----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "CheerpRegisterize"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace cheerp {

char Registerize::ID = 0;

void Registerize::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addPreserved<cheerp::PointerAnalyzer>();
	llvm::Pass::getAnalysisUsage(AU);
}

bool Registerize::runOnModule(Module & M)
{
	for (Function& F: M)
		handleFunction(F);
	return false;
}

const char* Registerize::getPassName() const
{
	return "CheerpRegisterize";
}

uint32_t Registerize::getRegisterId(const llvm::Instruction* I) const
{
	assert(registersMap.count(I));
	return registersMap.find(I)->second;
}

void Registerize::InstructionLiveRange::addUse(uint32_t curCodePath, uint32_t thisIndex)
{
	// If we are still in the same code path we can directly extend the last range
	// since inside a code path the instructions are numbered sequentially
	if(codePathId==curCodePath)
		range.back().end=thisIndex;
	else
	{
		range.push_back({curCodePath, thisIndex});
		codePathId=curCodePath;
	}
}

void Registerize::handleFunction(Function& F)
{
	if (F.empty())
		return;
	if (NoRegisterize)
	{
		// Do a fake run and assign every instruction to a different register
		uint32_t nextRegister=0;
		for(BasicBlock& BB: F)
		{
			for(Instruction& I: BB)
			{
				if (isInlineable(I) || I.getType()->isVoidTy())
					continue;
				registersMap[&I]=nextRegister++;
			}
		}
	}
	else
	{
		InstIdMapTy instIdMap;
		// First, build live ranges for all instructions
		LiveRangesTy liveRanges=computeLiveRanges(F, instIdMap);
		// Assign each instruction to a virtual register
		assignToRegisters(liveRanges);
		// To debug we need to know the ranges for each instructions and the assigned register
		DEBUG(dbgs() << F);
		for(auto it: liveRanges)
		{
			DEBUG(dbgs() << "Instruction " << *it.first << " alive in ranges ");
			for(const Registerize::LiveRangeChunk& chunk: it.second.range)
				DEBUG(dbgs() << '[' << chunk.start << ',' << chunk.end << ')');
			DEBUG(dbgs() << "\n");
			DEBUG(dbgs() << "\tMapped to register " << registersMap[it.first] << "\n");
		}
	}
}

Registerize::LiveRangesTy Registerize::computeLiveRanges(Function& F, InstIdMapTy& instIdMap)
{
	BlocksState blocksState;
	for(BasicBlock& BB: F)
	{
		for(Instruction& I: BB)
		{
			// Start from each use and trace up until the definition is found
			for(Use& U: I.uses())
			{
				Instruction* userI = cast<Instruction>(U.getUser());
				BasicBlock* useBB=userI->getParent();
				if(PHINode* phi=dyn_cast<PHINode>(userI))
				{
					// We want to set instruction I as alive at the end of the predecessor
					// And start going up from the predecessor itself
					useBB=phi->getIncomingBlock(U.getOperandNo());
					BlockState& blockState=blocksState[useBB];
					blockState.addLiveOut(&I);
				}
				doUpAndMark(blocksState, useBB, &I);
			}
		}
	}
	// Also print the inSet and outSet for each block while debugging
	DEBUG(
		for(auto it: blocksState)
		{
			llvm::errs() << "Block:\n" << *it.first << "\n";
			llvm::errs() << "Inst in:\n";
			for(Instruction* I: it.second.inSet)
				llvm::errs() << *I << "\n";
			llvm::errs() << "Inst out:\n";
			for(Instruction* I: it.second.outSet)
				llvm::errs() << *I << "\n";
		}
	);
	// Depth first analysis of blocks, starting from the entry block
	LiveRangesTy liveRanges;
	dfsLiveRangeInBlock(blocksState, liveRanges, instIdMap, F.getEntryBlock(), 1, 1);
	return liveRanges;
}

void Registerize::doUpAndMark(BlocksState& blocksState, BasicBlock* BB, Instruction* I)
{
	// Defined here, no propagation needed
	if(I->getParent()==BB && !isa<PHINode>(I))
		return;
	BlockState& blockState=blocksState[BB];
	// Already propagated
	if(blockState.isLiveIn(I))
		return;
	blockState.addLiveIn(I);
	if(I->getParent()==BB && isa<PHINode>(I))
		return;
	// Run on predecessor blocks
	for(::pred_iterator it=pred_begin(BB);it!=pred_end(BB);++it)
	{
		BasicBlock* pred=*it;
		BlockState& predBlockState=blocksState[pred];
		if(!predBlockState.isLiveOut(I))
			predBlockState.addLiveOut(I);
		doUpAndMark(blocksState, pred, I);
	}
}

uint32_t Registerize::dfsLiveRangeInBlock(BlocksState& blocksState, LiveRangesTy& liveRanges, InstIdMapTy& instIdMap,
					BasicBlock& BB, uint32_t nextIndex, uint32_t codePathId)
{
	BlockState& blockState=blocksState[&BB];
	if(blockState.indexesAssigned)
		return nextIndex;
	// Iterate over instructions
	// For each instruction start an empty range
	// For each use used operands extend their live ranges to here
	for (Instruction& I: BB)
	{
		assert(liveRanges.count(&I)==0);
		uint32_t thisIndex = nextIndex++;
		instIdMap[&I]=thisIndex;
		// Inlineable instructions extends the life of the not-inlineable instructions they use.
		// This happens inside extendRangeForUsedOperands.
		if (isInlineable(I))
			continue;
		// Void instruction do not need any lifetime computation
		if (!I.getType()->isVoidTy())
		{
			InstructionLiveRange& range=liveRanges.emplace(&I,
				InstructionLiveRange(codePathId)).first->second;
			range.range.push_back({thisIndex, thisIndex});
		}
		// Operands of PHIs are declared as live out from the source block.
		// This is handled below.
		if (isa<PHINode>(I))
			continue;
		extendRangeForUsedOperands(I, liveRanges, thisIndex, codePathId);
	}
	// Extend the live range of live-out instrution to the end of the block
	uint32_t endOfBlockIndex=nextIndex;
	TerminatorInst* term=BB.getTerminator();
	for(Instruction* outLiveInst: blockState.outSet)
	{
		// If inlineable we need to extend the life of the not-inlineable operands
		if (isInlineable(*outLiveInst))
			extendRangeForUsedOperands(*outLiveInst, liveRanges, endOfBlockIndex, codePathId);
		else
		{
			InstructionLiveRange& range=liveRanges.find(outLiveInst)->second;
			range.addUse(codePathId, endOfBlockIndex);
		}
	}
	blockState.indexesAssigned=true;
	// Run on successor blocks
	for(uint32_t i=0;i<term->getNumSuccessors();i++)
	{
		BasicBlock* succ=term->getSuccessor(i);
		uint32_t newNextIndex=dfsLiveRangeInBlock(blocksState, liveRanges, instIdMap, *succ, nextIndex, codePathId);
		// If any new instruction has ben added (i.e. nextIndex if changed) update codePathId
		if(newNextIndex!=nextIndex)
			codePathId = newNextIndex;
		nextIndex = newNextIndex;
	}
	return nextIndex;
}

void Registerize::extendRangeForUsedOperands(Instruction& I, LiveRangesTy& liveRanges,
						uint32_t thisIndex, uint32_t codePathId)
{
	for(Value* op: I.operands())
	{
		Instruction* usedI = dyn_cast<Instruction>(op);
		// Uses which are not instruction do not require live range analysis
		if(!usedI)
			continue;
		// Recursively traverse inlineable operands
		if(isInlineable(*usedI))
			extendRangeForUsedOperands(*usedI, liveRanges, thisIndex, codePathId);
		else
		{
			assert(liveRanges.count(usedI));
			InstructionLiveRange& range=liveRanges.find(usedI)->second;
			range.addUse(codePathId, thisIndex);
		}
	}
}

void Registerize::assignToRegisters(const LiveRangesTy& liveRanges)
{
	llvm::SmallVector<RegisterRange, 4> registers;
	// First try to assign all PHI operands to the same register as the PHI itself
	for(auto it: liveRanges)
	{
		Instruction* I=it.first;
		if(!isa<PHINode>(I))
			continue;
		handlePHI(*I, liveRanges, registers);
	}
	// Assign a register to the remaining instructions
	for(auto it: liveRanges)
	{
		Instruction* I=it.first;
		if(isa<PHINode>(I))
			continue;
		InstructionLiveRange& range=it.second;
		// Move on if a register is already assigned
		if(registersMap.count(I))
			continue;
		uint32_t chosenRegister=findOrCreateRegister(registers, range, getRegKindFromType(I->getType()));
		registersMap[I] = chosenRegister;
	}
}

void Registerize::handlePHI(Instruction& I, const LiveRangesTy& liveRanges, llvm::SmallVector<RegisterRange, 4>& registers)
{
	uint32_t chosenRegister=0xffffffff;
	const InstructionLiveRange& PHIrange=liveRanges.find(&I)->second;
	// A PHI may already have an assigned register if it's an operand to another PHI
	if(registersMap.count(&I))
		chosenRegister = registersMap[&I];
	else
	{
		// If one of the operands already has a register allocated try to use that register again
		for(Value* op: I.operands())
		{
			Instruction* usedI=dyn_cast<Instruction>(op);
			if(!usedI)
				continue;
			assert(liveRanges.count(usedI));
			if(registersMap.count(usedI)==0)
				continue;
			uint32_t operandRegister=registersMap[usedI];
			if(addRangeToRegisterIfPossible(registers[operandRegister], PHIrange,
							getRegKindFromType(usedI->getType())))
			{
				chosenRegister=operandRegister;
				break;
			}
		}
	}
	// If a register has not been chosen yet, find or create a new one
	if(chosenRegister==0xffffffff)
		chosenRegister=findOrCreateRegister(registers, PHIrange, getRegKindFromType(I.getType()));
	registersMap[&I]=chosenRegister;
	// Iterate again on the operands and try to map as many as possible into the same register
	for(Value* op: I.operands())
	{
		Instruction* usedI=dyn_cast<Instruction>(op);
		if(!usedI)
			continue;
		assert(liveRanges.count(usedI));
		// Skip already assigned operands
		if(registersMap.count(usedI))
			continue;
		const InstructionLiveRange& opRange=liveRanges.find(usedI)->second;
		bool spaceFound=addRangeToRegisterIfPossible(registers[chosenRegister], opRange,
								getRegKindFromType(usedI->getType()));
		if (spaceFound)
		{
			// Update the mapping
			registersMap[usedI]=chosenRegister;
		}
	}
}

uint32_t Registerize::findOrCreateRegister(llvm::SmallVector<RegisterRange, 4>& registers, const InstructionLiveRange& range,
						REGISTER_KIND kind)
{
	for(uint32_t i=0;i<registers.size();i++)
	{
		if(addRangeToRegisterIfPossible(registers[i], range, kind))
			return i;
	}
	// Create a new register with the range of the current instruction already used
	registers.push_back(RegisterRange(range.range, kind));
	return registers.size()-1;
}

Registerize::REGISTER_KIND Registerize::getRegKindFromType(llvm::Type* t)
{
	if(t->isIntegerTy())
		return INTEGER;
	else if(t->isFloatTy())
		return FLOAT;
	else if(t->isDoubleTy())
		return DOUBLE;
	else
		return OBJECT;
}

bool Registerize::LiveRange::doesInterfere(const LiveRange& other) const
{
	// Check if all the ranges in this range fit inside other's holes
	llvm::SmallVector<LiveRangeChunk, 4>::const_iterator otherIt = other.begin();
	llvm::SmallVector<LiveRangeChunk, 4>::const_iterator otherItE = other.end();
	llvm::SmallVector<LiveRangeChunk, 4>::const_iterator thisIt = begin();
	llvm::SmallVector<LiveRangeChunk, 4>::const_iterator thisItE = end();
	while(otherIt!=otherItE && thisIt!=thisItE)
	{
		// Case 1: thisRange << otherRange
		if(thisIt->start < otherIt->start && thisIt->end <= otherIt->start)
		{
			// Move to the next range of this
			++thisIt;
			continue;
		}
		// Case 2: otherRange << thisRange
		if(otherIt->start < thisIt->start && otherIt->end <= thisIt->start)
		{
			// Move the the next range of other
			++otherIt;
			continue;
		}
		return true;
	}
	return false;
}

void Registerize::LiveRange::merge(const LiveRange& other)
{
	insert(end(), other.begin(), other.end());
	std::sort(begin(), end());
	//TODO: Merge adjacent ranges
}

void Registerize::LiveRange::dump() const
{
	for(const Registerize::LiveRangeChunk& chunk: *this)
		dbgs() << '[' << chunk.start << ',' << chunk.end << ')';
}

bool Registerize::addRangeToRegisterIfPossible(RegisterRange& regRange, const InstructionLiveRange& liveRange,
						REGISTER_KIND kind)
{
	if(regRange.regKind!=kind)
		return false;
	if(regRange.range.doesInterfere(liveRange.range))
		return false;
	regRange.range.merge(liveRange.range);
	return true;
}

ModulePass* createRegisterizePass(bool NoRegisterize)
{
	return new Registerize(NoRegisterize);
}

}

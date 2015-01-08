//===-- Registerize.cpp - Compute live ranges to minimize variables--------===//
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
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace cheerp {

char Registerize::ID = 0;

void Registerize::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addPreserved<cheerp::PointerAnalyzer>();
	AU.addRequired<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
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
		range.push_back(LiveRangeChunk(curCodePath, thisIndex));
		codePathId=curCodePath;
	}
}

void Registerize::handleFunction(Function& F)
{
	if (F.empty())
		return;
	cheerp::PointerAnalyzer & PA = getAnalysis<cheerp::PointerAnalyzer>();
	if (NoRegisterize)
	{
		// Do a fake run and assign every instruction to a different register
		uint32_t nextRegister=0;
		for(BasicBlock& BB: F)
		{
			for(Instruction& I: BB)
			{
				if(AllocaInst* AI=dyn_cast<AllocaInst>(&I))
				{
					// Initialize an empty live range, which means that the alloca escapes analysis
					allocaLiveRanges[AI];
				}
				if (isInlineable(I, PA) || I.getType()->isVoidTy())
					continue;
				registersMap[&I]=nextRegister++;
			}
		}
	}
	else
	{
		AllocaSetTy allocaSet;
		InstIdMapTy instIdMap;
		// First, build live ranges for all instructions
		LiveRangesTy liveRanges=computeLiveRanges(F, instIdMap, allocaSet);
		// Assign each instruction to a virtual register
		assignToRegisters(liveRanges);
		// Now compute live ranges for alloca memory which is not in SSA form
		computeAllocaLiveRanges(allocaSet, instIdMap);
		// To debug we need to know the ranges for each instructions and the assigned register
		DEBUG(dbgs() << F;
		for(auto it: liveRanges)
		{
			dbgs() << "Instruction " << *it.first << " alive in ranges ";
			for(const Registerize::LiveRangeChunk& chunk: it.second.range)
				dbgs() << '[' << chunk.start << ',' << chunk.end << ')';
			dbgs() << "\n";
			dbgs() << "\tMapped to register " << registersMap[it.first] << "\n";
		}
		for(auto it: allocaLiveRanges)
		{
			dbgs() << "Alloca " << *it.first << " alive in ranges ";
			for(const Registerize::LiveRangeChunk& chunk: it.second)
				dbgs() << '[' << chunk.start << ',' << chunk.end << ')';
			dbgs() << "\n";
		});
	}
}

Registerize::LiveRangesTy Registerize::computeLiveRanges(Function& F, InstIdMapTy& instIdMap, AllocaSetTy& allocaSet)
{
	BlocksState blocksState;
	for(BasicBlock& BB: F)
	{
		for(Instruction& I: BB)
		{
			// Take our chance to store away all alloca, they are registerized
			// later on using non-SSA logic
			if (isa<AllocaInst>(I))
				allocaSet.push_back(cast<AllocaInst>(&I));
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
	blockState.setLiveIn(I);
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
	cheerp::PointerAnalyzer & PA = getAnalysis<cheerp::PointerAnalyzer>();
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
		if (isInlineable(I, PA))
			continue;
		// Void instruction do not need any lifetime computation
		if (!I.getType()->isVoidTy())
		{
			InstructionLiveRange& range=liveRanges.emplace(&I,
				InstructionLiveRange(codePathId)).first->second;
			range.range.push_back(LiveRangeChunk(thisIndex, thisIndex));
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
		if (isInlineable(*outLiveInst, PA))
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
	cheerp::PointerAnalyzer & PA = getAnalysis<cheerp::PointerAnalyzer>();
	for(Value* op: I.operands())
	{
		Instruction* usedI = dyn_cast<Instruction>(op);
		// Uses which are not instruction do not require live range analysis
		if(!usedI)
			continue;
		// Recursively traverse inlineable operands
		if(isInlineable(*usedI, PA))
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

void Registerize::computeAllocaLiveRanges(AllocaSetTy& allocaSet, const InstIdMapTy& instIdMap)
{
	for(AllocaInst* alloca: allocaSet)
	{
		AllocaBlocksState blocksState;
		RangeChunksTy ranges;
		// For each alloca gather all uses and derived uses
		std::unordered_set<Instruction*> allUses=gatherDerivedMemoryAccesses(alloca);
		if(allUses.empty())
		{
			// Initialize an empty live range to signal that no analysis is possible
			allocaLiveRanges[alloca];
			continue;
		}
		// Now we run a modified up and mark algorithm on every use
		// For each use, get the parent block
		// 1) Find all uses in the block to build a base use range, also set the inLoop flag for the block
		// 2) Recursively navigate predecessors, if any use is found extend the live range
		// 3) If no use is found in a block, put it in a list of pending blocks and proceed to the predecessors
		// 4) If a loop is found and the inLoop flag is set mark all pending blocks with the inLoop and live-out flags
		// 4) When a use is found mark all pending blocks with the inLoop and live-out flags
		// 4) If a lifetime_start intrinsic is found we can stop immediately as above the memory would be not valid
		for(Instruction* I: allUses)
		{
			assert(instIdMap.count(I));
			uint32_t instId=instIdMap.find(I)->second;
			auto it=ranges.upper_bound(instId);
			// Check if the instruction id is inside an already explored range and if so skip it.
			if(it!=ranges.begin())
			{
				--it;
				if(it->first <= instId && instId < it->second)
					continue;
			}
			// Memory may be used until the end of the opcode (especially valid for calls)
			LiveRangeChunk localRange(instId, instId+1);
			BasicBlock* BB=I->getParent();
			AllocaBlockState& blockState=blocksState[BB];
			blockState.visited=true;
			blockState.inLoop=true;
			bool onlyLocal=false;
			// Iterate on instructions of the parent block while expanding the range as much as possible
			for(Instruction& I: *BB)
			{
				if(allUses.count(&I))
				{
					assert(instIdMap.count(&I));
					uint32_t useId=instIdMap.find(&I)->second;
					if(useId < localRange.start)
						localRange.start=useId;
					if(useId+1 > localRange.end)
						localRange.end=useId+1;
					// If we find a lifetime_start intrinsic we can skip the predecessors
					if(IntrinsicInst* II=dyn_cast<IntrinsicInst>(&I))
					{
						if(II->getIntrinsicID()==Intrinsic::lifetime_start)
							onlyLocal=true;
					}
				}
			}
			// We need to know if the range should start from the beginning of the block,
			// so we need to check the predecessors blocks
			if(!onlyLocal)
			{
				bool alreadyLiveIn=false;
				for(::pred_iterator it=pred_begin(BB);it!=pred_end(BB);++it)
				{
					bool predecessorUse=doUpAndMarkForAlloca(blocksState, allUses, ranges,
										instIdMap, *it, alloca);
					if(predecessorUse && !alreadyLiveIn)
					{
						localRange.start=instIdMap.find(&(*BB->begin()))->second;
						alreadyLiveIn=true;
					}
				}
			}
			blockState.visited=false;
			if(blockState.liveOut)
				localRange.end=instIdMap.find(&(*BB->rbegin()))->second+1;
			ranges[localRange.start]=localRange.end;
		}
		// Construct the vector uf use ranges
		LiveRange& allocaRanges=allocaLiveRanges[alloca];
		for(auto& it: ranges)
		{
			// Extend the previous range if possible
			if(!allocaRanges.empty() && allocaRanges.back().end==it.first)
				allocaRanges.back().end=it.second;
			else
				allocaRanges.push_back(LiveRangeChunk(it.first,it.second));
		}
	}
}

/*
	Returns the set of instruction which access memory derived from the passed Alloca
*/
std::unordered_set<Instruction*> Registerize::gatherDerivedMemoryAccesses(AllocaInst* rootI)
{
	SmallVector<Use*, 10> allUses;
	for(Use& U: rootI->uses())
		allUses.push_back(&U);

	bool escapes = false;
	// NOTE: allUses.size() will grow over time, that's fine
	for(uint32_t i=0;i<allUses.size();i++)
	{
		Use* U = allUses[i];
		Instruction* I = cast<Instruction>(U->getUser());
		switch(I->getOpcode())
		{
			case Instruction::BitCast:
			case Instruction::GetElementPtr:
			{
				// Check derived uses
				for(Use& U: I->uses())
					allUses.push_back(&U);
				break;
			}
			case Instruction::Store:
			{
				// If we are storing away one of the values, it escape
				// NOTE: Operand 0 is the value
				if (U->getOperandNo() == 0)
					escapes = true;
				break;
			}
			case Instruction::Load:
			{
				// Loads are fine
				break;
			}
			case Instruction::Call:
			{
				const CallInst* CI = cast<CallInst>(I);
				const Function* F = CI->getCalledFunction();
				// Lifetime intrinsics are ok
				if(F && (F->getIntrinsicID()==Intrinsic::lifetime_start ||
					F->getIntrinsicID()==Intrinsic::lifetime_end))
				{
					break;
				}
				// This escapes, unless the argument is flagged as nocapture
				//NOTE: Parameter attributes start at index 1
				if(F && F->getAttributes().hasAttribute(U->getOperandNo()+1, Attribute::NoCapture))
					break;
				escapes = true;
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
	std::unordered_set<Instruction*> ret;
	for(const Use* U: allUses)
	{
		Instruction* userI=cast<Instruction>(U->getUser());
		// Skip instruction which only touch the pointer and not the actual memory
		if(isa<BitCastInst>(userI) || isa<GetElementPtrInst>(userI))
			continue;
		ret.insert(userI);
	}
	return ret;
}

bool Registerize::doUpAndMarkForAlloca(AllocaBlocksState& blocksState, const std::unordered_set<Instruction*>& allUses,
				RangeChunksTy& allocaRanges, const InstIdMapTy& instIdMap, BasicBlock* BB, AllocaInst* alloca)
{
	AllocaBlockState& blockState=blocksState[BB];
	// We have encountered this block before so we are inside a loop.
	if (blockState.visited)
	{
		// If the inLoop flag is set we need to set all pending blocks to live-out
		if(blockState.inLoop)
		{
			blocksState.markAndFlushPendingBlocks();
			return true;
		}
		return false;
	}
	// Even if not in a loop we may have already visited this block from some other path
	if (blockState.liveOut)
	{
		blocksState.markAndFlushPendingBlocks();
		return true;
	}
	// Or we could now already that the alloca is not live out from this block
	if (blockState.notLiveOut)
		return false;
	blockState.visited=true;
	uint32_t firstUseId=0xffffffff;
	bool alreadyLiveIn=false;
	bool onlyLocal=false;
	// We only need to look for the earliest use, since the ones in the middle are inside the range anyway
	for(Instruction& I: *BB)
	{
		if(allUses.count(&I))
		{
			// We have found a use inside a predecessor block
			// Flag the alloca as live-out
			blockState.liveOut=true;
			assert(instIdMap.count(&I));
			firstUseId=instIdMap.find(&I)->second+1;
			if(IntrinsicInst* II=dyn_cast<IntrinsicInst>(&I))
			{
				if(II->getIntrinsicID()==Intrinsic::lifetime_start)
					onlyLocal=true;
			}
			blocksState.markAndFlushPendingBlocks();
			break;
		}
	}
	if(!onlyLocal)
	{
		// Add this block to the pending list
		blocksState.pendingBlocks.push_back(BB);
		for(::pred_iterator it=pred_begin(BB);it!=pred_end(BB);++it)
		{
			bool predecessorUse=doUpAndMarkForAlloca(blocksState, allUses, allocaRanges, instIdMap, *it, alloca);
			// If the alloca is used in a predecessor extend the range from the beginning of the block
			if(predecessorUse && !alreadyLiveIn)
			{
				assert(blockState.liveOut);
				firstUseId=instIdMap.find(&(*BB->begin()))->second;
				alreadyLiveIn=true;
			}
		}
		if(!blocksState.pendingBlocks.empty())
		{
			assert(blocksState.pendingBlocks.back()==BB);
			blocksState.pendingBlocks.pop_back();
		}
	}
	blockState.visited=false;
	if(blockState.liveOut)
	{
		// Create a range from the computed first use to the end of the block
		allocaRanges[firstUseId]=instIdMap.find(&(*BB->rbegin()))->second+1;
		return true;
	}
	else
	{
		// Now we know that the alloca is not used in this block or above. Cache this information.
		blockState.notLiveOut=true;
		return false;
	}
}

void Registerize::invalidateFunction(llvm::Function& F)
{
	for(const llvm::BasicBlock& BB: F)
	{
		for(const llvm::Instruction& I: BB)
		{
			// It's safe to delete non existing keys
			registersMap.erase(&I);
			if(const AllocaInst* AI=dyn_cast<AllocaInst>(&I))
				allocaLiveRanges.erase(AI);
		}
	}
}

ModulePass* createRegisterizePass(bool NoRegisterize)
{
	return new Registerize(NoRegisterize);
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(Registerize, "Registerize", "Allocate stack registers for each virtual register",
			false, false)
INITIALIZE_PASS_END(Registerize, "Registerize", "Allocate stack registers for each virtual register",
			false, false)

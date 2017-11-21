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
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

// Uncomment the following line to enable very verbose debugging
// #define VERBOSEDEBUG

STATISTIC(NumRegisters, "Total number of registers allocated to functions");

namespace cheerp {

char Registerize::ID = 0;

void Registerize::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	llvm::Pass::getAnalysisUsage(AU);
}

bool Registerize::runOnModule(Module & M)
{
	for (Function& F: M)
	{
		// In asm.js functions we distinguish between FLOAT and DOUBLE
		computeLiveRangeForAllocas(F);
	}
	return false;
}

void Registerize::assignRegisters(Module & M, cheerp::PointerAnalyzer& PA)
{
	assert(!RegistersAssigned);
	for (Function& F: M)
		assignRegistersToInstructions(F, PA);
#ifndef NDEBUG
	RegistersAssigned = true;
#endif
}

const char* Registerize::getPassName() const
{
	return "CheerpRegisterize";
}

uint32_t Registerize::getRegisterId(const llvm::Instruction* I) const
{
	assert(RegistersAssigned);
	assert(registersMap.count(I));
	uint32_t regId = registersMap.find(I)->second;
	if(!edgeContext.isNull())
	{
		auto it=edgeRegistersMap.find(InstOnEdge(edgeContext.fromBB, edgeContext.toBB, regId));
		if (it!=edgeRegistersMap.end())
			return it->second;
	}
	return regId;
}

uint32_t Registerize::getRegisterIdForEdge(const llvm::Instruction* I, const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB) const
{
	assert(registersMap.count(I));
	uint32_t regId = registersMap.find(I)->second;
	auto it=edgeRegistersMap.find(InstOnEdge(fromBB, toBB, regId));
	if (it!=edgeRegistersMap.end())
		return it->second;
	return regId;
}

uint32_t Registerize::getSelfRefTmpReg(const llvm::Instruction* I, const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB) const
{
	assert(registersMap.count(I));
	uint32_t regId = registersMap.find(I)->second;
	auto it = selfRefRegistersMap.find(InstOnEdge(fromBB, toBB, regId));
	assert(it != selfRefRegistersMap.end());
	return it->second;
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
	assert(!range.back().empty());
}

void Registerize::assignRegistersToInstructions(Function& F, cheerp::PointerAnalyzer & PA)
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
				if (isInlineable(I, PA) || I.getType()->isVoidTy() || I.use_empty())
					continue;
				registersMap[&I]=nextRegister++;
			}
		}
	}
	else
	{
		InstIdMapTy instIdMap;
		AllocaSetTy allocaSet;
		// Assign sequential identifiers to all instructions
		assignInstructionsIds(instIdMap, F, allocaSet, &PA);
		// First, build live ranges for all instructions
		LiveRangesTy liveRanges=computeLiveRanges(F, instIdMap, PA);
		// Assign each instruction to a virtual register
		uint32_t registersCount = assignToRegisters(F, instIdMap, liveRanges, PA);
		NumRegisters += registersCount;
		// To debug we need to know the ranges for each instructions and the assigned register
		DEBUG(if (registersCount) dbgs() << "Function " << F.getName() << " needs " << registersCount << " registers\n");
		// Very verbose debugging below, activate if needed
#ifdef VERBOSEDEBUG
		for(auto it: liveRanges)
		{
			if(it.first->getParent()->getParent() != &F)
				continue;
			dbgs() << "Instruction " << *it.first << " alive in ranges ";
			for(const Registerize::LiveRangeChunk& chunk: it.second.range)
				dbgs() << '[' << chunk.start << ',' << chunk.end << ')';
			dbgs() << "\n";
			dbgs() << "\tMapped to register " << registersMap[it.first] << "\n";
		}
#endif
	}
}

void Registerize::computeLiveRangeForAllocas(Function& F)
{
	assert(!RegistersAssigned);
	if (F.empty())
		return;
	if (NoRegisterize)
	{
		for(BasicBlock& BB: F)
		{
			for(Instruction& I: BB)
			{
				// Initialize an empty live range, which means that the alloca escapes analysis
				if(AllocaInst* AI=dyn_cast<AllocaInst>(&I))
					allocaLiveRanges[AI];
			}
		}
	}
	else
	{
		AllocaSetTy allocaSet;
		InstIdMapTy instIdMap;
		// Assign sequential identifiers to all instructions
		assignInstructionsIds(instIdMap, F, allocaSet, NULL);
		// Now compute live ranges for alloca memory which is not in SSA form
		computeAllocaLiveRanges(allocaSet, instIdMap);
		// Very verbose debugging below, activate if needed
#ifdef VERBOSEDEBUG
		for(auto it: allocaLiveRanges)
		{
			if(it.first->getParent()->getParent() != &F)
				continue;
			dbgs() << "Alloca " << *it.first << " alive in ranges ";
			for(const Registerize::LiveRangeChunk& chunk: it.second)
				dbgs() << '[' << chunk.start << ',' << chunk.end << ')';
			dbgs() << "\n";
		}
#endif
	}
}

Registerize::LiveRangesTy Registerize::computeLiveRanges(Function& F, const InstIdMapTy& instIdMap, cheerp::PointerAnalyzer & PA)
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
	// Remove verbose debugging output
#ifdef VERBOSEDEBUG
	for(auto it: blocksState)
	{
		llvm::errs() << "Block:\n" << *it.first << "\n";
		llvm::errs() << "Inst out:\n";
		for(Instruction* I: it.second.outSet)
			llvm::errs() << *I << "\n";
	}
#endif
	// Depth first analysis of blocks, starting from the entry block
	LiveRangesTy liveRanges(instIdMap);
	dfsLiveRangeInBlock(blocksState, liveRanges, instIdMap, F.getEntryBlock(), PA, 1, 1);
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

void Registerize::assignInstructionsIds(InstIdMapTy& instIdMap, const Function& F, AllocaSetTy& allocaSet, const PointerAnalyzer* PA)
{
	SmallVector<const BasicBlock*, 4> bbQueue;
	std::set<const BasicBlock*> doneBlocks;
	uint32_t nextIndex = 1;

	bbQueue.push_back(&F.getEntryBlock());
	while(!bbQueue.empty())
	{
		const BasicBlock* BB = bbQueue.pop_back_val();
		if(!doneBlocks.insert(BB).second)
			continue;

		for (const Instruction& I: *BB)
		{
			// Take our chance to store away all alloca, they are registerized using non-SSA logic
			if (isa<AllocaInst>(I))
				allocaSet.push_back(cast<AllocaInst>(&I));
			uint32_t thisIndex = nextIndex++;
			// SPLIT_REGULAR inst consumes 2 indexes
			if(PA && I.getType()->isPointerTy() && PA->getPointerKind(&I) == SPLIT_REGULAR)
				thisIndex = nextIndex++;
			instIdMap[&I]=thisIndex;
		}

		const TerminatorInst* term=BB->getTerminator();
		uint32_t numSuccessors = term->getNumSuccessors();
		for(uint32_t i=0;i<numSuccessors;i++)
		{
			// Push them from the last to the first to be consistent with dfsLiveRangeInBlock
			BasicBlock* succ=term->getSuccessor(numSuccessors - i - 1);
			if (doneBlocks.count(succ))
				continue;
			bbQueue.push_back(succ);
		}
	}
}

uint32_t Registerize::dfsLiveRangeInBlock(BlocksState& blocksState, LiveRangesTy& liveRanges, const InstIdMapTy& instIdMap,
					BasicBlock& BB, cheerp::PointerAnalyzer & PA, uint32_t nextIndex, uint32_t codePathId)
{
	BlockState& blockState=blocksState[&BB];
	if(blockState.completed)
		return nextIndex;
	// Iterate over instructions
	// For each instruction start an empty range
	// For each use used operands extend their live ranges to here
	for (Instruction& I: BB)
	{
		assert(liveRanges.count(&I)==0);
		assert(instIdMap.count(&I));
		uint32_t thisIndex = instIdMap.find(&I)->second;
		nextIndex = thisIndex + 1;
		bool splitRegularDest = false;
		// SPLIT_REGULAR pointers keep alive the register until after the instruction. Calls are an exception as the offset is stored in a global.
		if(I.getType()->isPointerTy() && PA.getPointerKind(&I) == SPLIT_REGULAR)
		{
			nextIndex++;
			CallInst* CI = dyn_cast<CallInst>(&I);
			if(!CI || (CI->getCalledFunction() && CI->getCalledFunction()->getIntrinsicID()==Intrinsic::cheerp_downcast))
				splitRegularDest = true;
		}
		// Inlineable instructions extends the life of the not-inlineable instructions they use.
		// This happens inside extendRangeForUsedOperands.
		if (isInlineable(I, PA))
			continue;
		// Void instruction and instructions without uses do not need any lifetime computation
		if (!I.getType()->isVoidTy() && !I.use_empty())
		{
			InstructionLiveRange& range=liveRanges.emplace(&I,
				InstructionLiveRange(codePathId)).first->second;
			range.range.push_back(LiveRangeChunk(thisIndex, thisIndex));
		}
		// Operands of PHIs are declared as live out from the source block.
		// This is handled below.
		if (isa<PHINode>(I))
			continue;
		extendRangeForUsedOperands(I, liveRanges, PA, thisIndex, codePathId, splitRegularDest);
	}
	// Extend the live range of live-out instrution to the end of the block
	uint32_t endOfBlockIndex=nextIndex;
	TerminatorInst* term=BB.getTerminator();
	for(Instruction* outLiveInst: blockState.outSet)
	{
		// If inlineable we need to extend the life of the not-inlineable operands
		if (isInlineable(*outLiveInst, PA))
		{
			// We don't care about splitRegularDest, the point is to extend to the end of the block
			extendRangeForUsedOperands(*outLiveInst, liveRanges, PA, endOfBlockIndex, codePathId, /*splitRegularDest*/false);
		}
		else
		{
			InstructionLiveRange& range=liveRanges.find(outLiveInst)->second;
			range.addUse(codePathId, endOfBlockIndex);
		}
	}
	blockState.completed=true;
	// Run on successor blocks
	for(uint32_t i=0;i<term->getNumSuccessors();i++)
	{
		BasicBlock* succ=term->getSuccessor(i);
		uint32_t newNextIndex=dfsLiveRangeInBlock(blocksState, liveRanges, instIdMap, *succ, PA, nextIndex, codePathId);
		// If any new instruction has ben added (i.e. nextIndex if changed) update codePathId
		if(newNextIndex!=nextIndex)
			codePathId = newNextIndex;
		nextIndex = newNextIndex;
	}
	return nextIndex;
}

void Registerize::extendRangeForUsedOperands(Instruction& I, LiveRangesTy& liveRanges, cheerp::PointerAnalyzer& PA,
						uint32_t thisIndex, uint32_t codePathId, bool splitRegularDest)
{
	for(Value* op: I.operands())
	{
		Instruction* usedI = dyn_cast<Instruction>(op);
		// Uses which are not instruction do not require live range analysis
		if(!usedI)
			continue;
		// Recursively traverse inlineable operands
		if(isInlineable(*usedI, PA))
			extendRangeForUsedOperands(*usedI, liveRanges, PA, thisIndex, codePathId, splitRegularDest);
		else
		{
			assert(liveRanges.count(usedI));
			InstructionLiveRange& range=liveRanges.find(usedI)->second;
			if(splitRegularDest && usedI->getType()->isPointerTy() && PA.getPointerKind(usedI) == SPLIT_REGULAR)
				thisIndex++;
			if(codePathId!=thisIndex)
				range.addUse(codePathId, thisIndex);
		}
	}
}

uint32_t Registerize::assignToRegisters(Function& F, const InstIdMapTy& instIdMap, const LiveRangesTy& liveRanges, const PointerAnalyzer& PA)
{
	llvm::SmallVector<RegisterRange, 4> registers;
	// First try to assign all PHI operands to the same register as the PHI itself
	for(auto it: liveRanges)
	{
		Instruction* I=it.first;
		if(!isa<PHINode>(I))
			continue;
		handlePHI(*I, liveRanges, registers, PA);
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
		bool asmjs = I->getParent()->getParent()->getSection()==StringRef("asmjs");
		uint32_t chosenRegister=findOrCreateRegister(registers, range, getRegKindFromType(I->getType(), asmjs), cheerp::needsSecondaryName(I, PA));
		registersMap[I] = chosenRegister;
	}
	// Assign registers for temporary values required to break loops in PHIs
	class RegisterizePHIHandler: public EndOfBlockPHIHandler
	{
	public:
		RegisterizePHIHandler(const BasicBlock* f, const BasicBlock* t, Registerize& r, llvm::SmallVector<RegisterRange, 4>& rs, const InstIdMapTy& i, const PointerAnalyzer& _PA):
			EndOfBlockPHIHandler(_PA), fromBB(f), toBB(t), registerize(r), PA(_PA),registers(rs),  instIdMap(i)
		{
			// We can't use twice the same tmpphi in the same edge
			// TODO: It could be possible in some cases but we need to keep track of subgroups of dependencies
			usedRegisters.resize(registers.size(), false);
		}
	private:
		const BasicBlock* fromBB;
		const BasicBlock* toBB;
		Registerize& registerize;
		const PointerAnalyzer& PA;
		llvm::SmallVector<RegisterRange, 4>& registers;
		const InstIdMapTy& instIdMap;
		std::vector<bool> usedRegisters;
		uint32_t assignTempReg(uint32_t regId, Registerize::REGISTER_KIND kind, bool needsSecondaryName)
		{
			for(unsigned i=0;i<registers.size();i++)
			{
				if(registers[i].info.regKind != kind)
					continue;
				if(usedRegisters[i])
					continue;
				// usedRegisters will skip all registers already assigned or used by PHIs
				// we still need to make sure we are not interfering with registers which are
				// alive across the whole range
				assert(instIdMap.count(&(*toBB->begin())));
				uint32_t beginOfToBlock = instIdMap.find(&(*toBB->begin()))->second;
				if(registers[i].range.doesInterfere(beginOfToBlock))
					continue;
				// We can use this register for the tmpphi, make sure we don't use it twice
				usedRegisters[i] = true;
				registers[i].info.needsSecondaryName |= needsSecondaryName;
				return i;
			}
			// Create a register which will have an empty live range
			// It is not a problem since we mark it as used in the block
			uint32_t chosenReg = registers.size();
			registers.push_back(RegisterRange(LiveRange(), kind, needsSecondaryName));
			usedRegisters.push_back(true);
			return chosenReg;
		}
		void handleRecursivePHIDependency(const Instruction* incoming) override
		{
			bool asmjs = incoming->getParent()->getParent()->getSection() == StringRef("asmjs");
			assert(registerize.registersMap.count(incoming));
			uint32_t regId=registerize.registersMap.find(incoming)->second;
			Registerize::REGISTER_KIND phiKind = registerize.getRegKindFromType(incoming->getType(), asmjs);
			uint32_t chosenReg = assignTempReg(regId, phiKind, cheerp::needsSecondaryName(incoming, PA));
			registerize.edgeRegistersMap.insert(std::make_pair(InstOnEdge(fromBB, toBB, regId), chosenReg));
		}
		void handlePHI(const Instruction* phi, const Value* incoming, bool selfReferencing) override
		{
			// Provide temporary regs for the offset part of SPLIT_REGULAR PHIs that reference themselves
			if(!selfReferencing)
				return;
			assert(cheerp::needsSecondaryName(phi, PA));
			assert(registerize.registersMap.count(phi));
			uint32_t regId=registerize.registersMap.find(phi)->second;
			// Check if there is already a tmp PHI for this PHI, if so it is not really self referencing
			if(registerize.edgeRegistersMap.count(InstOnEdge(fromBB, toBB, regId)))
				return;
			uint32_t chosenReg = assignTempReg(regId, Registerize::INTEGER, false);
			registerize.selfRefRegistersMap.insert(std::make_pair(InstOnEdge(fromBB, toBB, regId), chosenReg));
		}
		void setRegisterUsed(uint32_t regId) override
		{
			assert(regId < usedRegisters.size());
			usedRegisters[regId] = true;
		}
	};
#ifndef NDEBUG
	// Temporarily set and then reset this debug flag, EndOfBlockPHIHandler uses registerize
	// and we have actually already assigned all registers for the instructions
	RegistersAssigned = true;
#endif
	for (const BasicBlock & bb : F)
	{
		const TerminatorInst* term=bb.getTerminator();
		for(uint32_t i=0;i<term->getNumSuccessors();i++)
		{
			const BasicBlock* succBB=term->getSuccessor(i);
			RegisterizePHIHandler(&bb, succBB, *this, registers, instIdMap, PA).runOnEdge(*this, &bb, succBB);
		}
	}
#ifndef NDEBUG
	RegistersAssigned = false;
#endif
	// Populate the final register list for the function
	std::vector<RegisterInfo>& regsInfo = registersForFunctionMap[&F];
	regsInfo.reserve(registers.size());
	for(unsigned int i=0;i<registers.size();i++)
		regsInfo.push_back(registers[i].info);
	return registers.size();
}

void Registerize::handlePHI(Instruction& I, const LiveRangesTy& liveRanges, llvm::SmallVector<RegisterRange, 4>& registers, const PointerAnalyzer& PA)
{
	bool asmjs = I.getParent()->getParent()->getSection()==StringRef("asmjs");
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
			if(!usedI || isInlineable(*usedI, PA))
				continue;
			assert(liveRanges.count(usedI));
			if(registersMap.count(usedI)==0)
				continue;
			uint32_t operandRegister=registersMap[usedI];
			if(addRangeToRegisterIfPossible(registers[operandRegister], PHIrange,
							getRegKindFromType(usedI->getType(), asmjs),
							cheerp::needsSecondaryName(&I, PA)))
			{
				chosenRegister=operandRegister;
				break;
			}
		}
	}
	// If a register has not been chosen yet, find or create a new one
	if(chosenRegister==0xffffffff)
		chosenRegister=findOrCreateRegister(registers, PHIrange, getRegKindFromType(I.getType(), asmjs), cheerp::needsSecondaryName(&I, PA));
	registersMap[&I]=chosenRegister;
	// Iterate again on the operands and try to map as many as possible into the same register
	for(Value* op: I.operands())
	{
		Instruction* usedI=dyn_cast<Instruction>(op);
		if(!usedI || isInlineable(*usedI, PA))
			continue;
		assert(liveRanges.count(usedI));
		// Skip already assigned operands
		if(registersMap.count(usedI))
			continue;
		const InstructionLiveRange& opRange=liveRanges.find(usedI)->second;
		bool spaceFound=addRangeToRegisterIfPossible(registers[chosenRegister], opRange,
								getRegKindFromType(usedI->getType(), asmjs),
								cheerp::needsSecondaryName(usedI, PA));
		if (spaceFound)
		{
			// Update the mapping
			registersMap[usedI]=chosenRegister;
		}
	}
}

uint32_t Registerize::findOrCreateRegister(llvm::SmallVector<RegisterRange, 4>& registers, const InstructionLiveRange& range,
						REGISTER_KIND kind, bool needsSecondaryName)
{
	for(uint32_t i=0;i<registers.size();i++)
	{
		if(addRangeToRegisterIfPossible(registers[i], range, kind, needsSecondaryName))
			return i;
	}
	// Create a new register with the range of the current instruction already used
	registers.push_back(RegisterRange(range.range, kind, needsSecondaryName));
	return registers.size()-1;
}

Registerize::REGISTER_KIND Registerize::getRegKindFromType(const llvm::Type* t, bool asmjs) const
{
	if(t->isIntegerTy())
		return INTEGER;
	// We distinguish between FLOAT and DOUBLE only in asm.js functions
	else if(asmjs && useFloats && t->isFloatTy())
		return FLOAT;
	else if(t->isFloatingPointTy())
		return DOUBLE;
	// Pointers in asm.js are just integers
	else if(asmjs)
		return INTEGER;
	// Pointers to asm.js types are just integers
	else if(TypeSupport::isAsmJSPointer(t))
		return INTEGER;
	// NOTE: the Void type is considered an OBJECT
	else
		return OBJECT;
}

bool Registerize::LiveRange::doesInterfere(uint32_t id) const
{
	for(const LiveRangeChunk c: *this)
	{
		if(c.start <= id && c.end > id)
			return true;
	}
	return false;
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
						REGISTER_KIND kind, bool needsSecondaryName)
{
	if(regRange.info.regKind!=kind)
		return false;
	if(regRange.range.doesInterfere(liveRange.range))
		return false;
	regRange.range.merge(liveRange.range);
	regRange.info.needsSecondaryName |= needsSecondaryName;
	return true;
}

void Registerize::computeAllocaLiveRanges(AllocaSetTy& allocaSet, const InstIdMapTy& instIdMap)
{
	for(const AllocaInst* alloca: allocaSet)
	{
		AllocaBlocksState blocksState;
		RangeChunksTy ranges;
		// For each alloca gather all uses and derived uses
		InstructionSetOrderedByID allUses=gatherDerivedMemoryAccesses(alloca, instIdMap);
		if(allUses.empty())
		{
			// Initialize an empty live range to signal that no analysis is possible
			allocaLiveRanges[alloca];
			continue;
		}

		// The instructions are ordered by their ID, so the instruction in the same block are consecutive
		// Build the range containing the uses directly inside the block.
		BasicBlock* currentBlock = NULL;
		LiveRangeChunk localRange(0, 0);
		for(Instruction* I: allUses)
		{
			assert(instIdMap.count(I));
			uint32_t instId=instIdMap.find(I)->second;
			// If the current use is the first of a new block
			if(I->getParent() != currentBlock)
			{
				// Save the range if it is valid when we change block
				if(!localRange.empty())
					ranges[localRange.start] = localRange.end;

				localRange = LiveRangeChunk(instId, instId+1);

				// Set the initial state for this block
				currentBlock = I->getParent();
				AllocaBlockState& blockState=blocksState[currentBlock];
				blockState.hasUse=true;
				// If the first instruction in the block is lifetime_start we need to set the notLiveIn flag
				if(IntrinsicInst* II=dyn_cast<IntrinsicInst>(I))
				{
					if(II->getIntrinsicID()==Intrinsic::lifetime_start)
						blockState.notLiveIn = true;
				}
			}
			else
			{
				assert(!localRange.empty());
				assert(instId+1 > localRange.end);
				localRange.end=instId+1;
				// If we find a lifetime_end intrinsic we have to close the range and start again
				if(IntrinsicInst* II=dyn_cast<IntrinsicInst>(I))
				{
					if(II->getIntrinsicID()==Intrinsic::lifetime_end)
					{
						ranges[localRange.start] = localRange.end;
						localRange = LiveRangeChunk(0, 0);
						currentBlock = NULL;
					}
				}
			}
		}
		// Add the last range if necessary
		if(!localRange.empty())
			ranges[localRange.start] = localRange.end;

		// Now we have marked all blocks which contains a use of the alloca with:
		// 1) The hasUse flag
		// 2) The notLiveIn if predecessors should not be checked
		// For each block now we have to find out if the range should be extended
		// till the first and/or the last instruction of the block
		SmallVector<BasicBlock*, 4> startingBlocksList;
		for(auto& it: blocksState)
			startingBlocksList.push_back(it.first);

		for(BasicBlock* BB: startingBlocksList)
		{
			// Explore the graph of the predecessors
			// 1) If a use is found in the predecessors, set the liveIn flag
			// 2) If no use is found in the predecessors, set the notLiveIn flag
			// 3) If there is a loop in the predecessors, the return value will be the depth of the block
			//    which is the lowest one that can make the pending blocks live.
			// 3.1) If the returned value >= the current upAndMarkId, there is no hope to
			//      make the pending blocks alive anymore
			// 3.2) If the returned value is lower than the current upAndMarkId, we have to propagate it
			assert(blocksState.pendingBlocks.empty());
			// We keep track of the explored depth starting from USE_UNKNOWN, this is useful since
			// any value >= USE_UNKNOWN means: "Unknown and depending on this depth of exploration"
			UpAndMarkAllocaState state = doUpAndMarkForAlloca(blocksState, BB, USE_UNKNOWN);
			if(state == USE_FOUND)
				blocksState.markPendingBlocksAsLiveOut(0);
			else
				blocksState.markPendingBlocksAsNotLiveOut(0);
		}

		// Now we have all blocks correctly flagged, use the information to extend range as required
		for(auto& it: blocksState)
		{
			const AllocaBlockState& blockState = it.second;
			assert(!(blockState.liveOut && blockState.notLiveOut));
			assert(!(blockState.liveIn && blockState.notLiveIn));
			BasicBlock* BB = it.first;
			// Skip blocks which are not reachable
			if(pred_begin(BB) == pred_end(BB) && BB!=&BB->getParent()->getEntryBlock())
				continue;
			assert(instIdMap.count(&(*BB->begin())));
			assert(instIdMap.count(&(*BB->rbegin())));
			uint32_t firstIdInBlock = instIdMap.find(&(*BB->begin()))->second;
			uint32_t lastIdInBlock = instIdMap.find(&(*BB->rbegin()))->second+1;
			// If the alloca is used in a predecessor extend the range from the beginning of the block
			if(blockState.hasUse)
			{
				if(blockState.liveIn)
				{
					auto firstUseIterator = ranges.lower_bound(firstIdInBlock);
					assert(firstUseIterator != ranges.end() && firstUseIterator->first < lastIdInBlock);
					// If there already is a range contained in this block, make a new one from the beginning to the first use
					if(firstUseIterator->first != firstIdInBlock)
						ranges[firstIdInBlock] = firstUseIterator->first;
				}
				if(blockState.liveOut)
				{
					auto lastUseIterator = ranges.upper_bound(lastIdInBlock-1);
					assert(lastUseIterator != ranges.begin());
					--lastUseIterator;
					assert(lastUseIterator->first >= instIdMap.find(&(*BB->begin()))->second);
					// Extend the last range of the block to the last instruction
					lastUseIterator->second = lastIdInBlock+1;
				}
			}
			else if(blockState.liveIn || blockState.liveOut)
			{
				// Make a new range from the first to the last instruction of the block
				ranges[firstIdInBlock] = lastIdInBlock;
			}
		}

		// Construct the vector of use ranges
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
Registerize::InstructionSetOrderedByID Registerize::gatherDerivedMemoryAccesses(const AllocaInst* rootI, const InstIdMapTy& instIdMap)
{
	SmallVector<const Use*, 10> allUses;
	for(const Use& U: rootI->uses())
		allUses.push_back(&U);

	bool escapes = false;
	// NOTE: allUses.size() will grow over time, that's fine
	for(uint32_t i=0;i<allUses.size();i++)
	{
		const Use* U = allUses[i];
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
	InstructionSetOrderedByID ret(instIdMap);
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

Registerize::UpAndMarkAllocaState Registerize::doUpAndMarkForAlloca(AllocaBlocksState& blocksState, BasicBlock* BB, uint32_t upAndMarkId)
{
	AllocaBlockState& blockState=blocksState[BB];
	if(upAndMarkId > USE_UNKNOWN)
	{
		// Use cached information from previous runs if available
		if (blockState.liveOut)
			return USE_FOUND;
		else if (blockState.notLiveOut)
			return USE_NOT_FOUND;
	}

	// We have encountered this block before so we are inside a loop.
	if (blockState.upAndMarkId)
	{
		// We are looping over a block which contains a use
		if(blockState.hasUse)
		{
			blockState.liveOut = true;
			return USE_FOUND;
		}
		// We return the upAndMarkId for the block, to signal that the result is unknown
		// and depends at least from blockState.upAndMarkId depth level in the graph.
		// When we reach a block with a lower upAndMarkId than this one we are sure that there were
		// no uses in the predecessors of this block
		// blockState.upAndMarkId is guaranteed to be >= USE_UNKNOWN
		return UpAndMarkAllocaState(blockState.upAndMarkId);
	}

	// Keep track of the current size of the pending blocks list, we don't want to interfere with blocks already on the list
	uint32_t currentPendingSize = blocksState.pendingBlocks.size();

	UpAndMarkAllocaState finalState(blockState.hasUse ? USE_FOUND : USE_NOT_FOUND);
	// If the notLiveIn flag the variable is reset at the beginning of the block and we don't have to check predecessors
	if(!blockState.notLiveIn)
	{
		// Check predecessors now

		// Flag this block as being visited using the current upAndMarkId. This is always >= than USE_UNKNOWN
		blockState.upAndMarkId = upAndMarkId;
		// By default we want to reset upAndMarkId of the block to 0, which means "not being visited".
		// Unless we find a USE_UNKNOWN, in which case the block is left in the pending list with
		// upAndMarkId set to lowest upAndMarkId of the blocks it depends on.
		// This is used to avoid exploring again a part of the graph which we know will not provide a definite value
		// until we get back to the block it depends on.
		uint32_t upAndMarkIdResetValue = 0;

		UpAndMarkAllocaState predecessorsState(USE_NOT_FOUND);
		for(::pred_iterator it=pred_begin(BB);it!=pred_end(BB);++it)
		{
			uint32_t pendingSizeBeforePredecessor = blocksState.pendingBlocks.size();
			(void)pendingSizeBeforePredecessor;
			UpAndMarkAllocaState predecessorState = doUpAndMarkForAlloca(blocksState, *it, upAndMarkId+1);
			if(predecessorState.state < USE_UNKNOWN)
				assert(blocksState.pendingBlocks.size() == pendingSizeBeforePredecessor);
			// If the returned predecessorState is higher than upAndMarkId it is also USE_UNKNOWN,
			// and the lowest block it depends on has been already explored, so there is no hope anymore
			// of finding a use
			if(predecessorState.state > upAndMarkId)
			{
				blocksState.discardPendingBlocks(currentPendingSize);
				continue;
			}
			predecessorsState |= predecessorState;
		}
		// We have a USE_UNKNOWN depending on this or earlier block
		// We have already ignored USE_UNKNOWNs depending on later blocks
		if(predecessorsState.state >= USE_UNKNOWN)
		{
			if(predecessorsState.state < upAndMarkId)
			{
				// If depending on earlier blocks there is still hope that a use will be found somewhere
				// Add this block to the pending list
				assert(find(blocksState.pendingBlocks.begin(),blocksState.pendingBlocks.end(),BB)==blocksState.pendingBlocks.end());
				blocksState.pendingBlocks.push_back(BB);
				upAndMarkIdResetValue = predecessorsState.state;
			}
			else
			{
				// If equal to upAndMarkId, this block was the last hope to find any use.
				// Convert to USE_NOT_FOUND, if this block had uses it would have returned USE_FOUND during the visit.
				assert(predecessorsState.state == upAndMarkId);
				predecessorsState = UpAndMarkAllocaState(USE_NOT_FOUND);
			}
		}
		// Please note that USE_NOT_FOUND could have been converted above from a USE_UNKNOWN depending on this block.
		if(predecessorsState == USE_NOT_FOUND)
		{
			// We definitely have no use in the predecessors
			blocksState.markPendingBlocksAsNotLiveOut(currentPendingSize);
			blockState.notLiveIn = true;
		}
		else if(predecessorsState == USE_FOUND)
		{
			// We definitely have a use in the predecessors, so this block is also liveIn.
			blocksState.markPendingBlocksAsLiveOut(currentPendingSize);
			blockState.liveIn = true;
		}

		// Combine the predecessorsState with the local state to obtain the final state
		finalState |= predecessorsState;

		// Reset the upAndMarkId for the block as required
		blockState.upAndMarkId = upAndMarkIdResetValue;
	}

	// Final checks. Discard all pending blocks if we have a definite result
	if(finalState == USE_FOUND && upAndMarkId > USE_UNKNOWN)
	{
		// If upAndMarkId has been increment at least once, this block is a predecessor of a block with uses, so it's liveOut
		blockState.liveOut = true;
		blocksState.markPendingBlocksAsLiveOut(currentPendingSize);
	}
	else if(finalState == USE_NOT_FOUND)
	{
		// Now we know that the alloca is not used in this block or above. Cache this information.
		blockState.notLiveOut = true;
		assert(blocksState.pendingBlocks.size() == currentPendingSize);
	}

	return finalState;
}

void Registerize::invalidateLiveRangeForAllocas(llvm::Function& F)
{
	assert(!RegistersAssigned);
	for(const llvm::BasicBlock& BB: F)
	{
		for(const llvm::Instruction& I: BB)
		{
			// It's safe to delete non existing keys
			if(const AllocaInst* AI=dyn_cast<AllocaInst>(&I))
				allocaLiveRanges.erase(AI);
		}
	}
}

ModulePass* createRegisterizePass(bool useFloats, bool NoRegisterize)
{
	return new Registerize(useFloats, NoRegisterize);
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(Registerize, "Registerize", "Allocate stack registers for each virtual register",
			false, false)
INITIALIZE_PASS_END(Registerize, "Registerize", "Allocate stack registers for each virtual register",
			false, false)

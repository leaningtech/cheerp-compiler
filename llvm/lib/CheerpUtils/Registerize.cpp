//===-- Registerize.cpp - Compute live ranges to minimize variables--------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "CheerpRegisterize"
#include "llvm/InitializePasses.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PHIHandler.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/PostDominators.h"
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
	AU.addRequired<LoopInfoWrapperPass>();
	AU.addRequired<DominatorTreeWrapperPass>();
	AU.addRequired<PostDominatorTreeWrapperPass>();
	AU.addPreserved<LoopInfoWrapperPass>();
	AU.addPreserved<DominatorTreeWrapperPass>();
	AU.addPreserved<PostDominatorTreeWrapperPass>();
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

StringRef Registerize::getPassName() const
{
	return "CheerpRegisterize";
}

bool Registerize::hasRegister(const llvm::Instruction* I) const
{
	return registersMap.count(I);
}

uint32_t Registerize::getRegisterId(const llvm::Instruction* I, const EdgeContext& edgeContext) const
{
	assert(RegistersAssigned);
	assert(registersMap.count(I));
	uint32_t regId = registersMap.find(I)->second;
	if(!edgeContext.isNull())
	{
		return edgeRegistersMap.findCurrentRegisterId(regId, edgeContext);
	}
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
		range.extendOrPush(LiveRangeChunk(curCodePath, thisIndex));
		codePathId=curCodePath;
	}
	assert(!range.back().empty());
}

void Registerize::assignRegistersToInstructions(Function& F, cheerp::PointerAnalyzer & PA)
{
	if (F.empty())
		return;
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
	LLVM_DEBUG(if (registersCount) dbgs() << "Function " << F.getName() << " needs " << registersCount << " registers\n");
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

void Registerize::computeLiveRangeForAllocas(const Function& F)
{
	assert(!RegistersAssigned);
	if (F.empty())
		return;
	//We need to cast away the cast since data will be memoized by the (post-)dominator tree builder
	DT = &getAnalysis<DominatorTreeWrapperPass>(const_cast<Function&>(F)).getDomTree();
	PDT = &getAnalysis<PostDominatorTreeWrapperPass>(const_cast<Function&>(F)).getPostDomTree();
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
#ifndef NDEBUG
	bool hasPreds = false;
#endif
	for(::pred_iterator it=pred_begin(BB);it!=pred_end(BB);++it)
	{
#ifndef NDEBUG
		hasPreds = true;
#endif
		BasicBlock* pred=*it;
		BlockState& predBlockState=blocksState[pred];
		if(!predBlockState.isLiveOut(I))
			predBlockState.addLiveOut(I);
		doUpAndMark(blocksState, pred, I);
	}
	assert(hasPreds);
}

void Registerize::assignInstructionsIds(InstIdMapTy& instIdMap, const Function& F, AllocaSetTy& allocaSet, const PointerAnalyzer* PA)
{
	SmallVector<const BasicBlock*, 4> bbQueue;
	llvm::DenseSet<const BasicBlock*> doneBlocks;
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

		const Instruction* term=BB->getTerminator();
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
	Instruction* term=BB.getTerminator();
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
			auto it = liveRanges.find(outLiveInst);
			assert(it != liveRanges.end());
			InstructionLiveRange& range= it->second;
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
	LI = &(getAnalysis<LoopInfoWrapperPass>(F).getLoopInfo());

	llvm::SmallVector<RegisterRange, 4> registers;

	if (RegisterizeLegacy)
	{
		// First try to assign all PHI operands to the same register as the PHI itself
		for(auto it: liveRanges)
		{
			const Instruction* I=it.first;
			if(!isa<PHINode>(I))
				continue;
			handlePHI(*I, liveRanges, registers, PA);
		}
		const bool asmjs = F.getSection()==StringRef("asmjs");
		// Assign a register to the remaining instructions
		for(auto it: liveRanges)
		{
			const Instruction* I=it.first;
			if(isa<PHINode>(I))
				continue;
			InstructionLiveRange& range=it.second;
			// Move on if a register is already assigned
			if(registersMap.count(I))
				continue;
			uint32_t chosenRegister=findOrCreateRegister(registers, range, getRegKindFromType(I->getType(), asmjs), cheerp::needsSecondaryName(I, PA));
			registersMap[I] = chosenRegister;
		}
	}
	else
	{
		RegisterAllocatorInst registerAllocatorInst(F, instIdMap, liveRanges, PA, this);
		registerAllocatorInst.solve();

		registerAllocatorInst.materializeRegisters(registers);
	}

	EdgeContext edgeContext;
	// Assign registers for temporary values required to break loops in PHIs
	class RegisterizePHIHandler: public PHIHandlerUsingTemp
	{
		enum REGISTER_STATUS{FREE=0, USED=-1u};
	public:
		RegisterizePHIHandler(Registerize& r, llvm::SmallVector<RegisterRange, 4>& rs,
				const InstIdMapTy& i, const PointerAnalyzer& _PA, EdgeContext& edgeContext):
			PHIHandlerUsingTemp(_PA, edgeContext), registerize(r), PA(_PA),registers(rs),  instIdMap(i)
		{
			// We should be carefull when using again the same tmpphi in the same edge
			statusRegisters.resize(registers.size(), REGISTER_STATUS::FREE);
		}
	private:
		Registerize& registerize;
		const PointerAnalyzer& PA;
		llvm::SmallVector<RegisterRange, 4>& registers;
		const InstIdMapTy& instIdMap;

		std::vector<uint32_t> statusRegisters;
		uint32_t assignTempReg(uint32_t regId, Registerize::REGISTER_KIND kind, bool needsSecondaryName)
		{
			assert(statusRegisters.at(regId) != REGISTER_STATUS::FREE);
			for(unsigned i=0;i<registers.size();i++)
			{
				if(registers[i].info.regKind != kind)
					continue;
				//registers currently not free could not be used
				if(statusRegisters[i] != REGISTER_STATUS::FREE)
					continue;
				// The check on statusRegisters will skip all registers already assigned or used by PHIs
				// we still need to make sure we are not interfering with registers which are
				// alive across the whole range
				assert(instIdMap.count(&(*edgeContext.toBB->begin())));
				uint32_t beginOfToBlock = instIdMap.find(&(*edgeContext.toBB->begin()))->second;
				assert(statusRegisters[i] == 0);
				if(registers[i].range.doesInterfere(beginOfToBlock))
				{
					statusRegisters[i] = REGISTER_STATUS::USED;
					continue;
				}
				// We can use this register for the tmpphi, make sure we don't use it twice
				statusRegisters[i] = statusRegisters[regId];
				registers[i].info.needsSecondaryName |= needsSecondaryName;
				return i;
			}
			// Create a register which will have an empty live range
			// It is not a problem since we mark it as used in the block
			uint32_t chosenReg = registers.size();
			registers.push_back(RegisterRange(LiveRange(), kind, needsSecondaryName));
			statusRegisters.push_back(statusRegisters[regId]);
			return chosenReg;
		}
		void handleRecursivePHIDependency(const Instruction* incoming) override
		{
			bool asmjs = incoming->getParent()->getParent()->getSection() == StringRef("asmjs");
			assert(registerize.registersMap.count(incoming));
			uint32_t regId=registerize.registersMap.find(incoming)->second;
			Registerize::REGISTER_KIND phiKind = registerize.getRegKindFromType(incoming->getType(), asmjs);
			uint32_t chosenReg = assignTempReg(regId, phiKind, cheerp::needsSecondaryName(incoming, PA));
			registerize.edgeRegistersMap.insertUpdate(regId, chosenReg, edgeContext);
		}
		void handlePHI(const PHINode* phi, const Value* incoming, bool selfReferencing) override
		{
			// Provide temporary regs for the offset part of SPLIT_REGULAR PHIs that reference themselves
			if(!selfReferencing)
				return;
			assert(cheerp::needsSecondaryName(phi, PA));
			assert(registerize.registersMap.count(phi));
			uint32_t regId=registerize.registersMap.find(phi)->second;
			// Check if there is already a tmp PHI for this PHI, if so it is not really self referencing
			if(registerize.edgeRegistersMap.count(regId, edgeContext))
				return;
			uint32_t chosenReg = assignTempReg(regId, Registerize::INTEGER, false);
			registerize.selfRefRegistersMap.insert(std::make_pair(InstOnEdge(edgeContext.fromBB, edgeContext.toBB, regId), chosenReg));
		}
		void setRegisterUsed(uint32_t regId) override
		{
			assert(regId < statusRegisters.size());
			statusRegisters[regId] = REGISTER_STATUS::USED;
		}
		void reportRegisterUse() const override
		{
			int i = 0;
			for (auto x : statusRegisters)
			{
				llvm::errs() << i++ << ":" << x << "  ";
			}
			llvm::errs() << "\n";
		}
		void addRegisterUse(uint32_t regId) override
		{
			assert(regId < statusRegisters.size());
			if (statusRegisters[regId] == REGISTER_STATUS::USED)
				return;
			++statusRegisters[regId];
		}
		void removeRegisterUse(uint32_t regId) override
		{
			assert(regId < statusRegisters.size());
			regId = registerize.edgeRegistersMap.findCurrentRegisterId(regId, edgeContext);
			if (statusRegisters[regId] == REGISTER_STATUS::USED)
				return;
			assert(regId < statusRegisters.size());
			assert(statusRegisters[regId]!=0);
			--statusRegisters[regId];
		}
	};
#ifndef NDEBUG
	// Temporarily set and then reset this debug flag, EndOfBlockPHIHandler uses registerize
	// and we have actually already assigned all registers for the instructions
	RegistersAssigned = true;
#endif
#ifdef REGISTERIZE_DEBUG
	const uint32_t originalRegistersSize = registers.size();
#endif

	if (F.getSection() == "asmjs" && wasm)
	{
		//Wasm-like stack based handling requires no temporaries
	}
	else
	{
		for (const BasicBlock & bb : F)
		{
			const Instruction* term=bb.getTerminator();
			for(uint32_t i=0;i<term->getNumSuccessors();i++)
			{
				//TODO: improve how thet are assigned
				const BasicBlock* succBB=term->getSuccessor(i);
				RegisterizePHIHandler(*this, registers, instIdMap, PA, edgeContext).runOnEdge(*this, &bb, succBB);
			}
		}
	}
#ifndef NDEBUG
	RegistersAssigned = false;
#endif

#ifdef REGISTERIZE_DEBUG
	if (originalRegistersSize != registers.size())
		dbgs() << originalRegistersSize << "\t different than " << registers.size() << "\n";
#endif

	// Populate the final register list for the function
	std::vector<RegisterInfo>& regsInfo = registersForFunctionMap[&F];
	regsInfo.reserve(registers.size());
	for(unsigned int i=0;i<registers.size();i++)
		regsInfo.push_back(registers[i].info);
	return registers.size();
}

uint32_t Registerize::FrequencyInfo::getWeight (const llvm::BasicBlock* from, const llvm::BasicBlock* to) const
{
	//Takes a phi_edge as input, return his weight calculated as 10^(depth of the edge)
	const llvm::Loop* loop = findCommonLoop(LI, from, to);
	const uint32_t depth = loop ? loop->getLoopDepth() : 0;
	uint32_t res = 1;
	for (uint32_t i=0; i<depth && res < 100000; i++)
	{
		res *= 10;
	}
	return res;
}

Registerize::RegisterAllocatorInst::RegisterAllocatorInst(llvm::Function& F_, const InstIdMapTy& instIdMap, const LiveRangesTy& liveRanges, const PointerAnalyzer& PA, Registerize* registerize)
	: F(F_), registerize(registerize), PA(PA), emptyFunction(false), frequencyInfo(F, registerize->LI)
{
	using namespace llvm;

	//Do a first pass to collect instructions (and count them implicitly)
	for(const auto& it: liveRanges)
	{
		const Instruction* I=it.first;
		assert(registerize->registersMap.count(I) == 0);
		indexer.insert(I);
	}
	if (indexer.size() == 0)
	{
		emptyFunction = true;
		return;
	}
	//There are indexer.size() initial register, and they are merged pair-wise, since every merge requires an additional register, it makes at most indexer.size()-1 extra, so 2*N-1
	const int maxSize = indexer.size() * 2 - 1;
	virtualRegisters.reserve(maxSize);
	instructionLocations.reserve(indexer.size());
	parentRegister.reserve(maxSize);
	const bool asmjs = F.getSection()==StringRef("asmjs");
	//Do a second pass to set virtualRegisters environment
	for(const auto& it: liveRanges)
	{
		const Instruction* I=it.first;
		const InstructionLiveRange& range=it.second;
		assert(registerize->registersMap.count(I) == 0);
		parentRegister.push_back(size());
		virtualRegisters.push_back(RegisterRange(
					range.range,
					registerize->getRegKindFromType(I->getType(), asmjs),
					cheerp::needsSecondaryName(I, PA)
					));
		const auto& iter = instIdMap.find(I);
		assert(iter != instIdMap.end());
		instructionLocations.push_back(iter->second);
	}
	computeBitsetConstraints();
	buildFriends(PA);
	buildEdgesData(F);
}

std::vector<uint32_t> VertexColorer::keepMerging(SearchState& state)
{
	//Build a decent solution given some (possibly none) already made choices
	const uint32_t index = state.processedFriendships;
	if (index == friendships.size() || friendships[index].first == 0)
	{
		//There are no more positive weight friendship to be decided: assign the colors greedily according to "least neightbours goes last" euristics
		return assignGreedily();
	}

	//F is a positive weight friendship (and the one with higher weight, since they are ordered)
	const Friendship& F = friendships[state.processedFriendships];
	const uint32_t a = findParent(F.second.first);
	const uint32_t b = findParent(F.second.second);

	state.processedFriendships++;

	std::vector<uint32_t> solution;

	if (a!=b && areMergeable(a, b))
	{
		//either force A and B to have the same color contracting the edge between them, then recurse
		doContraction(a, b);
		solution = keepMerging(state);
		undoContraction(a, b);
	}
	else
	{
		//or calling directly the recursion, this happens in 2 cases:
		//1. A and B are already merged, so nothing to be done
		//2. A and B are already conflicting, so nothing to be done
		solution = keepMerging(state);
	}

	state.processedFriendships--;

	return solution;
}

void VertexColorer::DFSwithLimitedDepth(SearchState& state)
{
	//Do a DFS with a limited search depth, and store inside state the best found solution
	if (state.iterationsCounter.remaining() == 0)
		return;
#ifdef REGISTERIZE_DEBUG
	state.debugStats[NODE_VISITED]++;
#endif
	if (state.shouldBeEvaluated())
	{
		//We reached the depth limit
		if (!state.isEvaluationAlreadyDone())
		{
			//Evaluate the current node with keepMerging(): merge friends in order, then greedily assign colors
#ifdef REGISTERIZE_DEBUG
			state.debugStats[GREEDY_EVALUATIONS]++;
#endif
			const Coloring colors = getColors(keepMerging(state));
			const Solution localSolution = {computeScore(colors, lowerBoundOnNumberOfColors(/*forceEvaluation*/true)), colors};

			//This condition has sides effects (it modify the best score stored into state
			if (state.improveScore(localSolution))
			{
#ifdef REGISTERIZE_DEBUG_EXAUSTIVE_SEARCH
				llvm:: errs() << "|\tCurrent state evaluated: \tcolors="<< computeNumberOfColors(colors) <<" (minimal is "<<state.minimalNumberOfColors<< ")\t\t score=" << localSolution.first <<"\t\tchoices made up to now = ";
				state.printChoicesMade();
				llvm::errs () << "\n";
#endif
			}
			state.iterationsCounter.consumeIteration();
		}
		state.leafs++;
		return;
	}

	const Friendship& F = friendships[state.processedFriendships];
	const uint32_t a = findParent(F.second.first);
	const uint32_t b = findParent(F.second.second);

	const uint32_t oldSize = state.choicesMade.size();
	state.choicesMade.resize(oldSize + 1);
	state.processedFriendships++;
	//When should we try to add two nodes: always
	{
		state.choicesMade.set(oldSize);
		if (a == b)
		{
			//A and B are already merged, go one deeper
			DFSwithLimitedDepth(state);
		}
		else if (areMergeable(a, b))
		{
			//Contract the edge AB, and search one level deeper
			//do/undo contraction modify the state to keep it consistent with the current choice
			doContraction(a, b);
			DFSwithLimitedDepth(state);
			undoContraction(a, b);
		}
	}

	//When should we try to keep 2 nodes separated: when they are not already merged, and we could still improve the current best
	if (a != b && state.iterationsCounter.remaining() && state.couldCurrentImproveScore(F.first) )
	{
		state.choicesMade.reset(oldSize);
		state.currentScore += F.first;

		while (state.processedLinks < orderedLinks.size() &&
				orderedLinks[state.processedLinks].first.first == F.second.first &&
				orderedLinks[state.processedLinks].first.second == F.second.second)
		{
			const uint32_t x = orderedLinks[state.processedLinks].second;
			if (areGroupedStillGood[x] == 0)
			{
				state.currentScore += costPerPhiEdge;
			}
			areGroupedStillGood[x]++;
			++state.processedLinks;
		}

		if (areMergeable(a, b))
		{
			//set as unmergiable, and recurse
			setAdditionalConstraint(a, b, /*direct*/true);
			DFSwithLimitedDepth(state);
			setAdditionalConstraint(a, b, /*direct*/false);
		}
		else
		{
			//Are already unmergiable, so no need to change anythings
			DFSwithLimitedDepth(state);
		}

		while (state.processedLinks > 0 &&
				orderedLinks[state.processedLinks-1].first.first == F.second.first &&
				orderedLinks[state.processedLinks-1].first.second == F.second.second)
		{
			--state.processedLinks;
			const uint32_t x = orderedLinks[state.processedLinks].second;
			areGroupedStillGood[x]--;
			if (areGroupedStillGood[x] == 0)
			{
				state.currentScore -= costPerPhiEdge;
			}
		}

		state.currentScore -= F.first;
	}
	state.processedFriendships--;
	state.choicesMade.resize(oldSize);

	if (state.processedFriendships == 0)
		assert(state.currentScore == 0);
}

uint32_t VertexColorer::chromaticNumberWithNoFriends(uint32_t lowerBound, uint32_t minimalColors) const
{
	if (howManyWaysHasLowerBoundBeenEvaluated >= 2)
		return lowerBoundChromaticNumber;
	//If we discard all friends, and minimize score, and we find an optimal solution, we have the chromatic number (and so also a lower bound on it)
	VertexColorer noFriendships(N, *this);
	noFriendships.setAll(/*conflicting*/false);
	for (const Link& link : constraintIterable())
	{
		noFriendships.addConstraint(link.first, link.second);
	}

	noFriendships.improveLowerBound(lowerBound);
	noFriendships.solve();
	const Coloring& col = noFriendships.getSolution();

	//If we do not have the guarantee for the solution to be optimal, this work only as upper bound but not as lower bound
	if (!noFriendships.isSolutionOptimal())
		return lowerBound;

#ifdef REGISTERIZE_DEBUG
	llvm::errs() << "Computing lower bound on graph with no friendships: " << lowerBound << "/" << minimalColors << " to " << computeNumberOfColors(col) << "\n";
#endif
	return computeNumberOfColors(col);
}

void VertexColorer::iterativeDeepening(IterationsCounter& counter)
{
	//TODO: try to split it in multiple solutions first
	Solution best;
	best.second = getColors(assignGreedily());
	best.first = computeScore(best.second, lowerBoundOnNumberOfColors(/*forceEvaluation*/true));
	uint32_t minimalColors = computeNumberOfColors(best.second);
	uint32_t lowerBound = lowerBoundOnNumberOfColors(/*forceEvaluation*/true);

	if (lowerBound < minimalColors && friendships.size() > 0 && friendships.front().first > 0)
	{
		//if the lower bound, based on the maximal clique found, happens not to be tight enough, recompute it an a simplified problem
		uint32_t estimate = chromaticNumberWithNoFriends(lowerBound, minimalColors);

		improveLowerBound(estimate);
		howManyWaysHasLowerBoundBeenEvaluated=2;

		if (estimate > lowerBound && removeRowsWithFewConstraints())
		{
			//lowerBound has increased, so we needs to check again rows with few constraints
			//removeRowsWithFewConstraints will set retColors to the solution
			return;
		}

		lowerBound = lowerBoundOnNumberOfColors(/*forceEvaluation*/true);
	}
	assert(counter.remaining() > 0);

	uint32_t positiveFriendships = 0;
	while (positiveFriendships < friendships.size() && friendships[positiveFriendships].first > 0)
	{
		++positiveFriendships;
	}

	uint32_t depth = 0;
	uint32_t previousDepth = 0;
	constraints.reserve(N * 2);
	while (counter.remaining()>0 && depth <= friendships.size())
	{
#ifdef REGISTERIZE_DEBUG_EXAUSTIVE_SEARCH
		llvm::errs() << "| ----- Exaustive search starting at depths "<<previousDepth<<" - " <<depth<<"\n";
#endif
		SearchState state(best, lowerBound, counter.remaining(), depth, previousDepth, positiveFriendships, costPerColor);
		DFSwithLimitedDepth(state);
		counter.consumeIterations(state.iterationsCounter.evaluationsDone());
#ifdef REGISTERIZE_DEBUG
		for (uint32_t i = 0; i<4; i++)
		{
			debugStats[i] += state.debugStats[i];
		}
#endif
		previousDepth = depth;
		uint32_t maxIterations = state.leafsEvaluated();
		do {
			depth++;
			maxIterations*=2;
		} while (maxIterations *2<= counter.remaining() && depth < friendships.size());
	}
	retColors = best.second;
}

std::vector<uint32_t> VertexColorer::assignGreedily() const
{
/*
	This function treats only the constraint part of the problem: it try to minimize the number of colors without trying to satisfy friendships.
	This semplified problem has a known greedy strategy that works reasonably:
	-pick the node with the least number of neighbours (as in the number of constrains he has)
	-remove it from the problem, solve the remaining subproblem
	-insert it back with the lowest possible color. A node with N constraints require at maximum color N+1
	(if no constraints have the same color, and they already occupy every label in 1...N)

	Since the node was chosen for having few neighbours, a low number of colors will be used.

	It's implemented by first counting the number of constraint for each vertex, then building a stack of low-neighbours count nodes (and keeping the node count correct)
	and only then choosing the label/color for each node
*/

	//Here it returns the parent vector, not a coloring
	std::vector<uint32_t> res = parent;
	std::vector<uint32_t> neighboursCount(N);
	for (uint32_t i=0; i<N; i++)
	{
		if (!isAlive(i))
			continue;
		for (uint32_t j=0; j<N; j++)
		{
			if (!isAlive(j))
				continue;
			if (constraints[i][j])
				++neighboursCount[i];
		}
	}

	llvm::BitVector processed(N);
	for (uint32_t i=0; i<N; i++)
	{
		processed[i] = !isAlive(i);
	}
	std::vector<uint32_t> stack;
	for (uint32_t i=0; i<N; i++)
	{
		uint32_t b=N;
		for (uint32_t j=0; j<N; j++)
		{
			if (processed[j]==false)
			{
				if (b==N || neighboursCount[j] < neighboursCount[b])
					b = j;
			}
		}
		if (b == N)
			break;
		processed.set(b);
		stack.push_back(b);
		for (uint32_t j=0; j<N; j++)
		{
			if (!isAlive(j))
				continue;
			if (constraints[b][j])
				neighboursCount[j] --;
		}
	}

	std::vector<std::pair<uint32_t, llvm::BitVector>> assignedColors;
	while (!stack.empty())
	{
		uint32_t i = stack.back();
		stack.pop_back();

		bool done = false;
		for (std::pair<uint32_t, llvm::BitVector>& pair : assignedColors)
		{
			if (!pair.second[i])
			{
				//Check all assigned colors, looking whether there is clash with any constraints (and so the or between them)
				done = true;
				res[i] = pair.first;
				pair.second |= constraints[i];
				break;
			}
		}
		if (!done)
		{
			//Otherwise create a new color, and initialize with the constraints of i
			assignedColors.push_back({i, constraints[i]});
		}
	}
	return res;
}

bool Registerize::RegisterAllocatorInst::couldAvoidToBeMaterialized(const BasicBlock& BB) const
{
	//Only blocks containing only phi and exactly 1 successor can avoid to be materialized
	if (!isNumStatementsLessThan<1>(&BB, PA, *registerize, /*skipPhi*/true))
		return false;
	const Instruction* term=BB.getTerminator();
	return (term->getNumSuccessors() == 1);
}

void Registerize::RegisterAllocatorInst::buildEdgesData(Function& F)
{
	for (const BasicBlock & fromBB : F)
	{
		if (!couldAvoidToBeMaterialized(fromBB))
			continue;

		const Instruction* term=fromBB.getTerminator();
		for(uint32_t i=0;i<term->getNumSuccessors();i++)
		{
			const BasicBlock* toBB=term->getSuccessor(i);
			edges.resize(edges.size() + 1);
			BasicBlock::const_iterator I=toBB->begin();
			BasicBlock::const_iterator IE=toBB->end();
			for(;I!=IE;++I)
			{
				const PHINode* phi=dyn_cast<PHINode>(I);
				if(!phi)
					break;
				if(phi->use_empty())
					continue;
				const Value* val=phi->getIncomingValueForBlock(&fromBB);
				const Instruction* pre=dyn_cast<Instruction>(val);
				if(pre && indexer.count(pre))
				{
					edges.back().push_back(std::make_pair(indexer.id(phi), indexer.id(pre)));
				}
			}
			if (edges.back().empty())
				edges.pop_back();
		}
	}
}

void Registerize::RegisterAllocatorInst::buildFriendsSinglePhi(const uint32_t phi, const PointerAnalyzer& PA)
{
	const PHINode* I = dyn_cast<PHINode>(indexer.at(phi));
	if (!I)
		return;
	uint32_t n = I->getNumIncomingValues();
	for(uint32_t i = 0; i<n; i++)
	{
		const Instruction* usedI=getUniqueIncomingInst(I->getIncomingValue(i), PA);
		if(!usedI)
			continue;
		assert(!isInlineable(*usedI, PA));
		addFriendship(phi, indexer.id(usedI), frequencyInfo.getWeight(I->getIncomingBlock(i), I->getParent()));
	}
}

void Registerize::RegisterAllocatorInst::createSingleFriendship(const uint32_t i, const Value* operand)
{
	//Introduce a friendships of weight 1
	const Instruction* I = dyn_cast<Instruction>(operand);
	if (I && indexer.count(I))
		addFriendship(i, indexer.id(I), 1);
}

void Registerize::RegisterAllocatorInst::buildFriendsSingleCompressibleInstr(const uint32_t i)
{
	//Try to force the first operand of a sum/difference/multiplication/... to be the same of the result
	const Instruction* I = indexer.at(i);
	assert (!!I);
	// Compount operator cannot be used in asm.js and do not exists in wasm
	if(I->getParent()->getParent()->getSection() == StringRef("asmjs"))
		return;

	switch (I->getOpcode())
	{
		//Commutative operations
		case Instruction::FAdd:
		case Instruction::FMul:
		case Instruction::Or:
		case Instruction::And:
		case Instruction::Xor:
			assert(I->getNumOperands() == 2);
			createSingleFriendship(i, I->getOperand(1));
			[[clang::fallthrough]];
			//Fall through!
		//Non-commutative operations
		case Instruction::FSub:
		case Instruction::FDiv:
		case Instruction::FRem:
		case Instruction::LShr:
		case Instruction::AShr:
		case Instruction::Shl:
			assert(I->getNumOperands() == 2);
			createSingleFriendship(i, I->getOperand(0));
			break;
	}
	return;
}

#ifdef REGISTERIZE_STATS
const uint32_t VERTEX_COLORER_NUMBER_OF_REDUCTIONS = VertexColorer::ReductionPasses::REDUCTIONPASSES_FINAL_ELEMENT;
std::array<StatisticCollector<32>, VERTEX_COLORER_NUMBER_OF_REDUCTIONS> REGISTERIZE_STATISTICS_ON_REDUCTIONS;
StatisticCollector<32> REGISTERIZE_STATISTICS_ON_INPUT_PROBLEMS;
StatisticCollector<32> REGISTERIZE_STATISTICS_ON_PROBLEMS_COLORED;
StatisticCollector<32> REGISTERIZE_STATISTICS_ON_PROBLEMS_COLORED_BY_DFS;
void reportRegisterizeStatistics ()
{
	REGISTERIZE_STATISTICS_ON_INPUT_PROBLEMS.report("Input function processed");
	REGISTERIZE_STATISTICS_ON_PROBLEMS_COLORED.report("Subproblem solved by VertexColorer");
	REGISTERIZE_STATISTICS_ON_PROBLEMS_COLORED_BY_DFS.report("Subproblems solved by VertexColorer::iterativeDeepening");
	uint32_t index = 0;
	for (const auto& stat : REGISTERIZE_STATISTICS_ON_REDUCTIONS)
	{
		stat.report(VertexColorer::reductionPassesNames(index));
		++index;
	}
}
#endif

bool VertexColorer::isDominatingFriend(const uint32_t a, const uint32_t b) const
{
	const std::vector<Friend>& F = friends[a];
	if (F.size() == 0)
		return true;
	uint32_t index = F.size();
	uint32_t sum = 0;
	for (uint32_t i=0; i<F.size(); i++)
	{
		if (F[i].first == b)
			index = i;
		sum += F[i].second;
	}
	if (index == F.size())
		return false;
	return F[index].second *2 >= sum;
}

std::vector<uint32_t> RemoveDominated::whoIsDominatingFriend(const uint32_t a) const
{
	std::vector<uint32_t> res;
	const std::vector<VertexColorer::Friend>& F = instance.friends[a];
	uint32_t sum = 0;
	for (uint32_t i=0; i<F.size(); i++)
	{
		sum += F[i].second;
		assert(F[i].second > 0);
	}
	for (uint32_t i=0; i<F.size(); i++)
	{
		if (F[i].second * 2 >= sum)
			res.push_back(F[i].first);
	}
	if (F.empty())
	{
		for (uint32_t i=0; i<instance.N; i++)
		{
			if (i != a)
				res.push_back(i);
		}
	}
	return res;
}

bool VertexColorer::canBeAddedToClique(const uint32_t index, const llvm::BitVector& unionConstraint, const llvm::BitVector& used) const
{
	if (!unionConstraint[index])
		return false;
	llvm::BitVector local = constraints[index];
	local &= used;

	return local == used;
}

void VertexColorer::addToClique(const uint32_t index, llvm::BitVector& unionConstraint, llvm::BitVector& used) const
{
	assert(!used[index]);
	assert(canBeAddedToClique(index, unionConstraint, used));

	unionConstraint &= constraints[index];
	unionConstraint.set(index);

	used.set(index);
}

uint32_t VertexColorer::maximalGeneratedClique() const
{
	//Try to generate a clique as big as possible (this serves as lower bound on chromatic number)
	//Take advantage than most instance are already sort of block diagonal, so identify a block and try to expand it

	if (N == 0)
		return 0;
	uint32_t low = 1;

	std::vector<uint32_t> blocks = findAlreadyDiagonalized();

	llvm::BitVector unionConstraint(N, true);
	llvm::BitVector used(N, false);
	for (uint32_t i=0; i<N; )
	{
		uint32_t j = i;
		while (j < N && blocks[j] == blocks[i])
		{
			addToClique(j, unionConstraint, used);
			j++;
		}

		for (uint32_t k=0; k<N; k++)
		{
			if (!used[k] && canBeAddedToClique(k, unionConstraint, used))
				addToClique(k, unionConstraint, used);
		}
		low = std::max(low, used.count());
		unionConstraint.set();
		used.reset();
		i = j;
	}
	return low;
}

void VertexColorer::improveLowerBound(const uint32_t x)
{
	lowerBoundChromaticNumber = std::max(lowerBoundChromaticNumber, x);
}

uint32_t VertexColorer::lowerBoundOnNumberOfColors(const bool forceEvaluation)
{
	if (howManyWaysHasLowerBoundBeenEvaluated == 0 && forceEvaluation)
	{
		howManyWaysHasLowerBoundBeenEvaluated++;
		improveLowerBound(maximalGeneratedClique());
	}
	return lowerBoundChromaticNumber;
}

void VertexColorer::floodFillOnBits(llvm::BitVector& region, const uint32_t start, const bool conflicting) const
{
	floodFillOnBitsWithArticulationPoints(region, start, conflicting, llvm::BitVector(N, false));
}

void VertexColorer::floodFillOnBitsWithArticulationPoints(llvm::BitVector& region, const uint32_t start, const bool conflicting, const llvm::BitVector& isArticulationPoint) const
{
	//Assign every node connected to start to the region labelled start
	//What connected means depends:
	//conflicting == false		there is either a positive-weight friendship or they are adjacent (so there is a constraint)
	//conflicting == true		the node are not adjacent (or there is no constraint)
	//articulationPoint != -1	as above, but it do not count if the vertex is equal to articulationPoint (as to disconnect regions on that point)
	region = llvm::BitVector(N, false);
	region.set(start);

	std::vector<uint32_t> toProcess;
	toProcess.reserve(N);
	toProcess.push_back(start);

	while (!toProcess.empty())
	{
		const uint32_t x = toProcess.back();
		toProcess.pop_back();

		//Articulations point are splitting point between regions, so they do not have to be processed
		if (isArticulationPoint[x])
			continue;

		for (uint32_t i=0; i<N; i++)
		{
			if (i != x && !region[i])
			{
				if (conflicting != constraints[x][i])
				{
					region.set(i);
					toProcess.push_back(i);
				}
			}
		}
		if (!conflicting)
		{
			for (const Friend& f : friends[x])
			{
				uint32_t i = f.first;
				assert(i != x);
				assert(!constraints[x][i]);
				if (!region[i])
				{
					region.set(i);
					toProcess.push_back(i);
				}
			}
		}
	}
}

void VertexColorer::HopcroftTarjanData::visit(const uint32_t i, const uint32_t d)
{
	//Tarjan-Hopcroft linear algorithm to discover articulation points, see for example https://en.wikipedia.org/wiki/Biconnected_component
	assert(!visited[i]);
	visited.set(i);
	depth[i] = d;
	low[i] = d;

	for (const Link& link : sol.constraintOrFriendsIterable(i))
	{
		assert(i != link.first);
		processChildren(i, link.first, d);
	}

	if ((parent[i] < sol.N && isArticulation[i]) || (parent[i] == sol.N && numChildren[i] > 1))
		isArticulation.set(i);
	else
		isArticulation.reset(i);
}

void VertexColorer::HopcroftTarjanData::processChildren(const uint32_t i, const uint32_t j, const uint32_t d)
{
	if (!visited[j])
	{
		parent[j] = i;
		numChildren[i]++;
		visit(j, d+1);
		if (low[j] >= depth[i])
			isArticulation.set(i);
		low[i] = std::min(low[i], low[j]);
	}
	else if (j != parent[i])
	{
		low[i] = std::min(low[i], depth[j]);
	}
}

std::vector<uint32_t> VertexColorer::getArticulationPoints() const
{
	//Tarjan-Hopcroft linear algorithm, see for example https://en.wikipedia.org/wiki/Biconnected_component
	HopcroftTarjanData data(*this);

	data.visit(0, 0);

	std::vector<uint32_t> res;
	for (uint32_t i=0; i<N; i++)
	{
		if (data.isArticulation[i])
			res.push_back(i);
	}

	return res;
}

std::vector<uint32_t> VertexColorer::findAlreadyDiagonalized() const
{
	assert(areAllAlive());
	std::vector<uint32_t> res = parent;

	uint32_t i=0;
	uint32_t firstUnused = 0;
	for (uint32_t j=0; j<N; j++)
	{
		bool good = true;
		for (uint32_t k=i; k<j; k++)
		{
			if (!constraints[j][k])
			{
				good = false;
				break;
			}
		}
		if (!good)
		{
			i = j;
			firstUnused++;
		}
		res[j] = firstUnused;
	}
	return res;
}


bool VertexColorer::areAllAlive() const
{
	for (uint32_t i=0; i<N; i++)
	{
		if (!isAlive(i))
			return false;
	}
	return true;
}

void VertexColorer::permuteFirstElements(Coloring& coloring, const uint32_t N)
{
	const uint32_t M = computeNumberOfColors(coloring);

	//We keep 2 permutation, one the inverse of the other, so at each moment inveresePermutation[directPermutation[i]] == i
	std::vector<uint32_t> directPermutation(M);
	std::vector<uint32_t> inversePermutation(M);
	for (uint32_t i=0; i<M; i++)
	{
		directPermutation[i] = i;
		inversePermutation[i] = i;
		assert(inversePermutation[directPermutation[i]] == i);
	}

	for (uint32_t i=0; i<N; ++i)
	{
		//We find the element k were our current element should end up i, do both the inverse and direct permuation
		const uint32_t k = coloring[i];
		const uint32_t z = inversePermutation[i];
		std::swap(inversePermutation[directPermutation[z]], inversePermutation[directPermutation[k]]);
		std::swap(directPermutation[z], directPermutation[k]);
		//And check everything went ok, and the invariant is kept for the next loop
		assert(inversePermutation[directPermutation[k]] == k);
		assert(inversePermutation[directPermutation[z]] == z);
	}

	//Apply the directPermutation
	for (uint32_t& c : coloring)
	{
		c = directPermutation[c];
	}

	for (uint32_t i=0; i<N; ++i)
	{
		assert(coloring[i] == i);
	}
}

template<typename Derived>
bool Reduction<Derived>::perform()
{
	Derived& derived = getDerived();

	if (derived.couldBeAvoided())
		return false;
	if (!derived.couldBePerformed())
		return false;

	//Find relabeling that split the problem into subproblem(s)
	derived.relabelNodes();

	if (!derived.couldBePerformedPhiEdges())
		return false;

#ifdef REGISTERIZE_DEBUG
	//Dump informations
	derived.dumpDescription();
#endif
#ifdef REGISTERIZE_STATS
	REGISTERIZE_STATISTICS_ON_REDUCTIONS[derived.id()].add(instance.N);
#endif
	//Build the connection matrix and the friend list for the subproblem(s)
	derived.buildSubproblems();
	//Solve them
	derived.reduce();

	assert(instance.checkConstraintsAreRespected(instance.retColors));

	return true;
}

template<typename Derived>
std::vector<uint32_t> Reduction<Derived>::computeLeaders (llvm::IntEqClasses& eqClasses, const uint32_t N) const
{
	eqClasses.compress();

	std::vector<uint32_t> toWhichSubproblem(N);

	for (uint32_t i=0; i<N; i++)
	{
		toWhichSubproblem[i] = eqClasses[i];
	}

	return toWhichSubproblem;
}

template<typename Derived>
std::vector<uint32_t> Reduction<Derived>::computeLeaders (llvm::IntEqClasses& eqClasses) const
{
	return computeLeaders(eqClasses, instance.N);
}

template<typename Derived>
void Reduction<Derived>::assignIndexes(const std::vector<uint32_t>& whichSubproblems, std::vector<uint32_t>& numerositySubproblem, std::vector<uint32_t>& newIndexes, const uint32_t startingFrom)
{
	const uint32_t N = whichSubproblems.size();
	const uint32_t M = *std::max_element(whichSubproblems.begin(), whichSubproblems.end()) + 1;

	numerositySubproblem = std::vector<uint32_t>(M, startingFrom);
	numerositySubproblem.front() = 0;
	newIndexes = std::vector<uint32_t>(N);

	for (uint32_t i=0; i<N; ++i)
	{
		const uint32_t m = whichSubproblems[i];

		newIndexes[i] = numerositySubproblem[m]++;
	}
}

bool VertexColorer::removeRowsWithFewConstraints()
{
	//Vertex with no positive-weight friends and "few" constraints can be removed and assigned a color at the end
	//The idea is that someone has less than N-1 constraint, for the pigeon hole there is a color <=N than is valid, and this color can be assigned at the end

	RemoveFewConstraints reduction(*this);

	return reduction.perform();
}

bool RemoveFewConstraints::couldBePerformed()
{
	//Vertex with no positive-weight friends and "few" constraints can be removed and assigned a color at the end
	//The idea is that someone has less than N-1 constraint, for the pigeon hole there is a color <=N than is valid, and this color can be assigned at the end

	uint32_t cutoff = instance.lowerBoundOnNumberOfColors(/*forceEvaluation*/true) - 1;

	bool anyToBePostProcessed = false;

	howManyOnCutoff = 0;

	for (uint32_t i = 0; i<instance.N; i++)
	{
		if (instance.friends[i].empty() && instance.constraints[i].count() <= cutoff)
		{
			toBePostProcessed[i] = true;
			anyToBePostProcessed = true;
			if (instance.constraints[i].count() == cutoff)
				howManyOnCutoff++;
		}
	}

	return anyToBePostProcessed;
}

bool RemoveFewConstraints::couldBePerformedPhiEdges()
{
	//TODO: assert me
	//Since only nodes without friendships are removed, it's always valid to perform this transformation
	return true;
}

void RemoveFewConstraints::relabelNodes()
{
	uint32_t firstUnused = 0;
	for (uint32_t i=0; i<instance.N; i++)
	{
		if (!toBePostProcessed[i])
		{
			alive.push_back(i);
			newIndex[i] = firstUnused++;
		}
	}
}

void RemoveFewConstraints::dumpSpecificDescription() const
{
	llvm::errs() << instance.N << " -> " << alive.size();
}

void RemoveFewConstraints::buildSubproblems()
{
	subproblems.emplace_back(alive.size(), instance);

	VertexColorer& subproblem = subproblems.front();

	for (const VertexColorer::Link& link : instance.constraintIterable())
	{
		const uint32_t a = link.first;
		const uint32_t b = link.second;
		if (!toBePostProcessed[a] && !toBePostProcessed[b])
			subproblem.addConstraint(newIndex[a], newIndex[b]);
	}
	for (const VertexColorer::Link& link : instance.positiveWeightFriendshipIterable())
	{
		const uint32_t a = link.first;
		const uint32_t b = link.second;
		assert(!toBePostProcessed[a] && !toBePostProcessed[b]);
		subproblem.addFriendship(link.weight, newIndex[a], newIndex[b]);
	}
	for (const std::vector<VertexColorer::Link>& edge : instance.groupedLinks)
	{
		subproblem.addNewEdge();
		for (const VertexColorer::Link& link : edge)
		{
			const uint32_t a = link.first;
			const uint32_t b = link.second;
			//TODO: reintroduce this, maybe
			assert(!toBePostProcessed[a] && !toBePostProcessed[b]);	//This is guaranteed since no friendships are allowed in this reduction
			subproblem.addOnEdge(newIndex[a], newIndex[b]);
		}
	}
}

void RemoveFewConstraints::preprocessing(VertexColorer& subproblem) const
{
	//Needed as not run into under-flow on unsigned ints
	const uint32_t lowerBound = std::max(instance.lowerBoundOnNumberOfColors(), howManyOnCutoff) - howManyOnCutoff;
	subproblem.improveLowerBound(lowerBound);
}

void RemoveFewConstraints::postprocessing(VertexColorer& subproblem)
{
	instance.improveLowerBound(subproblem.lowerBoundOnNumberOfColors());
}

void RemoveFewConstraints::reduce()
{
	VertexColorer& subproblem = subproblems.front();

	preprocessing(subproblem);
	subproblem.solve();
	postprocessing(subproblem);

	const VertexColorer::Coloring& subcolors = subproblem.getSolution();

	instance.retColors = VertexColorer::Coloring(instance.N, instance.N);
	for (uint32_t i = 0; i<alive.size(); i++)
	{
		instance.retColors[alive[i]] = subcolors[i];
	}
	for (uint32_t i = 0; i<instance.N; i++)
	{
		if (toBePostProcessed[i])
		{
			std::vector<bool> conflicting(instance.N+1, false);
			for (uint32_t j=0; j<instance.N; j++)
			{
				if (instance.constraints[i][j])
					conflicting[instance.retColors[j]] = true;
			}
			uint32_t& color = instance.retColors[i];
			color = 0;
			while (conflicting[color])
			{
				color++;
			}
		}
	}
}

bool EnumerateAllPhiEdges::couldBePerformed()
{
	//This reduction make sense only if there are groupedLinks and and there are not too many of them
	//Exploring every grouped links multiply the cost roughly by 2 so it's natural to be bound by the logarithm
	return instance.groupedLinks.size() > 0 &&
		(1u<<instance.groupedLinks.size() <= instance.times);
}

bool EnumerateAllPhiEdges::couldBePerformedPhiEdges()
{
	return true;
}

void EnumerateAllPhiEdges::relabelNodes()
{
	eqClasses.grow(instance.N);

	const auto& edge = instance.groupedLinks.back();
	for (const VertexColorer::Link& link : edge)
	{
		eqClasses.join(link.first, link.second);
	}

	newIndex = computeLeaders(eqClasses);
}

void EnumerateAllPhiEdges::reduce()
{
	VertexColorer& good = subproblems.front();
	VertexColorer& bad = subproblems.back();

	if (goodIsValid)
	{
		//Since we explore 2 problems parallely, we limit the exploration to only half the nodes on each
		good.times = instance.times/2;
		bad.times = instance.times/2;
	}
	else
		bad.times = instance.times;

	bad.solve();
	VertexColorer::Coloring badSolution = bad.getSolution();
	instance.retColors = badSolution;

	if (goodIsValid)
	{
		//Give good informations over bad solution: it has to be beaten
		good.solve();
		VertexColorer::Coloring goodSolution = good.getSolution();

		if (good.computeScore(goodSolution) < bad.computeScore(badSolution) + bad.costPerPhiEdge)
		{
			instance.retColors.resize(instance.N);
			for (uint32_t i=0; i<instance.N; i++)
			{
				instance.retColors[i] = goodSolution[newIndex[i]];
			}
		}
	}
}

void EnumerateAllPhiEdges::dumpSpecificDescription() const
{
	llvm::errs() <<"with " <<instance.groupedLinks.size() << " phi edges";
}

bool RemoveDominated::mergeIfDominated(const uint32_t dominator, const uint32_t dominated)
{
	const uint32_t& i = dominated;
	const uint32_t& j = dominator;

	if (i != j &&								//They should be different
		isAlive(j) &&							//Both alive (i is already checked in the outer cycle)
		!instance.constraints[i][j] &&					//There should be no constraints between them
		isSubset(instance.constraints[i], instance.constraints[j]) )	//and i's constraints should be a subset of j's constraints
	{
		isNodeAlive.reset(i);
		eqClasses.join(i, j);
		return true;
	}
	return false;
}

RemoveDominated::SamplesData RemoveDominated::precomputeSamples() const
{
	//Computing whether a row is a subset of another takes O(N) (possibly every index has to be checked)
	//We precompute a sample 64 arbitrary columns, and store only this in samples
	//This way first we first could test whether samples[x] is a subset of samples[y], and only whenever this is true we move to the more expensive check
	//In the worst case this still requires O(N), but a lot of cases could be solved in O(1), so while the algorithm complexity remains the same
	//(at least in degenerate cases), there is a practical speed-up

	SamplesData data;
	auto& samples = data.samples;
	auto& bucketsSameSample = data.bucketsSameSample;

	for (uint32_t i=0; i<instance.N; ++i)
	{
		samples.push_back(computeSample(instance.constraints[i]));
	}

	std::vector<std::pair<uint64_t, uint32_t>> sampleIndexPairs;
	for (uint32_t i=0; i<instance.N; i++)
	{
		sampleIndexPairs.push_back({samples[i], i});
	}
	std::sort(sampleIndexPairs.begin(), sampleIndexPairs.end());

	for (const auto& pair : sampleIndexPairs)
	{
		if (bucketsSameSample.empty() || bucketsSameSample.back().first != pair.first)
		{
			bucketsSameSample.push_back({pair.first, std::vector<uint32_t>()});
		}
		bucketsSameSample.back().second.push_back(pair.second);
	}

	return data;
}

bool RemoveDominated::couldBePerformed()
{
	//Try to find rows that are dominated by another. In this case, they can get the same colors of the dominating vertex and the solutions remain optimal
	//One vertex dominated AND can become parent of another if:
	//-the constraint of the dominated are a (possibly non proper) subset of the constraint of the dominating
	//-there are no constraint between them (otherwise it could be dominating, but it could not be merged)
	//-either the dominated has no friends, or has the dominating as only friend, or in any case the dominating is a dominating friend
	//			(this means that it's the friend that weights 50%+ of the total of the dominating friendships)

	eqClasses.grow(instance.N);

	SamplesData data = precomputeSamples();

	//TODO: iterates multiple times
	for (uint32_t i = 0; i<instance.N; i++)
	{
		if (!isAlive(i))
			continue;
		if (instance.friends[i].empty())
		{
			//Nodes whitout friends accept every node as possible dominator.
			//We restrict the choice by first checking if a certain bucket is valid, and only then iterating inside the bucket
			for (const std::pair<uint64_t, std::vector<uint32_t>>& pairSampleIndexes : data.bucketsSameSample)
			{
				if (!isSubset(data.samples[i], pairSampleIndexes.first))
					continue;
				for (uint32_t j : pairSampleIndexes.second)
				{
					if (mergeIfDominated(j, i))
						break;
				}
				if (!isAlive(i))
					break;
			}
		}
		else
		{
			//Nodes with friends accept as dominator only dominating friends, or friends that are connected by a majority weight link
			for (uint32_t j : whoIsDominatingFriend(i))
			{
				if (!isSubset(data.samples[i], data.samples[j]) || !isAlive(j))
					continue;
				if (phiEdgesWouldBeBroken(i, j))
					continue;
				if (mergeIfDominated(j, i))
					break;
			}
		}
	}

	for (uint32_t i=0; i<instance.N; i++)
	{
		if (!isAlive(i))
			return true;
	}

	return false;
}

bool RemoveDominated::phiEdgesWouldBeBroken(const uint32_t dominated, const uint32_t dominator)
{
	const uint32_t I = eqClasses.findLeader(dominated);
	const uint32_t J = eqClasses.findLeader(dominator);
	assert(I != J);
	for (const auto& E : instance.groupedLinks)
	{
		for (const VertexColorer::Link& link : E)
		{
			const uint32_t A = eqClasses.findLeader(link.first);
			const uint32_t B = eqClasses.findLeader(link.second);

			//If they end up with the same ancestor, it's good
			if (A == B)
				continue;
			if ((A == I && B != J) || (B == I && A != J))
			{
				return true;
			}
		}
	}
	return false;
}

bool RemoveDominated::couldBePerformedPhiEdges()
{
	for (const auto& E : instance.groupedLinks)
	{
		for (const VertexColorer::Link& link : E)
		{
			const uint32_t a = link.first;
			const uint32_t b = link.second;
			//If they end up with the same ancestor, it's good
			if (eqClasses[a] == eqClasses[b])
				continue;
			//Otherwise, do not remove nodes that have a phi edge
			if (!isAlive(a))
			{
				return false;
			}
			if (!isAlive(b))
			{
				return false;
			}
		}
	}
	relabelNodes();
	return true;
}

void RemoveDominated::relabelNodes()
{
	newIndex = computeLeaders(eqClasses);
}

void RemoveDominated::dumpSpecificDescription() const
{
	llvm::errs() << instance.N << " -> " << isNodeAlive.count();
}

void RemoveDominated::buildSubproblems()
{
	subproblems.emplace_back(eqClasses.getNumClasses(), instance);
	VertexColorer& subproblem = subproblems.front();

	for (const VertexColorer::Link& link : instance.constraintIterable())
	{
		subproblem.addConstraint(newIndex[link.first], newIndex[link.second]);
	}

	//Add friendships (if they do not clash with constraints)
	for (const VertexColorer::Link& link : instance.positiveWeightFriendshipIterable())
	{
		assert(link.weight > 0);
		const uint32_t a = newIndex[link.first];
		const uint32_t b = newIndex[link.second];
		assert(subproblem.constraints[a][b] == subproblem.constraints[b][a]);
		if (!subproblem.constraints[a][b])
			subproblem.addFriendship(link.weight, a, b);
	}
	for (const std::vector<VertexColorer::Link>& edge : instance.groupedLinks)
	{
		subproblem.addNewEdge();
		for (const VertexColorer::Link& link : edge)
		{
			const uint32_t a = newIndex[link.first];
			const uint32_t b = newIndex[link.second];
			assert(subproblem.constraints[a][b] == subproblem.constraints[b][a]);
			if (!subproblem.constraints[a][b])
				subproblem.addOnEdge(a, b);
			else
			{
				subproblem.popLastEdge();
				break;
			}
		}
	}
}

void EnumerateAllPhiEdges::buildSubproblems()
{
	//We process the more numerous grouped link
	uint32_t indexGroupedLink = 0;
	for (uint32_t i=1; i<instance.groupedLinks.size(); i++)
	{
		if (instance.groupedLinks[i].size() > instance.groupedLinks[indexGroupedLink].size())
			indexGroupedLink = i;
	}
	std::swap(instance.groupedLinks[indexGroupedLink], instance.groupedLinks.back());

	subproblems.emplace_back(eqClasses.getNumClasses(), instance);
	subproblems.emplace_back(instance.N, instance);

	VertexColorer& good = subproblems.front();
	VertexColorer& bad = subproblems.back();
	goodIsValid = true;

	for (const VertexColorer::Link& link : instance.constraintIterable())
	{
		const uint32_t i=link.first;
		const uint32_t j=link.second;
		if (newIndex[i] == newIndex[j])
			goodIsValid = false;
		good.addConstraint(newIndex[i], newIndex[j]);
		bad.addConstraint(i, j);
	}

	//Add friendships (if they do not clash with constraints)
	for (const VertexColorer::Link& link : instance.positiveWeightFriendshipIterable())
	{
		assert(link.weight > 0);
		const uint32_t a = newIndex[link.first];
		const uint32_t b = newIndex[link.second];
		assert(good.constraints[a][b] == good.constraints[b][a]);
		if (!good.constraints[a][b])
			good.addFriendship(link.weight, a, b);
		assert(!bad.constraints[link.first][link.second]);
		bad.addFriendship(link.weight, link.first, link.second);
	}
	for (uint32_t i=0; i+1<instance.groupedLinks.size(); i++)
	{
		const std::vector<VertexColorer::Link>& edge = instance.groupedLinks[i];
		good.addNewEdge();
		for (const VertexColorer::Link& link : edge)
		{
			const uint32_t a = newIndex[link.first];
			const uint32_t b = newIndex[link.second];
			assert(good.constraints[a][b] == good.constraints[b][a]);
			if (!good.constraints[a][b])
				good.addOnEdge(a, b);
			else
			{
				good.popLastEdge();
				break;
			}
		}
		bad.addNewEdge();
		for (const VertexColorer::Link& link : edge)
		{
			const uint32_t a = link.first;
			const uint32_t b = link.second;
			assert(instance.constraints[a][b] == instance.constraints[b][a]);
			assert(!instance.constraints[a][b]);
			bad.addOnEdge(a, b);
		}
	}
}

void RemoveDominated::preprocessing(VertexColorer& subproblem) const
{
	subproblem.improveLowerBound(instance.lowerBoundOnNumberOfColors());
	subproblem.avoidPass[VertexColorer::SPLIT_UNCONNECTED]=true;
}

void RemoveDominated::postprocessing(VertexColorer& subproblem)
{
	instance.improveLowerBound(subproblem.lowerBoundOnNumberOfColors());
}

bool RemoveDominated::isAlive(const uint32_t i) const
{
	return isNodeAlive[i];
}

void RemoveDominated::reduce()
{
	VertexColorer& subproblem = subproblems.front();

	preprocessing(subproblem);
	subproblem.solve();
	postprocessing(subproblem);

	const VertexColorer::Coloring& subcolors = subproblem.getSolution();

	instance.retColors.resize(instance.N);
	for (uint32_t i = 0; i<instance.N; i++)
	{
		instance.retColors[i] = subcolors[eqClasses[i]];
	}
}

bool SplitArticulation::couldBePerformed()
{
	//This pass assign each node to a clique, in particular working with the hypothesys that the connection matrix it's already sort in a block form
	//Now it assumes that clique are continguous (eg nodes i, i+1, i+2, .... i+k) but could be generalized
	//Then:
	//-it builds a graph where each clique gets substituted with single node (and this macro nodes are connected iff the original clique were connected)
	//-find (if any) the articulation points of the simplified graph
	//-split the original graph into subproblems
	//-combine optimally the subsolutions into a solution for the bigger problem

	//This pass currently sort of assumes that the graph is connected (or not?)

	const uint32_t M = blockNumber.back() + 1;

	start = std::vector<uint32_t>(M, instance.N);
	end = std::vector<uint32_t>(M, 0);

	for (uint32_t i=0; i<instance.N; i++)
	{
		const uint32_t num = blockNumber[i];
		start[num] = std::min(start[num], i);
		end[num] = std::max(end[num], i);
		if (start[num] != i)
		{
			//Assert the current assumption that the blocks are contiguous
			assert(blockNumber[i] == blockNumber[i-1]);
		}
	}

	assert(instance.N > 1);

	blocks.setAll(/*conflicting*/false);

	for (const VertexColorer::Link& link : instance.constraintOrFriendshipIterable())
	{
		blocks.addConstraint(blockNumber[link.first], blockNumber[link.second]);
	}

	//Find the articulation points for the reduced block graph
	//If there is one that actually split the graph in more than 1 part, split the graph there and then recombine it
	articulations = blocks.getArticulationPoints();

	return !articulations.empty();
}

template<typename Derived>
bool SplitConflictingBase<Derived>::couldBePerformed()
{

	//If the graph can be divided in unconnected areas, do so and recombine the partial solutions
	//There are two kind of connectedness:
	//-two node are connected if there is no constraint between them
	//	in this case when combining the subsolutions, colors should be keept separated between sub-solutions (since it is guaranteed they conflict)
	//-two node are connected if there is a constraint or a non-zero friendship between them
	//	in this case the same colors can be reused, since there are no possible conflicts
	assert(this->instance.areAllAlive());

	eqClasses.grow(this->instance.N);
	llvm::BitVector region(this->instance.N, false);
	llvm::BitVector processed(this->instance.N, false);

	for (uint32_t i=0; i<this->instance.N; i++)
	{
		if (processed[i])
			continue;

		this->instance.floodFillOnBits(region, i, conflicting);
		processed |= region;
		for (uint32_t j=0; j<this->instance.N; j++)
		{
			if (!region[j])
				continue;
			eqClasses.join(i, j);
		}
	}

	eqClasses.compress();

	return (eqClasses.getNumClasses() > 1);
}

bool SplitArticulation::couldBePerformedPhiEdges()
{
	return whichSubproblem.front() != instance.N;
}

template<typename Derived>
bool SplitConflictingBase<Derived>::couldBePerformedPhiEdges()
{
	eqClasses.uncompress();

	for (const auto& E : this->instance.groupedLinks)
	{
		uint32_t edgeLeader = this->instance.N;
		for (const VertexColorer::Link& link : E)
		{
			const uint32_t a = link.first;
			const uint32_t b = link.second;
			if (edgeLeader == this->instance.N)
				edgeLeader = eqClasses.findLeader(a);
			eqClasses.join(edgeLeader, a);
			eqClasses.join(edgeLeader, b);
		}
	}

	eqClasses.compress();

	if (eqClasses.getNumClasses() < 2)
		return false;

	relabelNodes();

	return true;
}

template<typename Derived>
void SplitConflictingBase<Derived>::dumpSpecificDescription() const
{
	SplitConflictingBase::dumpSubproblems();
}

void SplitArticulation::dumpSpecificDescription() const
{
	llvm::errs() << "clique of size " << numerositySubproblem.front()<<": ";
	SplitArticulation::dumpSubproblems();
}

template<typename Derived>
void SplitConflictingBase<Derived>::dumpSubproblems() const
{
	for (const uint32_t n : numerositySubproblem)
	{
		llvm::errs() << n << " ";
	}
}

void SplitArticulation::dumpSubproblems() const
{
	//On SplitArticulation the 0 index subproblem serve to represent the nodes in the clique we are splitting on, so they are effectively part of all other subproblems as well
	for (uint32_t i=1; i<numerositySubproblem.size(); ++i)
	{
		llvm::errs() << numerositySubproblem[i] << " ";
	}
}

void SplitArticulation::preprocessing(VertexColorer& subsolution) const
{
	subsolution.improveLowerBound(instance.lowerBoundOnNumberOfColors());
	subsolution.avoidPass[VertexColorer::SPLIT_UNCONNECTED]=true;
}

void SplitUnconnected::preprocessing(VertexColorer& subsolution) const
{
	subsolution.improveLowerBound(instance.lowerBoundOnNumberOfColors());

	//TODO: currently, we always call solve(), while solveInvariantsEstablished could in some cases called
	//In the non-conflicting case, friendship have to be possibly unificated
	//sub.establishInvariants();
	subsolution.avoidPass[VertexColorer::SPLIT_UNCONNECTED]=true;
}

void SplitInverseUnconnected::preprocessing(VertexColorer& subsolution) const
{
	subsolution.avoidPass[VertexColorer::SPLIT_CONFLICTING]=true;
}

void SplitUnconnected::postprocessing(VertexColorer& subsolution)
{
	sumColors += subsolution.lowerBoundOnNumberOfColors();
	instance.improveLowerBound(sumColors);
}

void SplitArticulation::postprocessing(VertexColorer& subsolution)
{
	instance.improveLowerBound(subsolution.lowerBoundOnNumberOfColors());
}

void SplitInverseUnconnected::postprocessing(VertexColorer& subsolution)
{
	instance.improveLowerBound(subsolution.lowerBoundOnNumberOfColors());
}

template<typename Derived>
void SplitConflictingBase<Derived>::relabelNodes()
{
	whichSubproblem = this->computeLeaders(eqClasses);
	this->assignIndexes(whichSubproblem, numerositySubproblem, newIndex);
}

void SplitArticulation::relabelNodes()
{
	//There are multiple possible articulation points,
	//We try each one of them, checking how many subproblem that would produce
	//Whenever we compute a succesfull spitting, we exit the function

	const uint32_t M = blockNumber.back() + 1;

	assert(blocks.areAllAlive());
	whichSubproblem = std::vector<uint32_t> (instance.N, instance.N);

	uint32_t finalSplit = instance.N;
	for (const uint32_t split : articulations)
	{
		llvm::IntEqClasses eqClasses;
		eqClasses.grow(blocks.N);
		llvm::BitVector processed(M, false);
		llvm::BitVector region(M, false);
		llvm::BitVector articulation(M);
		articulation.set(split);
		processed.set(split);

		for (uint32_t i=0; i<M; i++)
		{
			if (processed[i])
				continue;

			blocks.floodFillOnBitsWithArticulationPoints(region, i, /*conflicting*/false, articulation);
			processed |= region;
			for (uint32_t j=0; j<M; j++)
			{
				if (!region[j])
					continue;
				if (j == split)
					continue;
				assert(i != split && j!= split);
				eqClasses.join(i, j);
			}
		}

		//Check the phi-edges
		for (const auto& E : instance.groupedLinks)
		{
			uint32_t edgeLeader = M;
			for (const VertexColorer::Link& link : E)
			{
				const uint32_t a = blockNumber[link.first];
				const uint32_t b = blockNumber[link.second];
				if (a == split || b == split)
					continue;
				if (edgeLeader == M)
				{
					edgeLeader = a;
				}
				assert(a != split && b!= split);
				eqClasses.join(edgeLeader, a);
				eqClasses.join(edgeLeader, b);
			}
		}

		eqClasses.compress();

		if (eqClasses.getNumClasses() > 2)
		{
			//Success! Assign each vertex to a subproblem, and break out of the loop
			std::vector<uint32_t> smallWhichSubproblem = computeLeaders(eqClasses, M);
			const uint32_t toBeSwitched = smallWhichSubproblem[split];
			for (auto& x : smallWhichSubproblem)
			{
				if (x == toBeSwitched)
					x = 0;
				else if (x == 0)
					x = toBeSwitched;
			}

			for (uint32_t m=0; m<M; m++)
			{
				if (m == split)
					continue;
				for (uint32_t i=start[m]; i<=end[m]; ++i)
				{
					whichSubproblem[i] = smallWhichSubproblem[m];
				}
			}
			for (uint32_t i=start[split]; i<=end[split]; i++)
			{
				whichSubproblem[i] = 0;
			}
			finalSplit = split;
			break;
		}
	}
	if (finalSplit != instance.N)
		assignIndexes(whichSubproblem, numerositySubproblem, newIndex, end[finalSplit] + 1 - start[finalSplit]);
}

void SplitArticulation::buildSubproblems()
{

	for (uint32_t i=1; i<numerositySubproblem.size(); i++)
	{
		subproblems.emplace_back(numerositySubproblem[i], instance);
	}

	for (const VertexColorer::Link& link : instance.constraintIterable())
	{
		uint32_t a = link.first;
		uint32_t b = link.second;

		if (whichSubproblem[a] > whichSubproblem[b])
			std::swap(a, b);

		if (whichSubproblem[b] == 0)
		{
			//This is part of the articulation clique, so it should be added to each subproblem
			for (VertexColorer& s : subproblems)
			{
				s.addConstraint(newIndex[a], newIndex[b]);
			}
		}
		else if (whichSubproblem[a] == whichSubproblem[b] || whichSubproblem[a] == 0)
		{
			subproblems[whichSubproblem[b]-1].addConstraint(newIndex[b], newIndex[a]);
		}
		else
		{
			llvm_unreachable("There should be no links between subproblems (other than the clique)");
		}
	}
	for (const VertexColorer::Link& link : instance.positiveWeightFriendshipIterable())
	{
		uint32_t a = link.first;
		uint32_t b = link.second;

		if (whichSubproblem[a] > whichSubproblem[b])
			std::swap(a, b);

		assert(whichSubproblem[b] != 0); //It's a clique by construction, so there should be no friendships in a well formed situation between 0 and 0

		if (whichSubproblem[a] == whichSubproblem[b] || whichSubproblem[a] == 0)
		{
			subproblems[whichSubproblem[b]-1].addFriendship(link.weight, newIndex[b], newIndex[a]);
		}
		else
		{
			llvm_unreachable("There should be no links between subproblems (other than the clique)");
		}
	}
	for (const std::vector<VertexColorer::Link>& edge : instance.groupedLinks)
	{
		for (VertexColorer& sub : subproblems)
		{
			sub.addNewEdge();
		}
		for (const VertexColorer::Link& link : edge)
		{
			uint32_t a = link.first;
			uint32_t b = link.second;

			if (whichSubproblem[a] > whichSubproblem[b])
				std::swap(a, b);

			assert(whichSubproblem[b] != 0); //It's a clique by construction, so there should be no friendships in a well formed situation between 0 and 0

			if (whichSubproblem[a] == whichSubproblem[b] || whichSubproblem[a] == 0)
			{
				subproblems[whichSubproblem[b]-1].addOnEdge(newIndex[b], newIndex[a]);
			}
			else
			{
				llvm_unreachable("By construction");
			}
		}
	}
}

void SplitArticulation::reduce()
{
	const uint32_t splitNode = articulations.front();

	std::vector<VertexColorer::Coloring> solutions;
	for (VertexColorer& sub : subproblems)
	{
		preprocessing(sub);
		sub.solve();
		postprocessing(sub);
		solutions.push_back(sub.getSolution());

		VertexColorer::permuteFirstElements(solutions.back(), numerositySubproblem.front());
	}

	instance.retColors.resize(instance.N, 0);
	for (uint32_t i = 0; i<instance.N; i++)
	{
		if (blockNumber[i] == splitNode)
			instance.retColors[i] = i - start[splitNode];
		else
			instance.retColors[i] = solutions[whichSubproblem[i]-1][newIndex[i]];
	}
}

template<typename Derived>
void SplitConflictingBase<Derived>::buildSubproblems()
{

	for (const uint32_t dim : numerositySubproblem)
	{
		subproblems.emplace_back(dim, this->instance);
	}

	for (const VertexColorer::Link& link : this->instance.constraintIterable())
	{
		const uint32_t a = link.first;
		const uint32_t b = link.second;
		if (whichSubproblem[a] == whichSubproblem[b])
			subproblems[whichSubproblem[a]].addConstraint(newIndex[a], newIndex[b]);
	}
	for (const VertexColorer::Link& link : this->instance.positiveWeightFriendshipIterable())
	{
		const uint32_t a = link.first;
		const uint32_t b = link.second;
		assert(whichSubproblem[a] == whichSubproblem[b]);
		subproblems[whichSubproblem[a]].addFriendship(link.weight, newIndex[a], newIndex[b]);
	}
	for (const std::vector<VertexColorer::Link>& edge : this->instance.groupedLinks)
	{
		for (VertexColorer& sub : subproblems)
		{
			sub.addNewEdge();
		}
		for (const VertexColorer::Link& link : edge)
		{
			const uint32_t a = link.first;
			const uint32_t b = link.second;
			assert(whichSubproblem[a] == whichSubproblem[b]);
			subproblems[whichSubproblem[a]].addOnEdge(newIndex[a], newIndex[b]);
		}
	}
}

template<typename Derived>
void SplitConflictingBase<Derived>::reduce()
{
	std::vector<VertexColorer::Coloring> solutions;

	for (VertexColorer& sub : subproblems)
	{
		getDerived().preprocessing(sub);
		sub.solve();
		getDerived().postprocessing(sub);

		solutions.push_back(sub.getSolution());
	}

	std::vector<uint32_t> toAdd(solutions.size(), 0);
	if (conflicting)
	{
		for (uint32_t i=1; i<solutions.size(); i++)
		{
			toAdd[i] = toAdd[i-1] + VertexColorer::computeNumberOfColors(solutions[i-1]);
		}
	}

	this->instance.retColors.resize(this->instance.N);
	for (uint32_t i = 0; i<this->instance.N; i++)
	{
		this->instance.retColors[i] = solutions[whichSubproblem[i]][newIndex[i]] + toAdd[whichSubproblem[i]];
	}
}

void VertexColorer::establishInvariantsFriendships()
{
	//Standardize the edge representation
	for (Friendship& a : friendships)
	{
		if (a.second.first > a.second.second)
		{
			std::swap(a.second.first, a.second.second);
		}
		assert(a.second.first != a.second.second);
	}

	//Merge equivalents
	std::sort(friendships.begin(), friendships.end(), [](const Friendship& a, const Friendship& b) -> bool
			{
				if (a.second.first != b.second.first)
					return a.second.first < b.second.first;
				return a.second.second < b.second.second;
			});
	uint32_t i = 0, j=0;
	while (j < friendships.size())
	{
		if (i == j)
			j++;
		else if (friendships[i].second == friendships[j].second)
		{
			friendships[i].first += friendships[j].first;
			friendships[j].first = -1;
			j++;
		}
		else
			i=j;
	}

	//Sort in inverse weight order
	std::sort(friendships.begin(), friendships.end(), [](const Friendship& a, const Friendship& b)->bool
			{
				return a.first > b.first;
			});

	friendships.erase(std::remove_if(friendships.begin(), friendships.end(), [](const Friendship& a) -> bool
			{
				return a.first == (uint32_t)-1;
			}
			), friendships.end());
}

void VertexColorer::establishInvariantsFriends()
{
	for (std::vector<Friend> & F : friends)
	{
		establishInvariantsFriend(F);
	}
}

void VertexColorer::establishInvariantsFriend(std::vector<Friend>& F)
{
	std::sort(F.begin(), F.end());

	uint32_t i = 0, j = 0;

	//Merge equivalent friends
	while (j < F.size())
	{
		if (i == j)
			++j;
		else if (F[i].first == F[j].first)
		{
			F[i].second += F[j].second;
			F[j].second = 0;
			++j;
		}
		else
			i=j;
	}

	F.erase(std::remove_if(F.begin(), F.end(), [](const Friend& a) -> bool
			{
				return a.second == 0;
			}
			), F.end());
}

bool VertexColorer::friendshipsInvariantsHolds() const
{
	//friendships have to be sorted in decreasing weight
	for (uint32_t i=1; i<friendships.size(); i++)
	{
		if (friendships[i-1].first < friendships[i].first)
			return false;
	}
	std::vector<std::pair<uint32_t, uint32_t>> edges;
	for (const Friendship& F : friendships)
	{
		edges.push_back(F.second);
	}
	std::sort(edges.begin(), edges.end());
	for (uint32_t i=1; i<edges.size(); i++)
	{
		if (edges[i-1] == edges[i])
			return false;
	}
	return true;
}

bool VertexColorer::friendInvariantsHolds() const
{
	for (uint32_t j=0; j<friends.size(); ++j)
	{
		const std::vector<Friend>& F = friends[j];

		for (uint32_t i=0; i<F.size(); i++)
		{
			if (constraints[F[i].first][j])
				return false;
			if (F[i].second == 0 || F[i].first == j)
				return false;
		}
		for (uint32_t i=1; i<F.size(); i++)
		{
			if (F[i-1].first >= F[i].first)
				return false;
		}
	}
	return true;
}

bool VertexColorer::checkConstraintsAreRespected(const Coloring& colors) const
{
	assert(colors.size() == N);
	for (const Link& link : constraintIterable())
	{
		assert(link.first != link.second);
		if (colors[link.first] == colors[link.second])
			return false;
	}
	return true;
}

void VertexColorer::buildOrderedLinks()
{
	std::vector<std::pair<std::pair<uint32_t,uint32_t>, uint32_t>> orderedFriendships;
	for (uint32_t i=0; i<friendships.size(); i++)
	{
		orderedFriendships.push_back({friendships[i].second, i});
	}
	for (uint32_t i=0; i<groupedLinks.size(); i++)
	{
		const auto& V = groupedLinks[i];
		for (const Link& link : V)
		{
			orderedLinks.push_back({link, i});
		}
	}

	//Lexicographical order work in this case
	std::sort(orderedFriendships.begin(), orderedFriendships.end());

	std::sort(orderedLinks.begin(), orderedLinks.end(), [](const std::pair<Link, uint32_t>& a, const std::pair<Link,uint32_t>& b) -> bool
			{
				if (a.first.first != b.first.first)
					return a.first.first < b.first.first;
				return a.first.second < b.first.second;
			});

	uint32_t i=0, j=0;

	while (i<orderedFriendships.size() && j<orderedLinks.size())
	{
		if (orderedFriendships[i].first.first != orderedLinks[j].first.first)
		{
			if (orderedFriendships[i].first.first < orderedLinks[j].first.first)
				++i;
			else
			{
				assert(false);
				++j;
			}
			continue;
		}
		if (orderedFriendships[i].first.second != orderedLinks[j].first.second)
		{
			if (orderedFriendships[i].first.second < orderedLinks[j].first.second)
				++i;
			else
			{
				assert(false);
				++j;
			}
			continue;
		}
		//If we are here, it means they are equal
		orderedLinks[j].first.weight = orderedFriendships[i].second;
		++j;
	}

	std::sort(orderedLinks.begin(), orderedLinks.end(), [](const std::pair<Link, uint32_t>& a, const std::pair<Link,uint32_t>& b) -> bool
			{
				return a.first.weight < b.first.weight;
			});
}

void VertexColorer::establishInvariants()
{
	establishInvariantsGroupedLinks();
	establishInvariantsFriends();
	establishInvariantsFriendships();
}

void VertexColorer::solve()
{
#ifdef REGISTERIZE_STATS
	REGISTERIZE_STATISTICS_ON_PROBLEMS_COLORED.add(N);
#endif
	establishInvariants();
	solveInvariantsAlreadySet();
	assert(checkConstraintsAreRespected(retColors));
}

void VertexColorer::solveInvariantsAlreadySet()
{
	//Small cases have obvious solutions
	if (N <= 1)
	{
		retColors = Coloring(N, 0);
		return;
	}
	assert(friendships.empty());
	assert(friendInvariantsHolds());

	{	//If the inverese connection graph is unconnected, split!
		SplitInverseUnconnected reduction(*this);
		if (reduction.perform())
			return;
	}

	{	//If the connection graph is unconnected, split!
		SplitUnconnected reduction(*this);
		if (reduction.perform())
			return;
	}

	{	//If there is clique that splits the graph (sort of an articulation point), do it
		SplitArticulation reduction(*this, /*limitSize*/false);
		if (reduction.perform())
			return;
	}

	{	//Single node articulation point are mostly treated in the previous case,
		//but there are some cases in which is the clique that should be splitted that are not treated, so we do another pass keeping the nodes from forming a clique
		SplitArticulation reduction(*this, /*limitSize*/true);
		if (reduction.perform())
			return;
	}

	{	//If there are rows with no weighted friends and less than lowerBound constraints, remove them
		RemoveFewConstraints reduction(*this);
		if (reduction.perform())
			return;
	}

	{	//If there are rows dominated by others, remove them
		RemoveDominated reduction(*this);
		if (reduction.perform())
			return;
	}

	{	//Take the most complex phi-edges, either apply it or skip it
		EnumerateAllPhiEdges reduction(*this);
		if (reduction.perform())
			return;
	}

	//TODO: whenever there are 2 clique, and the only additional constraints/friends are between them, they can be solved independenty from the rest

	//TODO: introduce state as not redo unnecessary computations (eg after splitConflicting(true), there is no need to run it again, only certain optimizations require to re-run on the whole)

	buildFriendships();
	establishInvariants();
	assert(friendshipsInvariantsHolds());
	buildOrderedLinks();

#ifdef REGISTERIZE_STATS
	REGISTERIZE_STATISTICS_ON_PROBLEMS_COLORED_BY_DFS.add(N);
#endif


	IterationsCounter counter(times);
	//iterativeDeepening takes care of exploring as much of the Zykov tree as possible given the resources
	//Zykov tree: the tree generated by either merging two registers or setting a constraint between them
	iterativeDeepening(counter);

	if (counter.remaining() == 0)
		isOptimal = false;
#ifdef REGISTERIZE_DEBUG
	const Coloring& colors = retColors;
	if (colors.size() > 1)
	{
		llvm::errs() <<"|"<< std::string(depthRecursion, ' ') << "Solving subproblem of size\t" << colors.size() << ":\tscore="; 
		llvm::errs() << computeScore(colors, lowerBoundOnNumberOfColors(/*forceEvaluation*/true)) <<"\tcolors (minimal/current)=" << lowerBoundOnNumberOfColors(/*forceEvaluation*/true) << "/"<<computeNumberOfColors(colors)<<"\n";
		llvm::errs() << "| |\t\t";
		llvm::errs() << "greedy evaluations=" << debugStats[GREEDY_EVALUATIONS] << "\t";
		llvm::errs() << "node visited=" << debugStats[NODE_VISITED] << "\t";
		llvm::errs() << "forced union=" << debugStats[CONTRACTIONS] << "\t";
		llvm::errs() << "forced separation=" << debugStats[SEPARATIONS] << "\t";
		if (counter.remaining() == 0)
			llvm::errs() << "search not exausted";
		llvm::errs() << "\n";
		dump();
	}
#endif
}

uint32_t findRepresentative(std::vector<uint32_t>& parent, uint32_t index)
{
	assert(index < parent.size());
	while (parent[index] != index)
	{
		index = parent[index];
	}
	return parent[index];
}

void Registerize::RegisterAllocatorInst::solve()
{
#ifdef REGISTERIZE_STATS
	REGISTERIZE_STATISTICS_ON_INPUT_PROBLEMS.add(numInst());
#endif
	VertexColorer colorer(numInst(), /*cost of using an extra color*/6, MAXIMAL_NUMBER_OF_ITERATIONS_VERTEX_COLORER);
	colorer.setAll(/*conflict*/true);
	//TODO: fine tune the paramethers

	for (uint32_t i=0; i<numInst(); i++)
	{
		const auto& F = friends[i];
		for (const auto& x : F)
		{
			colorer.addWeightedFriendship(i, x.first, x.second);
		}
		for (uint32_t j=i+1; j<numInst(); j++)
		{
			if (!bitsetConstraint[i][j])
				colorer.addAllowed(i, j);
		}
	}
	uint32_t phiEdgesAdded = 0;
#ifdef REGISTERIZE_DEBUG
	uint32_t unsatisfayable = 0;
#endif
	for (const auto& E : edges)
	{
		bool allPossiblySatisfyable = true;
		for (const auto&e : E)
		{
			if (e.first != e.second && bitsetConstraint[e.first][e.second])
			{
				allPossiblySatisfyable = false;
				break;
			}
		}
		if (!allPossiblySatisfyable)
			continue;
		llvm::IntEqClasses eqClasses(numInst());
		for (const auto&e : E)
		{
			eqClasses.join(e.first, e.second);
		}
		std::vector<std::pair<uint32_t, uint32_t>> V;
		for (uint32_t i=0; i<numInst(); i++)
		{
			V.push_back({eqClasses.findLeader(i), i});
		}
		std::sort(V.begin(), V.end());
		for (uint32_t i=0; i<V.size(); i++)
		{
			for (uint32_t j=i+1; j<V.size() && V[i].first == V[j].first; j++)
			{
				if (bitsetConstraint[V[i].second][V[j].second])
				{
					allPossiblySatisfyable = false;
					i = V.size();
					break;
				}
			}
		}
		if (allPossiblySatisfyable)
		{
			++phiEdgesAdded;
			colorer.addNewEdge();
			for (const auto&e : E)
			{
				if (e.first == e.second)
					continue;
				colorer.addOnEdge(e.first, e.second);
			}
		}
#ifdef REGISTERIZE_DEBUG
		else
		{
			++unsatisfayable;
		}
#endif
	}

#ifdef REGISTERIZE_DEBUG
	llvm::errs () << "\n\nSolving function of size " << numInst() << " with " << phiEdgesAdded << " phi edges";
	if (unsatisfayable>0)
		llvm::errs() << "("<<unsatisfayable<< " already elimined)";
	llvm::errs() << "\n";
#endif

	colorer.solve();
	const std::vector<uint32_t>& color = colorer.getSolution();

	for (uint32_t i = 0; i<color.size(); i++)
	{
		for (uint32_t j=i+1; j<color.size(); j++)
		{
			if (color[i] == color[j] && isAlive(findParent(i)) && isAlive(findParent(j)) && findParent(i)!=findParent(j))
			{
				mergeVirtual(findParent(i), findParent(j));
			}
		}
	}
}

void Registerize::handlePHI(const Instruction& I, const LiveRangesTy& liveRanges, llvm::SmallVector<RegisterRange, 4>& registers, const PointerAnalyzer& PA)
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
			const Instruction* usedI=getUniqueIncomingInst(op, PA);
			if(!usedI)
				continue;
			assert(!isInlineable(*usedI, PA));
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
		const Instruction* usedI=getUniqueIncomingInst(op, PA);
		if(!usedI)
			continue;
		assert(!isInlineable(*usedI, PA));
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
	if(t->isIntegerTy(64))
		return INTEGER64;
	else if(t->isIntegerTy())
		return INTEGER;
	// We distinguish between FLOAT and DOUBLE only in asmjs functions
	// We don't in actual asm.js if fround is not available. We always do in wasm
	else if(asmjs && (froundAvailable || wasm) && t->isFloatTy())
		return FLOAT;
	else if(t->isFloatingPointTy())
		return DOUBLE;
	// Raw pointers are just integers
	else if(TypeSupport::isRawPointer(t, asmjs))
		return INTEGER;
	// NOTE: the Void type is considered an OBJECT
	else
		return OBJECT;
}

bool Registerize::LiveRange::invariantsHold() const
{
	assert(std::is_sorted(begin(), end()));
	LiveRange::const_iterator it = begin();
	if (it == end())
		return true;
	LiveRange::const_iterator next = begin();
	++next;
	while (next != end())
	{
		if (it->end >= next->start)
			return false;
		it = next;
		++next;
	}
	for (LiveRange::const_iterator it=begin(); it!=end(); ++it)
	{
		if (it->end < it->start || it->end == 0)
			return false;
	}
	return true;
}

bool Registerize::LiveRange::doesInterfere(uint32_t id) const
{
	//The range are sosted by construction and never interfere, so we can do a binary search
#ifndef NDEBUG
	bool solution = false;
	for (const LiveRangeChunk& c: *this)
	{
		if (c.start <= id && c.end > id)
		{
			solution = true;
		}
	}
#endif
	uint32_t low = 0;
	uint32_t high = size();
	while (low < high)
	{
		const uint32_t med = (low + high)/2;
		const LiveRangeChunk& middle = operator[](med);
		if (middle.start <= id && middle.end > id)
		{
			assert(solution);
			return true;
		}
		if (id < middle.start)
			high = med;
		else
			low = med + 1;
	}
	assert(!solution);
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
	assert(invariantsHold());
	assert(other.invariantsHold());

	insert(end(), other.begin(), other.end());
	std::inplace_merge(begin(), end() - other.size(), end());
	for (LiveRange::iterator a = begin(), b = begin(); b < end(); )
	{
		if (a >= b || b->end == 0)
		{
			++b;
		}
		else if (a->end == 0)
		{
			++a;
		}
		else if (a->end == b->start)
		{
			//Merge contiguous ranges into a
			a->end = b->end;
			b->end = 0;
			b++;
		}
		else
		{
			a++;
		}
	}
	erase(std::remove_if(begin(), end(), [](const LiveRangeChunk& c) -> bool
			{
				return (c.end == 0);
			}
			), end());

	assert(invariantsHold());
}

void Registerize::LiveRange::dump() const
{
	for(const Registerize::LiveRangeChunk& chunk: *this)
		dbgs() << '[' << chunk.start << ',' << chunk.end << ')';
	dbgs() << "\n";
}

bool Registerize::couldBeMerged(const RegisterRange& a, const RegisterRange& b)
{
	if(a.info.regKind!=b.info.regKind)
		return false;
	return !a.range.doesInterfere(b.range);
}

bool Registerize::couldBeMerged(const std::pair<const RegisterRange&, uint32_t>& a, const std::pair<const RegisterRange&, uint32_t>& b)
{
	//If a LiveRange represent an SSA instruction, his definition dominates all the uses
	//This means that we only need to check the definitions location against the live range of the other
	//This make sense thinking of it in the dominator tree:
	//Either one instruction dominates the other, and then the only problem we may have is if that the uppermost collides with the definition of the lower one
	//Or they are in two separate sub-trees, but then they can never interfere
	//TODO: a function numbering accoring to a visit of the DT could simplify this code

	//The ideas comes from here: "Revisiting Out-of-SSA Translation for Correctness, Code Quality, and Efficiency"
	//				https://hal.inria.fr/inria-00349925v1/document

	if(a.first.info.regKind!=b.first.info.regKind)
		return false;
	if (a.first.range.doesInterfere(b.second))
		return false;
	if (b.first.range.doesInterfere(a.second))
		return false;
	return true;
}

void Registerize::mergeRegisterInPlace(RegisterRange& a, const RegisterRange& b)
{
	a.range.merge(b.range);
	a.info.needsSecondaryName |= b.info.needsSecondaryName;
}

Registerize::RegisterRange Registerize::mergeRegister(const RegisterRange& a, const RegisterRange& b)
{
	RegisterRange tmp(a);
	mergeRegisterInPlace(tmp, b);
	return tmp;
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

void Registerize::FloodFillState::addSource(const BasicBlock* BB)
{
	assert(BB->getParent() == &F);
	toBeVisited</*isForwardVisit*/true>(BB);
}

void Registerize::FloodFillState::addSink(const BasicBlock* BB)
{
	assert(BB->getParent() == &F);
	toBeVisited</*isForwardVisit*/false>(BB);
}

void Registerize::FloodFillState::addLifetimeEnd(const BasicBlock* BB)
{
	assert(BB->getParent() == &F);
	BasicBlockInfo& info = getInfo(BB);
	info.hasLifetimeEnd = true;
}

void Registerize::FloodFillState::addLifetimeStart(const BasicBlock* BB)
{
	assert(BB->getParent() == &F);
	BasicBlockInfo& info = getInfo(BB);
	info.hasLifetimeStart = true;
}

void Registerize::FloodFillState::processAlloca()
{
	{
		auto& queue = getToProcessQueue</*isForwardVisit*/true>();
		while (!queue.empty())
		{
			const BasicBlock* BB = queue.front();
			queue.pop();
			processForward(BB);
		}
	}
	{
		auto& queue = getToProcessQueue</*isForwardVisit*/false>();
		while (!queue.empty())
		{
			const BasicBlock* BB = queue.front();
			queue.pop();
			processBackward</*isComplete*/true>(BB);
		}
	}
}

template <bool isForwardVisit>
std::queue<const llvm::BasicBlock*>& Registerize::FloodFillState::getToProcessQueue()
{
	return isForwardVisit ? toProcessForward : toProcessBackward;
}

template <bool isForwardVisit>
void Registerize::FloodFillState::toBeVisited(const BasicBlock* BB)
{
	BasicBlockInfo& info = getInfo(BB);

	bool& alreadyQueued = isForwardVisit ? info.visitedForward : info.visitedBackward;

	if (!alreadyQueued)
	{
		getToProcessQueue<isForwardVisit>().push(BB);
		alreadyQueued = true;
	}
}

template <bool isForwardVisit>
void Registerize::FloodFillState::visitSegment(BasicBlockInfo& info, const SegmentKind& segmentKind)
{
	SegmentKind& bitMaskVisited = isForwardVisit ? info.segmentsOnVisitForward : info.segmentsOnVisitBackward;

	bitMaskVisited = SegmentKind(bitMaskVisited | segmentKind);
}

void Registerize::FloodFillState::processForward(const BasicBlock* BB)
{
	const bool isForwardVisit = true;

	BasicBlockInfo& info = getInfo(BB);

	visitSegment<isForwardVisit>(info, SegmentKind::Middle);

	if (info.hasLifetimeEnd)
		return;

	visitSegment<isForwardVisit>(info, SegmentKind::End);

	for (const BasicBlock* N : successors(BB))
	{
		BasicBlockInfo& infoN = getInfo(N);

		visitSegment<isForwardVisit>(infoN, SegmentKind::Begin);

		if (infoN.hasLifetimeStart)
			continue;

		toBeVisited<isForwardVisit>(N);
	}
}

template <bool isComplete>
void Registerize::FloodFillState::processBackward(const BasicBlock* BB)
{
	const bool isForwardVisit = false;

	BasicBlockInfo& info = getInfo(BB);

	visitSegment<isForwardVisit>(info, SegmentKind::Middle);

	//We have visited a block both forward and backward, so the Alloca is alive in some segment of the BB
	if (isComplete)
		doublyVisitedBlocks.push_back(BB);

	if (info.hasLifetimeStart)
		return;

	visitSegment<isForwardVisit>(info, SegmentKind::Begin);

	for (const BasicBlock* N : predecessors(BB))
	{
		BasicBlockInfo& infoN = getInfo(N);

		visitSegment<isForwardVisit>(infoN, SegmentKind::End);

		if (isComplete && !infoN.visitedForward)
			continue;	//On a backward visit we should visit only the subset of already forward visited nodes

		if (infoN.hasLifetimeEnd)
			continue;

		toBeVisited<isForwardVisit>(N);
	}
}

Registerize::FloodFillState::BasicBlockInfo& Registerize::FloodFillState::getInfo(const BasicBlock* BB)
{
	if (!mapBBtoInfo.count(BB))
		mapBBtoInfo.emplace(BB, BasicBlockInfo());

	return mapBBtoInfo[BB];
}

std::vector<std::pair<const llvm::BasicBlock*, const Registerize::FloodFillState::SegmentKind>> Registerize::FloodFillState::aliveSegments()
{
	std::vector<std::pair<const llvm::BasicBlock*, const SegmentKind>> segments;

	for (const auto& X : doublyVisitedBlocks)
	{
		const SegmentKind visited = getInfo(X).visitedBothWays();

		segments.push_back({X, visited});
	}

	return segments;
}

void Registerize::computeAllocaLiveRanges(AllocaSetTy& allocaSet, const InstIdMapTy& instIdMap)
{
	for(const AllocaInst* alloca: allocaSet)
	{
		FloodFillState floodFillState(*alloca->getFunction());

		RangeChunksTy ranges;
		// For each alloca gather all uses and derived uses
		InstructionSetOrderedByID allUses=gatherDerivedMemoryAccesses(alloca, instIdMap, floodFillState);
		if(allUses.empty())
		{
			// Initialize an empty live range to signal that no analysis is possible
			allocaLiveRanges[alloca];
			continue;
		}

		std::unordered_map<const BasicBlock*, uint32_t> minInstructionId;
		std::unordered_map<const BasicBlock*, uint32_t> maxInstructionId;

		auto getMinInstId = [&minInstructionId](const BasicBlock* bb) -> uint32_t&
		{
			auto it = minInstructionId.find(bb);
			if (it == minInstructionId.end())
				it = minInstructionId.insert({bb, -1}).first;

			return it->second;
		};
		auto getMaxInstId = [&maxInstructionId](const BasicBlock* bb) -> uint32_t&
		{
			auto it = maxInstructionId.find(bb);
			if (it == maxInstructionId.end())
				it = maxInstructionId.insert({bb, 0}).first;

			return it->second;
		};

		for (const Instruction* I : allUses)
		{
			const BasicBlock* parentBB = I->getParent();

			const uint32_t instId = instIdMap.find(I)->second;

			uint32_t& currMin = getMinInstId(parentBB);
			currMin = std::min(currMin, instId);

			uint32_t& currMax = getMaxInstId(parentBB);
			currMax = std::max(currMax, instId);

			floodFillState.addSource(parentBB);
			floodFillState.addSink(parentBB);
		}

		floodFillState.processAlloca();

		const auto& segments = floodFillState.aliveSegments();

		//Find the starting ID on a given BasicBlock (mirror of findEndId)
		auto findStartId = [&instIdMap, &getMinInstId](const FloodFillState::SegmentKind& livenessBitmask, const BasicBlock* BB)
		{
			//IF it's not marked as alive AND we have another valid starting point, return that
			if ((livenessBitmask & FloodFillState::SegmentKind::Begin) == false)
			{
				//This might fail since we might not have a valid instruction in the BB
				//(eg. the entry point or a lifetime_start not included in allUses)
				const uint32_t id = getMinInstId(BB);
				if (id != getMinInstId(nullptr))
					return id;
			}

			//Otherwise return the first instruction
			return instIdMap.find(&*BB->begin())->second;
		};

		//Finding the ending ID on a given BasicBlock (mirror of findStartId)
		auto findEndId = [&instIdMap, &getMaxInstId](const FloodFillState::SegmentKind& livenessBitmask, const BasicBlock* BB)
		{
			//IF it's not marked as alive AND we have another valid ending point, return that
			if ((livenessBitmask & FloodFillState::SegmentKind::End) == false)
			{
				//This might fail since we might not have a valid instruction in the BB
				//(eg. the functions returns or a lifetime_end not included in allUses)
				const uint32_t id = getMaxInstId(BB);
				if (id != getMaxInstId(nullptr))
					return id;
			}

			//Otherwise return the last instruction
			return instIdMap.find(&*BB->rbegin())->second;
		};

		for (const auto& s : segments)
		{
			const BasicBlock* BB = s.first;
			const FloodFillState::SegmentKind livenessBitmask = s.second;

			assert( livenessBitmask | FloodFillState::SegmentKind::Middle );

			const uint32_t startId = findStartId(livenessBitmask, BB);
			const uint32_t endId = findEndId(livenessBitmask, BB);

			ranges[startId] = endId + 1;
		}

		// Construct the vector of use ranges
		LiveRange& allocaRanges=allocaLiveRanges[alloca];
		for(auto& it: ranges)
		{
			allocaRanges.extendOrPush(LiveRangeChunk(it.first, it.second));
		}
	}
}

/*
	Returns the set of instruction which access memory derived from the passed Alloca
*/
Registerize::InstructionSetOrderedByID Registerize::gatherDerivedMemoryAccesses(const AllocaInst* rootI, const InstIdMapTy& instIdMap, FloodFillState& floodFillState)
{
	SmallVector<const Use*, 10> allUses;
	for(const Use& U: rootI->uses())
		allUses.push_back(&U);

	bool escapes = false;
	SmallVector<const IntrinsicInst*, 1> lifetimeStarts;
	SmallVector<const IntrinsicInst*, 1> lifetimeEnds;
	SmallPtrSet<const Instruction*, 4> escapingInsts;
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
					escapingInsts.insert(I);
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
				if(F)
				{
					// Lifetime intrinsics are ok
					if(F->getIntrinsicID()==Intrinsic::lifetime_start)
					{
						floodFillState.addLifetimeStart(CI->getParent());
						lifetimeStarts.push_back(cast<IntrinsicInst>(CI));
						break;
					}
					else if(F->getIntrinsicID()==Intrinsic::lifetime_end)
					{
						floodFillState.addLifetimeEnd(CI->getParent());
						lifetimeEnds.push_back(cast<IntrinsicInst>(CI));
						break;
					}
					// This escapes, unless the argument is flagged as nocapture
					//NOTE: Parameter attributes start at index 1
					if(F->getAttributes().hasAttribute(U->getOperandNo()+1, Attribute::NoCapture))
						break;
				}
				escapingInsts.insert(CI);
				break;
			}
			default:
				// Be conservative
				escapes = true;
				break;
		}
	}

	for (auto E: escapingInsts)
	{
		bool dominated = false, postdominated = false;
		for (auto S: lifetimeStarts)
		{
			if (DT->dominates(S, E))
			{
				dominated = true;
				break;
			}
		}
		if (!dominated)
		{
			escapes = true;
			break;
		}
		auto postDominates = [this](const Instruction* A, const Instruction* B) -> bool
		{
			//If they are the same instruction, there can't be no postdomination
			if (A == B)
				return false;

			const BasicBlock* blockA = A->getParent();
			const BasicBlock* blockB = B->getParent();

			if (blockA != blockB)
			{
				//If the blocks are different, then PDT can be queried whether blockA is postdominator than blockB
				return PDT->dominates(blockA, blockB);
			}
			else
			{
				//If the blocks are equal AND the intruction different,
						//then DT can be queried whether B dominates A
						//(that means that A postDominates B)
				return DT->dominates(B, A);
			}
		};
		for (auto S: lifetimeEnds)
		{
			if (postDominates(S, E))
			{
				postdominated = true;
				break;
			}
		}
		if (!postdominated)
		{
			escapes = true;
			break;
		}
	}
	if (escapes)
	{
		allUses.clear();
	}
	InstructionSetOrderedByID ret(instIdMap);
	for(const Use* U: allUses)
	{
		Instruction* userI=cast<Instruction>(U->getUser());
		// Skip instruction which only touch the pointer and not the actual memory
		if(isa<BitCastInst>(userI) || isa<GetElementPtrInst>(userI))
			continue;
		// Skip instructions that are enclosed in lifetime_start/lifetime_end
		if (escapingInsts.count(userI))
			continue;
		// If no instruction escapes, do not include the lifetime intrinsics
		if(IntrinsicInst* II = dyn_cast<IntrinsicInst>(userI))
		{
			if(escapingInsts.empty() &&
				(II->getIntrinsicID() == Intrinsic::lifetime_start || II->getIntrinsicID() == Intrinsic::lifetime_end))
				continue;
		}
		ret.insert(userI);
	}
	return ret;
}

void Registerize::invalidateLiveRangeForAllocas(const llvm::Function& F)
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

ModulePass* createRegisterizePass(bool froundAvailable, bool wasm)
{
	return new Registerize(froundAvailable, wasm);
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(Registerize, "Registerize", "Allocate stack registers for each virtual register",
			false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTreeWrapperPass)
INITIALIZE_PASS_END(Registerize, "Registerize", "Allocate stack registers for each virtual register",
			false, false)

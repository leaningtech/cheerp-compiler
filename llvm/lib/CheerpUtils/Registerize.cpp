
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
	AU.addRequired<LoopInfoWrapperPass>();
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

void Registerize::computeLiveRangeForAllocas(Function& F)
{
	assert(!RegistersAssigned);
	if (F.empty())
		return;
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
	bool hasPreds = false;
	for(::pred_iterator it=pred_begin(BB);it!=pred_end(BB);++it)
	{
		hasPreds = true;
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
		void handlePHI(const PHINode* phi, const Value* incoming, bool selfReferencing) override
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
#ifdef REGISTERIZE_DEBUG
	const uint32_t originalRegistersSize = registers.size();
#endif

	for (const BasicBlock & bb : F)
	{
		const TerminatorInst* term=bb.getTerminator();
		for(uint32_t i=0;i<term->getNumSuccessors();i++)
		{
			//TODO: improve how thet are assigned
			const BasicBlock* succBB=term->getSuccessor(i);
			RegisterizePHIHandler(&bb, succBB, *this, registers, instIdMap, PA).runOnEdge(*this, &bb, succBB);
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

Registerize::RegisterAllocatorInst::RegisterAllocatorInst(llvm::Function& F_, const InstIdMapTy& instIdMap, const LiveRangesTy& liveRanges, const PointerAnalyzer& PA, Registerize* registerize)
	: F(F_), registerize(registerize), emptyFunction(false), frequencyInfo(F, registerize->LI)
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
				llvm:: errs() << "\t"<< computeNumberOfColors(colors) <<"\t"<<state.minimalNumberOfColors<< "\t" << localSolution.first <<"\t";
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
				state.currentScore += 3;
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
				state.currentScore -= 3;
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
	while (counter.remaining()>0 && depth <= friendships.size())
	{
#ifdef REGISTERIZE_DEBUG_EXAUSTIVE_SEARCH
		llvm::errs() << "--------------------- starting at "<<previousDepth<<" up to reaching " <<depth<<"\n";
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
		neighboursCount[i] = constraints[i].count();
	}

	llvm::BitVector processed(N);
	std::vector<uint32_t> stack;
	for (uint32_t i=0; i<N; i++)
	{
		uint32_t b=N;
		for (uint32_t j=0; j<N; j++)
		{
			if (res[j] == j && processed[j]==false)
			{
				if (b==N || neighboursCount[j] < neighboursCount[b])
					b = j;
			}
		}
		if (b == N)
			break;
		processed[b] = true;
		stack.push_back(b);
		for (uint32_t j=0; j<N; j++)
		{
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

void Registerize::RegisterAllocatorInst::buildEdgesData(Function& F)
{
	for (const BasicBlock & fromBB : F)
	{
		const TerminatorInst* term=fromBB.getTerminator();
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
	//TODO: implement the writer, and check the list of operations

	//Try to force the first operand of a sum/difference/multiplication/... to be the same of the result
	const Instruction* I = indexer.at(i);
	assert (!!I);

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

void VertexColorer::floodFill(std::vector<uint32_t>& regions, const uint32_t start, const bool conflicting, const uint32_t articulationPoint) const
{
	//Assign every node connected to start to the region labelled start
	//What connected means depends:
	//conflicting == false		there is either a positive-weight friendship or they are adjacent (so there is a constraint)
	//conflicting == true		the node are not adjacent (or there is no constraint)
	//articulationPoint != -1	as above, but it do not count if the vertex is equal to articulationPoint (as to disconnect regions on that point)
	assert(regions[start] == start);

	std::vector<uint32_t> toProcess;
	toProcess.push_back(start);

	while (!toProcess.empty())
	{
		uint32_t x = toProcess.back();
		toProcess.pop_back();

		for (uint32_t i=0; i<N; i++)
		{
			if (i != x && regions[i] == i)
			{
				if (conflicting != constraints[x][i])
				{
					if (i != articulationPoint)
					{
						regions[i] = start;
						toProcess.push_back(i);
					}
				}
			}
		}
		if (!conflicting)
		{
			for (const Friend& f : friends[x])
			{
				uint32_t i = f.first;
				assert(i != x);
				assert(regions[i] == i || regions[i] == start);
				assert(!constraints[x][i]);
				if (regions[i] == i)
				{
					if (i != articulationPoint)
					{
						regions[i] = start;
						toProcess.push_back(i);
					}
				}
			}
		}
	}
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

uint32_t findRepresentative(std::vector<uint32_t>& parent, uint32_t index)
{
	assert(index < parent.size());
	while (parent[index] != index)
	{
		index = parent[index];
	}
	return parent[index];
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

bool Reduction::perform()
{
	if (!couldBePerformed())
		return false;

	//Find relabeling that split the problem into subproblem(s)
	relabelNodes();

	if (!couldBePerformedPhiEdges())
		return false;

#ifdef REGISTERIZE_DEBUG
	//Dump informations
	dumpDescription();
#endif
	//TODO: add here external loop for phi_edge links
	//Build the connection matrix and the friend list for the subproblem(s)
	buildSubproblems();
	//Solve them
	reduce();

	instance.checkConstraintsAreRespected(instance.retColors);

	return true;
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

void RemoveFewConstraints::dumpDescription() const
{
	llvm::errs() << std::string(instance.depthRecursion, ' ') << reductionName() <<":\t"<< instance.N << " -> " << alive.size() << "\n";
}

std::string RemoveFewConstraints::reductionName() const
{
	return "Remove rows with few constraints";
}

void RemoveFewConstraints::buildSubproblems()
{
	subproblems.push_back(VertexColorer(alive.size(), instance));

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

bool RemoveDominated::couldBePerformed()
{
	if (couldBeAvoided())
		return false;
	//Try to find rows that are dominated by another. In this case, they can get the same colors of the dominating vertex and the solutions remain optimal
	//One vertex dominated AND can become parent of another if:
	//-the constraint of the dominated are a (possibly non proper) subset of the constraint of the dominating
	//-there are no constraint between them (otherwise it could be dominating, but it could not be merged)
	//-either the dominated has no friends, or has the dominating as only friend, or in any case the dominating is a dominating friend
	//			(this means that it's the friend that weights 50%+ of the total of the dominating friendships)
	std::vector<uint64_t> samples;

	if (instance.N >= 128)
	{
		//Computing whether a row is a subset of another takes O(N) (possibly every index has to be checked)
		//When N is big enough, we precompute a sample 64 arbitrary columns, and store only this in samples
		//This way first we first could test whether samples[x] is a subset of samples[y], and only whenever this is true we move to the more expensive check
		//In the worst case this still requires O(N), but a lot of cases could be solved in O(1), so while the algorithm complexity remains the same
		//(at least in degenerate cases), there is a practical speed-up
		for (uint32_t i=0; i<instance.N; ++i)
		{
			samples.push_back(computeSample(instance.constraints[i]));
		}
	}

	for (uint32_t i=0; i<instance.N; ++i)
		parent[i] = i;

	//TODO: iterates multiple times
	for (uint32_t i = 0; i<instance.N; i++)
	{
		if (!isAlive(i))
			continue;
		//TODO: having a default constructed identity vector
		if (instance.friends[i].empty())
		{
			for (uint32_t j=0; j<instance.N; j++)
			{
				if (i != j && isAlive(j) && !instance.constraints[i][j] && (samples.empty() || isSubset(samples[i], samples[j])) && isSubset(instance.constraints[i], instance.constraints[j]) )
				{
					parent[i] = j;
					break;
				}
			}
		}
		else
		{
			for (uint32_t j : whoIsDominatingFriend(i))
			{
				if (i != j && isAlive(j) && !instance.constraints[i][j] && (samples.empty() || isSubset(samples[i], samples[j])) && isSubset(instance.constraints[i], instance.constraints[j]) )
				{
					parent[i] = j;
					break;
				}
			}
		}
	}

	for (uint32_t i=0; i<instance.N; i++)
	{
		if (!isAlive(i))
			continue;
		alive.push_back(i);
	}

	return alive.size() < instance.N;
}

bool RemoveDominated::couldBePerformedPhiEdges()
{
	for (const auto& E : instance.groupedLinks)
	{
		for (const VertexColorer::Link& link : E)
		{
			const uint32_t a = link.first;
			const uint32_t b = link.second;
			uint32_t Pa = a;
			uint32_t Pb = b;
			//TODO: create findParentNoShortening
			while (parent[Pa] != Pa)
			{
				Pa = parent[Pa];
			}
			while (parent[Pb] != Pb)
			{
				Pb = parent[Pb];
			}
			//If they end up with the same ancestor, it's good
			if (Pa == Pb)
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
	return true;
}

void RemoveDominated::relabelNodes()
{
	uint32_t firstUnused = 0;
	for (uint32_t i=0; i<instance.N; i++)
	{
		if (!isAlive(i))
			continue;
		newIndex[i] = firstUnused++;
	}
}

void RemoveDominated::dumpDescription() const
{
	llvm::errs() <<std::string(instance.depthRecursion, ' ')<< reductionName() << ":\t"<< instance.N << " -> " << alive.size() << "\n";
}

std::string RemoveDominated::reductionName() const
{
	return "Remove dominated rows";
}

void RemoveDominated::buildSubproblems()
{
	subproblems.push_back(VertexColorer(alive.size(), instance));
	VertexColorer& subproblem = subproblems.front();
	ancestor = parent;

	for (const VertexColorer::Link& link : instance.constraintIterable())
	{
		const uint32_t i=findAncestor(link.first);
		const uint32_t j=findAncestor(link.second);
		subproblem.addConstraint(newIndex[i], newIndex[j]);
	}

	//Add friendships (if they do not clash with constraints)
	for (const VertexColorer::Link& link : instance.positiveWeightFriendshipIterable())
	{
		assert(link.weight > 0);
		const uint32_t a = findAncestor(link.first);
		const uint32_t b = findAncestor(link.second);
		assert(instance.constraints[a][b] == instance.constraints[b][a]);
		if (!instance.constraints[a][b])
			subproblem.addFriendship(link.weight, newIndex[a], newIndex[b]);
	}
	for (const std::vector<VertexColorer::Link>& edge : instance.groupedLinks)
	{
		subproblem.addNewEdge();
		for (const VertexColorer::Link& link : edge)
		{
			const uint32_t a = findAncestor(link.first);
			const uint32_t b = findAncestor(link.second);
			assert(instance.constraints[a][b] == instance.constraints[b][a]);
			if (!instance.constraints[a][b])
				subproblem.addOnEdge(newIndex[a], newIndex[b]);
			else
			{
				subproblem.popLastEdge();
				break;
			}
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
	return parent[i] == i;
}

uint32_t RemoveDominated::findAncestor(const uint32_t i)
{
	//We do at most O(N) lazily executed preprocessing + O(1) per call, that averages to O(1) since we do roughly 2*N calls
	if (!isAlive(i))
		ancestor[i] = findAncestor(ancestor[i]);
	return ancestor[i];
}

void RemoveDominated::reduce()
{
	VertexColorer& subproblem = subproblems.front();

	preprocessing(subproblem);
	subproblem.solve();
	postprocessing(subproblem);

	const VertexColorer::Coloring& subcolors = subproblem.getSolution();

	instance.retColors.resize(instance.N);
	for (uint32_t i = 0; i<alive.size(); i++)
	{
		instance.retColors[alive[i]] = subcolors[i];
	}
	for (uint32_t i = 0; i<instance.N; i++)
	{
		assert(isAlive(findAncestor(i)));
		if (!isAlive(i))
			instance.retColors[i] = instance.retColors[findAncestor(i)];
	}
}

bool SplitArticulation::couldBePerformed()
{
	if (couldBeAvoided())
		return false;
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

bool SplitConflictingBase::couldBePerformed()
{
	if (couldBeAvoided())
		return false;

	//If the graph can be divided in unconnected areas, do so and recombine the partial solutions
	//There are two kind of connectedness:
	//-two node are connected if there is no constraint between them
	//	in this case when combining the subsolutions, colors should be keept separated between sub-solutions (since it is guaranteed they conflict)
	//-two node are connected if there is a constraint or a non-zero friendship between them
	//	in this case the same colors can be reused, since there are no possible conflicts
	assert(instance.areAllAlive());

	llvm::BitVector region(instance.N, false);
	llvm::BitVector processed(instance.N, false);
	uint32_t firstUnused = 0;

	for (uint32_t i=0; i<instance.N; i++)
	{
		if (processed[i])
			continue;

		instance.floodFillOnBits(region, i, conflicting);
		processed |= region;
		for (uint32_t j=0; j<instance.N; j++)
		{
			if (!region[j])
				continue;
			whichSubproblem[j] = firstUnused;
			newIndex[j] = numerositySubproblem[i]++;
		}
		assert(numerositySubproblem[i] > 0);
		if (numerositySubproblem[i] < instance.N)
		{
			seeds.push_back(i);
		}
		++firstUnused;
	}

	assert(seeds.size() != 1);

	return (seeds.size() > 1);
}

bool SplitArticulation::couldBePerformedPhiEdges()
{
	for (const auto& E : instance.groupedLinks)
	{
		uint32_t subproblem = instance.N;
		for (const VertexColorer::Link& link : E)
		{
			uint32_t a = link.first;
			uint32_t b = link.second;
			if (whichSubproblem[a] > whichSubproblem[b])
				std::swap(a, b);
			assert(whichSubproblem[b] > 0);
			if (subproblem == instance.N)
				subproblem = whichSubproblem[b];
			if (subproblem != whichSubproblem[b])
				return false;
			if (whichSubproblem[a] != 0 && whichSubproblem[a] != subproblem)
				return false;
		}
	}
	return true;
}

bool SplitConflictingBase::couldBePerformedPhiEdges()
{
	std::vector<uint32_t> parentRegion(seeds.size());
	for (uint32_t i=0; i<parentRegion.size(); i++)
	{
		parentRegion[i] = i;
	}

	for (const auto& E : instance.groupedLinks)
	{
		uint32_t subproblem = instance.N;
		for (const VertexColorer::Link& link : E)
		{
			uint32_t a = findRepresentative(parentRegion, whichSubproblem[link.first]);
			uint32_t b = findRepresentative(parentRegion, whichSubproblem[link.second]);
			assert(parentRegion[a] == a);
			assert(parentRegion[b] == b);
			if (subproblem == instance.N)
				subproblem = a;
			if (a != subproblem)
				parentRegion[a] = subproblem;
			if (b != subproblem)
				parentRegion[b] = subproblem;
		}
	}

	std::vector<uint32_t> representative(parentRegion.size());
	std::set<uint32_t> set;
	for (uint32_t i=0; i<parentRegion.size(); i++)
	{
		representative[i] = findRepresentative(parentRegion, i);
		set.insert(representative[i]);
	}
	if (set.size() < 2)
		return false;


	numerositySubproblem = std::vector<uint32_t> (instance.N, 0);
	newIndex = std::vector<uint32_t> (instance.N, instance.N);
	std::vector<uint32_t> oldSubproblem = whichSubproblem;
	whichSubproblem = std::vector<uint32_t> (instance.N, instance.N);
	uint32_t firstUnused = 0;
	std::vector<uint32_t> newSeeds;
	for (uint32_t R : set)
	{
		newSeeds.push_back(seeds[R]);
		for (uint32_t i=0; i<instance.N; i++)
		{
			if (representative[oldSubproblem[i]] != R)
				continue;
			whichSubproblem[i] = firstUnused;
			newIndex[i] = numerositySubproblem[seeds[R]]++;
		}
		firstUnused++;
	}
	for (uint32_t i=0; i<instance.N; i++)
	{
		assert(whichSubproblem[i] < instance.N);
		assert(newIndex[i] < instance.N);
	}
	seeds = newSeeds;
	return true;
}

void SplitConflictingBase::dumpDescription() const
{
	llvm::errs() <<std::string(instance.depthRecursion, ' ') << reductionName()<<"\t";
	SplitConflictingBase::dumpSubproblems();
	llvm::errs() << "\n";
}

void SplitArticulation::dumpDescription() const
{
	llvm::errs() <<std::string(instance.depthRecursion, ' ') << reductionName();
	if (!limitSize)
	{
		llvm::errs() << " clique of size " << numerositySubproblem[articulations.front()];
	}
	llvm::errs() << ":\t";
	SplitArticulation::dumpSubproblems();
	llvm::errs() << "\n";
}

std::string SplitArticulation::reductionName() const
{
	return "Split on articulation";
}

std::string SplitInverseUnconnected::reductionName() const
{
	return "Split conflicting";
}

std::string SplitUnconnected::reductionName() const
{
	return "Split unconnected";
}

void SplitConflictingBase::dumpSubproblems() const
{
	for (const uint32_t s : seeds)
	{
		llvm::errs() << numerositySubproblem[s] << " ";
	}
}

void SplitArticulation::dumpSubproblems() const
{
	for (const uint32_t s : seeds)
	{
		llvm::errs() << numerositySubproblem[s] << " ";
	}
}

bool RemoveDominated::couldBeAvoided() const
{
	return instance.avoidPass[VertexColorer::REMOVE_DOMINATED];
}

bool SplitArticulation::couldBeAvoided() const
{
	return instance.avoidPass[VertexColorer::SPLIT_ARTICULATION];
}

bool SplitUnconnected::couldBeAvoided() const
{
	return instance.avoidPass[VertexColorer::SPLIT_UNCONNECTED];
}

bool SplitInverseUnconnected::couldBeAvoided() const
{
	return instance.avoidPass[VertexColorer::SPLIT_CONFLICTING];
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

void SplitConflictingBase::relabelNodes()
{
	//Already performed by couldBePerformed()
}

void SplitArticulation::relabelNodes()
{
	const uint32_t M = blockNumber.back() + 1;

	assert(blocks.areAllAlive());

	whichSubproblem = std::vector<uint32_t> (instance.N, instance.N);

	for (const uint32_t split : articulations)
	{
		std::vector<uint32_t> regions = blocks.parent;
		uint32_t firstUnused = 1;

		for (uint32_t i=0; i<M; i++)
		{
			if (regions[i] == i && i != split)
			{
				blocks.floodFill(regions, i, false, split);
				whichSubproblem[start[i]] = firstUnused++;
			}
		}

		for (uint32_t i=start[split]; i<=end[split]; i++)
		{
			whichSubproblem[i] = 0;
			newIndex[i] = numerositySubproblem[split]++;
		}

		numerositySubproblem = std::vector<uint32_t> (M, numerositySubproblem[split]);

		for (uint32_t m=0; m<M; m++)
		{
			if (m == split)
				continue;
			for (uint32_t i=start[m]; i<=end[m]; ++i)
			{
				whichSubproblem[i] = whichSubproblem[start[regions[m]]];
				newIndex[i] = numerositySubproblem[regions[m]]++;
			}
		}

		seeds.clear();

		for (uint32_t i=0; i<M; i++)
		{
			if (numerositySubproblem[i] > numerositySubproblem[split] && numerositySubproblem[i]<instance.N)
			{
				seeds.push_back(i);
			}
		}

		assert (seeds.size() > 1);
	//TODO: this means it splits only on the first articulation point, but could be generalized to split on all at the same time
		break;
	}
}

void SplitArticulation::buildSubproblems()
{
	subproblems.reserve(seeds.size());

	for (uint32_t s : seeds)
	{
		subproblems.push_back(VertexColorer(numerositySubproblem[s], instance));
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

		VertexColorer::permuteFirstElements(solutions.back(), numerositySubproblem[splitNode]);
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

void SplitConflictingBase::buildSubproblems()
{
	subproblems.reserve(seeds.size());

	for (const uint32_t s : seeds)
	{
		subproblems.push_back(VertexColorer(numerositySubproblem[s], instance));
	}

	for (const VertexColorer::Link& link : instance.constraintIterable())
	{
		const uint32_t a = link.first;
		const uint32_t b = link.second;
		if (whichSubproblem[a] == whichSubproblem[b])
			subproblems[whichSubproblem[a]].addConstraint(newIndex[a], newIndex[b]);
	}
	for (const VertexColorer::Link& link : instance.positiveWeightFriendshipIterable())
	{
		const uint32_t a = link.first;
		const uint32_t b = link.second;
		assert(whichSubproblem[a] == whichSubproblem[b]);
		subproblems[whichSubproblem[a]].addFriendship(link.weight, newIndex[a], newIndex[b]);
	}
	for (const std::vector<VertexColorer::Link>& edge : instance.groupedLinks)
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

void SplitConflictingBase::reduce()
{
	std::vector<VertexColorer::Coloring> solutions;

	for (VertexColorer& sub : subproblems)
	{
		preprocessing(sub);
		sub.solve();
		postprocessing(sub);

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

	instance.retColors.resize(instance.N);
	for (uint32_t i = 0; i<instance.N; i++)
	{
		instance.retColors[i] = solutions[whichSubproblem[i]][newIndex[i]] + toAdd[whichSubproblem[i]];
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
	sort(friendships.begin(), friendships.end(), [](const Friendship& a, const Friendship& b) -> bool
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
	sort(friendships.begin(), friendships.end(), [](const Friendship& a, const Friendship& b)->bool
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
	sort(F.begin(), F.end());

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
	sort(edges.begin(), edges.end());
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
	typedef std::pair<std::pair<uint32_t,uint32_t>, uint32_t> FrienshipWithOrder;
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
	sort(orderedFriendships.begin(), orderedFriendships.end());

	sort(orderedLinks.begin(), orderedLinks.end(), [](const std::pair<Link, uint32_t>& a, const std::pair<Link,uint32_t>& b) -> bool
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

	sort(orderedLinks.begin(), orderedLinks.end(), [](const std::pair<Link, uint32_t>& a, const std::pair<Link,uint32_t>& b) -> bool
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
	establishInvariants();
	solveInvariantsAlreadySet();
//	dump();
//	for (uint32_t x : retColors)
//	llvm::errs() << x << " ";
//	llvm::errs() << "\n\n\n";
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

	//TODO: whenever there are 2 clique, and the only additional constraints/friends are between them, they can be solved independenty from the rest

	//TODO: introduce state as not redo unnecessary computations (eg after splitConflicting(true), there is no need to run it again, only certain optimizations require to re-run on the whole)

	buildFriendships();
	establishInvariants();
	assert(friendshipsInvariantsHolds());
	buildOrderedLinks();

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
		llvm::errs() << std::string(depthRecursion, ' ') << "Solving subproblem of size\t" << colors.size() << ":\t(score/colors/iterations)\t"; 
		llvm::errs() << computeScore(colors, lowerBoundOnNumberOfColors(/*forceEvaluation*/true)) <<"\t" << lowerBoundOnNumberOfColors(/*forceEvaluation*/true) << "/"<<computeNumberOfColors(colors)<<"\t\t";
		for (uint32_t i=0; i<debugStats.size(); i++)
		{
			llvm::errs () << debugStats[i] << "\t";
		}
		if (counter.remaining() == 0)
			llvm::errs() << "\tsearch not exausted";
		llvm::errs() << "\n";
		dump();
	}
#endif
}

void Registerize::RegisterAllocatorInst::solve()
{
	VertexColorer colorer(numInst(), /*cost of using an extra color*/6, /*maximal number of iterations*/100);
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
		if (allPossiblySatisfyable)
		{
			colorer.addNewEdge();
			for (const auto&e : E)
			{
				colorer.addOnEdge(e.first, e.second);
			}
		}
	}

#ifdef REGISTERIZE_DEBUG
	llvm::errs () << "\n\nSolving function of size " << numInst() << "\n";
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

bool Registerize::LiveRange::invariantsHold() const
{
	assert(std::is_sorted(begin(), end()));
	for (LiveRange::const_iterator it=begin(); it!=end(); ++it)
	{
		if (it->end == 0)
			return false;
	}
	return true;
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
			allocaRanges.extendOrPush(LiveRangeChunk(it.first, it.second));
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
		if(IntrinsicInst* II = dyn_cast<IntrinsicInst>(userI))
		{
			if(II->getIntrinsicID() == Intrinsic::lifetime_start || II->getIntrinsicID() == Intrinsic::lifetime_end)
				continue;
		}
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

ModulePass* createRegisterizePass(bool useFloats)
{
	return new Registerize(useFloats);
}

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(Registerize, "Registerize", "Allocate stack registers for each virtual register",
			false, false)
INITIALIZE_PASS_END(Registerize, "Registerize", "Allocate stack registers for each virtual register",
			false, false)

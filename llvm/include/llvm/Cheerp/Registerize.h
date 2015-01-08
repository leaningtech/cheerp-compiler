//===-- Cheerp/Registerize.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_REGISTERIZE_H
#define _CHEERP_REGISTERIZE_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include <unordered_set>
#include <unordered_map>

namespace cheerp
{

/**
 * Registerize - Map not-inlineable instructions to the minimal number of local variables
 */
class Registerize : public llvm::ModulePass
{
public:
	struct LiveRangeChunk
	{
		// [start,end)
		uint32_t start;
		uint32_t end;
		LiveRangeChunk(uint32_t s, uint32_t e):start(s),end(e)
		{
		}
		bool operator<(const LiveRangeChunk& r) const
		{
			return start < r.start;
		}
	};
	struct LiveRange: public llvm::SmallVector<LiveRangeChunk, 4>
	{
		LiveRange()
		{
		}
		template<class Iterator>
		LiveRange(const Iterator& begin, const Iterator& end):llvm::SmallVector<LiveRangeChunk, 4>(begin,end)
		{
		}
		bool doesInterfere(const LiveRange& other) const;
		void merge(const LiveRange& other);
		void dump() const;
	};

	static char ID;
	
	explicit Registerize(bool n = false) : ModulePass(ID), NoRegisterize(n) { }
	
	void getAnalysisUsage(llvm::AnalysisUsage & AU) const;

	bool runOnModule(llvm::Module& M) override;
	
	const char *getPassName() const override;

	uint32_t getRegisterId(const llvm::Instruction* I) const;

	void handleFunction(llvm::Function& F);
	void invalidateFunction(llvm::Function& F);

	const LiveRange& getLiveRangeForAlloca(const llvm::AllocaInst* alloca) const
	{
		assert(allocaLiveRanges.count(alloca));
		return allocaLiveRanges.find(alloca)->second;
	}
private:
	// Final data structures
	std::unordered_map<const llvm::Instruction*, uint32_t> registersMap;
	std::unordered_map<const llvm::AllocaInst*, LiveRange> allocaLiveRanges;
	bool NoRegisterize;
	// Temporary data structure used to compute the live range of an instruction
	struct InstructionLiveRange
	{
		// codePathId is used to efficently coalesce uses in a sequential range when possible
		uint32_t codePathId;
		LiveRange range;
		InstructionLiveRange(uint32_t c): codePathId(c)
		{
		}
		void addUse(uint32_t codePathId, uint32_t thisIndex);
	};
	// Map from instructions to their live ranges
	typedef std::unordered_map<llvm::Instruction*, InstructionLiveRange> LiveRangesTy;
	// Map from instructions to their unique identifier
	typedef std::unordered_map<llvm::Instruction*, uint32_t> InstIdMapTy;
	// Registers should have a consistent JS type
	enum REGISTER_KIND { OBJECT=0, INTEGER, FLOAT, DOUBLE };
	struct RegisterRange
	{
		LiveRange range;
		REGISTER_KIND regKind;
		RegisterRange(const LiveRange& range, REGISTER_KIND k):range(range),regKind(k)
		{
		}
	};
	// Temporary data structures used while exploring the CFG
	struct BlockState
	{
		llvm::Instruction* inInst;
		llvm::SmallVector<llvm::Instruction*, 4> outSet;
		void addLiveOut(llvm::Instruction* I)
		{
			if(outSet.empty() || outSet.back()!=I)
				outSet.push_back(I);
		}
		void setLiveIn(llvm::Instruction* I)
		{
			inInst=I;
		}
		bool isLiveOut(llvm::Instruction* I) const
		{
			return !outSet.empty() && outSet.back()==I;
		}
		bool isLiveIn(llvm::Instruction* I) const
		{
			return inInst==I;
		}
		bool indexesAssigned;
		BlockState():indexesAssigned(false)
		{
		}
	};
	typedef std::unordered_map<llvm::BasicBlock*, BlockState> BlocksState;
	// Temporary data used to registerize allocas
	typedef std::vector<llvm::AllocaInst*> AllocaSetTy;
	typedef std::map<uint32_t, uint32_t> RangeChunksTy;
	struct AllocaBlockState
	{
		bool liveOut:1;
		// If notLiveOut is true we know that the blocks above do not ever use the alloca
		bool notLiveOut:1;
		bool visited:1;
		bool inLoop:1;
		AllocaBlockState():liveOut(false),notLiveOut(false),visited(false)
		{
		}
	};
	struct AllocaBlocksState: public std::unordered_map<llvm::BasicBlock*, AllocaBlockState>
	{
		std::vector<llvm::BasicBlock*> pendingBlocks;
		void markAndFlushPendingBlocks()
		{
			for(llvm::BasicBlock* BB: pendingBlocks)
			{
				assert(count(BB));
				find(BB)->second.liveOut=true;
			}
			pendingBlocks.clear();
		}
	};

	LiveRangesTy computeLiveRanges(llvm::Function& F, InstIdMapTy& instIdMap, AllocaSetTy& allocaSet);
	void doUpAndMark(BlocksState& blocksState, llvm::BasicBlock* BB, llvm::Instruction* I);
	uint32_t dfsLiveRangeInBlock(BlocksState& blockState, LiveRangesTy& liveRanges, InstIdMapTy& instIdMap,
					llvm::BasicBlock& BB, uint32_t nextIndex, uint32_t codePathId);
	void extendRangeForUsedOperands(llvm::Instruction& I, LiveRangesTy& liveRanges,
					uint32_t thisIndex, uint32_t codePathId);
	void assignToRegisters(const LiveRangesTy& F);
	void handlePHI(llvm::Instruction& I, const LiveRangesTy& liveRanges, llvm::SmallVector<RegisterRange, 4>& registers);
	uint32_t findOrCreateRegister(llvm::SmallVector<RegisterRange, 4>& registers, const InstructionLiveRange& range,
					REGISTER_KIND kind);
	static REGISTER_KIND getRegKindFromType(llvm::Type*);
	bool addRangeToRegisterIfPossible(RegisterRange& regRange, const InstructionLiveRange& liveRange, REGISTER_KIND kind);
	void computeAllocaLiveRanges(AllocaSetTy& allocaSet, const InstIdMapTy& instIdMap);
	std::unordered_set<llvm::Instruction*> gatherDerivedMemoryAccesses(llvm::AllocaInst* rootI);
	bool doUpAndMarkForAlloca(AllocaBlocksState& blocksState, const std::unordered_set<llvm::Instruction*>& allUses,
				RangeChunksTy& allocaRanges, const InstIdMapTy& instIdMap,
				llvm::BasicBlock* BB, llvm::AllocaInst* alloca);
};

llvm::ModulePass *createRegisterizePass(bool NoRegisterize);

}

#endif

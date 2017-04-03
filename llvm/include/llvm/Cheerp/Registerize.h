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
#include "llvm/Cheerp/PointerAnalyzer.h"
#include <set>
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
		bool empty() const
		{
			return start == end;
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
		bool doesInterfere(uint32_t id) const;
		void merge(const LiveRange& other);
		void dump() const;
	};

	static char ID;
	
	explicit Registerize(bool useFloats = false, bool n = false) : ModulePass(ID), NoRegisterize(n), useFloats(useFloats)
#ifndef NDEBUG
			, RegistersAssigned(false)
#endif
	{ }
	
	void getAnalysisUsage(llvm::AnalysisUsage & AU) const override;

	bool runOnModule(llvm::Module& M) override;
	
	const char *getPassName() const override;

	uint32_t getRegisterId(const llvm::Instruction* I) const;
	uint32_t getRegisterIdForEdge(const llvm::Instruction* I, const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB) const;

	void assignRegisters(llvm::Module& M, cheerp::PointerAnalyzer& PA);
	void computeLiveRangeForAllocas(llvm::Function& F);
	void invalidateLiveRangeForAllocas(llvm::Function& F);

	const LiveRange& getLiveRangeForAlloca(const llvm::AllocaInst* alloca) const
	{
		assert(allocaLiveRanges.count(alloca));
		return allocaLiveRanges.find(alloca)->second;
	}

	// Registers should have a consistent JS type
	enum REGISTER_KIND { OBJECT=0, INTEGER, DOUBLE, FLOAT };
	REGISTER_KIND getRegKindFromType(const llvm::Type*, bool asmjs) const;

	// Context used to disambiguate temporary values used in PHI resolution
	void setEdgeContext(const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB)
	{
		assert(edgeContext.isNull());
		edgeContext.fromBB=fromBB;
		edgeContext.toBB=toBB;
	}

	void clearEdgeContext()
	{
		edgeContext.clear();
	}

private:
	// Final data structures
	struct InstOnEdge
	{
		const llvm::BasicBlock* fromBB;
		const llvm::BasicBlock* toBB;
		uint32_t registerId;
		InstOnEdge(const llvm::BasicBlock* f, const llvm::BasicBlock* t, uint32_t r):fromBB(f),toBB(t),registerId(r)
		{
		}
		bool operator==(const InstOnEdge& r) const
		{
			return fromBB==r.fromBB && toBB==r.toBB && registerId==r.registerId;
		}
		struct Hash
		{
			size_t operator()(const InstOnEdge& i) const
			{
				return std::hash<const llvm::BasicBlock*>()(i.fromBB) ^
					std::hash<const llvm::BasicBlock*>()(i.toBB) ^
					std::hash<uint32_t>()(i.registerId);
			}
		};
	};
	std::unordered_map<const llvm::Instruction*, uint32_t> registersMap;
	std::unordered_map<InstOnEdge, uint32_t, InstOnEdge::Hash> edgeRegistersMap;
	std::unordered_map<const llvm::AllocaInst*, LiveRange> allocaLiveRanges;
	struct EdgeContext
	{
		const llvm::BasicBlock* fromBB;
		const llvm::BasicBlock* toBB;
		EdgeContext():fromBB(NULL), toBB(NULL)
		{
		}
		bool isNull() const
		{
			return fromBB==NULL;
		}
		void clear()
		{
			fromBB=NULL;
			toBB=NULL;
		}
	};
	EdgeContext edgeContext;
	bool NoRegisterize;
	bool useFloats;
#ifndef NDEBUG
	bool RegistersAssigned;
#endif
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
	// Map from instructions to their unique identifier
	typedef std::unordered_map<const llvm::Instruction*, uint32_t> InstIdMapTy;
	struct CompareInstructionByID
	{
	private:
		const InstIdMapTy* instIdMap;
	public:
		CompareInstructionByID(const InstIdMapTy& i):instIdMap(&i)
		{
		}
		bool operator()(llvm::Instruction* l, llvm::Instruction*r) const
		{
			assert(instIdMap->count(l) && instIdMap->count(r));
			return instIdMap->find(l)->second < instIdMap->find(r)->second;
		}
	};
	// Map from instructions to their live ranges
	typedef std::map<llvm::Instruction*, InstructionLiveRange, CompareInstructionByID> LiveRangesTy;
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
		bool completed;
		BlockState():inInst(NULL),completed(false)
		{
		}
	};
	typedef std::unordered_map<llvm::BasicBlock*, BlockState> BlocksState;
	// Temporary data used to registerize allocas
	typedef std::vector<const llvm::AllocaInst*> AllocaSetTy;
	typedef std::map<uint32_t, uint32_t> RangeChunksTy;
	struct AllocaBlockState
	{
		bool liveOut:1;
		// If notLiveOut is true neither this block or the blocks above do not use the alloca
		bool notLiveOut:1;
		bool liveIn:1;
		// If notLiveIn is true we know that the alloca is reset using lifetime_start in the block
		bool notLiveIn:1;
		// Is hasUse is true there is a use for the alloca inside the block
		bool hasUse:1;
		// upAndMarkId is used for various purposes:
		// 1) Is non-zero if the block is currently being explored
		// 2) Is non-zero if the block is in the pending list, the value is the lowest upAndMarkId on which the block state depend
		uint32_t upAndMarkId;
		AllocaBlockState():liveOut(false),notLiveOut(false),liveIn(false),notLiveIn(false),hasUse(false),upAndMarkId(0)
		{
		}
	};
	struct AllocaBlocksState: public std::unordered_map<llvm::BasicBlock*, AllocaBlockState>
	{
		std::vector<llvm::BasicBlock*> pendingBlocks;
		void markPendingBlocksAsLiveOut(uint32_t index)
		{
			for(uint32_t i=index;i<pendingBlocks.size();i++)
			{
				find(pendingBlocks[i])->second.liveOut = true;
				find(pendingBlocks[i])->second.upAndMarkId = 0;
			}
			pendingBlocks.resize(index);
		}
		void markPendingBlocksAsNotLiveOut(uint32_t index)
		{
			for(uint32_t i=index;i<pendingBlocks.size();i++)
			{
				find(pendingBlocks[i])->second.notLiveOut = true;
				find(pendingBlocks[i])->second.upAndMarkId = 0;
			}
			pendingBlocks.resize(index);
		}
		void discardPendingBlocks(uint32_t index)
		{
			for(uint32_t i=index;i<pendingBlocks.size();i++)
				find(pendingBlocks[i])->second.upAndMarkId = 0;
			pendingBlocks.resize(index);
		}
	};

	LiveRangesTy computeLiveRanges(llvm::Function& F, const InstIdMapTy& instIdMap, cheerp::PointerAnalyzer& PA);
	void doUpAndMark(BlocksState& blocksState, llvm::BasicBlock* BB, llvm::Instruction* I);
	static void assignInstructionsIds(InstIdMapTy& instIdMap, const llvm::Function& F, AllocaSetTy& allocaSet);
	uint32_t dfsLiveRangeInBlock(BlocksState& blockState, LiveRangesTy& liveRanges, const InstIdMapTy& instIdMap,
					llvm::BasicBlock& BB, cheerp::PointerAnalyzer& PA, uint32_t nextIndex, uint32_t codePathId);
	void extendRangeForUsedOperands(llvm::Instruction& I, LiveRangesTy& liveRanges, cheerp::PointerAnalyzer& PA,
					uint32_t thisIndex, uint32_t codePathId);
	uint32_t assignToRegisters(llvm::Function& F, const InstIdMapTy& instIdMap, const LiveRangesTy& liveRanges, const PointerAnalyzer& PA);
	void handlePHI(llvm::Instruction& I, const LiveRangesTy& liveRanges, llvm::SmallVector<RegisterRange, 4>& registers, const PointerAnalyzer& PA);
	uint32_t findOrCreateRegister(llvm::SmallVector<RegisterRange, 4>& registers, const InstructionLiveRange& range,
					REGISTER_KIND kind);
	bool addRangeToRegisterIfPossible(RegisterRange& regRange, const InstructionLiveRange& liveRange, REGISTER_KIND kind);
	void computeAllocaLiveRanges(AllocaSetTy& allocaSet, const InstIdMapTy& instIdMap);
	typedef std::set<llvm::Instruction*, CompareInstructionByID> InstructionSetOrderedByID;
	InstructionSetOrderedByID gatherDerivedMemoryAccesses(const llvm::AllocaInst* rootI, const InstIdMapTy& instIdMap);
	enum UP_AND_MARK_ALLOCA_STATE { USE_FOUND = 0, USE_NOT_FOUND, USE_UNKNOWN };
	struct UpAndMarkAllocaState
	{
		uint32_t state;
		UpAndMarkAllocaState(uint32_t s):state(s)
		{
		}
		UpAndMarkAllocaState& operator|=(const UpAndMarkAllocaState& rhs)
		{
			UpAndMarkAllocaState& lhs = *this;
			// 1) FOUND | Any = FOUND
			// 2) UNKNOWN | Any = UNKNOWN
			// 3) NOT_FOUND | NOT_FOUND = NOT_FOUND
			if (lhs.state == USE_FOUND || rhs.state == USE_FOUND)
			{
				lhs.state = USE_FOUND;
				return lhs;
			}
			// Return the smaller one as it is the one closest to the start
			if (lhs.state >= USE_UNKNOWN && rhs.state >= USE_UNKNOWN)
			{
				if (rhs.state < lhs.state)
					lhs.state = rhs.state;
				return lhs;
			}
			if (rhs.state >= USE_UNKNOWN)
			{
				lhs.state = rhs.state;
				return lhs;
			}
			if (lhs.state >= USE_UNKNOWN)
			{
				return lhs;
			}
			assert(lhs.state == USE_NOT_FOUND && rhs.state == USE_NOT_FOUND);
			return lhs;
		}
		bool operator==(UP_AND_MARK_ALLOCA_STATE r) const
		{
			return state == r;
		}
		bool operator!=(UP_AND_MARK_ALLOCA_STATE r) const
		{
			return state != r;
		}
	};
	UpAndMarkAllocaState doUpAndMarkForAlloca(AllocaBlocksState& blocksState, llvm::BasicBlock* BB, uint32_t upAndMarkId);
	void assignRegistersToInstructions(llvm::Function& F, cheerp::PointerAnalyzer& PA);
};

llvm::ModulePass *createRegisterizePass(bool useFloats, bool NoRegisterize);

}

#endif

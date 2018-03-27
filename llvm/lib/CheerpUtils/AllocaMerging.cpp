//===-- AllocaMerging.cpp - The Cheerp JavaScript generator ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2015 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "CheerpAllocaMerging"
#include <algorithm>
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/CaptureTracking.h"
#include "llvm/Cheerp/AllocaMerging.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/IntrinsicInst.h"

using namespace llvm;

STATISTIC(NumAllocaMerged, "Number of alloca which are merged");

namespace cheerp {

void AllocaMergingBase::analyzeBlock(const cheerp::Registerize& registerize, BasicBlock& BB,
				AllocaInfos& allocaInfos)
{
	for(Instruction& I: BB)
	{
		if(I.getOpcode() == Instruction::Alloca)
		{
			AllocaInst* AI = cast<AllocaInst>(&I);
			if(AI->isArrayAllocation())
				continue;
			allocaInfos.push_back(std::make_pair(AI, registerize.getLiveRangeForAlloca(AI)));
		}
	}
}

bool AllocaMerging::areTypesEquivalent(const cheerp::TypeSupport& types, cheerp::PointerAnalyzer& PA, Type* a, Type* b, bool asmjs)
{
	//TODO: Integer types may be equivalent as well
	if(a==b)
		return true;
	else if(asmjs && ((a->isPointerTy()||a->isIntegerTy(32))&&(b->isPointerTy()||b->isIntegerTy(32))))
		return true;
	else if(a->isPointerTy() && b->isPointerTy())
		return true;
	else if(asmjs && a->isFloatTy() && b->isFloatTy())
		return true;
	else if(asmjs && a->isDoubleTy() && b->isDoubleTy())
		return true;
	else if(!asmjs && a->isFloatingPointTy() && b->isFloatingPointTy())
		return true;
	else if(a->isArrayTy() && b->isArrayTy())
	{
		return cast<ArrayType>(a)->getNumElements()==cast<ArrayType>(b)->getNumElements() &&
			areTypesEquivalent(types, PA, a->getArrayElementType(), b->getArrayElementType(), asmjs);
	}
	else if(a->isStructTy() && b->isStructTy())
	{
		// TODO: Byte layout structs with the same size are equivalent
		if(cast<StructType>(a)->hasByteLayout() ||
			cast<StructType>(b)->hasByteLayout())
			return false;
		StructType* stA = cast<StructType>(a);
		StructType* stB = cast<StructType>(b);
		if(stA->getNumElements() != stB->getNumElements())
			return false;
		for(uint32_t i=0;i<stA->getNumElements();i++)
		{
			Type* elementA = stA->getElementType(i);
			Type* elementB = stB->getElementType(i);
			// The types needs to have consistent wrapper arrays
			if(types.useWrapperArrayForMember(PA, stA, i) ^ types.useWrapperArrayForMember(PA, stB, i))
				return false;
			if(!areTypesEquivalent(types, PA, elementA, elementB, asmjs))
				return false;
		}
		return true;
	}
	else
		return false;
}

bool AllocaMerging::runOnFunction(Function& F)
{
	cheerp::PointerAnalyzer & PA = getAnalysis<cheerp::PointerAnalyzer>();
	cheerp::Registerize & registerize = getAnalysis<cheerp::Registerize>();
	cheerp::TypeSupport types(*F.getParent());
	bool asmjs = F.getSection()==StringRef("asmjs");
	AllocaInfos allocaInfos;
	// Gather all the allocas
	for(BasicBlock& BB: F)
		analyzeBlock(registerize, BB, allocaInfos);
	if (allocaInfos.size() < 2)
		return false;
	bool Changed = false;
	BasicBlock& entryBlock=F.getEntryBlock();
	// Look if we can merge allocas of the same type
	for(auto targetCandidate=allocaInfos.begin();targetCandidate!=allocaInfos.end();++targetCandidate)
	{
		AllocaInst* targetAlloca = targetCandidate->first;
		Type* targetType = targetAlloca->getAllocatedType();
		// The range storing the sum of all ranges merged into target
		cheerp::Registerize::LiveRange targetRange(targetCandidate->second);
		// If the range is empty, we have an alloca that we can't analyze
		if (targetRange.empty())
			continue;
		std::vector<AllocaInfos::iterator> mergeSet;
		auto sourceCandidate=targetCandidate;
		++sourceCandidate;
		for(;sourceCandidate!=allocaInfos.end();++sourceCandidate)
		{
			AllocaInst* sourceAlloca = sourceCandidate->first;
			Type* sourceType = sourceAlloca->getAllocatedType();
			// Bail out for non compatible types
			if(!areTypesEquivalent(types, PA, targetType, sourceType, asmjs))
				continue;
			const cheerp::Registerize::LiveRange& sourceRange = sourceCandidate->second;
			// Bail out if this source candidate is not analyzable
			if(sourceRange.empty())
				continue;
			// Bail out if the allocas interfere
			if(targetRange.doesInterfere(sourceRange))
				continue;
			// Add the range to the target range and the source alloca to the mergeSet
			mergeSet.push_back(sourceCandidate);
			PA.invalidate(sourceAlloca);
			targetRange.merge(sourceRange);
		}

		// If the merge set is empty try another target
		if(mergeSet.empty())
			continue;

		PA.invalidate(targetAlloca);

		if(!Changed)
			registerize.invalidateLiveRangeForAllocas(F);

		// Make sure that this alloca is in the entry block
		if(targetAlloca->getParent()!=&entryBlock)
			targetAlloca->moveBefore(entryBlock.begin());
		// We can merge the allocas
		for(const AllocaInfos::iterator& it: mergeSet)
		{
			AllocaInst* allocaToMerge = it->first;
			Instruction* targetVal=targetAlloca;
			if(targetVal->getType()!=allocaToMerge->getType())
			{
				targetVal=new BitCastInst(targetVal, allocaToMerge->getType());
				targetVal->insertAfter(targetAlloca);
			}
			allocaToMerge->replaceAllUsesWith(targetVal);
			allocaToMerge->eraseFromParent();
			if(targetVal != targetAlloca)
				PA.getPointerKind(targetVal);
			allocaInfos.erase(it);
			NumAllocaMerged++;
		}
		PA.getPointerKind(targetAlloca);
		Changed = true;
	}
	if(Changed)
		registerize.computeLiveRangeForAllocas(F);
	return Changed;
}

const char *AllocaMerging::getPassName() const {
	return "AllocaMerging";
}

void AllocaMerging::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addRequired<cheerp::Registerize>();
	AU.addPreserved<cheerp::Registerize>();
	AU.addRequired<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::PointerAnalyzer>();
	AU.addRequired<cheerp::GlobalDepsAnalyzer>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();

	llvm::FunctionPass::getAnalysisUsage(AU);
}

char AllocaMerging::ID = 0;

FunctionPass *createAllocaMergingPass() { return new AllocaMerging(); }

bool AllocaArraysMerging::checkUsesForArrayMerging(AllocaInst* alloca)
{
	for(User* user: alloca->users())
	{
		// GEPs with a single op are indexing an array-of-arrays
		// We don't deal with them
		if (isa<GetElementPtrInst>(user) && user->getNumOperands()>2)
		{
			// Check that we are simply dereferencing the alloca pointer
			ConstantInt* CI=dyn_cast<ConstantInt>(user->getOperand(1));
			if(!CI || !CI->isNullValue())
				return false;
		}
		// BitCast for the lifetime instrinsics are ok
		else if (isa<BitCastInst>(user))
		{
			for(User* bitcastUser: user->users())
			{
				if(IntrinsicInst* II=dyn_cast<IntrinsicInst>(bitcastUser))
				{
					if(II->getIntrinsicID()!=Intrinsic::lifetime_start &&
						II->getIntrinsicID()!=Intrinsic::lifetime_end)
					{
						return false;
					}
				}
				else
					return false;
			}
		}
		else
			return false;
	}
	return true;
}

bool AllocaArraysMerging::runOnFunction(Function& F)
{
	class ArraysToMerge
	{
	private:
		std::map<AllocaInst*, uint32_t> arraysToMerge;
		uint32_t currentOffset;
	public:
		ArraysToMerge():currentOffset(0)
		{
		}
		bool empty() const
		{
			return arraysToMerge.empty();
		}
		std::map<AllocaInst*, uint32_t>::iterator begin()
		{
			return arraysToMerge.begin();
		}
		std::map<AllocaInst*, uint32_t>::iterator end()
		{
			return arraysToMerge.end();
		}
		void add(AllocaInst* a)
		{
			arraysToMerge.insert(std::make_pair(a, currentOffset));
			currentOffset+=cast<ArrayType>(a->getAllocatedType())->getNumElements();
		}
		uint32_t getNewSize() const
		{
			return currentOffset;
		}
	};

	cheerp::PointerAnalyzer & PA = getAnalysis<cheerp::PointerAnalyzer>();
	cheerp::Registerize & registerize = getAnalysis<cheerp::Registerize>();
	cheerp::GlobalDepsAnalyzer & GDA = getAnalysis<cheerp::GlobalDepsAnalyzer>();
	std::list<std::pair<AllocaInst*, cheerp::Registerize::LiveRange>> allocaInfos;
	// Gather all the allocas
	for(BasicBlock& BB: F)
		analyzeBlock(registerize, BB, allocaInfos);
	if (allocaInfos.size() < 2)
		return false;
	bool Changed = false;
	// We can also try to merge arrays of the same type, if only pointers to values are passed around
	while(!allocaInfos.empty())
	{
		// Build a map of array to be merged and their offseet into the new array
		ArraysToMerge arraysToMerge;
		auto targetCandidate = allocaInfos.begin();
		AllocaInst* targetAlloca = targetCandidate->first;
		if(!targetAlloca->getAllocatedType()->isArrayTy() ||
			// Check target uses
			!checkUsesForArrayMerging(targetAlloca))
		{
				allocaInfos.erase(targetCandidate);
				continue;
		}
		Type* targetElementType = targetAlloca->getAllocatedType()->getSequentialElementType();
		auto sourceCandidate=targetCandidate;
		++sourceCandidate;
		// Now that we have computed the sourceCandidate we can invalidate the targetCandidate
		allocaInfos.erase(targetCandidate);
		while(sourceCandidate!=allocaInfos.end())
		{
			AllocaInst* sourceAlloca = sourceCandidate->first;
			// Check that allocas are arrays of the same type
			if(!sourceAlloca->getAllocatedType()->isArrayTy())
			{
				++sourceCandidate;
				continue;
			}
			// Both are arrays, check the types
			if(targetElementType != sourceAlloca->getAllocatedType()->getSequentialElementType())
			{
				++sourceCandidate;
				continue;
			}
			// Verify that the source candidate has supported uses
			if(!checkUsesForArrayMerging(sourceAlloca))
			{
				++sourceCandidate;
				continue;
			}
			// We can merge the source and the target
			// If the set is empty add the target as well
			if(arraysToMerge.empty())
				arraysToMerge.add(targetAlloca);
			arraysToMerge.add(sourceAlloca);
			auto oldCandidate = sourceCandidate;
			++sourceCandidate;
			// Now that we have moved to the next candidate, we can invalidate the old one
			allocaInfos.erase(oldCandidate);
		}
		// If we have a non-empty set of alloca merge them
		if (arraysToMerge.empty())
			continue;

		if(!Changed)
			registerize.invalidateLiveRangeForAllocas(F);
		// Build new alloca
		Type* newAllocaType = ArrayType::get(targetElementType, arraysToMerge.getNewSize());
		// Add the new struct type to the GlobalDepsAnalyzer, it may need the createArray helper
		GDA.visitType(newAllocaType, /*forceTypedArray*/ false);
		AllocaInst* newAlloca = new AllocaInst(newAllocaType, "mergedArray", &(*F.getEntryBlock().begin()));
		Type* indexType = IntegerType::get(newAllocaType->getContext(), 32);
		// Change every use of every merged array with an appropiate GEP
		for(auto it: arraysToMerge)
		{
			AllocaInst* allocaToMerge = it.first;
			uint32_t baseOffset = it.second;
			SmallVector<User*, 4> users(allocaToMerge->users());
			for(User* u: users)
			{
				if(GetElementPtrInst* oldGep = dyn_cast<GetElementPtrInst>(u))
				{
					// Build 2 GEPs, one to reach the first element in the merged array
					// and the other for the rest of the offsets
					SmallVector<Value*, 4> indices;
					// Dereference array
					indices.push_back(ConstantInt::get(indexType, 0));
					// Reach offset
					indices.push_back(ConstantInt::get(indexType, baseOffset));
					Value* gep1 = GetElementPtrInst::Create(newAlloca, indices, "", oldGep);
					// Apply all the old offsets but the first one using a new GEP
					indices.clear();
					indices.insert(indices.begin(), oldGep->idx_begin()+1, oldGep->idx_end());
					Value* gep2 = GetElementPtrInst::Create(gep1, indices, "", oldGep);
					// Replace all uses with gep2
					oldGep->replaceAllUsesWith(gep2);
					PA.invalidate(oldGep);
					oldGep->eraseFromParent();
				}
				else if(BitCastInst* BI=dyn_cast<BitCastInst>(u))
				{
					//Only used for lifetime intrinsics
					Value* newBitCast=new BitCastInst(newAlloca, BI->getType(), "", BI);
					BI->replaceAllUsesWith(newBitCast);
					PA.invalidate(BI);
					BI->eraseFromParent();
				}
				else
					assert(false && "Unexpected use while merging arrays");
			}
			// Kill the alloca itself now
			PA.invalidate(allocaToMerge);
			allocaToMerge->eraseFromParent();
			Changed = true;
		}
	}
	if(Changed)
		registerize.computeLiveRangeForAllocas(F);
	return Changed;
}

const char *AllocaArraysMerging::getPassName() const {
	return "AllocaArraysMerging";
}

void AllocaArraysMerging::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addRequired<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::PointerAnalyzer>();
	AU.addRequired<cheerp::Registerize>();
	AU.addPreserved<cheerp::Registerize>();
	AU.addRequired<cheerp::GlobalDepsAnalyzer>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();

	llvm::FunctionPass::getAnalysisUsage(AU);
}

char AllocaArraysMerging::ID = 0;

FunctionPass *createAllocaArraysMergingPass() { return new AllocaArraysMerging(); }

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(AllocaMerging, "AllocaMerging", "Merge alloca instructions used on non-overlapping ranges",
			false, false)
INITIALIZE_PASS_END(AllocaMerging, "AllocaMerging", "Merge alloca instructions used on non-overlapping ranges",
			false, false)


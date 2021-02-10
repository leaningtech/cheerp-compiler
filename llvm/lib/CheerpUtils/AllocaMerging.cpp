//===-- AllocaMerging.cpp - The Cheerp JavaScript generator ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "CheerpAllocaMerging"
#include <algorithm>
#include "llvm/InitializePasses.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/CaptureTracking.h"
#include "llvm/Cheerp/AllocaMerging.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/GEPOptimizer.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Local.h"

using namespace llvm;

STATISTIC(NumAllocaMerged, "Number of alloca which are merged");

namespace cheerp {

template<typename Container>
void AllocaMergingBase::analyzeBlock(const cheerp::Registerize& registerize, BasicBlock& BB,
				Container& allocaInfos)
{
	for(Instruction& I: BB)
	{
		if(I.getOpcode() == Instruction::Alloca)
		{
			AllocaInst* AI = cast<AllocaInst>(&I);
			if(AI->isArrayAllocation())
				continue;
			allocaInfos.push_back(std::make_pair(AI, registerize.getLiveRangeForAlloca(AI)));
			assert(registerize.getLiveRangeForAlloca(AI).invariantsHold());
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
			if (asmjs)
				continue;

			if (elementA->isPointerTy())
			{
				assert(elementB->isPointerTy());

				auto getPointerKindPA = [&PA](const StructType* sTy, int index)
				{
					TypeAndIndex baseAndIndex(sTy, index, TypeAndIndex::STRUCT_MEMBER);
					return PA.getPointerKindForMemberPointer(baseAndIndex);
				};

				if (getPointerKindPA(stA, i) != getPointerKindPA(stB, i))
					return false;
			}
		}
		return true;
	}
	else
		return false;
}


bool AllocaMerging::runOnFunctionLegacy(Function& F)
{
	cheerp::PointerAnalyzer & PA = getAnalysis<cheerp::PointerAnalyzer>();
	DominatorTree* DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
	cheerp::Registerize & registerize = getAnalysis<cheerp::Registerize>();
	cheerp::TypeSupport types(*F.getParent());
	bool asmjs = F.getSection()==StringRef("asmjs");
	typedef std::list<AllocaInfo> AllocaInfos;
	AllocaInfos allocaInfos;
	// Gather all the allocas
	for(BasicBlock& BB: F)
		analyzeBlock(registerize, BB, allocaInfos);
	if (allocaInfos.size() < 2)
		return false;
	bool Changed = false;
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

		// Find out the insertion point
		Instruction* insertionPoint = targetAlloca->getNextNode();
		for(const AllocaInfos::iterator& it: mergeSet)
			insertionPoint = cheerp::findCommonInsertionPoint(nullptr, DT, insertionPoint, it->first);
		targetAlloca->moveBefore(insertionPoint);
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

bool AllocaMerging::runOnFunction(Function& F)
{
	if (RegisterizeLegacy)
	{
		return runOnFunctionLegacy(F);
	}
	cheerp::PointerAnalyzer & PA = getAnalysis<cheerp::PointerAnalyzer>();
	DominatorTree* DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
	cheerp::Registerize & registerize = getAnalysis<cheerp::Registerize>();
	cheerp::TypeSupport types(*F.getParent());
	bool asmjs = F.getSection()==StringRef("asmjs");
	std::vector<AllocaInfo> allocaInfos;
	// Gather all the allocas
	for(BasicBlock& BB: F)
	{
		analyzeBlock(registerize, BB, allocaInfos);
	}
	if (allocaInfos.size() < 2)
		return false;

	VertexColorer colorer(allocaInfos.size(),
			/*weight extra color, 1 since there are no other weights*/1,
			/*maximum number of iterations, most likely never reached*/100);
	colorer.setAll(/*conflicting*/true);

	//Iterate over every pair (i, j) to check edge validity
	for(uint32_t i=0; i<allocaInfos.size(); ++i)
	{
		auto& targetCandidate = allocaInfos[i];
		AllocaInst* targetAlloca = targetCandidate.first;
		Type* targetType = targetAlloca->getAllocatedType();
		const cheerp::Registerize::LiveRange targetRange(targetCandidate.second);
		// If the range is empty, we have an alloca that we can't analyze
		if (targetRange.empty())
			continue;
		for (uint32_t j=i+1; j<allocaInfos.size(); ++j)
		{
			auto& sourceCandidate = allocaInfos[j];

			AllocaInst* sourceAlloca = sourceCandidate.first;
			Type* sourceType = sourceAlloca->getAllocatedType();
			// Bail out for non compatible types
			if(!areTypesEquivalent(types, PA, targetType, sourceType, asmjs))
				continue;
			const cheerp::Registerize::LiveRange& sourceRange = sourceCandidate.second;
			// Bail out if this source candidate is not analyzable
			if(sourceRange.empty())
				continue;
			// Bail out if the allocas interfere
			if(targetRange.doesInterfere(sourceRange))
				continue;

			// If the rest worked, we can finally set i,j as a valid connection
			colorer.addAllowed(i, j);
		}
	}

	//Solve the vertex coloring equivalent problem
	colorer.solve();
	const std::vector<uint32_t> col = colorer.getSolution();

	if (VertexColorer::hasAnythingBeenMerged(col) == false)
		return false;

	//Merge the alloca with the same colors, and do some cleaning/updating
	registerize.invalidateLiveRangeForAllocas(F);

	typedef std::pair<uint32_t, llvm::AllocaInst*> ColorInstPair;
	std::vector<ColorInstPair> orderedByColor;

	for (uint32_t i=0; i<col.size(); i++)
	{
		orderedByColor.push_back({col[i], allocaInfos[i].first});
	}

	stable_sort(orderedByColor.begin(), orderedByColor.end(), [](const ColorInstPair& a, const ColorInstPair& b)
			{
				//Stable sort on the colors
				return a.first < b.first;
			});

	for (uint32_t i=0, j=0; i<orderedByColor.size(); i=j)
	{
		const uint32_t currentColor = orderedByColor[i].first;
		//Take the first item of a certain color, and merge all the other alloca with the same color to it
		while (j < orderedByColor.size() && orderedByColor[j].first == currentColor)
		{
			++j;
		}

		if (j-i == 1)
		{
			//There is only a single alloca, so nothing to do
			continue;
		}

		AllocaInst* targetAlloca = orderedByColor[i].second;
		Instruction* insertionPoint = targetAlloca->getNextNode();
		PA.invalidate(targetAlloca);

		//Calculate the cumulative insertionPoint
		for (uint32_t k = i+1; k<j; k++)
		{
			insertionPoint = cheerp::findCommonInsertionPoint(nullptr, DT, insertionPoint, orderedByColor[k].second);
		}

		targetAlloca->moveBefore(insertionPoint);

		//Do the actual merging and related bookkeeping
		for (uint32_t k = i+1; k<j; k++)
		{
			AllocaInst* allocaToMerge = orderedByColor[k].second;
			PA.invalidate(allocaToMerge);

			Instruction* targetVal=targetAlloca;
			//TODO: check why this is needed
			if(targetVal->getType()!=allocaToMerge->getType())
			{
				targetVal=new BitCastInst(targetVal, allocaToMerge->getType());
				targetVal->insertAfter(targetAlloca);
			}
			allocaToMerge->replaceAllUsesWith(targetVal);
			allocaToMerge->eraseFromParent();
			if(targetVal != targetAlloca)
				PA.getPointerKind(targetVal);

			NumAllocaMerged++;
		}

		PA.getPointerKind(targetAlloca);
	}

	registerize.computeLiveRangeForAllocas(F);

	return true;
}

StringRef AllocaMerging::getPassName() const {
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
	AU.addRequired<DominatorTreeWrapperPass>();
	AU.addPreserved<DominatorTreeWrapperPass>();
	AU.addPreserved<LinearMemoryHelper>();

	llvm::FunctionPass::getAnalysisUsage(AU);
}

char AllocaMerging::ID = 0;

FunctionPass *createAllocaMergingPass() { return new AllocaMerging(); }

bool AllocaArraysMerging::checkUsesForArrayMerging(AllocaInst* alloca) const
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

llvm::Type* AllocaArraysMerging::collectUniformAlloca(std::vector<AllocaInst*>& uniformAllocaArrays, std::list<AllocaInfo>& allocaInfos) const
{
	//Modify allocaInfos, moving a group of uniform alloca (uniform means that they are possibly mergeable toghether) into uniformAllocaArrays

	auto targetCandidate = allocaInfos.begin();
	AllocaInst* targetAlloca = targetCandidate->first;
	if(!targetAlloca->getAllocatedType()->isArrayTy() ||
		// Check target uses
		!checkUsesForArrayMerging(targetAlloca))
	{
			allocaInfos.erase(targetCandidate);
			return NULL;
	}

	Type* elementType = targetAlloca->getAllocatedType()->getArrayElementType();
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
		if(elementType != sourceAlloca->getAllocatedType()->getArrayElementType())
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
		if(uniformAllocaArrays.empty())
			uniformAllocaArrays.push_back(targetAlloca);
		uniformAllocaArrays.push_back(sourceAlloca);
		auto oldCandidate = sourceCandidate;
		++sourceCandidate;
		// Now that we have moved to the next candidate, we can invalidate the old one
		allocaInfos.erase(oldCandidate);
	}
	return elementType;
}

bool AllocaArraysMerging::runOnFunction(Function& F)
{
	if (F.getSection()==StringRef("asmjs"))
		return false;

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
		uint32_t numberOfSubArrays() const
		{
			return arraysToMerge.size();
		}
		uint32_t getNewSize() const
		{
			return currentOffset;
		}
	};

	cheerp::PointerAnalyzer & PA = getAnalysis<cheerp::PointerAnalyzer>();
	DominatorTree* DT = &getAnalysis<DominatorTreeWrapperPass>().getDomTree();
	cheerp::Registerize & registerize = getAnalysis<cheerp::Registerize>();
	cheerp::GlobalDepsAnalyzer & GDA = getAnalysis<cheerp::GlobalDepsAnalyzer>();
	std::list<AllocaInfo> allocaInfos;
	// Gather all the allocas
	for(BasicBlock& BB: F)
		analyzeBlock(registerize, BB, allocaInfos);
	if (allocaInfos.size() < 2)
		return false;
	bool Changed = false;

	// We can also try to merge arrays of the same type, if only pointers to values are passed around
	while(!allocaInfos.empty())
	{
		std::vector<AllocaInst*> uniformAlloca;

		//Move Alloca from allocaInfos to uniformAlloca
		//After this call, at least the first element of allocaInfos would have been processed
		//and moved away (either into uniformAlloca, or nowhere since it was not mergeable)
		//uniformAlloca will store all AllocaArrays of a certain kind
		Type* elementType = collectUniformAlloca(uniformAlloca, allocaInfos);
		//By construction uniformAlloca would be either empty or with multiple elements
		assert(uniformAlloca.size() != 1);

		if (uniformAlloca.empty())
			continue;

		std::vector<BasicBlock*> blocks;
		blocks.reserve(uniformAlloca.size());
		for (AllocaInst* x : uniformAlloca)
		{
			blocks.push_back(x->getParent());
		}

		const std::vector<BasicBlock*> representative = findRepresentingBasicBlock(DT, blocks);

		llvm::BitVector done(uniformAlloca.size(), false);
		for (uint32_t firstToBeDone = 0; firstToBeDone < uniformAlloca.size(); ++firstToBeDone)
		{
			if (done[firstToBeDone])
				continue;

			// Build a map of array to be merged and their offset into the new array
			ArraysToMerge arraysToMerge;
			for (uint32_t i=firstToBeDone; i<uniformAlloca.size(); i++)
			{
				if (done[i])
					continue;
				//Skip arrays that do not have the same representative as firstToBeDone
				//It means that they are in separate regions, and regions are separated if there are codepath that do not end using an array of the good kind
				if (representative[i] != representative[firstToBeDone])
					continue;
				done[i] = true;
				arraysToMerge.add(uniformAlloca[i]);
			}

			// If we have more that 1 arrays, we have something to merge
			if (arraysToMerge.numberOfSubArrays() <= 1)
				continue;

			if(!Changed)
				registerize.invalidateLiveRangeForAllocas(F);
			// Build new alloca
			Type* newAllocaType = ArrayType::get(elementType, arraysToMerge.getNewSize());
			// Add the new struct type to the GlobalDepsAnalyzer, it may need the createArray helper
			GDA.visitType(newAllocaType, /*forceTypedArray*/ false);
			// Find out the insertion point
			Instruction* insertionPoint = nullptr;
			for(auto it: arraysToMerge)
				insertionPoint = cheerp::findCommonInsertionPoint(nullptr, DT, insertionPoint, it.first);
			AllocaInst* newAlloca = new AllocaInst(newAllocaType, 0, "mergedArray", insertionPoint);
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
						Value* gep1 = GetElementPtrInst::Create(newAllocaType, newAlloca, indices, "", oldGep);
						// Apply all the old offsets but the first one using a new GEP
						indices.clear();
						indices.insert(indices.begin(), oldGep->idx_begin()+1, oldGep->idx_end());
						Value* gep2 = GetElementPtrInst::Create(gep1->getType()->getPointerElementType(), gep1, indices, "", oldGep);
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
	}

	if(Changed)
		registerize.computeLiveRangeForAllocas(F);
	return Changed;
}

StringRef AllocaArraysMerging::getPassName() const {
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
	AU.addRequired<DominatorTreeWrapperPass>();
	AU.addPreserved<DominatorTreeWrapperPass>();
	AU.addPreserved<LinearMemoryHelper>();

	llvm::FunctionPass::getAnalysisUsage(AU);
}

char AllocaArraysMerging::ID = 0;

FunctionPass *createAllocaArraysMergingPass() { return new AllocaArraysMerging(); }

bool AllocaStoresExtractor::validType(llvm::Type* t, const Module& module)
{
	if(TypeSupport::hasByteLayout(t))
		return false;
	StructType* ST = dyn_cast<StructType>(t);
	if(ST)
	{
		if(ST->getNumElements() > V8MaxLiteralProperties)
			return false;
		if(TypeSupport::isJSExportedType(ST, module))
			return false;
	}
	ArrayType* AT = dyn_cast<ArrayType>(t);
	if(!AT)
		return true;
	// NOTE: This is slightly conservative, we assume double typed arrays are always used
	if(TypeSupport::isTypedArrayType(AT->getElementType(), /*forceTypedArray*/true))
		return false;
	if(AT->getNumElements() > V8MaxLiteralProperties)
		return false;
	return true;
}

bool AllocaStoresExtractor::runOnBasicBlock(BasicBlock& BB, const Module& module)
{
	if(!DL)
	{
		DL = &module.getDataLayout();
		assert(DL);
	}
	// Map between insts and the underlying allocas/offsets in bytes
	// NOTE: A negative offset encodes that we can't analyze across this inst, but we still need to account for it to check for escapes
	std::unordered_map<llvm::Instruction*, std::pair<llvm::AllocaInst*, int32_t>> instsToAlloca;
	std::unordered_set<llvm::AllocaInst*> notEscapedAllocas;
	// NOTE: Until the alloca escapes it is actually safe to analyze even across side effects
	//       Even arbitrary code cannot touch an alloca with an unknown address
	for(Instruction& I: BB)
	{
		if(AllocaInst* AI = dyn_cast<AllocaInst>(&I))
		{
			if(AI->isArrayAllocation())
				continue;
			// Only handle types which use the literal representation
			if(!validType(AI->getAllocatedType(), module))
				continue;
			// Put in the same map as GEPs/bitcast to simpify lookups
			instsToAlloca.insert(std::make_pair(AI, std::make_pair(AI, 0)));
			notEscapedAllocas.insert(AI);
			continue;
		}
		else if(GetElementPtrInst* GEP = dyn_cast<GetElementPtrInst>(&I))
		{
			// Check if the GEP references directly or indirectly an alloca
			Instruction* opI = dyn_cast<Instruction>(GEP->getOperand(0));
			if(!opI)
				continue;
			auto it = instsToAlloca.find(opI);
			if(it == instsToAlloca.end())
			{
				// Nope, nothing to do with this GEP
				continue;
			}
			// If the operand is already invalid, this GEP also is
			if(it->second.second < 0)
			{
				instsToAlloca.insert(std::make_pair(GEP, it->second));
				continue;
			}
			// Manually traverse the GEP, while
			// 1) Checking that all the offsets are constants
			// 2) Accumulating the total offset
			// 3) Verying that we don't cross a big structs, those are not trackable
			int32_t totalOffset = 0;
			Type* curType = opI->getType();
			for(unsigned i=1;i<GEP->getNumOperands();i++)
			{
				Value* op = GEP->getOperand(i);
				ConstantInt* CI = dyn_cast<ConstantInt>(op);
				if(!CI)
				{
					totalOffset = -1;
					break;
				}
				int32_t index = CI->getSExtValue();
				if(index < 0)
				{
					totalOffset = -1;
					break;
				}

				// Only handle types which use the literal representation
				if(!validType(curType, module))
				{
					totalOffset = -1;
					break;
				}

				//curType is modifyed by the call
				totalOffset += partialOffset(curType, *DL, index);
			}
			// If totalOffset is negative we can't analyze across this GEP, but we still need to check for memory escaping it
			if(totalOffset >= 0)
			{
				totalOffset += it->second.second;
			}
			instsToAlloca.insert(std::make_pair(GEP, std::make_pair(it->second.first, totalOffset)));
			continue;
		}
		else if(BitCastInst* BI = dyn_cast<BitCastInst>(&I))
		{
			// Only allow type safe bitcasts, namely the ones that cast to a direct base
			Instruction* opI = dyn_cast<Instruction>(BI->getOperand(0));
			if(!opI)
				continue;
			auto it = instsToAlloca.find(opI);
			if(it == instsToAlloca.end())
			{
				// Nope, nothing to do
				continue;
			}
			// If the operand is already invalid, the bitcast also is
			if(it->second.second < 0)
			{
				instsToAlloca.insert(std::make_pair(BI, it->second));
				continue;
			}
			StructType* srcType = dyn_cast<StructType>(opI->getType());
			StructType* dstType = dyn_cast<StructType>(BI->getType());
			// If either are not structs fall to escaping logic
			if(srcType && dstType)
			{
				bool isDirectBase = false;
				while(StructType* directBase = srcType->getDirectBase())
				{
					if(directBase == dstType)
					{
						isDirectBase = true;
						break;
					}
					srcType = directBase;
				}
				if(isDirectBase)
				{
					instsToAlloca.insert(std::make_pair(BI, it->second));
					continue;
				}
				// Not a direct base cast, fall to escaping logic
			}
		}
		else if(StoreInst* SI = dyn_cast<StoreInst>(&I))
		{
			// If we are storing to a tracked pointer we can remove this store
			// Otherwise we fall through below to the escaping logic
			Instruction* opI = dyn_cast<Instruction>(SI->getPointerOperand());
			if(opI)
			{
				auto it = instsToAlloca.find(opI);
				if(it != instsToAlloca.end())
				{
					// TODO: Improve logic to handle non-constants, we could also allow insts defined above the alloca
					if(it->second.second < 0 || isa<Instruction>(SI->getValueOperand()))
					{
						// This offset/value is not trackable, but it is also not an escape
						// use 'continue' to avoid falling in the escaping logic
						continue;
					}
					// We can only handle floating point values with a fractional part, otherwise we will confuse the type system
					if(ConstantFP* fp = dyn_cast<ConstantFP>(SI->getValueOperand()))
					{
						if(fp->getValueAPF().isInteger())
						{
							continue;
						}
					}
					OffsetToValueMap& map = allocaStores[it->second.first];
					// NOTE: We use the [] operator here, and we might override a previous store.
					// This is fine as we don't handle Loads in this pass, the first load will cause the alloca to escape
					// If this change we need to be more careful here!
					map[it->second.second] = SI->getValueOperand();
					instsToRemove.push_back(SI);
					continue;
				}
			}
		}
		else if(IntrinsicInst* II = dyn_cast<IntrinsicInst>(&I))
		{
			// Skip some intrinsics which do not cause allocas to escape
                        if(II->getIntrinsicID()==Intrinsic::lifetime_start ||
                                II->getIntrinsicID()==Intrinsic::lifetime_end ||
                                II->getIntrinsicID()==Intrinsic::dbg_declare ||
                                II->getIntrinsicID()==Intrinsic::dbg_value ||
                                II->getIntrinsicID()==Intrinsic::assume)
			{
				continue;
			}
		}
		// If any of the tracked values are used by not handled instructions, the corresponding alloca escapes
		for(Value* op: I.operands())
		{
			Instruction* opI = dyn_cast<Instruction>(op);
			if(!opI)
				continue;
			auto it = instsToAlloca.find(opI);
			if(it == instsToAlloca.end())
				continue;
			AllocaInst* escapingAlloca = it->second.first;
			bool erased = notEscapedAllocas.erase(escapingAlloca);
			(void)erased;
			assert(erased);
			// TODO: Maybe add another structure to speed this up
			it = instsToAlloca.begin();
			auto itE = instsToAlloca.end();
			while(it != itE)
			{
				auto prevIt = it;
				++it;
				if(prevIt->second.first == escapingAlloca)
					instsToAlloca.erase(prevIt);
			}
		}
	}
	return false;
}

void AllocaStoresExtractor::destroyStores()
{
	std::set<BasicBlock*> modifiedBlocks;
	// Erase all queued instructions
	for(Instruction* I: instsToRemove)
	{
		modifiedBlocks.insert(I->getParent());
		I->eraseFromParent();
	}
	// Go over insts in the blocks backward to remove all insts without uses
	for(BasicBlock* BB: modifiedBlocks)
	{
		auto *TLIP = getAnalysisIfAvailable<TargetLibraryInfoWrapperPass>();
		const llvm::TargetLibraryInfo* TLI = TLIP ? &TLIP->getTLI(*BB->getParent()) : nullptr;
		assert(TLI);
		auto it = BB->end();
		--it;
		do
		{
			Instruction* I = &(*it);
			--it;
			if(isInstructionTriviallyDead(I, TLI))
				I->eraseFromParent();
		}
		while(it != BB->begin());
	}
}

bool AllocaStoresExtractor::runOnModule(Module& M)
{
	bool Changed = false;
	for(Function& F: M)
	{
		for(BasicBlock& BB: F)
			Changed |= runOnBasicBlock(BB, M);
	}
	return Changed;
}

const AllocaStoresExtractor::OffsetToValueMap* AllocaStoresExtractor::getValuesForAlloca(const llvm::AllocaInst* AI) const
{
	auto it = allocaStores.find(AI);
	if(it == allocaStores.end())
		return nullptr;
	else
		return &it->second;
}

StringRef AllocaStoresExtractor::getPassName() const {
	return "AllocaStoresExtractor";
}

void AllocaStoresExtractor::getAnalysisUsage(AnalysisUsage & AU) const
{
	AU.addPreserved<cheerp::PointerAnalyzer>();
	AU.addPreserved<cheerp::Registerize>();
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
	AU.addPreserved<LinearMemoryHelper>();
	llvm::ModulePass::getAnalysisUsage(AU);
}

char AllocaStoresExtractor::ID = 0;

ModulePass *createAllocaStoresExtractor() { return new AllocaStoresExtractor(); }
}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(AllocaMerging, "AllocaMerging", "Merge alloca instructions used on non-overlapping ranges",
			false, false)
INITIALIZE_PASS_END(AllocaMerging, "AllocaMerging", "Merge alloca instructions used on non-overlapping ranges",
			false, false)
INITIALIZE_PASS_BEGIN(AllocaStoresExtractor, "AllocaStoresExtractor", "Removes stores to just allocated memory and keeps track of the values separately",
			false, false)
INITIALIZE_PASS_END(AllocaStoresExtractor, "AllocaStoresExtractor", "Removes stores to just allocated memory and keeps track of the values separately",
			false, false)


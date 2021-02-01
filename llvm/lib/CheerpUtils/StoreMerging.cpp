//===-- StoreMerging.cpp - Merging of adjacent stores --------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/StoreMerging.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

using namespace cheerp;

StringRef StoreMerging::getPassName() const {
	return "StoreMerging";
}

bool StoreMerging::runOnFunction(Function& F)
{
	bool Changed = false;

	for (BasicBlock& BB : F)
		Changed |= runOnBasicBlock(BB);

	return Changed;
}

bool StoreMerging::runOnBasicBlock(BasicBlock& BB)
{
	assert(toErase.empty());

	DL = &(BB.getParent()->getParent()->getDataLayout());
	assert(DL);
	const bool asmjs = BB.getParent()->getSection() == StringRef("asmjs");

	if (!asmjs)
		return false;

	const llvm::Value* currentPtr = nullptr;
	std::vector<std::pair<llvm::StoreInst*, int> > basedOnCurrentPtr;

	for (Instruction& I : BB)
	{
		if (StoreInst* SI = dyn_cast<StoreInst>(&I))
		{
			auto pair = findBasePointerAndOffset(SI->getPointerOperand());

			if (currentPtr == nullptr)
				currentPtr = pair.first;

			if (currentPtr != pair.first)
			{
				processBlockOfStores(basedOnCurrentPtr);
				basedOnCurrentPtr.clear();
			}

			currentPtr = pair.first;
			basedOnCurrentPtr.push_back({SI, pair.second});
			continue;
		}

		if (I.mayReadOrWriteMemory() || I.mayHaveSideEffects())
		{
			processBlockOfStores(basedOnCurrentPtr);
			currentPtr = nullptr;
			basedOnCurrentPtr.clear();
		}
	}

	processBlockOfStores(basedOnCurrentPtr);

	const bool Changed = !toErase.empty();

	//Only now delete StoreInsts from the BasicBlock
	for (auto store : toErase)
		store->eraseFromParent();

	toErase.clear();

	return Changed;
}

static void filterAlreadyProcessedStores(std::vector<std::pair<llvm::StoreInst*, int>>& groupedSamePointer, std::vector<uint32_t>& dimension)
{
	//Bookkeeping 3: remove the stores with dimension set to 0
	//We use two temporary vectors, and then swap them out for the older ones
	std::vector<std::pair<llvm::StoreInst*, int> > newGroupedSamePointer;
	std::vector<uint32_t> newDimension;

	for (uint32_t i=0; i<dimension.size(); i++)
		if (dimension[i])
	{
		newGroupedSamePointer.push_back(groupedSamePointer[i]);
		newDimension.push_back(dimension[i]);
	}

	std::swap(newGroupedSamePointer, groupedSamePointer);
	std::swap(newDimension, dimension);
}

void StoreMerging::processBlockOfStores(std::vector<std::pair<llvm::StoreInst*, int> > groupedSamePointer)
{
	if (groupedSamePointer.size() < 2)
		return;

	//The insertion point will be the last Store in the consecutive block
	llvm::Instruction* insertionPoint = groupedSamePointer.back().first;
	IRBuilder<> IRB(insertionPoint);

	//Sort based on the offset
	std::sort(groupedSamePointer.begin(), groupedSamePointer.end(),
			[](const std::pair<llvm::StoreInst*, int>& left, const std::pair<llvm::StoreInst*, int>& right) -> bool
			{
				return left.second < right.second;
			});

	//Calculate dimension of the various pieces
	const uint32_t N = groupedSamePointer.size();
	std::vector<uint32_t> dimension(N);
	for (uint32_t i=0; i<N; i++)
	{
		auto T = groupedSamePointer[i].first->getValueOperand()->getType();
		dimension[i] = DL->getTypeAllocSize(T);
	}

	bool overlap = false;
	for (uint32_t i=0; i+1<N; i++)
	{
		if (groupedSamePointer[i].second + (int)dimension[i] > groupedSamePointer[i+1].second)
			overlap = true;
	}


	//Avoid the optimization if any store overlap
	if (overlap)
		return;

	//Alternatively process a block of stores and filter out already consumed ones
	//Processing with increasing dimension means that we may optimize even already optimized stores
	processBlockOfStores(1, groupedSamePointer, dimension, IRB);
	filterAlreadyProcessedStores(groupedSamePointer, dimension);

	processBlockOfStores(2, groupedSamePointer, dimension, IRB);
	filterAlreadyProcessedStores(groupedSamePointer, dimension);

	//Do not create 64-bit asmjs stores
	if (!isWasm)
		return;

	processBlockOfStores(4, groupedSamePointer, dimension, IRB);
	filterAlreadyProcessedStores(groupedSamePointer, dimension);
}

void StoreMerging::processBlockOfStores(const uint32_t dim, std::vector<std::pair<llvm::StoreInst*, int> > & groupedSamePointer, std::vector<uint32_t>& dimension, IRBuilder<>& builder)
{
	const uint32_t N = groupedSamePointer.size();

	for (uint32_t i=0; i+1<N; i++)
	{
		const uint32_t a = i;
		const uint32_t b = i+1;

		if (dimension[a] != dim)
			continue;
		if (dimension[b] != dim)
			continue;

		//Check they are consecutive
		if ((int)dim + groupedSamePointer[a].second != groupedSamePointer[b].second)
			continue;

		const uint32_t alignment = groupedSamePointer[a].first->getAlignment();

		if (!isWasm && alignment < dim * 2)
			continue;

		Value* lowValue = groupedSamePointer[a].first->getValueOperand();
		Value* highValue = groupedSamePointer[b].first->getValueOperand();

		//For now avoid complexities related to float/double to int bitcasts
		if (lowValue->getType()->isFloatTy())
			continue;
		if (highValue->getType()->isFloatTy())
			continue;

		const Constant* constantLowValue = dyn_cast<Constant>(lowValue);
		const Constant* constantHighValue = dyn_cast<Constant>(highValue);

		bool isConvenient = false;

		//Both ValueOperands constants -> folded in a single store
		if (constantLowValue && constantHighValue)
			isConvenient = true;
		//Higher ValueOperands 0 -> folded in a single store
		if (constantHighValue && constantHighValue->isNullValue())
			isConvenient = true;

		if (!isConvenient)
			continue;

		auto& context = groupedSamePointer[a].first->getParent()->getContext();
		Type* smallType = IntegerType::get(context, dim * 8);
		Type* bigType = IntegerType::get(context, dim * 16);

		auto convertToBigType = [&builder, &smallType, &bigType](Value* value) -> Value*
		{
			//Convert value to smallType
			if (value->getType()->isPointerTy())
				value = builder.CreatePtrToInt(value, smallType);
			else if (value->getType() != smallType)
				value = builder.CreateBitCast(value, smallType);

			//Extend value from smallType to bigType
			value = builder.CreateZExt(value, bigType);

			return value;
		};

		Value* low = convertToBigType(lowValue);

		Value* sum = low;

		//Add shifted higher part
		if (!constantHighValue || !constantHighValue->isNullValue())
		{
			Value* high = convertToBigType(highValue);
			Value* shiftToHigh = builder.CreateShl(high, dim*8);
			sum = builder.CreateAdd(low, shiftToHigh);
		}

		//BitCast the pointer operand
		Value* bitcast = builder.CreateBitCast(groupedSamePointer[a].first->getPointerOperand(), bigType->getPointerTo());

		//Actually create the store
		StoreInst* biggerStore = cast<StoreInst>(builder.CreateStore(sum, bitcast));
		biggerStore->setAlignment(llvm::Align(alignment));

		//Bookkeeping 1: take note of what to later erase
		toErase.push_back(groupedSamePointer[a].first);
		toErase.push_back(groupedSamePointer[b].first);

		//Bookkeeping 2: insert biggerStore at the right point in groupedSamePointer
		groupedSamePointer[a].first = biggerStore;
		dimension[a] = dim*2;
		dimension[b] = 0;

		i = b;
	}
}

std::pair<const llvm::Value*, int> StoreMerging::findBasePointerAndOffset(const llvm::Value* pointer)
{
	int totalOffset = 0;

	while (true)
	{
		if (const GetElementPtrInst* gep = dyn_cast<GetElementPtrInst>(pointer))
		{
			if (gep->hasAllConstantIndices())
			{
				llvm::Type* curType = gep->getOperand(0)->getType();
				for(unsigned i=1; i<gep->getNumOperands(); i++)
				{
					ConstantInt* CI = dyn_cast<ConstantInt>(gep->getOperand(i));
					assert(CI);

					//partialOffset modifyies curType
					totalOffset += partialOffset(curType, *DL, CI->getSExtValue());
				}

				pointer = gep->getPointerOperand();
				continue;
			}
		}
		else if (const BitCastInst* bci = dyn_cast<BitCastInst>(pointer))
		{
			//totalOffset remains the same
			pointer = cast<BitCastInst>(pointer)->getOperand(0);
			continue;
		}

		break;
	}

	return {pointer, totalOffset};
}

void StoreMerging::getAnalysisUsage(AnalysisUsage &AU) const
{
	AU.addPreserved<cheerp::GlobalDepsAnalyzer>();
}

char StoreMerging::ID = 0;

FunctionPass *cheerp::createStoreMergingPass(const bool isWasm) { return new StoreMerging(isWasm); }

INITIALIZE_PASS_BEGIN(StoreMerging, "StoreMerging", "Merge adjacent stores",
                      false, false)
INITIALIZE_PASS_END(StoreMerging, "StoreMerging", "Merge adjacent stores",
                    false, false)

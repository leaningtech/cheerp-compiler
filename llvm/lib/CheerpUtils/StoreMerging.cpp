//===-- StoreMerging.cpp - Merging of adjacent stores --------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2014-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/InvokeWrapping.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/StoreMerging.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

using namespace cheerp;

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
	std::vector<StoreAndOffset> basedOnCurrentPtr;

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
			basedOnCurrentPtr.emplace_back(SI, pair.second, basedOnCurrentPtr.size());
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

void StoreMerging::filterAlreadyProcessedStores(std::vector<StoreAndOffset>& groupedSamePointer, std::vector<uint32_t>& dimension)
{
	//Bookkeeping 3: remove the stores with dimension set to 0
	//We use two temporary vectors, and then swap them out for the older ones
	std::vector<StoreAndOffset> newGroupedSamePointer;
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

void StoreMerging::processBlockOfStores(std::vector<StoreAndOffset>& groupedSamePointer)
{
	if (groupedSamePointer.size() < 2)
		return;

	//Sort based on the offset
	std::sort(groupedSamePointer.begin(), groupedSamePointer.end(),
			[](const StoreAndOffset& left, const StoreAndOffset& right) -> bool
			{
				return left.offset < right.offset;
			});

	//Calculate dimension of the various pieces
	const uint32_t N = groupedSamePointer.size();
	std::vector<uint32_t> dimension(N);
	for (uint32_t i=0; i<N; i++)
	{
		auto T = groupedSamePointer[i].store->getValueOperand()->getType();
		dimension[i] = DL->getTypeAllocSize(T);
	}

	bool overlap = false;
	for (uint32_t i=0; i+1<N; i++)
	{
		if (groupedSamePointer[i].offset + (int)dimension[i] > groupedSamePointer[i+1].offset)
			overlap = true;
	}


	//Avoid the optimization if any store overlap
	if (overlap)
		return;

	//Alternatively process a block of stores and filter out already consumed ones
	//Processing with increasing dimension means that we may optimize even already optimized stores
	processBlockOfStores(1, groupedSamePointer, dimension);
	filterAlreadyProcessedStores(groupedSamePointer, dimension);

	processBlockOfStores(2, groupedSamePointer, dimension);
	filterAlreadyProcessedStores(groupedSamePointer, dimension);

	//Do not create 64-bit asmjs stores
	if (!isWasm)
		return;

	processBlockOfStores(4, groupedSamePointer, dimension);
	filterAlreadyProcessedStores(groupedSamePointer, dimension);
}

void StoreMerging::processBlockOfStores(const uint32_t dim, std::vector<StoreAndOffset> & groupedSamePointer, std::vector<uint32_t>& dimension)
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
		if ((int)dim + groupedSamePointer[a].offset != groupedSamePointer[b].offset)
			continue;

		StoreInst* lowStore = groupedSamePointer[a].store;
		StoreInst* highStore = groupedSamePointer[b].store;

		const uint32_t alignment = lowStore->getAlign().value();

		if (!isWasm && alignment < dim * 2)
			continue;

		Value* lowValue = lowStore->getValueOperand();
		Value* highValue = highStore->getValueOperand();

		//For now avoid complexities related to float/double to int bitcasts
		if (lowValue->getType()->isFloatTy() || lowValue->getType()->isVectorTy())
			continue;
		if (highValue->getType()->isFloatTy() || highValue->getType()->isVectorTy())
			continue;

		const Constant* constantLowValue = dyn_cast<Constant>(lowValue);
		const Constant* constantHighValue = dyn_cast<Constant>(highValue);

		enum STRATEGY { NOT_CONVENIENT = 0, CONSTANT = 1, ZERO_EXTEND = 2};
		STRATEGY strategy = NOT_CONVENIENT;

		//Both ValueOperands constants -> folded in a single store
		if (constantLowValue && constantHighValue)
			strategy = CONSTANT;
		//Higher ValueOperands 0 -> folded in a single store
		else if (constantHighValue && constantHighValue->isNullValue())
			strategy = ZERO_EXTEND;

		if (strategy == NOT_CONVENIENT)
			continue;

		auto& context = lowStore->getParent()->getContext();
		Type* bigType = IntegerType::get(context, dim * 16);
		Type* int32Type = IntegerType::get(context, 32);

		// The insertion point will be the store writing to the first byte
		IRBuilder<> builder(lowStore);

		auto convertToBigType = [&builder, &bigType, &int32Type, &context, this](Value* value) -> Value*
		{
			//Convert to integer (either from pointer or other type)
			if (value->getType()->isPointerTy())
				value = builder.CreatePtrToInt(value, int32Type);
			else if (!value->getType()->isIntegerTy())
			{
				Type* integerEquivalent = IntegerType::get(context, DL->getTypeAllocSize(value->getType()));
				value = builder.CreateBitCast(value, integerEquivalent);
			}

			//Then zero extend
			if (value->getType() != bigType)
				value = builder.CreateZExt(value, bigType);

			return value;
		};

		Value* sum = nullptr;
		if(strategy == CONSTANT || strategy == ZERO_EXTEND)
		{
			sum = convertToBigType(lowValue);

			//Add shifted higher part
			if (strategy == CONSTANT)
			{
				Value* high = convertToBigType(highValue);
				Value* shiftToHigh = builder.CreateShl(high, dim*8);
				sum = builder.CreateAdd(sum, shiftToHigh);
			}
		}
		else
		{
			assert(false);
		}

		//BitCast the pointer operand
		Value* bitcast = builder.CreateBitCast(lowStore->getPointerOperand(), bigType->getPointerTo());

		//Actually create the store
		StoreInst* biggerStore = cast<StoreInst>(builder.CreateStore(sum, bitcast));
		biggerStore->setAlignment(llvm::Align(alignment));

		//Bookkeeping 1: take note of what to later erase
		toErase.push_back(lowStore);
		toErase.push_back(highStore);

		//Bookkeeping 2: insert biggerStore at the right point in groupedSamePointer
		groupedSamePointer[a].store = biggerStore;
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
					totalOffset += partialOffset(curType, gep->getSourceElementType(), *DL, CI->getSExtValue());
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

llvm::PreservedAnalyses StoreMergingPass::run(Function& F, FunctionAnalysisManager& FAM)
{
	StoreMerging inner(isWasm);
	if (!inner.runOnFunction(F))
		return PreservedAnalyses::all();

	PreservedAnalyses PA;
	PA.preserve<InvokeWrappingAnalysis>();
	PA.preserve<GlobalDepsAnalysis>();
	return PA;
}

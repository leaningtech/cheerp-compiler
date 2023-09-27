//===-- Cheerp/StoreMerging.h - Merging store to adjacent memory locations ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2020-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_STORE_MERGING_H
#define CHEERP_STORE_MERGING_H

#include "llvm/IR/Module.h"
#include <vector>

namespace cheerp
{

class StoreMerging
{
private:
	struct StoreAndOffset
	{
		llvm::StoreInst* store;
		uint32_t size;
		int32_t offset;
		uint32_t blockIndex;
		StoreAndOffset(llvm::StoreInst* store, uint32_t size, int32_t offset, uint32_t blockIndex):store(store),size(size),offset(offset),blockIndex(blockIndex)
		{
		}
	};
	const llvm::DataLayout* DL;
	const bool isWasm;
	std::pair<const llvm::Value*, int> findBasePointerAndOffset(const llvm::Value* pointer);
	std::pair<bool, int> compatibleAndOffset(const llvm::Value* currPtr, const llvm::Value* referencePtr);
	static void filterAlreadyProcessedStores(std::vector<StoreAndOffset>& groupedSamePointer);
	bool checkReordering(llvm::Instruction* startInst, llvm::Instruction* endInst, const StoreAndOffset& movedInst);
	static void sortStores(std::vector<StoreAndOffset>& groupedSamePointer);
	bool processBlockOfStores(std::vector<StoreAndOffset>& groupedSamePointer);
	bool processBlockOfStores(const uint32_t dim, std::vector<StoreAndOffset> & groupedSamePointer);
	bool runOnBasicBlock(llvm::BasicBlock& BB);
public:
	explicit StoreMerging(const llvm::DataLayout& DL, const bool isWasm) : DL(&DL), isWasm(isWasm) { }
	bool runOnFunction(llvm::Function& F);
};



//===----------------------------------------------------------------------===//
//
// StoreMerging - This pass transform a pair of store to adjacent memory locations
// to a single store for the integer type twice as big
//
class StoreMergingPass : public llvm::PassInfoMixin<StoreMergingPass> {
public:
	const bool isWasm;
	StoreMergingPass(bool isWasm):
		isWasm(isWasm)
	{
	}
	llvm::PreservedAnalyses run(llvm::Function& M, llvm::FunctionAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

}	//end namespace cheerp

#endif

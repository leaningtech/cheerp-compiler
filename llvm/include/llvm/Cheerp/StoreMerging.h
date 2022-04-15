//===-- Cheerp/StoreMerging.h - Merging store to adjacent memory locations ---------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_STORE_MERGING_H
#define CHEERP_STORE_MERGING_H

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include <vector>

namespace cheerp
{

class StoreMerging
{
private:
	const bool isWasm;
	const llvm::DataLayout* DL;
	std::vector<llvm::StoreInst*> toErase;
	std::pair<const llvm::Value*, int> findBasePointerAndOffset(const llvm::Value* pointer);
	std::pair<bool, int> compatibleAndOffset(const llvm::Value* currPtr, const llvm::Value* referencePtr);
	void processBlockOfStores(std::vector<std::pair<llvm::StoreInst*, int> > groupedSamePointer);
	void processBlockOfStores(const uint32_t dim, std::vector<std::pair<llvm::StoreInst*, int> > & groupedSamePointer, std::vector<uint32_t>& dimension, llvm::IRBuilder<>& builder);
	bool runOnBasicBlock(llvm::BasicBlock& BB);
public:
	explicit StoreMerging(const bool isWasm = false) : isWasm(isWasm), DL(NULL) { }
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

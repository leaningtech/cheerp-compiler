//===-- Cheerp/ReplaceNopCastsAndByteSwaps.h ------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_REPLACE_NOP_CASTS_AND_BYTE_SWAPS_H
#define _CHEERP_REPLACE_NOP_CASTS_AND_BYTE_SWAPS_H

#include "llvm/CodeGen/IntrinsicLowering.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/PassManager.h"

namespace cheerp {

// Replace all NopCasts with BitCasts and bswap intrinsics with logical operations
class ReplaceNopCastsAndByteSwaps
{
public:
	explicit ReplaceNopCastsAndByteSwaps() : IL() { }

	bool runOnFunction(llvm::Function &F);
	
private:
	bool processBasicBlock(llvm::BasicBlock & BB);
	
	std::unique_ptr<llvm::IntrinsicLowering> IL;
};

//===----------------------------------------------------------------------===//
//
// ReplaceNopCastsAndByteSwaps
//
class ReplaceNopCastsAndByteSwapsPass : public llvm::PassInfoMixin<ReplaceNopCastsAndByteSwapsPass> {
public:
	llvm::PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM)
	{
		ReplaceNopCastsAndByteSwaps inner;
		if (!inner.runOnFunction(F))
			return llvm::PreservedAnalyses::all();
		return llvm::PreservedAnalyses::none();
	}
	static bool isRequired() { return true;}
};


}

#endif //_CHEERP_REPLACE_NOP_CASTS_AND_BYTE_SWAPS_H


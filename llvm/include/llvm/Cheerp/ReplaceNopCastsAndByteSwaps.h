//===-- Cheerp/ReplaceNopCastsAndByteSwaps.h ------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2016 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_REPLACE_NOP_CASTS_AND_BYTE_SWAPS_H
#define _CHEERP_REPLACE_NOP_CASTS_AND_BYTE_SWAPS_H

#include "llvm/CodeGen/IntrinsicLowering.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Pass.h"

namespace cheerp {

// Replace all NopCasts with BitCasts and bswap intrinsics with logical operations
class ReplaceNopCastsAndByteSwaps: public llvm::FunctionPass
{
public:
	static char ID;

	explicit ReplaceNopCastsAndByteSwaps() : FunctionPass(ID), IL(NULL) { }

	virtual bool runOnFunction(llvm::Function &F) override;
	
	virtual const char *getPassName() const override;
	
private:
	bool processBasicBlock(llvm::BasicBlock & BB);
	
	llvm::IntrinsicLowering* IL;
};

//===----------------------------------------------------------------------===//
//
// ReplaceNopCastsAndByteSwaps
//
llvm::FunctionPass *createReplaceNopCastsAndByteSwapsPass();
}

#endif //_CHEERP_REPLACE_NOP_CASTS_AND_BYTE_SWAPS_H


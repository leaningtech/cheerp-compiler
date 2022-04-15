//===-- Cheerp/AllocateArrayLowering.h - Cheerp optimization pass ---------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_ALLOCATE_ARRAY_LOWERING_H
#define _CHEERP_ALLOCATE_ARRAY_LOWERING_H

#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"

namespace llvm
{

/**
 * Lowers cheerp_allocate_array and cheerp_deallocate_array
 */
class AllocateArrayLowering: public FunctionPass
{
public:
	static char ID;
	explicit AllocateArrayLowering() : FunctionPass(ID) { }
	bool runOnFunction(Function &F) override;
	StringRef getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// AllocateArrayLowering
//
FunctionPass *createAllocateArrayLoweringPass();

}

#endif

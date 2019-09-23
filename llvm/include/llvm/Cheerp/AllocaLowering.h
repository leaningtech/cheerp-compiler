//===-- Cheerp/AllocaLowering.h - Cheerp optimization pass ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_ALLOCA_LOWERING_H
#define _CHEERP_ALLOCA_LOWERING_H

#include "llvm/Pass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"

namespace llvm
{

/**
 * Remove allocas to asmjs types and add stack manipulation intrinsics
 */
class AllocaLowering: public FunctionPass
{
public:
	static char ID;
	explicit AllocaLowering() : FunctionPass(ID) { }
	bool runOnFunction(Function &F) override;
	StringRef getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// AllocaLowering
//
FunctionPass *createAllocaLoweringPass();

}

#endif

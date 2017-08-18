//===-- Cheerp/CFGPasses.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_CFG_PASSES_H
#define _CHEERP_CFG_PASSES_H

#include "llvm/Pass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"

namespace llvm
{
/*
 * This pass removes useless blocks consisting only of an unconditional
 * branch, and whose successor does not contain PHIs referencing them.
 * This avoid the generation of empty if/else statements in Relooper.
 */
class RemoveFwdBlocks: public FunctionPass
{
public:
	static char ID;
	explicit RemoveFwdBlocks() : FunctionPass(ID) { }
	bool runOnFunction(Function &F) override;
	const char *getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// RemoveFwdBlocks
//
FunctionPass *createRemoveFwdBlocksPass();

}

#endif

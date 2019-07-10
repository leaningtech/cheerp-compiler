//===-- Cheerp/ByValLowering.h - Cheerp optimization pass ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_BYVAL_LOWERING_H
#define _CHEERP_BYVAL_LOWERING_H

#include "llvm/Pass.h"
#include "llvm/IR/Module.h"

namespace llvm
{

/**
 * Lower byval arguments by creating a copy of the argument as an alloca
 */
class ByValLowering: public ModulePass
{
public:
	static char ID;
	explicit ByValLowering() : ModulePass(ID) { }
	bool runOnModule(Module &M) override;
	const char *getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

//===----------------------------------------------------------------------===//
//
// ByValLowering
//
ModulePass *createByValLoweringPass();

}

#endif

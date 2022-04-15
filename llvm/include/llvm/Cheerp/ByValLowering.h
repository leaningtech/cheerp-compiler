//===-- Cheerp/ByValLowering.h - Cheerp optimization pass ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_BYVAL_LOWERING_H
#define _CHEERP_BYVAL_LOWERING_H

#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"

namespace llvm
{

class ByValLowering
{
public:
	explicit ByValLowering() { }
	bool runOnModule(Module &M);
};

//===----------------------------------------------------------------------===//
//
// ByValLowering - Lower byval arguments by creating a copy of the argument as an alloca
//
class ByValLoweringPass: public PassInfoMixin<ByValLoweringPass>
{
	static bool runOnModule(Module& Module);
public:
	PreservedAnalyses run(Module &Module, ModuleAnalysisManager&);
	static bool isRequired() { return true; }
	friend ByValLowering;
};

}

#endif

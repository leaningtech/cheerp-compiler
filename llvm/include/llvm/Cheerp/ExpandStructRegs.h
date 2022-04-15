//===-- Cheerp/ExpandStructRegs.h - Expand out variables with struct type ---==//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_EXPAND_STRUCT_REGS_H
#define CHEERP_EXPAND_STRUCT_REGS_H

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/PassManager.h"

namespace llvm
{

class ExpandStructRegs
{
public:
	explicit ExpandStructRegs() { }
	bool runOnFunction(Function &F);
};

//===----------------------------------------------------------------------===//
//
// ExpandStructRegs - This pass converts load/store/insertelement/extractelement/select
// operations on structs to the equivalent operation on the struct fields.
//
class ExpandStructRegsPass: public PassInfoMixin<ExpandStructRegsPass>
{
	static bool runOnFunction(Function& Func);
public:
	PreservedAnalyses run(Function &F, FunctionAnalysisManager&);
	static bool isRequired() { return true; }
	friend ExpandStructRegs;
};

}

#endif

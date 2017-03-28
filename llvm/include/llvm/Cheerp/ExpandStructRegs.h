//===-- Cheerp/ExpandStructRegs.h - Expand out variables with struct type ---==//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_EXPAND_STRUCT_REGS_H
#define CHEERP_EXPAND_STRUCT_REGS_H

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"

namespace llvm
{

class ExpandStructRegs: public FunctionPass
{
private:
	const DataLayout* DL;
public:
	static char ID;
	explicit ExpandStructRegs() : FunctionPass(ID), DL(NULL) { }
	bool runOnFunction(Function &F);
	const char *getPassName() const;
};

//===----------------------------------------------------------------------===//
//
// ExpandStructRegs - This pass converts load/store/insertelement/extractelement/select
// operations on structs to the equivalent operation on the struct fields.
//
FunctionPass *createExpandStructRegs();

}

#endif

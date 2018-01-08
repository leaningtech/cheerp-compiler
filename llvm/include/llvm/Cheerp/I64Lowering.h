//===-- Cheerp/I64Lowering.h ------------------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_I64_LOWERING_H
#define _CHEERP_I64_LOWERING_H

#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Pass.h"

namespace cheerp {

// Converts 64-bit integer operations into 32-bit ones
class I64Lowering: public llvm::FunctionPass
{
public:
	static char ID;

	explicit I64Lowering() : FunctionPass(ID) { }

	virtual bool runOnFunction(llvm::Function &F) override;
	
	virtual llvm::StringRef getPassName() const override;
};

//===----------------------------------------------------------------------===//
//
// HighIntLowering
//
llvm::FunctionPass *createI64LoweringPass();
}

#endif //_CHEERP_I64_LOWERING_H


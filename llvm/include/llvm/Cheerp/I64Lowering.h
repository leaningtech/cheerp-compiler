//===-- Cheerp/I64Lowering.h ------------------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_I64_LOWERING_H
#define _CHEERP_I64_LOWERING_H

#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/PassManager.h"

namespace cheerp {

// Converts 64-bit integer operations into 32-bit ones
//===----------------------------------------------------------------------===//
//
// I64LoweringPass
//
class I64LoweringPass : public llvm::PassInfoMixin<I64LoweringPass> {
public:
	llvm::PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};
}

#endif //_CHEERP_I64_LOWERING_H


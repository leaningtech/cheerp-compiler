//===-- SIMDLowering.h - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2022-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_SIMD_LOWERING_H
#define CHEERP_SIMD_LOWERING_H

#include "llvm/IR/PassManager.h"

namespace cheerp{

using namespace llvm;

class SIMDLoweringPass: public llvm::PassInfoMixin<SIMDLoweringPass> {

public:
	PreservedAnalyses run(llvm::Function& F, FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

}

#endif

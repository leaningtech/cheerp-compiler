//===-- Cheerp/UnionOptimizer.h - Cheerp optimization pass ----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2024 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_UNION_OPTIMIZER_H
#define _CHEERP_UNION_OPTIMIZER_H

#include "llvm/IR/PassManager.h"

namespace cheerp {

class UnionOptimizerPass: public llvm::PassInfoMixin<UnionOptimizerPass> {
public:
	llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager&);
};

}

#endif // _CHEERP_UNION_OPTIMIZER_H

//===-- Cheerp/CallConstructors.h - Cheerp backend pass -------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2022-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_CALL_CONSTRUCTORS_H
#define _CHEERP_CALL_CONSTRUCTORS_H

#include "llvm/IR/PassManager.h"

namespace cheerp {

class CallConstructorsPass: public llvm::PassInfoMixin<CallConstructorsPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module &M, llvm::ModuleAnalysisManager&);
};

}

#endif // _CHEERP_CALL_CONSTRUCTORS_H

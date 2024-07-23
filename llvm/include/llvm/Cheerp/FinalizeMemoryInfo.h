//===-- MemoryInit.h - Populate the __memory_init function with init intrinsics-------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2022-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_MEMORY_INIT_H
#define CHEERP_MEMORY_INIT_H

#include "llvm/IR/PassManager.h"

namespace cheerp
{

using namespace llvm;

class FinalizeMemoryInfoPass : public PassInfoMixin<FinalizeMemoryInfoPass> {
private:

public:
	PreservedAnalyses run(Module& M, ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

}
#endif

//===-- ThreadLocalLowering.h - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2024-2025 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_THREAD_LOCAL_LOWERING_H
#define CHEERP_THREAD_LOCAL_LOWERING_H

#include "llvm/IR/PassManager.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"

namespace cheerp{

using namespace llvm;

class ThreadLocalLoweringInnerPass: public PassInfoMixin<ThreadLocalLoweringInnerPass> {
	GlobalDepsAnalyzer& GDA;
public:
	ThreadLocalLoweringInnerPass(GlobalDepsAnalyzer& GDA) : GDA(GDA)
	{
	}
	PreservedAnalyses run(Function& F, FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

class ThreadLocalLoweringPass: public PassInfoMixin<ThreadLocalLoweringPass> {
public:
	PreservedAnalyses run(Module& M, ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

}

#endif

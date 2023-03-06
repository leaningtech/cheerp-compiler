//===-- Cheerp/CheerpLowerInvoke.h - Remove Invoke ---==//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2022-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_LOWER_INVOKE_H
#define CHEERP_LOWER_INVOKE_H

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/PassManager.h"

namespace llvm
{

class CheerpLowerInvokePass : public PassInfoMixin<CheerpLowerInvokePass> {
	static bool runOnFunction(Function& Func);
public:
	PreservedAnalyses run(Function &F, FunctionAnalysisManager&);
	static bool isRequired() { return true; }
};

}

#endif


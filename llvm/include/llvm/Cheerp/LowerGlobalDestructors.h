//===-- Cheerp/LowerGlobalDestructors.h - Cheerp helper -------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2025 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_LOWER_GLOBAL_DESTRUCTORS_H
#define CHEERP_LOWER_GLOBAL_DESTRUCTORS_H

#include "llvm/IR/PassManager.h"

namespace cheerp{

using namespace llvm;

class LowerGlobalDestructorsPass: public PassInfoMixin<LowerGlobalDestructorsPass> {
private:
	void filterGenericJSDestructors(Module& M);
public:
	PreservedAnalyses run(Module& M, ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

}

#endif

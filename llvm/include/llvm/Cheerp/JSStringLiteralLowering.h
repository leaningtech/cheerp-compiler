//===-- JSStringLiteralLowering.h - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_JSSTRINGLITERAL_LOWERING_H
#define CHEERP_JSSTRINGLITERAL_LOWERING_H

#include "llvm/IR/PassManager.h"

namespace cheerp{

using namespace llvm;

// This pass optimizes aways client::String constructors from constant string.
// The global use is removed and replaced with a metadata reference so that
// the global itself can be dropped as well
class JSStringLiteralLoweringPass: public llvm::PassInfoMixin<JSStringLiteralLoweringPass> {

public:
	PreservedAnalyses run(llvm::Function& M, FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

}

#endif

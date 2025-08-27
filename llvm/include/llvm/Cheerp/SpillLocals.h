//===-- Cheerp/SpillLocals.h - Push locals to the reserved stack ----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2025 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_SPILL_LOCALS_H
#define _CHEERP_SPILL_LOCALS_H

#include "llvm/Cheerp/PointerAnalyzer.h"

namespace cheerp {

class SpillLocals
{
public:
	static char ID;
	explicit SpillLocals() { }

	bool runOnFunction(llvm::Module& M, llvm::Function &F, const PointerAnalyzer& PA);
};

class SpillLocalsPass : public llvm::PassInfoMixin<SpillLocalsPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true; }
};

}

#endif //_CHEERP_SPILL_LOCALS_H

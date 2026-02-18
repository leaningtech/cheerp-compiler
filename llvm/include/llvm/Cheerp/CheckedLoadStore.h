//===-- Cheerp/CheckLoadStore.h - Check addresses before loads/stores -----===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2026 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_CHECK_LOAD_STORE_H
#define _CHEERP_CHECK_LOAD_STORE_H

#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"

namespace cheerp {

class CheckLoadStore
{
public:
	static char ID;
	explicit CheckLoadStore() { }

	bool runOnFunction(llvm::Module& M, llvm::Function &F);
};

class CheckLoadStorePass : public llvm::PassInfoMixin<CheckLoadStorePass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true; }
};

}

#endif //_CHEERP_CHECK_LOAD_STORE_H

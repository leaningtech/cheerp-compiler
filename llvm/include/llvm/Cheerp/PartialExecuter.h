//===-- Cheerp/PartialExecuter.h - Analyze functions CFG to mark unreachable BBs --===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_PARTIAL_EXECUTER_H
#define _CHEERP_PARTIAL_EXECUTER_H

#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"

namespace cheerp
{

/**
 */
class PartialExecuter
{
public:
	PartialExecuter() {}
	bool runOnModule(llvm::Module& module);
};

class PartialExecuterPass : public llvm::PassInfoMixin<PartialExecuterPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};


}

#endif

//===-- Cheerp/AllocaLowering.h - Cheerp optimization pass ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_ALLOCA_LOWERING_H
#define _CHEERP_ALLOCA_LOWERING_H

#include "llvm/IR/PassManager.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"

namespace llvm
{
class DominatorTree;
}
namespace cheerp
{
/**
 * Remove allocas to asmjs types and add stack manipulation intrinsics
 */
class AllocaLowering
{
public:
	explicit AllocaLowering() { }
	bool runOnFunction(llvm::Function &F, llvm::DominatorTree& DT, GlobalDepsAnalyzer& GDA);
};

//===----------------------------------------------------------------------===//
//
// AllocaLowering
//
class AllocaLoweringInnerPass : public llvm::PassInfoMixin<AllocaLoweringInnerPass> {
	GlobalDepsAnalyzer& GDA;
public:
	AllocaLoweringInnerPass(GlobalDepsAnalyzer& GDA) : GDA(GDA)
	{}
	llvm::PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};
class AllocaLoweringPass : public llvm::PassInfoMixin<AllocaLoweringPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

}

#endif

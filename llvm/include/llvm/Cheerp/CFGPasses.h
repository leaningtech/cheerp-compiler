//===-- Cheerp/CFGPasses.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_CFG_PASSES_H
#define _CHEERP_CFG_PASSES_H

#include "llvm/Pass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"

namespace llvm
{
/*
 * This pass removes useless blocks consisting only of an unconditional
 * branch, and whose successor does not contain PHIs referencing them.
 * This avoid the generation of empty if/else statements in Relooper.
 */
class RemoveFwdBlocks: public FunctionPass
{
public:
	static char ID;
	explicit RemoveFwdBlocks() : FunctionPass(ID) { }
	bool runOnFunction(Function &F) override;
	StringRef getPassName() const override;

	virtual void getAnalysisUsage(AnalysisUsage&) const override;
};

/*
 * This pass will expand branches on boolean and/or instructions
 * to multiple branches. Engines uses inefficient materialization
 * of flag values to compute the and/or result, so it's more efficient
 * to simply branch twice.
 */
class LowerAndOrBranches: public FunctionPass
{
private:
	void fixTargetPhis(llvm::BasicBlock* originalPred, llvm::BasicBlock* newBlock, llvm::BasicBlock* singleBlock, llvm::BasicBlock* doubleBlock);
public:
	static char ID;
	explicit LowerAndOrBranches() : FunctionPass(ID) { }
	bool runOnFunction(Function &F) override;
	StringRef getPassName() const override;
};

//===----------------------------------------------------------------------===//
//
// RemoveFwdBlocks
//
FunctionPass *createRemoveFwdBlocksPass();
//===----------------------------------------------------------------------===//
//
// LowerAndOrBranches
//
FunctionPass *createLowerAndOrBranchesPass();
//===----------------------------------------------------------------------===//
//
// CheerpLowerSwitch
//
FunctionPass* createCheerpLowerSwitchPass(bool onlyLowerI64);
//===----------------------------------------------------------------------===//
//
// FixFunctionCasts
//
ModulePass* createFixFunctionCastsPass();
//===----------------------------------------------------------------------===//
//
// CheerpLowerInvoke
//
FunctionPass* createCheerpLowerInvokePass();

}

#endif

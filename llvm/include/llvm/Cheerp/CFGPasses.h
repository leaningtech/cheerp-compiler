//===-- Cheerp/CFGPasses.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_CFG_PASSES_H
#define _CHEERP_CFG_PASSES_H

#include "llvm/IR/Instructions.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Module.h"

namespace cheerp
{
/*
 * This pass removes useless blocks consisting only of an unconditional
 * branch, and whose successor does not contain PHIs referencing them.
 * This avoid the generation of empty if/else statements in Relooper.
 */
class RemoveFwdBlocks
{
public:
	explicit RemoveFwdBlocks() { }
	bool runOnFunction(llvm::Function &F);
};

/*
 * This pass will expand branches on boolean and/or instructions
 * to multiple branches. Engines uses inefficient materialization
 * of flag values to compute the and/or result, so it's more efficient
 * to simply branch twice.
 */
class LowerAndOrBranches
{
private:
	void fixTargetPhis(llvm::BasicBlock* originalPred, llvm::BasicBlock* newBlock, llvm::BasicBlock* singleBlock, llvm::BasicBlock* doubleBlock);
public:
	explicit LowerAndOrBranches() { }
	bool runOnFunction(llvm::Function &F);
};
}//llvm

namespace cheerp{

using namespace llvm;

/*
 * This pass generates BasicBlocks such that they have only unconditional predecessors,
 * that in some metric are similar enough so that sinking logic (either GVNSink or
 * SimplifyCFG::Sink) are able to sink common instruction in the generated block.
 */
class SinkGenerator
{
private:
	typedef std::vector<std::vector<BasicBlock*> > VectorGroupOfBlocks;
	typedef std::vector<std::pair<BasicBlock*, VectorGroupOfBlocks> > SinkLocationToCreate;

	void examineBasicBlock(BasicBlock& BB, SinkLocationToCreate& groupedIncomings);
	void addSinkTargets(BasicBlock& BB, const VectorGroupOfBlocks& groups);
	void addSingleSinkTarget(BasicBlock& BB, const std::vector<BasicBlock*>& incomings);
public:
	explicit SinkGenerator() { }
	bool runOnFunction(llvm::Function &F);
};

class SinkGeneratorPass : public llvm::PassInfoMixin<SinkGeneratorPass> {
public:
	PreservedAnalyses run(llvm::Function& F, FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};
} //cheerp

namespace cheerp{
//===----------------------------------------------------------------------===//
//
// RemoveFwdBlocks
//
class RemoveFwdBlocksPass : public llvm::PassInfoMixin<RemoveFwdBlocksPass> {
public:
	PreservedAnalyses run(llvm::Function& F, FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

//===----------------------------------------------------------------------===//
//
// LowerAndOrBranches
//
class LowerAndOrBranchesPass : public llvm::PassInfoMixin<LowerAndOrBranchesPass> {
public:
	PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

//===----------------------------------------------------------------------===//
//
// CheerpLowerSwitch
//
class CheerpLowerSwitchPass : public llvm::PassInfoMixin<CheerpLowerSwitchPass> {
	const bool onlyLowerI64;
public:
	explicit CheerpLowerSwitchPass(bool onlyLowerI64 = true) :
		onlyLowerI64(onlyLowerI64)
	{
	}
	PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

//===----------------------------------------------------------------------===//
//
// FixFunctionCasts
//
class FixFunctionCastsPass : public llvm::PassInfoMixin<FixFunctionCastsPass> {
public:
	PreservedAnalyses run(Module& F, ModuleAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

}// llvm

namespace cheerp
{
//===----------------------------------------------------------------------===//
//
// RemoveFwdBlocks
//
}//cheerp
#endif

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
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/BuiltinInstructions.h"

namespace cheerp
{

/**
 */
class PartialExecuter : public llvm::ModulePass
{
public:
	static char ID;
	PartialExecuter();
	bool runOnModule(llvm::Module& module) override;
private:
	void getAnalysisUsage(llvm::AnalysisUsage& AU) const override;
	llvm::StringRef getPassName() const override;
};

inline llvm::Pass * createPartialExecuterPass()
{
	return new PartialExecuter();
}

}

#endif

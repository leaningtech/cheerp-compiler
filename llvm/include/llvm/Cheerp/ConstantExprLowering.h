//===-- Cheerp/ConstantExprLowering.h ------------------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_CONSTEXPR_LOWERING_H
#define _CHEERP_CONSTEXPR_LOWERING_H

#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Constants.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"

namespace cheerp {

// Converts 64-bit integer operations into 32-bit ones
class ConstantExprLowering
{
public:
	static char ID;

	explicit ConstantExprLowering(const LinearMemoryHelper* LH)
		: DL(nullptr), LH(LH)
	{ }

	bool runOnFunction(llvm::Function &F, bool& hasI64);
private:
	bool runOnInstruction(llvm::Instruction* I, bool& hasI64);
	llvm::Constant* visitConstantExpr(const llvm::ConstantExpr *CE, llvm::SmallDenseMap<llvm::Constant *, llvm::Constant *> &FoldedOps);

	const llvm::DataLayout* DL;
	const cheerp::LinearMemoryHelper* LH;
};

class ConstantExprLoweringPass : public llvm::PassInfoMixin<ConstantExprLoweringPass> {
public:
	llvm::PreservedAnalyses run(llvm::Module& M, llvm::ModuleAnalysisManager& MAM);
	static bool isRequired() { return true;}
};

}

#endif //_CHEERP_CONSTEXPR_LOWERING_H


//===-- Cheerp/ConstantExprLowering.h ------------------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_CONSTEXPR_LOWERING_H
#define _CHEERP_CONSTEXPR_LOWERING_H

#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Constants.h"
#include "llvm/Pass.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"

namespace cheerp {

// Converts 64-bit integer operations into 32-bit ones
class ConstantExprLowering: public llvm::FunctionPass
{
public:
	static char ID;

	explicit ConstantExprLowering()
		: FunctionPass(ID), DL(nullptr), LH(nullptr)
	{ }

	virtual bool runOnFunction(llvm::Function &F) override;
	virtual void getAnalysisUsage(llvm::AnalysisUsage & AU) const override;
	virtual llvm::StringRef getPassName() const override;

private:
	bool runOnInstruction(llvm::Instruction* I, bool& hasI64);
	llvm::Constant* visitConstantExpr(const llvm::ConstantExpr *CE, llvm::SmallDenseMap<llvm::Constant *, llvm::Constant *> &FoldedOps);

	const llvm::DataLayout* DL;
	const cheerp::LinearMemoryHelper* LH;
};

//===----------------------------------------------------------------------===//
//
// HighIntLowering
//
llvm::FunctionPass *createConstantExprLoweringPass();
}

#endif //_CHEERP_CONSTEXPR_LOWERING_H


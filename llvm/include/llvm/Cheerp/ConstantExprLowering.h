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
#include "llvm/Pass.h"

namespace cheerp {

// Converts 64-bit integer operations into 32-bit ones
class ConstantExprLowering: public llvm::FunctionPass
{
public:
	static char ID;

	explicit ConstantExprLowering()
		: FunctionPass(ID)
	{ }

	virtual bool runOnFunction(llvm::Function &F) override;
	virtual void getAnalysisUsage(llvm::AnalysisUsage & AU) const override;
	virtual llvm::StringRef getPassName() const override;
};

//===----------------------------------------------------------------------===//
//
// HighIntLowering
//
llvm::FunctionPass *createConstantExprLoweringPass();
}

#endif //_CHEERP_CONSTEXPR_LOWERING_H


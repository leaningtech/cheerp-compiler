//===-- SIMDLowering.h - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_SIMD_LOWERING_H
#define CHEERP_SIMD_LOWERING_H

#include "llvm/IR/PassManager.h"

namespace cheerp{

using namespace llvm;

class SIMDLoweringPass : public llvm::PassInfoMixin<SIMDLoweringPass> {
private:
	bool lowerExtractOrInsert(llvm::Instruction& I);
	bool lowerReduceIntrinsic(llvm::Instruction& I);
	bool isVariableExtractOrInsert(llvm::Instruction& I);
	bool isReduceIntrinsic(llvm::Instruction& I);
	std::vector<llvm::Instruction*> deleteList;
public:
	PreservedAnalyses run(llvm::Function& F, FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

}
#endif

//===-- SIMDTransform.h - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_SIMD_TRANSFORM_H
#define CHEERP_SIMD_TRANSFORM_H

#include "llvm/IR/PassManager.h"
#include "llvm/IR/Instructions.h"

namespace cheerp{

using namespace llvm;

class SIMDTransformPass : public llvm::PassInfoMixin<SIMDTransformPass> {
private:
	void checkVectorCorrectness(llvm::Instruction& I);
	bool lowerExtractOrInsert(llvm::Instruction& I);
	bool lowerReduceIntrinsic(llvm::Instruction& I);
	bool lowerBitShift(llvm::Instruction& I);
	bool lowerSplat(llvm::Instruction& I);
	bool lowerGeneralUnsupportedVectorOperation(llvm::Instruction& I);
	bool isVariableExtractOrInsert(llvm::Instruction& I);
	bool isReduceIntrinsic(llvm::Instruction& I);
	std::vector<llvm::Instruction*> deleteList;
	AllocaInst* extractInsertAlloca;
public:
	PreservedAnalyses run(llvm::Function& F, FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

}
#endif

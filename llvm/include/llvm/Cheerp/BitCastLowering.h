//===-- BitCastLowering.h - Cheerp helper -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_BITCAST_LOWERING_H
#define CHEERP_BITCAST_LOWERING_H

#include "llvm/IR/PassManager.h"

namespace cheerp{

using namespace llvm;

// This pass lowers bitcasts of scalar values to a store+load from memory.
// It only runs in asmjs, since genericjs never has these bitcasts (they are
// disabled in SROA) and Wasm has appropriate instructions to handle them.
// 64-bit bitcasts are handled separately in I64Lowering, and cheerpBitCastSlot
// is defined in GDA.
class BitCastLoweringPass: public llvm::PassInfoMixin<BitCastLoweringPass> {

public:
	PreservedAnalyses run(llvm::Function& M, FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

}

#endif

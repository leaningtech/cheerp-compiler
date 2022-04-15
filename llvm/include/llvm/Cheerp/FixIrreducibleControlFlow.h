//===-- Cheerp/FixIrreducibleControlFlow.h - Cheerp optimization pass ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2018-2022 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_FIX_IRREDUCIBLE_CONTROL_FLOW_H
#define _CHEERP_FIX_IRREDUCIBLE_CONTROL_FLOW_H

#include "llvm/IR/PassManager.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Dominators.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Cheerp/DeterministicUnorderedSet.h"

#include <unordered_map>
#include <unordered_set>
#include <queue>

namespace cheerp
{
//===----------------------------------------------------------------------===//
//
// FixIrreducibleControlFlow
//
class FixIrreducibleControlFlowPass : public llvm::PassInfoMixin<FixIrreducibleControlFlowPass> {
public:
	llvm::PreservedAnalyses run(llvm::Function& F, llvm::FunctionAnalysisManager& FAM);
	static bool isRequired() { return true;}
};

} //cheerp
#endif

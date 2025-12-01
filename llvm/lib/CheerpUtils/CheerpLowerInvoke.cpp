//===-- CheerpLowerInvoke.cpp - Cheerp optimization pass ------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2021-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/CheerpLowerInvoke.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/InitializePasses.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Utils/LowerInvoke.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
using namespace llvm;

#define DEBUG_TYPE "cheerplowerinvoke"

PreservedAnalyses CheerpLowerInvokePass::run(Function& F, FunctionAnalysisManager& AM)
{
  if (KeepInvokes)
    return PreservedAnalyses::all();
  F.setPersonalityFn(nullptr);
  PreservedAnalyses PA = LowerInvokePass().run(F, AM);
  // Changed is true if LowerInvokePass does not preserve any analysis
  bool Changed = !PA.areAllPreserved();
  // EliminateUnreachableBlocks runs no matter what
  if (llvm::EliminateUnreachableBlocks(F))
    Changed = true;
  if (Changed)
    return PreservedAnalyses::none();
  // If there are no unreachable blocks and lower invoke did nothing, we can safely preserve all analyses
  return PreservedAnalyses::all();
}





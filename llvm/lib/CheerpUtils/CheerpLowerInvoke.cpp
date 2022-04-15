//===-- CheerpLowerInvoke.cpp - Cheerp optimization pass ------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021-2022 Leaning Technologies
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
  llvm::EliminateUnreachableBlocks(F);

  return PreservedAnalyses::none();
}





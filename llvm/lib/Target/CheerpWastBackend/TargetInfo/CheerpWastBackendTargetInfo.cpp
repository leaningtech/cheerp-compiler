//===-- CheerpWastBackendTargetInfo.cpp - TargetInfo for the CheerpBackend ----===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technlogies
//
//===----------------------------------------------------------------------===//

#include "CheerpWastTargetMachine.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

Target llvm::TheCheerpWastBackendTarget;

extern "C" void LLVMInitializeCheerpWastBackendTargetInfo() { 
  RegisterTarget<Triple::cheerp, /*HasJIT=*/false>
    X(TheCheerpWastBackendTarget, "cheerp-wast", "Cheerp wast client side backend");
}

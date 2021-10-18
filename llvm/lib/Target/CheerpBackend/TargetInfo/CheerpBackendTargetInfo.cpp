//===-- CheerpBackendTargetInfo.cpp - TargetInfo for the CheerpBackend ----===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technlogies
//
//===----------------------------------------------------------------------===//

#include "CheerpTargetMachine.h"
#include "llvm/MC/TargetRegistry.h"

using namespace llvm;

Target llvm::TheCheerpBackendTarget;

extern "C" void LLVMInitializeCheerpBackendTargetInfo() { 
  RegisterTarget<Triple::cheerp, /*HasJIT=*/false>
    X(TheCheerpBackendTarget, "cheerp", "Cheerp client side backend", "Cheerp");
}

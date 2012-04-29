//===-- DuettoBackendTargetInfo.cpp - TargetInfo for the DuettoBackend ----===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technlogies
//===----------------------------------------------------------------------===//

#include "DuettoTargetMachine.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

Target llvm::TheDuettoBackendTarget;

extern "C" void LLVMInitializeDuettoBackendTargetInfo() { 
  RegisterTarget<Triple::duetto, /*HasJIT=*/false>
    X(TheDuettoBackendTarget, "duetto", "Duetto client side backend");
}

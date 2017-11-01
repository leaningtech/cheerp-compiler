//===-- CheerpWasmBackendTargetInfo.cpp - TargetInfo for the CheerpBackend ----===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technlogies
//
//===----------------------------------------------------------------------===//

#include "CheerpWasmTargetMachine.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

Target llvm::TheCheerpWastBackendTarget;
Target llvm::TheCheerpWasmBackendTarget;

extern "C" void LLVMInitializeCheerpWasmBackendTargetInfo() { 
  RegisterTarget<Triple::cheerp, /*HasJIT=*/false>
    X(TheCheerpWastBackendTarget, "cheerp-wast", "Cheerp wast client side backend");
  RegisterTarget<Triple::cheerp, /*HasJIT=*/false>
    Y(TheCheerpWasmBackendTarget, "cheerp-wasm", "Cheerp wasm client side backend");
}

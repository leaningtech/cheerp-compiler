//===-- DuettoBackendTargetInfo.cpp - TargetInfo for the DuettoBackend ----===//
//
//	Copyright 2011-2013 Leaning Technlogies
//===----------------------------------------------------------------------===//

#include "DuettoTargetMachine.h"
#include "llvm/Support/TargetRegistry.h"

using namespace llvm;

Target llvm::TheDuettoBackendTarget;

extern "C" void LLVMInitializeDuettoBackendTargetInfo() { 
  RegisterTarget<Triple::duetto, /*HasJIT=*/false>
    X(TheDuettoBackendTarget, "duetto", "Duetto client side backend");
}

//===-- CheerpWastMCAsmInfo.cpp - MCAsmInfo for Cheerp ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "CheerpWasmTargetMachine.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/TargetRegistry.h"

namespace llvm {
  class Triple;

  class CheerpWasmMCAsmInfo : public MCAsmInfo {
    virtual void anchor();
  public:
    explicit CheerpWasmMCAsmInfo(const Triple &Triple);
  };

void CheerpWasmMCAsmInfo::anchor() { }

CheerpWasmMCAsmInfo::CheerpWasmMCAsmInfo(const Triple &T) {

  // Debug Information
  SupportsDebugInformation = false;
}

static MCAsmInfo *createCheerpWasmMCAsmInfo(const MCRegisterInfo &T, const Triple& TheTriple) {
  return new CheerpWasmMCAsmInfo(TheTriple);
}

extern "C" void LLVMInitializeCheerpWasmBackendTargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn A(TheCheerpWastBackendTarget, createCheerpWasmMCAsmInfo);
  RegisterMCAsmInfoFn B(TheCheerpWasmBackendTarget, createCheerpWasmMCAsmInfo);
}

} // namespace llvm

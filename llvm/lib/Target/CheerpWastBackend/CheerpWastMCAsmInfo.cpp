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

#include "CheerpWastTargetMachine.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/TargetRegistry.h"

namespace llvm {
  class Triple;

  class CheerpWastMCAsmInfo : public MCAsmInfo {
    virtual void anchor();
  public:
    explicit CheerpWastMCAsmInfo(const Triple &Triple);
  };

void CheerpWastMCAsmInfo::anchor() { }

CheerpWastMCAsmInfo::CheerpWastMCAsmInfo(const Triple &T) {

  // Debug Information
  SupportsDebugInformation = false;
}

static MCAsmInfo *createCheerpWastMCAsmInfo(const MCRegisterInfo &T, StringRef TT) {
  Triple TheTriple(TT);
  return new CheerpWastMCAsmInfo(TheTriple);
}

extern "C" void LLVMInitializeCheerpWastBackendTargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn A(TheCheerpWastBackendTarget, createCheerpWastMCAsmInfo);
}

} // namespace llvm

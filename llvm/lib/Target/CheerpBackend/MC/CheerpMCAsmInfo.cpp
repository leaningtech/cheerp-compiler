//===-- CheerpBackend.cpp - MCAsmInfo for Cheerp ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "CheerpTargetMachine.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Support/TargetRegistry.h"

namespace llvm {
  class Triple;

  class CheerpMCAsmInfo : public MCAsmInfo {
    virtual void anchor();
  public:
    explicit CheerpMCAsmInfo(const Triple &Triple);
  };

void CheerpMCAsmInfo::anchor() { }

CheerpMCAsmInfo::CheerpMCAsmInfo(const Triple &T) {

  // Debug Information
  SupportsDebugInformation = false;
}

static MCAsmInfo *createCheerpMCAsmInfo(const MCRegisterInfo &T, const Triple& TheTriple, const MCTargetOptions&) {
  return new CheerpMCAsmInfo(TheTriple);
}

extern "C" void LLVMInitializeCheerpBackendTargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn A(TheCheerpBackendTarget, createCheerpMCAsmInfo);
}

} // namespace llvm

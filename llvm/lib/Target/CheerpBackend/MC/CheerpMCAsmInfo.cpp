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
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/ADT/Triple.h"

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

static MCAsmInfo *createCheerpMCAsmInfo(const MCRegisterInfo &T, const Triple& TheTriple, const llvm::MCTargetOptions&) {
  return new CheerpMCAsmInfo(TheTriple);
}

static MCRegisterInfo *createCheerpMCRegInfo(const Triple& TheTriple) {
  return new MCRegisterInfo();
}

static MCInstrInfo *createCheerpMCInstrInfo() {
  return new MCInstrInfo();
}

static MCSubtargetInfo *createCheerpMCSubtargetInfo(const Triple& TheTriple, StringRef a, StringRef b) {
  return new MCSubtargetInfo(TheTriple, a, b, "", {}, {}, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr);
}

extern "C" void LLVMInitializeCheerpBackendTargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn A(TheCheerpBackendTarget, createCheerpMCAsmInfo);
  RegisterMCInstrInfoFn I(TheCheerpBackendTarget, createCheerpMCInstrInfo);
  RegisterMCRegInfoFn R(TheCheerpBackendTarget, createCheerpMCRegInfo);
  RegisterMCSubtargetInfoFn S(TheCheerpBackendTarget, createCheerpMCSubtargetInfo);
}

} // namespace llvm

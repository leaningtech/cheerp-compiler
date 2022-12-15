//===-- CheerpRegisterInfo.cpp - Cheerp Register Information -------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technlogies
//
//===----------------------------------------------------------------------===//

#include "CheerpRegisterInfo.h"
#include "CheerpFrameLowering.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"

#define GET_REGINFO_MC_DESC
#define GET_REGINFO_TARGET_DESC
#include "CheerpBackendGenRegisterInfo.inc"
using namespace llvm;

CheerpRegisterInfo::CheerpRegisterInfo()
    : CheerpBackendGenRegisterInfo(0) {}

const MCPhysReg* CheerpRegisterInfo::getCalleeSavedRegs(const MachineFunction* MF) const {
  static const MCPhysReg CalleeSavedRegs[] = {0};
  return CalleeSavedRegs;
}

BitVector CheerpRegisterInfo::getReservedRegs(const MachineFunction& MF) const {
  BitVector Reserved(getNumRegs());
  for (auto Reg : {CheerpBackend::SP32, CheerpBackend::SP64, CheerpBackend::FP32, CheerpBackend::FP64})
    Reserved.set(Reg);
  return Reserved;
}

bool CheerpRegisterInfo::eliminateFrameIndex(MachineBasicBlock::iterator MI, int SPAdj,
                                             unsigned FIOperandNum, RegScavenger* RS) const {
  return false;
}

Register CheerpRegisterInfo::getFrameRegister(const MachineFunction& MF) const {
  return CheerpBackend::FP64;
}

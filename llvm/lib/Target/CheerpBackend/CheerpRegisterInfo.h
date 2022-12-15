//===-- CheerpRegisterInfo.h - Cheerp Register Information -------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technlogies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_REGISTERINFO_H
#define _CHEERP_REGISTERINFO_H

#define GET_REGINFO_ENUM
#define GET_REGINFO_HEADER
#include "CheerpBackendGenRegisterInfo.inc"

namespace llvm {

class CheerpRegisterInfo final : public CheerpBackendGenRegisterInfo {
public:
  explicit CheerpRegisterInfo();
  const MCPhysReg* getCalleeSavedRegs(const MachineFunction* MF) const override;
  BitVector getReservedRegs(const MachineFunction& MF) const override;
  bool eliminateFrameIndex(MachineBasicBlock::iterator MI, int SPAdj,
                           unsigned FIOperandNum,
                           RegScavenger* RS = nullptr) const override;
  Register getFrameRegister(const MachineFunction& MF) const override;
};

} // end namespace llvm

#endif

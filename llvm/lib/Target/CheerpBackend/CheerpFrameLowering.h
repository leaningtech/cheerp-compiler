//===-- CheerpFrameLowering.h - Cheerp Register Frame Lowering -------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technlogies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_FRAMELOWERING_H
#define _CHEERP_FRAMELOWERING_H

#include "llvm/CodeGen/TargetFrameLowering.h"

namespace llvm {

class CheerpBackendFrameLowering final : public TargetFrameLowering {
public:
  CheerpBackendFrameLowering()
      : TargetFrameLowering(StackGrowsDown, Align(16), 0, Align(16), true) {}

  void emitPrologue(MachineFunction& MF, MachineBasicBlock& MBB) const override;
  void emitEpilogue(MachineFunction& MF, MachineBasicBlock& MBB) const override;
  bool hasFP(const MachineFunction& MF) const override;
};

} // end namespace llvm

#endif


//===-- CheerpFrameLowering.cpp - Cheerp Frame Lowering -------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2022 Leaning Technlogies
//
//===----------------------------------------------------------------------===//

#include "CheerpFrameLowering.h"

using namespace llvm;


void CheerpBackendFrameLowering::emitPrologue(MachineFunction& MF, MachineBasicBlock& MBB) const {
  return;
}

void CheerpBackendFrameLowering::emitEpilogue(MachineFunction& MF, MachineBasicBlock& MBB) const {
  return;
}

bool CheerpBackendFrameLowering::hasFP(const MachineFunction& MF) const {
  return false;
}

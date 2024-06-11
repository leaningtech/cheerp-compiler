//===-- CheerpBackend.cpp - Backend wrapper for CheerpWriter---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2011-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/FileSystem.h"
#include "CheerpTargetMachine.h"
#include "CheerpTargetTransformInfo.h"
#include "CheerpWritePass.h"
#include "llvm/Cheerp/PassRegistry.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Scalar.h"
using namespace llvm;

extern "C" void LLVMInitializeCheerpBackendTarget() {
  // Register the target.
  RegisterTargetMachine<CheerpTargetMachine> X(TheCheerpBackendTarget);
}

//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool CheerpTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                           raw_pwrite_stream &o,
                                           raw_pwrite_stream *DwoOut,
                                           CodeGenFileType FileType,
                                           bool DisableVerify,
                                           MachineModuleInfoWrapperPass *MMIWP) {


  PM.add(new CheerpWritePass(o, (TargetMachine*)this));
  return false;
}

const CheerpTargetLowering* CheerpSubtarget::getTargetLowering() const
{
  return &targetLowering;
}

const CheerpSubtarget* CheerpTargetMachine::getSubtargetImpl(const Function &F) const
{
  return &subTargetInfo;
}

TargetTransformInfo CheerpTargetMachine::getTargetTransformInfo(const Function &F) const
{
  return TargetTransformInfo(CheerpTTIImpl(this, F));
}

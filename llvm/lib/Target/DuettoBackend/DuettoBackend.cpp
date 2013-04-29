//===-- DuettoBackend.cpp - Backend wrapper for DuettoWriter---------------===//
//
//	Copyright 2011-2013 Leaning Technlogies
//===----------------------------------------------------------------------===//

#include "DuettoTargetMachine.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/PassManager.h"

using namespace llvm;

extern "C" void LLVMInitializeDuettoBackendTarget() {
  // Register the target.
  RegisterTargetMachine<DuettoTargetMachine> X(TheDuettoBackendTarget);
}

//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool DuettoTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                           formatted_raw_ostream &o,
                                           CodeGenFileType FileType,
                                           bool DisableVerify) {
  if (FileType != TargetMachine::CGFT_AssemblyFile) return true;
  return false;
}

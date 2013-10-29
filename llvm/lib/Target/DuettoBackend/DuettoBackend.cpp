//===-- DuettoBackend.cpp - Backend wrapper for DuettoWriter---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "DuettoTargetMachine.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/PassManager.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Type.h"
#include "llvm/Duetto/Writer.h"

using namespace llvm;

extern "C" void LLVMInitializeDuettoBackendTarget() {
  // Register the target.
  RegisterTargetMachine<DuettoTargetMachine> X(TheDuettoBackendTarget);
}

namespace {
  class DuettoWritePass : public ModulePass {
  private:
    formatted_raw_ostream &Out;
    static char ID;
  public:
    explicit DuettoWritePass(formatted_raw_ostream &o) :
      ModulePass(ID), Out(o) { }
    bool runOnModule(Module &M);
  };
} // end anonymous namespace.

bool DuettoWritePass::runOnModule(Module& M)
{
  duetto::DuettoWriter writer(M, Out);
  writer.makeJS();
  return false;
}

char DuettoWritePass::ID = 0;

//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool DuettoTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                           formatted_raw_ostream &o,
                                           CodeGenFileType FileType,
                                           bool DisableVerify,
                                           AnalysisID StartAfter,
                                           AnalysisID StopAfter) {
  if (FileType != TargetMachine::CGFT_AssemblyFile) return true;
  PM.add(new DuettoWritePass(o));
  return false;
}

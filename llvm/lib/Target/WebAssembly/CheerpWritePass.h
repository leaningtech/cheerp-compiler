//===-- CheerpWritePass.h - Pass writer for CheerpWriter ------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2011-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Pass.h"
#include "WebAssemblyTargetMachine.h"
using namespace llvm;

class CheerpWritePass : public ModulePass {
private:
  raw_ostream &Out;
  static char ID;
  void getAnalysisUsage(AnalysisUsage& AU) const override { }
  LLVMTargetMachine* TM;
public:
  explicit CheerpWritePass(raw_ostream &o, LLVMTargetMachine* TM) :ModulePass(ID), Out(o), TM(TM) { }
  bool runOnModule(Module &M) override;
  StringRef getPassName() const override { return "CheerpWritePass"; }
};

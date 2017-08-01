//===-- CheerpWastTargetMachine.h - TargetMachine for the CheerpBackend -------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technlogies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_WAST_TARGETMACHINE_H
#define _CHEERP_WAST_TARGETMACHINE_H

#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetSubtargetInfo.h"
#include "llvm/IR/DataLayout.h"

namespace llvm {

class formatted_raw_ostream;

class CheerpBaseSubtarget : public TargetSubtargetInfo {
private:
  const DataLayout DL;

public:
  CheerpBaseSubtarget(const char* dlInit) : DL(dlInit) { }
  virtual const DataLayout* getDataLayout() const
  {
    return &DL;
  }
};

struct CheerpBaseTargetMachine : public TargetMachine {
  CheerpBaseTargetMachine(const Target &T, StringRef TT,
                   StringRef CPU, StringRef FS, const TargetOptions &Options,
                   Reloc::Model RM, CodeModel::Model CM,
                   CodeGenOpt::Level OL)
      : TargetMachine(T, TT, CPU, FS, Options),
           //NOTE: This is duplicate from clang target
           Subtarget("b-e-p:32:8:8-i1:8:8-i8:8:8-i16:8:8-i32:8:8-"
                        "i64:8:8-f32:8:8-f64:8:8-"
                        "a0:0:8-f80:8:8-n8:8:8-S8") { }
private:
  CheerpBaseSubtarget Subtarget;

public:
  const CheerpBaseSubtarget *getSubtargetImpl() const override { return &Subtarget; }
  virtual bool addPassesToEmitFile(PassManagerBase &PM,
                                   formatted_raw_ostream &Out,
                                   CodeGenFileType FileType,
                                   bool DisableVerify,
                                   AnalysisID StartAfter,
                                   AnalysisID StopAfter) override;
  virtual ModulePass* createCheerpWritePass(formatted_raw_ostream &o) = 0;
};

struct CheerpWastTargetMachine : public CheerpBaseTargetMachine {
	using CheerpBaseTargetMachine::CheerpBaseTargetMachine;
  virtual ModulePass* createCheerpWritePass(formatted_raw_ostream &o);
};

struct CheerpWasmTargetMachine : public CheerpBaseTargetMachine {
	using CheerpBaseTargetMachine::CheerpBaseTargetMachine;
  virtual ModulePass* createCheerpWritePass(formatted_raw_ostream &o);
};

extern Target TheCheerpWastBackendTarget;
extern Target TheCheerpWasmBackendTarget;

} // End llvm namespace

#endif

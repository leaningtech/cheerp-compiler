//===-- CheerpWasmTargetMachine.h - TargetMachine for the CheerpBackend -------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2019 Leaning Technlogies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_WAST_TARGETMACHINE_H
#define _CHEERP_WAST_TARGETMACHINE_H

#include "llvm/ADT/Optional.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/IR/DataLayout.h"

namespace llvm {

class formatted_raw_ostream;

struct CheerpBaseTargetMachine : public TargetMachine {
  CheerpBaseTargetMachine(const Target &T, const Triple& TT,
                   StringRef CPU, StringRef FS, const TargetOptions &Options,
                   Optional<Reloc::Model> RM, CodeModel::Model CM,
                   CodeGenOpt::Level OL)
      : TargetMachine(T, "b-e-p:32:8:8-i1:8:8-i8:8:8-i16:8:8-i32:8:8-"
                        "i64:8:8-f32:8:8-f64:8:8-"
                        "a0:0:8-f80:8:8-n8:8:8-S8",
           TT, CPU, FS, Options) {}
public:
  virtual bool addPassesToEmitFile(PassManagerBase &PM,
                                   raw_pwrite_stream &Out,
                                   CodeGenFileType FileType,
                                   bool DisableVerify,
                                   AnalysisID StartBefore,
                                   AnalysisID StartAfter,
                                   AnalysisID StopAfter,
                                   MachineFunctionInitializer* MFInit) override;
  virtual ModulePass* createCheerpWritePass(raw_ostream &o) = 0;
};

struct CheerpWastTargetMachine : public CheerpBaseTargetMachine {
	using CheerpBaseTargetMachine::CheerpBaseTargetMachine;
  virtual ModulePass* createCheerpWritePass(raw_ostream &o);
};

struct CheerpWasmTargetMachine : public CheerpBaseTargetMachine {
	using CheerpBaseTargetMachine::CheerpBaseTargetMachine;
  virtual ModulePass* createCheerpWritePass(raw_ostream &o);
};

extern Target TheCheerpWastBackendTarget;
extern Target TheCheerpWasmBackendTarget;

} // End llvm namespace

#endif

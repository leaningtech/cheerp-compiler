//===-- CheerpTargetMachine.h - TargetMachine for the CheerpBackend -------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2019 Leaning Technlogies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_TARGETMACHINE_H
#define _CHEERP_TARGETMACHINE_H

#include "llvm/ADT/Optional.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/CodeGen/TargetLowering.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/Target/TargetMachine.h"

namespace llvm {

class formatted_raw_ostream;

class CheerpTargetLowering: public TargetLowering {
public:
  CheerpTargetLowering(const TargetMachine &TM) : TargetLowering(TM) {}
};

class CheerpSubtarget: public TargetSubtargetInfo {
private:
  CheerpTargetLowering targetLowering;
public:
  CheerpSubtarget(const TargetMachine &TM, const Target &T, const Triple& TT, StringRef CPU, StringRef FS) :
        TargetSubtargetInfo(TT, CPU, FS, None, None, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr),
        targetLowering(TM) { }
  virtual const CheerpTargetLowering *getTargetLowering() const override;
};

struct CheerpTargetMachine : public LLVMTargetMachine {
  CheerpTargetMachine(const Target &T, const Triple& TT,
                   StringRef CPU, StringRef FS, const TargetOptions &Options,
                   Optional<Reloc::Model> RM, Optional<CodeModel::Model> CM,
                   CodeGenOpt::Level OL, bool JIT)
      : LLVMTargetMachine(T, "b-e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i24:8:8-i32:32:32-"
                        "i64:64:64-f32:32:32-f64:64:64-"
                        "a:0:32-f16:16:16-f32:32:32-f64:64:64-n8:16:32-S64",
                      TT, CPU, FS, Options, Reloc::PIC_, CM ? *CM : CodeModel::Medium, OL),
        subTargetInfo(*this, T, TT, CPU, FS) { }
  CheerpSubtarget subTargetInfo;

public:
  virtual bool addPassesToEmitFile(PassManagerBase &PM,
                                   raw_pwrite_stream &Out,
                                   raw_pwrite_stream *DwoOut,
                                   CodeGenFileType FileType,
                                   bool DisableVerify,
                                   MachineModuleInfoWrapperPass *MMIWP = nullptr) override;
  virtual const CheerpSubtarget* getSubtargetImpl(const Function &F) const override;
};

extern Target TheCheerpBackendTarget;

} // End llvm namespace

#endif

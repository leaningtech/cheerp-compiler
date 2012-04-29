//===-- DuettoTargetMachine.h - TargetMachine for the DuettoBackend -------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technlogies
//===----------------------------------------------------------------------===//

#ifndef _DUETTO_TARGETMACHINE_H
#define _DUETTO_TARGETMACHINE_H

#include "llvm/Target/TargetMachine.h"

namespace llvm {

class formatted_raw_ostream;

struct DuettoTargetMachine : public LLVMTargetMachine {
  DuettoTargetMachine(const Target &T, StringRef TT,
                   StringRef CPU, StringRef FS, const TargetOptions &Options,
                   Reloc::Model RM, CodeModel::Model CM,
                   CodeGenOpt::Level OL)
      : LLVMTargetMachine(T, TT, CPU, FS, Options, RM, CM, OL) { }

  virtual bool addPassesToEmitFile(PassManagerBase &PM,
                                   formatted_raw_ostream &Out,
                                   CodeGenFileType FileType,
                                   bool DisableVerify);
};

extern Target TheDuettoBackendTarget;

} // End llvm namespace

#endif

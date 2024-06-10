//===-- CheerpBackend.cpp - MCAsmInfo for Cheerp ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

//#include "MCTargetDesc/WebAssemblyMCTargetDesc.h"
#include "MC/WebAssemblyMCTargetDesc.h"
//#include "MCTargetDesc/WebAssemblyInstPrinter.h"
#include "MC/WebAssemblyInstPrinter.h"
//#include "MCTargetDesc/WebAssemblyTargetStreamer.h"
#include "MC/WebAssemblyTargetStreamer.h"
#include "MC/WebAssemblyMCTargetDesc.h"
//#include "TargetInfo/WebAssemblyTargetInfo.h"
#include "WebAssemblyUtilities.h"
#include "CheerpTargetMachine.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/ADT/Triple.h"

#define DEBUG_TYPE "wasm-mc-target-desc"

#define GET_INSTRINFO_MC_DESC
#define ENABLE_INSTR_PREDICATE_VERIFIER
#include "WebAssemblyGenInstrInfo.inc"

#define GET_SUBTARGETINFO_MC_DESC
#include "WebAssemblyGenSubtargetInfo.inc"

#define GET_REGINFO_MC_DESC
#include "WebAssemblyGenRegisterInfo.inc"

namespace llvm {
  class Triple;

  class CheerpMCAsmInfo : public MCAsmInfo {
    virtual void anchor();
  public:
    explicit CheerpMCAsmInfo(const Triple &Triple, const MCTargetOptions &);
  };

void CheerpMCAsmInfo::anchor() { }

CheerpMCAsmInfo::CheerpMCAsmInfo(const Triple &T, const MCTargetOptions &Options) {
  CodePointerSize = CalleeSaveStackSlotSize = T.isArch64Bit() ? 8 : 4;

  // TODO: What should MaxInstLength be?

  UseDataRegionDirectives = true;

  // Use .skip instead of .zero because .zero is confusing when used with two
  // arguments (it doesn't actually zero things out).
  ZeroDirective = "\t.skip\t";

  Data8bitsDirective = "\t.int8\t";
  Data16bitsDirective = "\t.int16\t";
  Data32bitsDirective = "\t.int32\t";
  Data64bitsDirective = "\t.int64\t";

  AlignmentIsInBytes = false;
  COMMDirectiveAlignmentIsInBytes = false;
  LCOMMDirectiveAlignmentType = LCOMM::Log2Alignment;

  // Debug Information TODO: enable Debug functions
  SupportsDebugInformation = false;

  // When compilation is done on a cpp file by clang, the exception model info
  // is stored in LangOptions, which is later used to set the info in
  // TargetOptions and then MCAsmInfo in LLVMTargetMachine::initAsmInfo(). But
  // this process does not happen when compiling bitcode directly with clang, so
  // we make sure this info is set correctly.
  if (WebAssembly::WasmEnableEH || WebAssembly::WasmEnableSjLj)
    ExceptionsType = ExceptionHandling::Wasm;
}

static MCAsmInfo *createCheerpMCAsmInfo(const MCRegisterInfo &T, const Triple& TheTriple, const llvm::MCTargetOptions& Options) {
  return new CheerpMCAsmInfo(TheTriple, Options);
}

static MCRegisterInfo *createCheerpMCRegInfo(const Triple& TheTriple) {
  auto *X = new MCRegisterInfo();
  InitWebAssemblyMCRegisterInfo(X, 0);
  return X;
}

static MCInstrInfo *createCheerpMCInstrInfo() {
  auto *X = new MCInstrInfo();
  InitWebAssemblyMCInstrInfo(X);
  return X;
}

static MCSubtargetInfo *createCheerpMCSubtargetInfo(const Triple& TheTriple, StringRef CPU, StringRef FS) {
  // We do not pass b on to the MCSubtargetInfo, but an empty string, to make the compiler stop giving a message
  // about +simd128 not being a recognized processor.
  return createWebAssemblyMCSubtargetInfoImpl(TheTriple, CPU, /*TuneCPU*/ CPU, FS);
}

static MCInstPrinter *createMCInstPrinter(const Triple & /*T*/,
                                          unsigned SyntaxVariant,
                                          const MCAsmInfo &MAI,
                                          const MCInstrInfo &MII,
                                          const MCRegisterInfo &MRI) {
  assert(SyntaxVariant == 0 && "WebAssembly only has one syntax variant");
  return new WebAssemblyInstPrinter(MAI, MII, MRI);
}

static MCCodeEmitter *createCodeEmitter(const MCInstrInfo &MCII,
                                        MCContext &Ctx) {
  return createWebAssemblyMCCodeEmitter(MCII);
}

static MCAsmBackend *createAsmBackend(const Target & /*T*/,
                                      const MCSubtargetInfo &STI,
                                      const MCRegisterInfo & /*MRI*/,
                                      const MCTargetOptions & /*Options*/) {
  return createWebAssemblyAsmBackend(STI.getTargetTriple());
}

static MCTargetStreamer *
createObjectTargetStreamer(MCStreamer &S, const MCSubtargetInfo &STI) {
  return new WebAssemblyTargetWasmStreamer(S);
}

static MCTargetStreamer *createAsmTargetStreamer(MCStreamer &S,
                                                 formatted_raw_ostream &OS,
                                                 MCInstPrinter * /*InstPrint*/,
                                                 bool /*isVerboseAsm*/) {
  return new WebAssemblyTargetAsmStreamer(S, OS);
}

static MCTargetStreamer *createNullTargetStreamer(MCStreamer &S) {
  return new WebAssemblyTargetNullStreamer(S);
}


extern "C" void LLVMInitializeCheerpBackendTargetMC() {
  // Register the MC asm info.
  RegisterMCAsmInfoFn A(TheCheerpBackendTarget, createCheerpMCAsmInfo);
  RegisterMCInstrInfoFn I(TheCheerpBackendTarget, createCheerpMCInstrInfo);
  RegisterMCRegInfoFn R(TheCheerpBackendTarget, createCheerpMCRegInfo);
  RegisterMCSubtargetInfoFn S(TheCheerpBackendTarget, createCheerpMCSubtargetInfo);

  // Register the MCInstPrinter.
  TargetRegistry::RegisterMCInstPrinter(TheCheerpBackendTarget, createMCInstPrinter);

  // Register the MC code emitter.
  TargetRegistry::RegisterMCCodeEmitter(TheCheerpBackendTarget, createCodeEmitter);

  // Register the ASM Backend.
  TargetRegistry::RegisterMCAsmBackend(TheCheerpBackendTarget, createAsmBackend);

  // Register the object target streamer.
  TargetRegistry::RegisterObjectTargetStreamer(TheCheerpBackendTarget,
                                               createObjectTargetStreamer);
  // Register the asm target streamer.
  TargetRegistry::RegisterAsmTargetStreamer(TheCheerpBackendTarget, createAsmTargetStreamer);
  // Register the null target streamer.
  TargetRegistry::RegisterNullTargetStreamer(TheCheerpBackendTarget, createNullTargetStreamer);
}

} // namespace llvm

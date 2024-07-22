//
// Created by c on 16-7-24.
//

#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/BinaryFormat/Wasm.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Cheerp/WasmWriter.h"
#include "llvm/IR/Attributes.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/SectionKind.h"
#include "llvm/MC/SectionKind.h"
#include "llvm/Target/TargetMachine.h"
#include "MCTargetDesc/WebAssemblyTargetStreamer.h"
#include "MCTargetDesc/WebAssemblyMCTargetDesc.h"
#include "MCTargetDesc/WebAssemblyTargetStreamer.h"
#include "TargetInfo/WebAssemblyTargetInfo.h"
#include "Utils/WebAssemblyTypeUtilities.h"
#include "Utils/WebAssemblyUtilities.h"
#include "WebAssembly.h"
#include "WebAssemblyMCInstLower.h"
#include "WebAssemblyMachineFunctionInfo.h"
#include "WebAssemblyRegisterInfo.h"
#include "WebAssemblyRuntimeLibcallSignatures.h"
#include "WebAssemblyTargetMachine.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/BinaryFormat/Wasm.h"
#include "llvm/CodeGen/Analysis.h"
#include "llvm/CodeGen/AsmPrinter.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineModuleInfoImpls.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Metadata.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionWasm.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCSymbolWasm.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/CodeGen/MachineFrameInfo.h"

using namespace llvm;

// utils only for this cpp


namespace cheerp{



void CheerpWasmWriter::emitStreamerWasm() {
//  if (linearHelper.getFunctionTypes().empty())
//    return;

  for (const Function* F:linearHelper.functions()) {
    setupSymbol(F);
    emitFunctionStreamer(F);
  }

  Streamer->finish();
  Streamer->reset();
}

void CheerpWasmWriter::emitFunctionStreamer(const Function* F) {
  emitFunctionHeader(F);
  emitFunctionBodyStart(F);
}

void CheerpWasmWriter::setupSymbol(const llvm::Function *F) {
  HasSplitStack = true;
  CurrentFnSym = getSymbol(F);

  CurrentFnSymForSize = CurrentFnSym;
  CurrentFnBegin = nullptr;
  CurrentFnBeginLocal = nullptr;
  CurrentSectionBeginSym = nullptr;

  bool NeedsLocalForSize = MAI->needsLocalForSize();

  if (F->hasFnAttribute("patchable-function-entry") ||
      F->hasFnAttribute("function-instrument") ||
      F->hasFnAttribute("xray-instruction-threshold") ||
      NeedsLocalForSize) {
    CurrentFnBegin = createTempSymbol("func_begin");
    if (NeedsLocalForSize)
      CurrentFnSymForSize = CurrentFnBegin;
  }
}

void CheerpWasmWriter::emitFunctionHeader(const Function* F) {
  MCSection *currSection = TM->getObjFileLowering()->getUniqueSectionForFunction(*F, *TM);
  Streamer->switchSection(currSection);

  if (!MAI->hasVisibilityOnlyWithLinkage())
    emitVisibility(CurrentFnSym, F->getVisibility());

  if (MAI->needsFunctionDescriptors())
    emitLinkage(F, nullptr);

  emitLinkage(F, CurrentFnSym);

  if (MAI->hasDotTypeDotSizeDirective())
    Streamer->emitSymbolAttribute(CurrentFnSym, MCSA_ELF_TypeFunction);

  if (F->hasFnAttribute(Attribute::Cold))
    Streamer->emitSymbolAttribute(CurrentFnSym, MCSA_Cold);
}

void CheerpWasmWriter::emitFunctionBodyStart(const Function* F) {

}

MCSymbolWasm *CheerpWasmWriter::getMCSymbolForFunction(const llvm::Function *F){
  MCSymbolWasm *WasmSym = nullptr;

  WasmSym = cast<MCSymbolWasm>(TM->getSymbol(F));
  return WasmSym;
}

void CheerpWasmWriter::emitSymbolType(const llvm::MCSymbolWasm *Sym) {
  Optional<wasm::WasmSymbolType> WasmTy = Sym->getType();
  if (!WasmTy)
    return;
  switch (*WasmTy) {
    case wasm::WASM_SYMBOL_TYPE_GLOBAL:
      static_cast<WebAssemblyTargetStreamer *>(Streamer->getTargetStreamer())->emitGlobalType(Sym);
      break;
    case wasm::WASM_SYMBOL_TYPE_TAG:
      static_cast<WebAssemblyTargetStreamer *>(Streamer->getTargetStreamer())->emitTagType(Sym);
      break;
    case wasm::WASM_SYMBOL_TYPE_TABLE:
      static_cast<WebAssemblyTargetStreamer *>(Streamer->getTargetStreamer())->emitTableType(Sym);
      break;
    default:
      break; // We only handle globals, tags and tables here
  }
}

void CheerpWasmWriter::emitLinkage(const GlobalValue *GV, MCSymbol *GVSym) const {
  GlobalValue::LinkageTypes Linkage = GV->getLinkage();
  switch (Linkage) {
  case GlobalValue::CommonLinkage:
  case GlobalValue::LinkOnceAnyLinkage:
  case GlobalValue::LinkOnceODRLinkage:
  case GlobalValue::WeakAnyLinkage:
  case GlobalValue::WeakODRLinkage:
    if (MAI->hasWeakDefDirective()) {
      // .globl _foo
      Streamer->emitSymbolAttribute(GVSym, MCSA_Global);

      Streamer->emitSymbolAttribute(GVSym, MCSA_WeakDefAutoPrivate);
    } else if (MAI->avoidWeakIfComdat() && GV->hasComdat()) {
      // .globl _foo
      Streamer->emitSymbolAttribute(GVSym, MCSA_Global);
      //NOTE: linkonce is handled by the section the symbol was assigned to.
    } else {
      // .weak _foo
      Streamer->emitSymbolAttribute(GVSym, MCSA_Weak);
    }
    return;
  case GlobalValue::ExternalLinkage:
    Streamer->emitSymbolAttribute(GVSym, MCSA_Global);
    return;
  case GlobalValue::PrivateLinkage:
  case GlobalValue::InternalLinkage:
    return;
  case GlobalValue::ExternalWeakLinkage:
  case GlobalValue::AvailableExternallyLinkage:
  case GlobalValue::AppendingLinkage:
    llvm_unreachable("Should never emit this");
  }
  llvm_unreachable("Unknown linkage type!");
}

void CheerpWasmWriter::emitVisibility(MCSymbol *Sym, unsigned Visibility,
                                      bool IsDefinition) const {
  MCSymbolAttr Attr = MCSA_Invalid;

  switch (Visibility) {
  default: break;
  case GlobalValue::HiddenVisibility:
    if (IsDefinition)
      Attr = MAI->getHiddenVisibilityAttr();
    else
      Attr = MAI->getHiddenDeclarationVisibilityAttr();
    break;
  case GlobalValue::ProtectedVisibility:
    Attr = MAI->getProtectedVisibilityAttr();
    break;
  }

  if (Attr != MCSA_Invalid)
    Streamer->emitSymbolAttribute(Sym, Attr);
}

}

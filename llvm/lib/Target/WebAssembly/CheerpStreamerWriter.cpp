//
// Created by c on 16-7-24.
//

#include "llvm/ADT/Optional.h"
#include "llvm/Cheerp/WasmWriter.h"
#include "WebAssemblyMachineFunctionInfo.h"
#include "WebAssemblySubtarget.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/Target/TargetMachine.h"
#include "MCTargetDesc/WebAssemblyMCTargetDesc.h"
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
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Metadata.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCSectionWasm.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSymbol.h"
#include "llvm/MC/MCSymbolWasm.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;
namespace cheerp{

void CheerpWasmWriter::emitStreamerWasm() {
  errs()<<"emitStreamerWasm()\n";
  for (const auto &F : module) {
    if (!F.isIntrinsic() && F.hasAddressTaken()) {
      MCSymbolWasm *FunctionTable =
          WebAssembly::getOrCreateFunctionTableSymbol(OutContext, nullptr);
      Streamer->emitSymbolAttribute(FunctionTable, MCSA_NoDeadStrip);
      break;
    }
  }

  for (const auto &G : module.globals()) {
    if (!G.hasInitializer() && G.hasExternalLinkage() &&
        !WebAssembly::isWasmVarAddressSpace(G.getAddressSpace()) &&
        G.getValueType()->isSized()) {
      uint16_t Size = module.getDataLayout().getTypeAllocSize(G.getValueType());
      Streamer->emitELFSize(getSymbol(&G),
                               MCConstantExpr::create(Size, OutContext));
    }
  }

  Streamer->finish();
  Streamer->reset();
}

MCSymbolWasm *CheerpWasmWriter::getMCSymbolForFunction(const llvm::Function *F){
  MCSymbolWasm *WasmSym = nullptr;

  WasmSym = cast<MCSymbolWasm>(getSymbol(F));
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

MCSymbol *CheerpWasmWriter::getSymbol(const llvm::GlobalValue *GV) const {
  return TM.getSymbol(GV);
}

}

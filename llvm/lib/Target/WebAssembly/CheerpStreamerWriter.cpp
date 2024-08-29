//
// Created by c on 16-7-24.
//

#include "llvm/MC/MCWasmObjectWriter.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/BinaryFormat/Wasm.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Cheerp/WasmWriter.h"
#include "llvm/IR/Attributes.h"
#include "llvm/MC/MCDirectives.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/SectionKind.h"
#include "llvm/MC/SectionKind.h"
#include "llvm/Support/ErrorHandling.h"
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

  std::vector<const llvm::Function*> exports;

  // Add the list of asmjs-exported functions.
  exports.insert(exports.end(), globalDeps.asmJSExports().begin(),
               globalDeps.asmJSExports().end());

  for (const Function* F: exports) {
    emitExportFunction(F);
  }

  for (const Function* F: globalDeps.asmJSImports()) {
    setupSymbol(F);
    emitFunctionStreamer(F);
  }

//  emitDataSection();

  Streamer->finish();
  Streamer->reset();
}

void CheerpWasmWriter::emitFunctionStreamer(const Function* F) {
  emitFunctionHeader(F);
  emitFunctionBodyStart(F);

  bool HasAnyRealCode = false;

  // If the function is empty and the object file uses .subsections_via_symbols,
  // then we need to emit *something* to the function body to prevent the
  // labels from collapsing together.  Just emit a noop.
  // Similarly, don't emit empty functions on Windows either. It can lead to
  // duplicate entries (two functions with the same RVA) in the Guard CF Table
  // after linking, causing the kernel not to load the binary:
  // https://developercommunity.visualstudio.com/content/problem/45366/vc-linker-creates-invalid-dll-with-clang-cl.html
  // FIXME: Hide this behind some API in e.g. MCAsmInfo or MCTargetStreamer.
  const Triple &TT = TM->getTargetTriple();

//  for (const auto &BB : *F) {
//    if (!BB.hasAddressTaken())
//      continue;
//    MCSymbol *Sym = GetBlockAddressSymbol(&BB);
//    if (Sym->isDefined())
//      continue;
//    Streamer->AddComment("Address of block that was removed by CodeGen");
//    Streamer->emitLabel(Sym);
//  }
  // Even though wasm supports .type and .size in general, function symbols
  // are automatically sized.
  bool EmitFunctionSize = MAI->hasDotTypeDotSizeDirective() && !TT.isCheerp();

  if (EmitFunctionSize) {
    // Create a symbol for the end of function.
    CurrentFnEnd = createTempSymbol("func_end");
    Streamer->emitLabel(CurrentFnEnd);
  }

  // If the target wants a .size directive for the size of the function, emit
  // it.
  if (EmitFunctionSize) {
    // We can get the size as difference between the function label and the
    // temp label.
    const MCExpr *SizeExp = MCBinaryExpr::createSub(
        MCSymbolRefExpr::create(CurrentFnEnd, OutContext),
        MCSymbolRefExpr::create(CurrentFnSymForSize, OutContext), OutContext);
    Streamer->emitELFSize(CurrentFnSym, SizeExp);
    if (CurrentFnBeginLocal)
      Streamer->emitELFSize(CurrentFnBeginLocal, SizeExp);
  }

  Streamer->addBlankLine();
}

void CheerpWasmWriter::emitDataSection() {
  uint32_t amountChunks = linearHelper.getAmountChunks();
  for (uint32_t i = 0; i < amountChunks; ++i) {
    const LinearMemoryHelper::GlobalDataChunk &chunk = linearHelper.getGlobalDataChunk(i);
    std::string buf(reinterpret_cast<const char *>(&chunk.view[0]), chunk.view.size());
    MCSymbolWasm *sym = createDataSymbol(buf);
    Streamer->emitSymbolAttribute(sym, MCSA_ELF_TypeObject);// FIXME: not sure if it should be this
  }
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

//  if (!MAI->hasVisibilityOnlyWithLinkage())
//    emitVisibility(CurrentFnSym, F->getVisibility());

//  if (MAI->needsFunctionDescriptors())
//    emitLinkage(F, nullptr);

//  emitLinkage(F, CurrentFnSym);

//  if (MAI->hasDotTypeDotSizeDirective())
    Streamer->emitSymbolAttribute(CurrentFnSym, MCSA_ELF_TypeFunction);
//    Streamer->emitSymbolAttribute(CurrentFnSym,MCSA_ELF_TypeObject);
/* MCSA_Global            Section custom linking
 * MCSA_ELF_TypeFunction  Section Import env.*
 * MCSA_Hidden            similar as Global
 * MCSA_WeakReference     similar as Global
 * MCSA_ELF_TypeTLS       similar as Global
 * MCSA_NoDeadStrip       similar as Global
 * MCSA_ELF_TypeObject    still in the custom section linking
 * */

//  if (F->hasFnAttribute(Attribute::Cold))
//    Streamer->emitSymbolAttribute(CurrentFnSym, MCSA_Cold);

  // Emit KCFI type information before patchable-function-prefix nops.
//  emitKCFITypeId(F);

  // Emit M NOPs for -fpatchable-function-entry=N,M where M>0. We arbitrarily
  // place prefix data before NOPs.
//  unsigned PatchableFunctionPrefix = 0;
//  unsigned PatchableFunctionEntry = 0;
//  (void)F->getFnAttribute("patchable-function-prefix")
//      .getValueAsString()
//      .getAsInteger(10, PatchableFunctionPrefix);
//  (void)F->getFnAttribute("patchable-function-entry")
//      .getValueAsString()
//      .getAsInteger(10, PatchableFunctionEntry);
//  if (PatchableFunctionPrefix) {
//    CurrentPatchableFunctionEntrySym =
//        OutContext.createLinkerPrivateTempSymbol();
//    Streamer->emitLabel(CurrentPatchableFunctionEntrySym);
//  } else if (PatchableFunctionEntry) {
    // May be reassigned when emitting the body, to reference the label after
    // the initial BTI (AArch64) or endbr32/endbr64 (x86).
//    CurrentPatchableFunctionEntrySym = CurrentFnBegin;
//  }

  // Emit the CurrentFnSym. This is a virtual function to allow targets to do
  // their wild and crazy things as required.
  //emitFunctionEntryLabel();

  if (CurrentFnBegin) {
    if (MAI->useAssignmentForEHBegin()) {
      MCSymbol *CurPos = OutContext.createTempSymbol();
      Streamer->emitLabel(CurPos);
      Streamer->emitAssignment(CurrentFnBegin,
                                  MCSymbolRefExpr::create(CurPos, OutContext));
    } else {
      Streamer->emitLabel(CurrentFnBegin);
    }
  }

  // Emit the prologue data.
  if (F->hasPrologueData())
    emitGlobalConstant(F->getParent()->getDataLayout(), F->getPrologueData());

  // Emit the function prologue data for the indirect call sanitizer.
  if (const MDNode *MD = F->getMetadata(LLVMContext::MD_func_sanitize)) {
    assert(MD->getNumOperands() == 2);

    auto *PrologueSig = mdconst::extract<Constant>(MD->getOperand(0));
    auto *FTRTTIProxy = mdconst::extract<Constant>(MD->getOperand(1));
    assert(PrologueSig && FTRTTIProxy);
    emitGlobalConstant(F->getParent()->getDataLayout(), PrologueSig);

    const MCExpr *Proxy = lowerConstant(FTRTTIProxy);
    const MCExpr *FnExp = MCSymbolRefExpr::create(CurrentFnSym, OutContext);
    const MCExpr *PCRel = MCBinaryExpr::createSub(Proxy, FnExp, OutContext);
    // Use 32 bit since only small code model is supported.
    Streamer->emitValue(PCRel, 4u);
  }
}

void CheerpWasmWriter::emitFunctionBodyStart(const Function* F) {
  SmallVector<MVT, 1> ResultVTs;
  SmallVector<MVT, 4> ParamVTs;
  computeSignatureVTs(F->getFunctionType(), F, *F, *TM, ParamVTs, ResultVTs);

  auto Signature = signatureFromMVTs(ResultVTs, ParamVTs);
  auto *WasmSym = cast<MCSymbolWasm>(CurrentFnSym);
  WasmSym->setSignature(Signature.get());
  addSignature(std::move(Signature));
  WasmSym->setType(wasm::WASM_SYMBOL_TYPE_FUNCTION);

//  static_cast<WebAssemblyTargetStreamer *>(Streamer->getTargetStreamer())->emitFunctionType(WasmSym);

//  // Emit the function index.
//  if (MDNode *Idx = F->getMetadata("wasm.index")) {
//    assert(Idx->getNumOperands() == 1);
//
//    static_cast<WebAssemblyTargetStreamer *>(Streamer->getTargetStreamer())->emitIndIdx(lowerConstant(
//        cast<ConstantAsMetadata>(Idx->getOperand(0))->getValue()));
//  }
//
//  SmallVector<wasm::ValType, 16> Locals;
//  valTypesFromMVTs(MFI->getLocals(), Locals);
//  static_cast<WebAssemblyTargetStreamer *>(Streamer->getTargetStreamer())->emitLocal(Locals);
}

void CheerpWasmWriter::emitExportFunction(const llvm::Function *F) {
  MCSection *currSection = TM->getObjFileLowering()->getUniqueSectionForFunction(*F, *TM);
  Streamer->switchSection(currSection);
  auto Sym = getSymbol(F);
// TODO: exported symbols need to be defined
  Streamer->emitSymbolAttribute(Sym, MCSA_Exported);
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

void CheerpWasmWriter::emitKCFITypeId(const Function* F){
  if (const MDNode *MD = F->getMetadata(LLVMContext::MD_kcfi_type))
    emitGlobalConstant(F->getParent()->getDataLayout(),
                       mdconst::extract<ConstantInt>(MD->getOperand(0)));
}

void CheerpWasmWriter::emitGlobalConstant(const DataLayout &DL, const Constant *CV) {
  if (MAI->hasSubsectionsViaSymbols()) {
    // If the global has zero size, emit a single byte so that two labels don't
    // look like they are at the same location.
    Streamer->emitIntValue(0, 1);
  }
}

void CheerpWasmWriter::EmitToStreamer(MCStreamer &S, const MCInst &Inst) {
  S.emitInstruction(Inst, *TM->getMCSubtargetInfo());
}

void CheerpWasmWriter::emitFunctionEntryLabel() {
  CurrentFnSym->redefineIfPossible();

  // The function label could have already been emitted if two symbols end up
  // conflicting due to asm renaming.  Detect this and emit an error.
  if (CurrentFnSym->isVariable())
    report_fatal_error("'" + Twine(CurrentFnSym->getName()) +
                       "' is a protected alias");

  Streamer->emitLabel(CurrentFnSym);
}

const MCExpr *CheerpWasmWriter::lowerConstant(const Constant *CV) {
  MCContext &Ctx = OutContext;

  if (CV->isNullValue() || isa<UndefValue>(CV))
    return MCConstantExpr::create(0, Ctx);

  if (const ConstantInt *CI = dyn_cast<ConstantInt>(CV))
    return MCConstantExpr::create(CI->getZExtValue(), Ctx);

  if (const GlobalValue *GV = dyn_cast<GlobalValue>(CV))
    return MCSymbolRefExpr::create(getSymbol(GV), Ctx);
///TODO get address label symbol
//  if (const BlockAddress *BA = dyn_cast<BlockAddress>(CV))
//    return MCSymbolRefExpr::create(GetBlockAddressSymbol(BA), Ctx);

  if (const auto *Equiv = dyn_cast<DSOLocalEquivalent>(CV))
    return TM->getObjFileLowering()->lowerDSOLocalEquivalent(Equiv, *TM);

  if (const NoCFIValue *NC = dyn_cast<NoCFIValue>(CV))
    return MCSymbolRefExpr::create(getSymbol(NC->getGlobalValue()), Ctx);

  const ConstantExpr *CE = dyn_cast<ConstantExpr>(CV);
  if (!CE) {
    llvm_unreachable("Unknown constant value to lower!");
  }

  // The constant expression opcodes are limited to those that are necessary
  // to represent relocations on supported targets. Expressions involving only
  // constant addresses are constant folded instead.
  switch (CE->getOpcode()) {
  default:
    break; // Error
  case Instruction::AddrSpaceCast: {
    const Constant *Op = CE->getOperand(0);
    unsigned DstAS = CE->getType()->getPointerAddressSpace();
    unsigned SrcAS = Op->getType()->getPointerAddressSpace();
    if (TM->isNoopAddrSpaceCast(SrcAS, DstAS))
      return lowerConstant(Op);

    break; // Error
  }
  case Instruction::GetElementPtr: {
    // Generate a symbolic expression for the byte address
    APInt OffsetAI(module.getDataLayout().getPointerTypeSizeInBits(CE->getType()), 0);
    cast<GEPOperator>(CE)->accumulateConstantOffset(module.getDataLayout(), OffsetAI);

    const MCExpr *Base = lowerConstant(CE->getOperand(0));
    if (!OffsetAI)
      return Base;

    int64_t Offset = OffsetAI.getSExtValue();
    return MCBinaryExpr::createAdd(Base, MCConstantExpr::create(Offset, Ctx),
                                   Ctx);
  }

  case Instruction::Trunc:
    // We emit the value and depend on the assembler to truncate the generated
    // expression properly.  This is important for differences between
    // blockaddress labels.  Since the two labels are in the same function, it
    // is reasonable to treat their delta as a 32-bit value.
    [[fallthrough]];
  case Instruction::BitCast:
    return lowerConstant(CE->getOperand(0));

  case Instruction::IntToPtr: {
    const DataLayout &DL = module.getDataLayout();

    // Handle casts to pointers by changing them into casts to the appropriate
    // integer type.  This promotes constant folding and simplifies this code.
    Constant *Op = CE->getOperand(0);
    Op = ConstantExpr::getIntegerCast(Op, DL.getIntPtrType(CV->getType()),
                                      false/*ZExt*/);
    return lowerConstant(Op);
  }

  case Instruction::PtrToInt: {
    const DataLayout &DL = module.getDataLayout();

    // Support only foldable casts to/from pointers that can be eliminated by
    // changing the pointer to the appropriately sized integer type.
    Constant *Op = CE->getOperand(0);
    Type *Ty = CE->getType();

    const MCExpr *OpExpr = lowerConstant(Op);

    // We can emit the pointer value into this slot if the slot is an
    // integer slot equal to the size of the pointer.
    //
    // If the pointer is larger than the resultant integer, then
    // as with Trunc just depend on the assembler to truncate it.
    if (DL.getTypeAllocSize(Ty).getFixedSize() <=
        DL.getTypeAllocSize(Op->getType()).getFixedSize())
      return OpExpr;

    break; // Error
  }

  case Instruction::Sub: {
    GlobalValue *LHSGV;
    APInt LHSOffset;
    DSOLocalEquivalent *DSOEquiv;
///TODO finish up or delete
//    if (IsConstantOffsetFromGlobal(CE->getOperand(0), LHSGV, LHSOffset,
//                                   module.getDataLayout(), &DSOEquiv)) {
//      GlobalValue *RHSGV;
//      APInt RHSOffset;
//      if (IsConstantOffsetFromGlobal(CE->getOperand(1), RHSGV, RHSOffset,
//                                     module.getDataLayout())) {
//        const MCExpr *RelocExpr =
//            TM->getObjFileLowering()->lowerRelativeReference(LHSGV, RHSGV, *TM);
//        if (!RelocExpr) {
//          const MCExpr *LHSExpr =
//              MCSymbolRefExpr::create(getSymbol(LHSGV), Ctx);
//          if (DSOEquiv &&
//              TM->getObjFileLowering()->supportDSOLocalEquivalentLowering())
//            LHSExpr =
//                TM->getObjFileLowering()->lowerDSOLocalEquivalent(DSOEquiv, TM);
//          RelocExpr = MCBinaryExpr::createSub(
//              LHSExpr, MCSymbolRefExpr::create(getSymbol(RHSGV), Ctx), Ctx);
//        }
//        int64_t Addend = (LHSOffset - RHSOffset).getSExtValue();
//        if (Addend != 0)
//          RelocExpr = MCBinaryExpr::createAdd(
//              RelocExpr, MCConstantExpr::create(Addend, Ctx), Ctx);
//        return RelocExpr;
//      }
//    }

    const MCExpr *LHS = lowerConstant(CE->getOperand(0));
    const MCExpr *RHS = lowerConstant(CE->getOperand(1));
    return MCBinaryExpr::createSub(LHS, RHS, Ctx);
    break;
  }

  case Instruction::Add: {
    const MCExpr *LHS = lowerConstant(CE->getOperand(0));
    const MCExpr *RHS = lowerConstant(CE->getOperand(1));
    return MCBinaryExpr::createAdd(LHS, RHS, Ctx);
  }
  }

//  // If the code isn't optimized, there may be outstanding folding
//  // opportunities. Attempt to fold the expression using DataLayout as a
//  // last resort before giving up.
//  Constant *C = ConstantFoldConstant(CE, module.getDataLayout());
//  if (C != CE)
//    return lowerConstant(C);

  // Otherwise report the problem to the user.
  std::string S;
  raw_string_ostream OS(S);
  OS << "Unsupported expression in static initializer: ";
  report_fatal_error(Twine(OS.str()));
}

MCSymbol *CheerpWasmWriter::getAddrLabel(const llvm::Function *F) {

}

MCSymbolWasm *CheerpWasmWriter::createDataSymbol(StringRef Name) const {
  auto *WasmSym = cast<MCSymbolWasm>(GetExternalSymbolSymbol(Name));
  if(WasmSym->getType()) return WasmSym;

  WasmSym->setType(wasm::WASM_SYMBOL_TYPE_DATA);
  return WasmSym;
}

MCSymbol *CheerpWasmWriter::GetExternalSymbolSymbol(llvm::StringRef Sym) const {
  SmallString<60> NameStr;
  NameStr = namegen.filterLLVMName(Sym, NameGenerator::GLOBAL);
  return OutContext.getOrCreateSymbol(NameStr);
}

}

//===-- CheerpBackend.cpp - Backend wrapper for CheerpWriter---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "CheerpTargetMachine.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Type.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/Cheerp/WasmWriter.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Cheerp/AllocaMerging.h"
#include "llvm/Cheerp/AllocaLowering.h"
#include "llvm/Cheerp/AllocateArrayLowering.h"
#include "llvm/Cheerp/FixIrreducibleControlFlow.h"
#include "llvm/Cheerp/IdenticalCodeFolding.h"
#include "llvm/Cheerp/ByValLowering.h"
#include "llvm/Cheerp/PointerPasses.h"
#include "llvm/Cheerp/GEPOptimizer.h"
#include "llvm/Cheerp/CFGPasses.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/SourceMaps.h"
#include "llvm/Cheerp/StructMemFuncLowering.h"
#include "llvm/Cheerp/ConstantExprLowering.h"
#include "llvm/Cheerp/StoreMerging.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Transforms/Scalar.h"

using namespace llvm;

extern "C" void LLVMInitializeCheerpBackendTarget() {
  // Register the target.
  RegisterTargetMachine<CheerpTargetMachine> X(TheCheerpBackendTarget);
}

namespace {
  class CheerpWritePass : public ModulePass {
  private:
    raw_ostream &Out;
    static char ID;
    void getAnalysisUsage(AnalysisUsage& AU) const;
  public:
    explicit CheerpWritePass(raw_ostream &o) :
      ModulePass(ID), Out(o) { }
    bool runOnModule(Module &M);
    StringRef getPassName() const {
	return "CheerpWritePass";
    }
  };
} // end anonymous namespace.

bool CheerpWritePass::runOnModule(Module& M)
{
  cheerp::PointerAnalyzer &PA = getAnalysis<cheerp::PointerAnalyzer>();
  cheerp::GlobalDepsAnalyzer &GDA = getAnalysis<cheerp::GlobalDepsAnalyzer>();
  cheerp::Registerize &registerize = getAnalysis<cheerp::Registerize>();
  cheerp::AllocaStoresExtractor &allocaStoresExtractor = getAnalysis<cheerp::AllocaStoresExtractor>();
  cheerp::LinearMemoryHelper &linearHelper = getAnalysis<cheerp::LinearMemoryHelper>();
  std::unique_ptr<cheerp::SourceMapGenerator> sourceMapGenerator;
  GDA.forceTypedArrays = ForceTypedArrays;
  if (!SourceMap.empty())
  {
    std::error_code ErrorCode;
    sourceMapGenerator.reset(new cheerp::SourceMapGenerator(SourceMap, SourceMapPrefix, SourceMapStandAlone, ErrorCode));
    if (ErrorCode)
    {
       // An error occurred opening the source map file, bail out
       llvm::report_fatal_error(ErrorCode.message(), false);
       return false;
    }
  }
  PA.fullResolve();
  PA.computeConstantOffsets(M);
  // Destroy the stores here, we need them to properly compute the pointer kinds, but we want to optimize them away before registerize
  allocaStoresExtractor.destroyStores();
  registerize.assignRegisters(M, PA);
#ifdef REGISTERIZE_STATS
  cheerp::reportRegisterizeStatistics();
#endif

  std::error_code ErrorCode;
  llvm::ToolOutputFile secondaryFile(SecondaryOutputFile, ErrorCode, sys::fs::F_None);
  std::unique_ptr<llvm::formatted_raw_ostream> secondaryOut;
  if (!SecondaryOutputFile.empty())
  {
    secondaryOut.reset(new formatted_raw_ostream(secondaryFile.os()));
  }
  else if (WasmOnly && LinearOutput != AsmJs)
  {
    secondaryOut.reset(new formatted_raw_ostream(Out));
  }

  // Build the ordered list of reserved names
  std::vector<std::string> reservedNames(ReservedNames.begin(), ReservedNames.end());
  std::sort(reservedNames.begin(), reservedNames.end());

  cheerp::NameGenerator namegen(M, GDA, registerize, PA, linearHelper, reservedNames, PrettyCode);

  std::string wasmFile;
  std::string asmjsMemFile;
  llvm::formatted_raw_ostream* memOut = nullptr;
  switch (LinearOutput)
  {
    case Wasm:
      if (!SecondaryOutputPath.empty())
        wasmFile = SecondaryOutputPath.getValue();
      else if (!SecondaryOutputFile.empty())
        wasmFile = llvm::sys::path::filename(SecondaryOutputFile.getValue());
      break;
    case AsmJs:
      if (!SecondaryOutputPath.empty())
        asmjsMemFile = SecondaryOutputPath.getValue();
      else if (!SecondaryOutputFile.empty())
        asmjsMemFile = llvm::sys::path::filename(SecondaryOutputFile.getValue());
      memOut = secondaryOut.get();
      break;
  }

  if (!WasmOnly)
  {
    cheerp::CheerpWriter writer(M, *this, Out, PA, registerize, GDA, linearHelper, namegen, allocaStoresExtractor, memOut, asmjsMemFile,
            sourceMapGenerator.get(), PrettyCode, MakeModule, !NoNativeJavaScriptMath,
            !NoJavaScriptMathImul, !NoJavaScriptMathFround, !NoCredits, MeasureTimeToMain, CheerpHeapSize,
            BoundsCheck, CfgLegacy, SymbolicGlobalsAsmJS, wasmFile, ForceTypedArrays);
    writer.makeJS();
  }

  if (LinearOutput != AsmJs && secondaryOut)
  {
    cheerp::CheerpWasmWriter wasmWriter(M, *this, *secondaryOut, PA, registerize, GDA, linearHelper, namegen,
                                    M.getContext(), CheerpHeapSize, !WasmOnly,
                                    PrettyCode, CfgLegacy, WasmSharedMemory,
                                    WasmExportedTable);
    wasmWriter.makeWasm();
  }
  if (!SecondaryOutputFile.empty() && ErrorCode)
  {
    // An error occurred opening the asm.js memory file, bail out
    llvm::report_fatal_error(ErrorCode.message(), false);
    return false;
  }
  if (!WasmOnly)
    secondaryFile.keep();
  return false;
}

void CheerpWritePass::getAnalysisUsage(AnalysisUsage& AU) const
{
  AU.addRequired<cheerp::GlobalDepsAnalyzer>();
  AU.addRequired<cheerp::PointerAnalyzer>();
  AU.addRequired<cheerp::Registerize>();
  AU.addRequired<cheerp::LinearMemoryHelper>();
  AU.addRequired<cheerp::AllocaStoresExtractor>();
  AU.addRequired<DominatorTreeWrapperPass>();
  AU.addRequired<LoopInfoWrapperPass>();
}

char CheerpWritePass::ID = 0;

//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool CheerpTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                           raw_pwrite_stream &o,
                                           raw_pwrite_stream *DwoOut,
                                           CodeGenFileType FileType,
                                           bool DisableVerify,
                                           MachineModuleInfoWrapperPass *MMIWP) {

  cheerp::GlobalDepsAnalyzer::MATH_MODE mathMode;
  if (NoNativeJavaScriptMath)
    mathMode = cheerp::GlobalDepsAnalyzer::NO_BUILTINS;
  else if(getTargetTriple().getEnvironment() == llvm::Triple::WebAssembly && LinearOutput != AsmJs)
    mathMode = cheerp::GlobalDepsAnalyzer::WASM_BUILTINS;
  else
    mathMode = cheerp::GlobalDepsAnalyzer::JS_BUILTINS;

  auto functionAddressMode = LinearOutput == LinearOutputTy::AsmJs
    ? cheerp::LinearMemoryHelper::FunctionAddressMode::AsmJS
    : cheerp::LinearMemoryHelper::FunctionAddressMode::Wasm;
  bool growMem = !WasmNoGrowMemory &&
                 functionAddressMode == cheerp::LinearMemoryHelper::FunctionAddressMode::Wasm &&
                 // NOTE: this is not actually required by the spec, but for now chrome
                 // doesn't like growing shared memory
                 !WasmSharedMemory;


  if (FixWrongFuncCasts)
    PM.add(createFixFunctionCastsPass());
  PM.add(createCheerpLowerSwitchPass(/*onlyLowerI64*/false));
  PM.add(createLowerAndOrBranchesPass());
  PM.add(createStructMemFuncLowering());
  PM.add(createFreeAndDeleteRemovalPass());
  PM.add(cheerp::createGlobalDepsAnalyzerPass(mathMode,/*resolveAliases*/true, WasmOnly));
  PM.add(createFixIrreducibleControlFlowPass());
  PM.add(createPointerArithmeticToArrayIndexingPass());
  PM.add(createPointerToImmutablePHIRemovalPass());
  PM.add(createGEPOptimizerPass());
  PM.add(cheerp::createStoreMergingPass(LinearOutput == Wasm));
  // Remove obviously dead instruction, this avoids problems caused by inlining of effectfull instructions
  // inside not used instructions which are then not rendered.
  PM.add(createDeadInstEliminationPass());
  PM.add(cheerp::createRegisterizePass(!NoJavaScriptMathFround, LinearOutput == Wasm));
  PM.add(createAllocaLoweringPass());
  if (!CheerpNoICF)
    PM.add(cheerp::createIdenticalCodeFoldingPass());

  PM.add(cheerp::createLinearMemoryHelperPass(functionAddressMode, CheerpHeapSize, CheerpStackSize, WasmOnly, growMem));
  PM.add(cheerp::createConstantExprLoweringPass());
  PM.add(cheerp::createPointerAnalyzerPass());
  PM.add(createDelayInstsPass());
  PM.add(cheerp::createAllocaMergingPass());
  PM.add(createAllocaArraysPass());
  PM.add(cheerp::createAllocaArraysMergingPass());
  PM.add(createRemoveFwdBlocksPass());
  // Keep this pass last, it is going to remove stores to memory from the LLVM visible code, so further optimizing afterwards will break
  PM.add(cheerp::createAllocaStoresExtractor());

  PM.add(new CheerpWritePass(o));
  return false;
}

const CheerpTargetLowering* CheerpSubtarget::getTargetLowering() const
{
  return &targetLowering;
}

const CheerpSubtarget* CheerpTargetMachine::getSubtargetImpl(const Function &F) const
{
  return &subTargetInfo;
}

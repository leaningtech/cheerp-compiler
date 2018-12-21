//===-- CheerpWasmBackend.cpp - Backend wrapper for CheerpWriter---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "CheerpWasmTargetMachine.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Cheerp/WasmWriter.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/Cheerp/PointerPasses.h"
#include "llvm/Cheerp/CFGPasses.h"
#include "llvm/Cheerp/AllocaMerging.h"
#include "llvm/Cheerp/AllocaLowering.h"
#include "llvm/Cheerp/AllocateArrayLowering.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/SourceMaps.h"
#include "llvm/Cheerp/IdenticalCodeFolding.h"
#include "llvm/Cheerp/FixIrreducibleControlFlow.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/StructMemFuncLowering.h"
#include "llvm/Cheerp/Utility.h"

using namespace llvm;

extern "C" void LLVMInitializeCheerpWasmBackendTarget() {
  // Register the target.
  RegisterTargetMachine<CheerpWastTargetMachine> X(TheCheerpWastBackendTarget);
  RegisterTargetMachine<CheerpWasmTargetMachine> Y(TheCheerpWasmBackendTarget);
}

namespace {
  class CheerpWasmWritePass : public ModulePass {
  private:
    raw_ostream &Out;
    cheerp::CheerpMode cheerpMode;
    static char ID;
    void getAnalysisUsage(AnalysisUsage& AU) const;
  public:
    explicit CheerpWasmWritePass(raw_ostream &o, cheerp::CheerpMode cheerpMode) :
      ModulePass(ID), Out(o), cheerpMode(cheerpMode) { }
    bool runOnModule(Module &M);
    const char *getPassName() const {
	return "CheerpWasmWritePass";
    }
  };
} // end anonymous namespace.

bool CheerpWasmWritePass::runOnModule(Module& M)
{
  cheerp::PointerAnalyzer &PA = getAnalysis<cheerp::PointerAnalyzer>();
  cheerp::GlobalDepsAnalyzer &GDA = getAnalysis<cheerp::GlobalDepsAnalyzer>();
  cheerp::Registerize &registerize = getAnalysis<cheerp::Registerize>();
  cheerp::AllocaStoresExtractor &allocaStoresExtractor = getAnalysis<cheerp::AllocaStoresExtractor>();
  cheerp::LinearMemoryHelper linearHelper(M, cheerp::LinearMemoryHelper::FunctionAddressMode::Wasm, GDA, CheerpHeapSize, 1);

  PA.fullResolve();
  PA.computeConstantOffsets(M);
  // Destroy the stores here, we need them to properly compute the pointer kinds, but we want to optimize them away before registerize
  allocaStoresExtractor.destroyStores();
  registerize.assignRegisters(M, PA);

  // Build the ordered list of reserved names
  std::vector<std::string> reservedNames(ReservedNames.begin(), ReservedNames.end());
  std::sort(reservedNames.begin(), reservedNames.end());

  if (WasmLoader.empty())
  {
    cheerp::NameGenerator namegen(M, GDA, registerize, PA, linearHelper, reservedNames, PrettyCode);
    cheerp::CheerpWasmWriter writer(M, *this, Out, PA, registerize, GDA, linearHelper, namegen,
                                    M.getContext(), CheerpHeapSize, !WasmLoader.empty(),
                                    PrettyCode, CfgStackifier, cheerpMode);
    writer.makeWasm();
  }
  else
  {
    cheerp::SourceMapGenerator* sourceMapGenerator = NULL;
    GDA.forceTypedArrays = ForceTypedArrays;
    if (!SourceMap.empty())
    {
      std::error_code ErrorCode;
      sourceMapGenerator = new cheerp::SourceMapGenerator(SourceMap, SourceMapPrefix, SourceMapStandAlone, ErrorCode);
      if (ErrorCode)
      {
         // An error occurred opening the source map file, bail out
         delete sourceMapGenerator;
         llvm::report_fatal_error(ErrorCode.message(), false);
         return false;
      }
    }

    std::error_code ErrorCode;
    llvm::tool_output_file jsFile(WasmLoader.c_str(), ErrorCode, sys::fs::F_None);
    llvm::formatted_raw_ostream jsOut(jsFile.os());

    cheerp::NameGenerator namegen(M, GDA, registerize, PA, linearHelper, reservedNames, PrettyCode);
    cheerp::CheerpWasmWriter wasmWriter(M, *this, Out, PA, registerize, GDA, linearHelper, namegen,
                                    M.getContext(), CheerpHeapSize, !WasmLoader.empty(),
                                    PrettyCode, CfgStackifier, cheerpMode);
    wasmWriter.makeWasm();

    cheerp::CheerpWriter writer(M, *this, jsOut, PA, registerize, GDA, linearHelper, namegen, allocaStoresExtractor, nullptr, std::string(),
            sourceMapGenerator, PrettyCode, MakeModule, NoRegisterize, !NoNativeJavaScriptMath,
            !NoJavaScriptMathImul, !NoJavaScriptMathFround, !NoCredits, MeasureTimeToMain, CheerpHeapSize,
            BoundsCheck, CfgStackifier, SymbolicGlobalsAsmJS, WasmFile, ForceTypedArrays);
    writer.makeJS();
    if (ErrorCode)
    {
       // An error occurred opening the wasm loader file, bail out
       llvm::report_fatal_error(ErrorCode.message(), false);
       delete sourceMapGenerator;
       return false;
    }
    jsFile.keep();
    delete sourceMapGenerator;
  }
  return false;
}

void CheerpWasmWritePass::getAnalysisUsage(AnalysisUsage& AU) const
{
  AU.addRequired<cheerp::GlobalDepsAnalyzer>();
  AU.addRequired<cheerp::PointerAnalyzer>();
  AU.addRequired<cheerp::Registerize>();
  AU.addRequired<cheerp::AllocaStoresExtractor>();
  AU.addRequired<DominatorTreeWrapperPass>();
  AU.addRequired<LoopInfoWrapperPass>();
}

char CheerpWasmWritePass::ID = 0;

//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool CheerpBaseTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                           raw_pwrite_stream &o,
                                           CodeGenFileType FileType,
                                           bool DisableVerify,
                                           AnalysisID StartBefore,
                                           AnalysisID StartAfter,
                                           AnalysisID StopAfter,
                                           MachineFunctionInitializer* MFInit) {
  PM.add(createStructMemFuncLowering());
  PM.add(createAllocaLoweringPass());
  PM.add(createFreeAndDeleteRemovalPass());
  PM.add(cheerp::createGlobalDepsAnalyzerPass(cheerp::GlobalDepsAnalyzer::NO_BUILTINS, /*resolveAliases*/true));
  PM.add(createFixIrreducibleControlFlowPass());
  if (!CheerpNoICF)
    PM.add(cheerp::createIdenticalCodeFoldingPass());
  PM.add(createPointerArithmeticToArrayIndexingPass());
  PM.add(createPointerToImmutablePHIRemovalPass());
  PM.add(createGEPOptimizerPass());
  PM.add(cheerp::createRegisterizePass(true, false));
  PM.add(cheerp::createPointerAnalyzerPass());
  PM.add(createDelayInstsPass());
  PM.add(cheerp::createAllocaMergingPass());
  PM.add(createIndirectCallOptimizerPass());
  PM.add(createAllocaArraysPass());
  PM.add(cheerp::createAllocaArraysMergingPass());
  PM.add(createRemoveFwdBlocksPass());
  // Keep this pass last, it is going to remove stores to memory from the LLVM visible code, so further optimizing afterwards will break
  PM.add(cheerp::createAllocaStoresExtractor());
  PM.add(createCheerpWritePass(o));
  return false;
}

ModulePass* CheerpWastTargetMachine::createCheerpWritePass(raw_ostream& o)
{
	return new CheerpWasmWritePass(o, cheerp::CHEERP_MODE_WAST);
}

ModulePass* CheerpWasmTargetMachine::createCheerpWritePass(raw_ostream& o)
{
	return new CheerpWasmWritePass(o, cheerp::CHEERP_MODE_WASM);
}

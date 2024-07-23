//===-- CheerpWritePass.cpp - Pass writer for CheerpWriter ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2011-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Cheerp/BitCastLowering.h"
#include "llvm/Cheerp/DTSWriter.h"
#include "llvm/Cheerp/I64Lowering.h"
#include "llvm/Cheerp/JSStringLiteralLowering.h"
#include "llvm/Cheerp/FinalizeMemoryInfo.h"
#include "llvm/Cheerp/PassRegistry.h"
#include "llvm/Cheerp/PassUtility.h"
#include "llvm/Cheerp/SIMDLowering.h"
#include "llvm/Cheerp/SIMDTransform.h"
#include "llvm/Cheerp/WasmWriter.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/PassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Transforms/Scalar/DCE.h"
#include "llvm/Transforms/Scalar/EarlyCSE.h"

#include "CheerpWritePass.h"
using namespace llvm;

cl::opt<bool> VerbosePassManager("cheerp-verbose-pm", cl::init(false), cl::Hidden,
                                        cl::desc("Emit verbose informations"));

PreservedAnalyses cheerp::CheerpWritePassImpl::run(Module& M, ModuleAnalysisManager& MAM)
{
  cheerp::Registerize &registerize = MAM.getResult<cheerp::RegisterizeAnalysis>(M);
  cheerp::GlobalDepsAnalyzer &GDA = MAM.getResult<cheerp::GlobalDepsAnalysis>(M);
  cheerp::PointerAnalyzer &PA = MAM.getResult<cheerp::PointerAnalysis>(M);
  cheerp::InvokeWrapping &IW = MAM.getResult<cheerp::InvokeWrappingAnalysis>(M);
  cheerp::AllocaStoresExtractor &allocaStoresExtractor = MAM.getResult<cheerp::AllocaStoresExtractorAnalysis>(M);
  cheerp::LinearMemoryHelper &linearHelper = MAM.getResult<LinearMemoryAnalysis>(M);
  std::unique_ptr<cheerp::SourceMapGenerator> sourceMapGenerator;
  GDA.forceTypedArrays = ForceTypedArrays;
  if (!SourceMap.empty())
  {
    std::error_code ErrorCode;
    sourceMapGenerator.reset(new cheerp::SourceMapGenerator(SourceMap, SourceMapPrefix, SourceMapStandAlone, ErrorCode));
    if (ErrorCode)
    {
      // An error occurred opening the source map file, bail out
      llvm::report_fatal_error(StringRef(ErrorCode.message()), false);
      return PreservedAnalyses::none();
    }
  }

  Triple TargetTriple(M.getTargetTriple());
  bool WasmOnly = TargetTriple.getOS() == Triple::WASI;
  std::error_code ErrorCode;
  llvm::ToolOutputFile secondaryFile(SecondaryOutputFile, ErrorCode, sys::fs::OF_None);
  std::unique_ptr<llvm::formatted_raw_ostream> secondaryOut;
  if (!SecondaryOutputFile.empty())
  {
    secondaryOut.reset(new formatted_raw_ostream(secondaryFile.os()));
  }
  else if (WasmOnly && LinearOutput != AsmJs)
  {
    secondaryOut.reset(new formatted_raw_ostream(Out));
  }

  std::error_code dtsErrorCode;
  llvm::ToolOutputFile dtsFile(DTSOutputFile, dtsErrorCode, sys::fs::OF_None);
  std::unique_ptr<llvm::formatted_raw_ostream> dtsOut;
  if (!DTSOutputFile.empty())
  {
    dtsOut.reset(new formatted_raw_ostream(dtsFile.os()));
  }

  // Build the ordered list of reserved names
  std::vector<std::string> reservedNames(ReservedNames.begin(), ReservedNames.end());
  std::sort(reservedNames.begin(), reservedNames.end());

  cheerp::NameGenerator namegen(M, GDA, registerize, PA, linearHelper, reservedNames, PrettyCode, WasmExportedMemory);

  std::string wasmFile;
  std::string asmjsMemFile;
  llvm::formatted_raw_ostream* memOut = nullptr;
  switch (LinearOutput)
  {
  case Wasm:
    if (!SecondaryOutputPath.empty())
      wasmFile = SecondaryOutputPath.getValue();
    else if (!SecondaryOutputFile.empty())
      wasmFile = std::string(llvm::sys::path::filename(SecondaryOutputFile.getValue()));
    break;
  case AsmJs:
    if (!SecondaryOutputPath.empty())
      asmjsMemFile = SecondaryOutputPath.getValue();
    else if (!SecondaryOutputFile.empty())
      asmjsMemFile = std::string(llvm::sys::path::filename(SecondaryOutputFile.getValue()));
    memOut = secondaryOut.get();
    break;
  }

  MODULE_TYPE makeModule = getModuleType(MakeModule);

  if (MakeDTS && dtsOut)
  {
    cheerp::CheerpDTSWriter dtsWriter(M, *dtsOut, sourceMapGenerator.get(), PrettyCode, makeModule);
    dtsWriter.makeDTS();
  }

  if (!WasmOnly)
  {
    cheerp::CheerpWriter writer(M, MAM, Out, PA, registerize, GDA, linearHelper, namegen, allocaStoresExtractor, IW.getLandingPadTable(), memOut, asmjsMemFile,
                                sourceMapGenerator.get(), PrettyCode, makeModule, !NoNativeJavaScriptMath,
                                !NoJavaScriptMathImul, !NoJavaScriptMathFround, !NoCredits, MeasureTimeToMain, CheerpHeapSize,
                                BoundsCheck, SymbolicGlobalsAsmJS, wasmFile, ForceTypedArrays);
    writer.makeJS();
  }

  if (LinearOutput != AsmJs && secondaryOut)
  {
    cheerp::CheerpWasmWriter wasmWriter(M, MAM, *secondaryOut, PA, registerize, GDA, linearHelper, IW.getLandingPadTable(), namegen,
                                        M.getContext(), CheerpHeapSize, !WasmOnly,
                                        PrettyCode, WasmSharedMemory,
                                        WasmExportedTable);
    wasmWriter.makeWasm();
  }

  allocaStoresExtractor.destroyStores();

  if (!SecondaryOutputFile.empty() && ErrorCode)
  {
    // An error occurred opening the asm.js memory file, bail out
    llvm::report_fatal_error(StringRef(ErrorCode.message()), false);
    return PreservedAnalyses::none();
  }
  if (!DTSOutputFile.empty() && dtsErrorCode)
  {
    llvm::report_fatal_error(StringRef(dtsErrorCode.message()), false);
    return PreservedAnalyses::none();
  }
  if (!WasmOnly)
    secondaryFile.keep();
  if (MakeDTS)
    dtsFile.keep();


  return PreservedAnalyses::none();
}

bool CheerpWritePass::runOnModule(Module& M)
{
  LoopAnalysisManager LAM;
  FunctionAnalysisManager FAM;
  CGSCCAnalysisManager CGAM;
  ModuleAnalysisManager MAM;

  PassInstrumentationCallbacks PIC;
  PrintPassOptions PrintPassOpts;
  PrintPassOpts.Indent = VerbosePassManager;
  PrintPassOpts.SkipAnalyses = false;
  StandardInstrumentations SI(M.getContext(), VerbosePassManager,
                              /*VerifyEach*/ false, PrintPassOpts);
  SI.registerCallbacks(PIC, &FAM);

  llvm::PipelineTuningOptions PTO;
  Optional<PGOOptions> PGOOpt;
  PassBuilder PB(TM, PTO, PGOOpt, &PIC);

#define HANDLE_EXTENSION(Ext)                                                  \
  get##Ext##PluginInfo().RegisterPassBuilderCallbacks(PB);
#include "llvm/Support/Extension.def"

  Triple TargetTriple(M.getTargetTriple());
  std::unique_ptr<TargetLibraryInfoImpl> TLII(
      new TargetLibraryInfoImpl(TargetTriple));
  FAM.registerPass([&] { return TargetLibraryAnalysis(*TLII); });

  // Register all the basic analyses with the managers.
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  ModulePassManager MPM;
  bool isWasmTarget = Triple(M.getTargetTriple()).isCheerpWasm();
  cheerp::GlobalDepsAnalyzer::MATH_MODE mathMode;
  if (NoNativeJavaScriptMath)
    mathMode = cheerp::GlobalDepsAnalyzer::NO_BUILTINS;
  else if(isWasmTarget && LinearOutput != AsmJs)

    mathMode = cheerp::GlobalDepsAnalyzer::WASM_BUILTINS;
  else
    mathMode = cheerp::GlobalDepsAnalyzer::JS_BUILTINS;

  auto functionAddressMode = LinearOutput == LinearOutputTy::AsmJs
                                 ? cheerp::LinearMemoryHelperInitializer::FunctionAddressMode::AsmJS
                                 : cheerp::LinearMemoryHelperInitializer::FunctionAddressMode::Wasm;
  bool growMem = !WasmNoGrowMemory &&
                 functionAddressMode == cheerp::LinearMemoryHelperInitializer::FunctionAddressMode::Wasm &&
                 // NOTE: this is not actually required by the spec, but for now chrome
                 // doesn't like growing shared memory
                 !WasmSharedMemory;
  bool hasAsmjsMem = functionAddressMode == cheerp::LinearMemoryHelperInitializer::FunctionAddressMode::AsmJS &&
                     (!SecondaryOutputFile.empty() || !SecondaryOutputPath.empty());

  if (FixWrongFuncCasts)
    MPM.addPass(cheerp::FixFunctionCastsPass());

  {
    //Wrap these in a FunctionPassManager
    FunctionPassManager FPM;

    FPM.addPass(cheerp::I64LoweringPass());
    // Run a simple constant elimination pass to clean up suboptimal code left
    // by I64Lowering.
    FPM.addPass(EarlyCSEPass());
    FPM.addPass(cheerp::JSStringLiteralLoweringPass());
    FPM.addPass(cheerp::BitCastLoweringPass());
    FPM.addPass(cheerp::SIMDTransformPass());
    FPM.addPass(cheerp::SIMDLoweringPass());
    FPM.addPass(cheerp::CheerpLowerSwitchPass(/*onlyLowerI64*/false));
    FPM.addPass(cheerp::LowerAndOrBranchesPass());
    FPM.addPass(cheerp::StructMemFuncLoweringPass());

    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
  }

  MPM.addPass(cheerp::FreeAndDeleteRemovalPass());
  MPM.addPass(cheerp::GlobalDepsAnalyzerPass(mathMode, /*resolveAliases*/true));
  MPM.addPass(cheerp::InvokeWrappingPass());
  if (isWasmTarget)
    MPM.addPass(cheerp::AllocaLoweringPass());
  MPM.addPass(cheerp::FFIWrappingPass());
  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::FixIrreducibleControlFlowPass()));
  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::PointerArithmeticToArrayIndexingPass()));
  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::PointerToImmutablePHIRemovalPass()));
  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::GEPOptimizerPass()));
  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::StoreMergingPass(LinearOutput == Wasm && !WasmNoUnalignedMem)));
  // Remove obviously dead instruction, this avoids problems caused by inlining of effectfull instructions
  // inside not used instructions which are then not rendered.
  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::PreserveCheerpAnalysisPassWrapper<DCEPass, Function, FunctionAnalysisManager>()));
  MPM.addPass(cheerp::RegisterizePass(!NoJavaScriptMathFround, LinearOutput == Wasm));
  MPM.addPass(cheerp::LinearMemoryHelperPass(cheerp::LinearMemoryHelperInitializer({functionAddressMode, CheerpHeapSize, CheerpStackSize, CheerpStackOffset, growMem, hasAsmjsMem})));
  MPM.addPass(cheerp::ConstantExprLoweringPass());
  MPM.addPass(cheerp::PointerAnalyzerPass());
  MPM.addPass(cheerp::DelayInstsPass());

  MPM.addPass(cheerp::AllocaMergingPass());
  MPM.addPass(cheerp::AllocaArraysPass());
  MPM.addPass(cheerp::AllocaArraysMergingPass());

  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::RemoveFwdBlocksPass()));
  // Keep this pass last, it is going to remove stores to memory from the LLVM visible code, so further optimizing afterwards will break
  MPM.addPass(cheerp::AllocaStoresExtractorPass());

  // This pass needs to be ran after the AllocaStoresExtractorPass and does not do any optimizing.
  MPM.addPass(cheerp::FinalizeMemoryInfoPass());

  MPM.addPass(cheerp::CheerpWritePassImpl(Out, TM));

  // Now that we have all the passes ready, run them.
  {
    PrettyStackTraceString CrashInfo("Optimizer");
    llvm::TimeTraceScope TimeScope("Optimizer");
    MPM.run(M, MAM);
  }

  return false;
}

char CheerpWritePass::ID = 0;

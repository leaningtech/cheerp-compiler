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
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Type.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/Cheerp/WasmWriter.h"
#include "llvm/Cheerp/PassRegistry.h"
#include "llvm/Cheerp/PassUtility.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Pass.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Transforms/Scalar/DCE.h"
using namespace llvm;

static cl::opt<bool> VerbosePassManager("cheerp-verbose-pm", cl::init(false), cl::Hidden,
                               cl::desc("Emit verbose informations"));

extern "C" void LLVMInitializeCheerpBackendTarget() {
  // Register the target.
  RegisterTargetMachine<CheerpTargetMachine> X(TheCheerpBackendTarget);
}

namespace {
  class CheerpWritePass : public ModulePass {
  private:
    raw_ostream &Out;
    static char ID;
    void getAnalysisUsage(AnalysisUsage& AU) const override;
    TargetMachine* TM;
  public:
    explicit CheerpWritePass(raw_ostream &o, TargetMachine* TM) :
      ModulePass(ID), Out(o), TM(TM) { }
    bool runOnModule(Module &M) override;
    StringRef getPassName() const override {
	return "CheerpWritePass";
    }
  };
} // end anonymous namespace.

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
  StandardInstrumentations SI(VerbosePassManager,
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
   
  cheerp::GlobalDepsAnalyzer::MATH_MODE mathMode;
  if (NoNativeJavaScriptMath)
    mathMode = cheerp::GlobalDepsAnalyzer::NO_BUILTINS;
  else if(Triple(M.getTargetTriple()).getEnvironment() == llvm::Triple::WebAssembly && LinearOutput != AsmJs)

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

  if (FixWrongFuncCasts)
    MPM.addPass(cheerp::FixFunctionCastsPass());
  
  {
    //Wrap these in a FunctionPassManager
    FunctionPassManager FPM;

    FPM.addPass(cheerp::CheerpLowerSwitchPass(/*onlyLowerI64*/false));
    FPM.addPass(cheerp::LowerAndOrBranchesPass());
    FPM.addPass(cheerp::StructMemFuncLoweringPass());

    MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
  }

  MPM.addPass(cheerp::FreeAndDeleteRemovalPass());
  MPM.addPass(cheerp::GlobalDepsAnalyzerPass(mathMode, /*resolveAliases*/true, WasmOnly));
  MPM.addPass(cheerp::AllocaLoweringPass());
  if (!CheerpNoICF)
    MPM.addPass(cheerp::IdenticalCodeFoldingPass());
  MPM.addPass(cheerp::InvokeWrappingPass());
  MPM.addPass(cheerp::FFIWrappingPass());
  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::FixIrreducibleControlFlowPass()));
  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::PointerArithmeticToArrayIndexingPass()));
  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::PointerToImmutablePHIRemovalPass()));
  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::GEPOptimizerPass()));
  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::StoreMergingPass(LinearOutput == Wasm)));
  // Remove obviously dead instruction, this avoids problems caused by inlining of effectfull instructions
  // inside not used instructions which are then not rendered.
  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::PreserveCheerpAnalysisPassWrapper<DCEPass, Function, FunctionAnalysisManager>()));
  MPM.addPass(cheerp::RegisterizePass(!NoJavaScriptMathFround, LinearOutput == Wasm));
  MPM.addPass(cheerp::LinearMemoryHelperPass(cheerp::LinearMemoryHelperInitializer({functionAddressMode, CheerpHeapSize, CheerpStackSize, WasmOnly, growMem})));
  MPM.addPass(cheerp::ConstantExprLoweringPass());
  MPM.addPass(cheerp::PointerAnalyzerPass());
  MPM.addPass(cheerp::DelayInstsPass());

  MPM.addPass(cheerp::AllocaMergingPass());
  MPM.addPass(cheerp::AllocaArraysPass());
  MPM.addPass(cheerp::AllocaArraysMergingPass());

  MPM.addPass(createModuleToFunctionPassAdaptor(cheerp::RemoveFwdBlocksPass()));
  // Keep this pass last, it is going to remove stores to memory from the LLVM visible code, so further optimizing afterwards will break
  MPM.addPass(cheerp::AllocaStoresExtractorPass());

  MPM.addPass(cheerp::CheerpWritePassImpl(Out, TM));

  // Now that we have all of the passes ready, run them.
  {
    PrettyStackTraceString CrashInfo("Optimizer");
    llvm::TimeTraceScope TimeScope("Optimizer");
    MPM.run(M, MAM);
  }

  return false;
}

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
  PA.fullResolve();
  PA.computeConstantOffsets(M);
  // Destroy the stores here, we need them to properly compute the pointer kinds, but we want to optimize them away before registerize
  allocaStoresExtractor.unlinkStores();

  registerize.assignRegisters(M, PA);
#ifdef REGISTERIZE_STATS
  cheerp::reportRegisterizeStatistics();
#endif

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

  if (!WasmOnly)
  {
    cheerp::CheerpWriter writer(M, MAM, Out, PA, registerize, GDA, linearHelper, namegen, allocaStoresExtractor, IW.getLandingPadTable(), memOut, asmjsMemFile,
            sourceMapGenerator.get(), PrettyCode, MakeModule, !NoNativeJavaScriptMath,
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
  if (!WasmOnly)
    secondaryFile.keep();



	return PreservedAnalyses::none();
}

void CheerpWritePass::getAnalysisUsage(AnalysisUsage& AU) const
{
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


  PM.add(new CheerpWritePass(o, (TargetMachine*)this));
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

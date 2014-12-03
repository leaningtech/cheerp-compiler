//===-- CheerpBackend.cpp - Backend wrapper for CheerpWriter---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "CheerpTargetMachine.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/PassManager.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Type.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/Cheerp/AllocaMerging.h"
#include "llvm/Cheerp/PointerPasses.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/ResolveAliases.h"
#include "llvm/Cheerp/SourceMaps.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

static cl::opt<std::string> SourceMap("cheerp-sourcemap", cl::Optional,
  cl::desc("If specified, the file name of the source map"), cl::value_desc("filename"));

static cl::opt<std::string> SourceMapPrefix("cheerp-sourcemap-prefix", cl::Optional,
  cl::desc("If specified, this prefix will be removed from source map file paths"), cl::value_desc("path"));

static cl::opt<bool> PrettyCode("cheerp-pretty-code", cl::desc("Generate human-readable JS") );

static cl::opt<bool> NoRegisterize("cheerp-no-registerize", cl::desc("Disable registerize pass") );

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
    const char *getPassName() const {
	return "CheerpWritePass";
    }
  };
} // end anonymous namespace.

bool CheerpWritePass::runOnModule(Module& M)
{
  cheerp::PointerAnalyzer &PA = getAnalysis<cheerp::PointerAnalyzer>();
  cheerp::GlobalDepsAnalyzer &GDA = getAnalysis<cheerp::GlobalDepsAnalyzer>();
  cheerp::Registerize &registerize = getAnalysis<cheerp::Registerize>();
  cheerp::SourceMapGenerator* sourceMapGenerator = NULL;
  if (!SourceMap.empty())
  {
    std::error_code ErrorCode;
    sourceMapGenerator = new cheerp::SourceMapGenerator(SourceMap, SourceMapPrefix, M.getContext(), ErrorCode);
    if (ErrorCode)
    {
       // An error occurred opening the source map file, bail out
       delete sourceMapGenerator;
       llvm::report_fatal_error(ErrorCode.message(), false);
       return false;
    }
  }
  cheerp::CheerpWriter writer(M, Out, PA, registerize, GDA, sourceMapGenerator, PrettyCode, NoRegisterize);
  writer.makeJS();
  delete sourceMapGenerator;
  return false;
}

void CheerpWritePass::getAnalysisUsage(AnalysisUsage& AU) const
{
  AU.addRequired<cheerp::GlobalDepsAnalyzer>();
  AU.addRequired<cheerp::PointerAnalyzer>();
  AU.addRequired<cheerp::Registerize>();
}

char CheerpWritePass::ID = 0;

//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool CheerpTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                           raw_pwrite_stream &o,
                                           CodeGenFileType FileType,
                                           bool DisableVerify,
                                           AnalysisID StartBefore,
                                           AnalysisID StartAfter,
                                           AnalysisID StopAfter,
                                           MachineFunctionInitializer* MFInit) {
  if (FileType != TargetMachine::CGFT_AssemblyFile) return true;
  PM.add(createResolveAliasesPass());
  PM.add(cheerp::createGlobalDepsAnalyzerPass());
  PM.add(createPointerArithmeticToArrayIndexingPass());
  PM.add(createPointerToImmutablePHIRemovalPass());
  PM.add(cheerp::createPointerAnalyzerPass());
  PM.add(createIndirectCallOptimizerPass());
  PM.add(cheerp::createRegisterizePass(NoRegisterize));
  PM.add(createAllocaMergingPass());
  PM.add(createAllocaArraysPass());
  PM.add(createAllocaArraysMergingPass());
  PM.add(new CheerpWritePass(o));
  return false;
}

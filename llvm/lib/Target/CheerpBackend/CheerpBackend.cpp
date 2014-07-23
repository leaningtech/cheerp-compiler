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
#include "llvm/Cheerp/ResolveAliases.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ToolOutputFile.h"

using namespace llvm;

static cl::opt<std::string> SourceMap("cheerp-sourcemap", cl::Optional,
  cl::desc("If specified, the file name of the source map"), cl::value_desc("filename"));

static cl::opt<bool> PrettyCode("cheerp-pretty-code", cl::desc("Generate human-readable JS") );

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
  };
} // end anonymous namespace.

bool CheerpWritePass::runOnModule(Module& M)
{
  AliasAnalysis &AA = getAnalysis<AliasAnalysis>();
  if (!SourceMap.empty())
  {
    std::error_code ErrorString;
    tool_output_file sourceMap(SourceMap.c_str(), ErrorString, sys::fs::F_None);
    if (ErrorString)
    {
       // An error occurred opening the source map file, bail out
       llvm::report_fatal_error(ErrorString.message(), false);
       return false;
    }
    cheerp::CheerpWriter writer(M, Out, AA, SourceMap, &sourceMap.os(), PrettyCode);
    sourceMap.keep();
    writer.makeJS();
  }
  else
  {
    cheerp::CheerpWriter writer(M, Out, AA, SourceMap, NULL, PrettyCode);
    writer.makeJS();
  }

  return false;
}

void CheerpWritePass::getAnalysisUsage(AnalysisUsage& AU) const
{
  AU.addRequired<AliasAnalysis>();
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
  PM.add(createAllocaMergingPass());
  PM.add(createIndirectCallOptimizerPass());
  PM.add(createAllocaArraysPass());
  PM.add(new CheerpWritePass(o));
  return false;
}

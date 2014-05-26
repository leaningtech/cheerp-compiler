//===-- DuettoBackend.cpp - Backend wrapper for DuettoWriter---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "DuettoTargetMachine.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/PassManager.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Type.h"
#include "llvm/Duetto/Writer.h"
#include "llvm/Duetto/AllocaMerging.h"
#include "llvm/Duetto/ResolveAliases.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ToolOutputFile.h"

using namespace llvm;

static cl::opt<std::string> SourceMap("duetto-sourcemap", cl::Optional,
  cl::desc("If specified, the file name of the source map"), cl::value_desc("filename"));


extern "C" void LLVMInitializeDuettoBackendTarget() {
  // Register the target.
  RegisterTargetMachine<DuettoTargetMachine> X(TheDuettoBackendTarget);
}

namespace {
  class DuettoWritePass : public ModulePass {
  private:
    formatted_raw_ostream &Out;
    static char ID;
    void getAnalysisUsage(AnalysisUsage& AU) const;
  public:
    explicit DuettoWritePass(formatted_raw_ostream &o) :
      ModulePass(ID), Out(o) { }
    bool runOnModule(Module &M);
  };
} // end anonymous namespace.

bool DuettoWritePass::runOnModule(Module& M)
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
    duetto::DuettoWriter writer(M, Out, AA, SourceMap, &sourceMap.os());
    sourceMap.keep();
    writer.makeJS();
  }
  else
  {
    duetto::DuettoWriter writer(M, Out, AA, SourceMap, NULL);
    writer.makeJS();
  }

  return false;
}

void DuettoWritePass::getAnalysisUsage(AnalysisUsage& AU) const
{
  AU.addRequired<AliasAnalysis>();
}

char DuettoWritePass::ID = 0;

//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool DuettoTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                           formatted_raw_ostream &o,
                                           CodeGenFileType FileType,
                                           bool DisableVerify,
                                           AnalysisID StartAfter,
                                           AnalysisID StopAfter) {
  if (FileType != TargetMachine::CGFT_AssemblyFile) return true;
  PM.add(createResolveAliasesPass());
  PM.add(createAllocaMergingPass());
  PM.add(new DuettoWritePass(o));
  return false;
}

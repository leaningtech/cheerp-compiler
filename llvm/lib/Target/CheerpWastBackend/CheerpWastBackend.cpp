//===-- CheerpWastBackend.cpp - Backend wrapper for CheerpWriter---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "CheerpWastTargetMachine.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/PassManager.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Type.h"
#include "llvm/Cheerp/WastWriter.h"
#include "llvm/Cheerp/AllocaMerging.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/ResolveAliases.h"
#include "llvm/Cheerp/SourceMaps.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

extern "C" void LLVMInitializeCheerpWastBackendTarget() {
  // Register the target.
  RegisterTargetMachine<CheerpWastTargetMachine> X(TheCheerpWastBackendTarget);
}

namespace {
  class CheerpWastWritePass : public ModulePass {
  private:
    formatted_raw_ostream &Out;
    static char ID;
    void getAnalysisUsage(AnalysisUsage& AU) const;
  public:
    explicit CheerpWastWritePass(formatted_raw_ostream &o) :
      ModulePass(ID), Out(o) { }
    bool runOnModule(Module &M);
    const char *getPassName() const {
	return "CheerpWastWritePass";
    }
  };
} // end anonymous namespace.

bool CheerpWastWritePass::runOnModule(Module& M)
{
  cheerp::PointerAnalyzer &PA = getAnalysis<cheerp::PointerAnalyzer>();
  cheerp::GlobalDepsAnalyzer &GDA = getAnalysis<cheerp::GlobalDepsAnalyzer>();
  cheerp::Registerize &registerize = getAnalysis<cheerp::Registerize>();
  PA.fullResolve();
  PA.computeConstantOffsets(M);
  registerize.assignRegisters(M, PA);
  cheerp::CheerpWastWriter writer(M, Out, PA, registerize, GDA);
  writer.makeWast();
  return false;
}

void CheerpWastWritePass::getAnalysisUsage(AnalysisUsage& AU) const
{
  AU.addRequired<cheerp::GlobalDepsAnalyzer>();
  AU.addRequired<cheerp::PointerAnalyzer>();
  AU.addRequired<cheerp::Registerize>();
}

char CheerpWastWritePass::ID = 0;

//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool CheerpWastTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                           formatted_raw_ostream &o,
                                           CodeGenFileType FileType,
                                           bool DisableVerify,
                                           AnalysisID StartAfter,
                                           AnalysisID StopAfter) {
  if (FileType != TargetMachine::CGFT_AssemblyFile) return true;
#if 0
  PM.add(createResolveAliasesPass());
  PM.add(createFreeAndDeleteRemovalPass());
#endif
  PM.add(cheerp::createGlobalDepsAnalyzerPass());
#if 0
  PM.add(createPointerArithmeticToArrayIndexingPass());
  PM.add(createPointerToImmutablePHIRemovalPass());
#endif
  PM.add(cheerp::createRegisterizePass(true, false));
  PM.add(cheerp::createPointerAnalyzerPass());
#if 0
  PM.add(cheerp::createAllocaMergingPass());
  PM.add(createIndirectCallOptimizerPass());
  PM.add(createAllocaArraysPass());
  PM.add(cheerp::createAllocaArraysMergingPass());
  PM.add(createDelayAllocasPass());
#endif
  PM.add(new CheerpWastWritePass(o));
  return false;
}

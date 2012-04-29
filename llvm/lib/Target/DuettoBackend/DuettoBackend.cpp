//===-- DuettoBackend.cpp - Backend wrapper for DuettoWriter---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technlogies
//===----------------------------------------------------------------------===//

#include "DuettoTargetMachine.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/PassManager.h"
#include "llvm/IR/CallingConv.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/Pass.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Config/config.h"
#include <algorithm>
#include <cstdio>
#include <iostream>

using namespace llvm;

enum WhatToGenerate {
  GenProgram,
  GenModule,
  GenContents,
  GenFunction,
  GenFunctions,
  GenInline,
  GenVariable,
  GenType
};

extern "C" void LLVMInitializeDuettoBackendTarget() {
  // Register the target.
  RegisterTargetMachine<DuettoTargetMachine> X(TheDuettoBackendTarget);
}

namespace {
  typedef std::vector<Type*> TypeList;

  class DuettoWriter : public ModulePass {
    formatted_raw_ostream &Out;
    uint64_t uniqueNum;
    bool is_inline;
    unsigned indent_level;

  public:
    static char ID;
    explicit DuettoWriter(formatted_raw_ostream &o) :
      ModulePass(ID), Out(o), uniqueNum(0), is_inline(false), indent_level(0){}

    virtual const char *getPassName() const { return "Duetto backend"; }

    bool runOnModule(Module &M);

    void printProgram(const std::string& fname, const std::string& modName );
    void printModule(const std::string& fname, const std::string& modName );
    void printContents(const std::string& fname, const std::string& modName );
    void printFunction(Function& F);
    void printFunctions(Module& M);
    void printInline(const std::string& fname, const std::string& funcName );
    void printVariable(const std::string& fname, const std::string& varName );
    void printType(const std::string& fname, const std::string& typeName );

    void error(const std::string& msg);

    
    formatted_raw_ostream& nl(formatted_raw_ostream &Out, int delta = 0);
    inline void in() { indent_level++; }
    inline void out() { if (indent_level >0) indent_level--; }
    
  private:
    void printLinkageType(GlobalValue::LinkageTypes LT);
    void printVisibilityType(GlobalValue::VisibilityTypes VisTypes);
    void printCallingConv(CallingConv::ID cc);
    void printEscapedString(const std::string& str);
    void printCFP(const ConstantFP* CFP);

    std::string getCppName(Type* val);
    inline void printCppName(Type* val);

    std::string getCppName(const Value* val);
    inline void printCppName(const Value* val);

    void printType(Type* Ty);
    void printTypes(const Module* M);

    void printConstant(const Constant *CPV);
    void printConstants(const Module* M);

    void printVariableUses(const GlobalVariable *GV);
    void printVariableHead(const GlobalVariable *GV);
    void printVariableBody(const GlobalVariable *GV);

    void printFunctionUses(const Function *F);
    void printFunctionHead(const Function *F);
    void printFunctionBody(const Function *F);
    void printInstruction(const Instruction *I, const std::string& bbname);
    std::string getOpName(const Value*);

    void printModuleBody();
  };
} // end anonymous namespace.

void DuettoWriter::printFunction(Function& F)
{
	if(F.hasFnAttribute(Attribute::Client))
		std::cerr << "CLIENT DETECTED" << std::endl;
	if(F.hasFnAttribute(Attribute::Server))
		std::cerr << "SERVER DETECTED" << std::endl;
}

void DuettoWriter::printFunctions(Module& M)
{
	Module::iterator F=M.begin();
	Module::iterator FE=M.end();
	for (; F != FE; ++F)
		printFunction(*F);
}

bool DuettoWriter::runOnModule(Module& M)
{
	printFunctions(M);
	return false;
}

char DuettoWriter::ID = 0;

//===----------------------------------------------------------------------===//
//                       External Interface declaration
//===----------------------------------------------------------------------===//

bool DuettoTargetMachine::addPassesToEmitFile(PassManagerBase &PM,
                                           formatted_raw_ostream &o,
                                           CodeGenFileType FileType,
                                           bool DisableVerify) {
  if (FileType != TargetMachine::CGFT_AssemblyFile) return true;
  PM.add(new DuettoWriter(o));
  return false;
}

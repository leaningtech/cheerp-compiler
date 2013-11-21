//===-- duetto-compiler.cpp - The Duetto JavaScript generator -------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Duetto/Writer.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/Debug.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/IRReader/IRReader.h"

using namespace llvm;
using namespace std;

static cl::opt<std::string>
InputFilename(cl::Positional, cl::desc("<input bitcode>"), cl::init("-"));

static cl::opt<std::string>
OutputFilename("o", cl::desc("Output filename"), cl::value_desc("filename"));

// Determine optimization level.
static cl::opt<char>
OptLevel("O",
         cl::desc("Optimization level. [-O0, -O1, -O2, or -O3] "
                  "(default = '-O2')"),
         cl::Prefix,
         cl::ZeroOrMore,
         cl::init(' '));

// main - Entry point for the duetto double target compiler.
//
int main(int argc, char **argv) {
  sys::PrintStackTraceOnErrorSignal();
  PrettyStackTraceProgram X(argc, argv);

  // Enable debug stream buffering.
  EnableDebugBuffering = true;

  LLVMContext &Context = getGlobalContext();
  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.

  cl::ParseCommandLineOptions(argc, argv, "duetto compiler\n");

  // Load the module to be compiled...
  SMDiagnostic Err;
  std::unique_ptr<Module> M(parseIRFile(InputFilename, Err, Context));
  if (M.get() == 0) {
    Err.print(argv[0], errs());
    return 1;
  }
  Module* clientMod = M.get();

  std::error_code errorInfo;
  raw_fd_ostream JSOut(OutputFilename.c_str(),errorInfo,sys::fs::F_None);
  if(errorInfo)
  {
     llvm::errs() << errorInfo.message() << '\n';
     return -1;
  }

  duetto::DuettoWriter writer(*clientMod, JSOut);
  writer.makeJS();

  JSOut.close();

  return 0;
}

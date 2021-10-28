//===-- Cheerp/CommandLine.h - Cheerp command line options -----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_COMMAND_LINE_H
#define _CHEERP_COMMAND_LINE_H

#include "llvm/Support/CommandLine.h"

enum LinearOutputTy {
  Wasm,
  AsmJs,
};
extern llvm::cl::opt<LinearOutputTy> LinearOutput;
extern llvm::cl::opt<std::string> SecondaryOutputFile;
extern llvm::cl::opt<std::string> SecondaryOutputPath;
extern llvm::cl::opt<std::string> SourceMap;
extern llvm::cl::opt<std::string> SourceMapPrefix;
extern llvm::cl::opt<std::string> MakeModule;
extern llvm::cl::opt<bool> WasmOnly;
extern llvm::cl::opt<bool> SourceMapStandAlone;
extern llvm::cl::opt<bool> PrettyCode;
extern llvm::cl::opt<bool> SymbolicGlobalsAsmJS;
extern llvm::cl::opt<bool> RegisterizeLegacy;
extern llvm::cl::opt<bool> NoNativeJavaScriptMath;
extern llvm::cl::opt<bool> NoJavaScriptMathImul;
extern llvm::cl::opt<bool> NoJavaScriptMathFround;
extern llvm::cl::opt<bool> NoCredits;
extern llvm::cl::opt<bool> MeasureTimeToMain;
extern llvm::cl::opt<bool> ForceTypedArrays;
extern llvm::cl::list<std::string> ReservedNames;
extern llvm::cl::opt<std::string> GlobalPrefix;
extern llvm::cl::opt<unsigned> CheerpHeapSize;
extern llvm::cl::opt<unsigned> CheerpStackSize;
extern llvm::cl::opt<bool> CheerpNoICF;
extern llvm::cl::opt<bool> BoundsCheck;
extern llvm::cl::opt<bool> AvoidWasmTraps;
extern llvm::cl::opt<bool> AggressiveGepOptimizer;
extern llvm::cl::opt<bool> FixWrongFuncCasts;
extern llvm::cl::opt<std::string> StrictLinking;
extern llvm::cl::opt<bool> WasmSharedMemory;
extern llvm::cl::opt<bool> WasmNoGrowMemory;
extern llvm::cl::opt<bool> WasmExportedTable;
extern llvm::cl::opt<bool> WasmBranchHints;
extern llvm::cl::opt<bool> WasmAnyref;
extern llvm::cl::opt<bool> WasmReturnCalls;
extern llvm::cl::opt<bool> UseBigInts;
extern llvm::cl::opt<bool> KeepInvokes;

#endif //_CHEERP_COMMAND_LINE_H

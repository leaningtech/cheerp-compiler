//===-- Cheerp/CommandLine.h - Cheerp command line options -----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_COMMAND_LINE_H
#define _CHEERP_COMMAND_LINE_H

#include "llvm/Support/CommandLine.h"

extern llvm::cl::opt<std::string> WasmLoader;
extern llvm::cl::opt<std::string> WasmFile;
extern llvm::cl::opt<std::string> AsmJSMemFile;
extern llvm::cl::opt<std::string> SourceMap;
extern llvm::cl::opt<std::string> SourceMapPrefix;
extern llvm::cl::opt<bool> PrettyCode;
extern llvm::cl::opt<bool> SymbolicGlobalsAsmJS;
extern llvm::cl::opt<bool> MakeModule;
extern llvm::cl::opt<bool> NoRegisterize;
extern llvm::cl::opt<bool> NoNativeJavaScriptMath;
extern llvm::cl::opt<bool> NoJavaScriptMathImul;
extern llvm::cl::opt<bool> NoJavaScriptMathFround;
extern llvm::cl::opt<bool> NoCredits;
extern llvm::cl::opt<bool> MeasureTimeToMain;
extern llvm::cl::opt<bool> ForceTypedArrays;
extern llvm::cl::list<std::string> ReservedNames;
extern llvm::cl::opt<unsigned> CheerpHeapSize;
extern llvm::cl::opt<bool> BoundsCheck;

#endif //_CHEERP_COMMAND_LINE_H

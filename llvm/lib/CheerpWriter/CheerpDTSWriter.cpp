//===-- CheerpDTSWriter.cpp - The Cheerp TypeScript declaration generator -===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2023-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/DTSWriter.h"

using namespace llvm;
using namespace std;
using namespace cheerp;

static const NewLineHandler NewLine;

void CheerpDTSWriter::declareModule()
{
}

void CheerpDTSWriter::declareGlobal()
{
}

void CheerpDTSWriter::makeDTS()
{
  if (makeModule == "commonjs")
  {
    stream << "declare const __export: Promise<{" << NewLine;
    declareModule();
    stream << "}>;" << NewLine;
    stream << "export = __export;" << NewLine;
  }
  else if (makeModule == "es6")
  {
    stream << "export default function(): Promise<{" << NewLine;
    declareModule();
    stream << "}>;" << NewLine;
  }
  else if (makeModule == "closure")
  {
    stream << "declare global {" << NewLine;
    declareGlobal();
    stream << "}" << NewLine;
  }
  else
  {
    // TODO: cannot make declarations if output is not a module
  }
}

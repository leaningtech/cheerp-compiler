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

void CheerpDTSWriter::makeDTS()
{
  stream << "export default function(): Promise<object>;" << NewLine;
}

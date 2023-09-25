//===-- Cheerp/DTSWriter.h - The Cheerp TypeScript declaration generator --===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2023-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_DTS_WRITER_H
#define _CHEERP_DTS_WRITER_H

#include "llvm/Cheerp/Writer.h"

namespace cheerp
{

class CheerpDTSWriter final
{
public:
  ostream_proxy stream;

  CheerpDTSWriter(llvm::raw_ostream& s, SourceMapGenerator* sourceMapGenerator, bool readableOutput):
    stream(s, sourceMapGenerator, readableOutput)
  {
  }

  void makeDTS();
};

}

#endif

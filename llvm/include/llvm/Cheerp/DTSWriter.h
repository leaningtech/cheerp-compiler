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
private:
  llvm::Module& module;
  ostream_proxy stream;
  llvm::StringRef makeModule;
  std::vector<const llvm::Function*> exportedFunctions;

  void declareFunction(const llvm::Function* f);
  void declareModule();
  void declareGlobal();

public:
  CheerpDTSWriter(llvm::Module& module, llvm::raw_ostream& s, SourceMapGenerator* sourceMapGenerator, bool readableOutput, llvm::StringRef makeModule):
    module(module),
    stream(s, sourceMapGenerator, readableOutput),
    makeModule(makeModule)
  {
  }

  void makeDTS();
};

}

#endif

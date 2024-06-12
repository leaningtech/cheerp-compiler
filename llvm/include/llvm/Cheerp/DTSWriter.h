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

#include <variant>

namespace cheerp
{

class CheerpDTSWriter final
{
private:
  enum struct FunctionType {
    MEMBER_FUNC,
    STATIC_FUNC,
    CONSTRUCTOR,
  };

  enum struct PropertyType {
    MEMBER,
    GLOBAL,
  };

  llvm::Module& module;
  ostream_proxy stream;
  MODULE_TYPE makeModule;
  JsExportModule exports;

  std::string getTypeName(llvm::StringRef name, bool pointer = false) const;
  std::string getStructName(const llvm::StructType* type) const;

  void declareFunction(const JsExportFunction& func, FunctionType type);
  void declareProperty(const JsExportProperty& prop, PropertyType type);
  void declareInterfaces(const JsExportModule& exports);
  void declareModule(const JsExportModule& exports);
  void declareGlobal(const JsExportModule& exports);

public:
  CheerpDTSWriter(llvm::Module& module, llvm::raw_ostream& s, SourceMapGenerator* sourceMapGenerator, bool readableOutput, MODULE_TYPE makeModule):
    module(module),
    stream(s, sourceMapGenerator, readableOutput),
    makeModule(makeModule)
  {
  }

  void makeDTS();
};

}

#endif

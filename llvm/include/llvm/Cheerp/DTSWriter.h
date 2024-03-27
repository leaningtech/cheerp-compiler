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
  struct Exports;

  struct Property
  {
    std::optional<JsExportFunction> getter;
    std::optional<JsExportFunction> setter;
  };

  struct ClassExport
  {
    std::vector<JsExportFunction> methods;
    llvm::StringMap<Property> properties;
  };

  using Export = std::variant<Exports, JsExportFunction, ClassExport>;
  using ExportRef = std::variant<Exports*, JsExportFunction*, ClassExport*>;

  struct Exports
  {
    llvm::StringMap<Export> exports;
    llvm::StringMap<Property> properties;
    bool hasTypes = false;

    Export& insert(llvm::StringRef name, Export&& ex);
  };

  enum struct FunctionType {
    MEMBER_FUNC,
    STATIC_FUNC,
    CONSTRUCTOR,
  };

  llvm::Module& module;
  ostream_proxy stream;
  MODULE_TYPE makeModule;
  Exports exports;

  std::string getTypeName(const llvm::Type* type) const;

  void declareFunction(const JsExportFunction& func, FunctionType type);
  void declareProperty(const Property& prop);
  void declareInterfaces(const Exports& exports);
  void declareModule(const Exports& exports);
  void declareGlobal(const Exports& exports);

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

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

  struct ClassExport
  {
    const llvm::StructType* type;
    const llvm::Function* constructor = nullptr;
    std::vector<std::pair<std::string, const llvm::Function*>> instanceMethods;
    std::vector<std::pair<std::string, const llvm::Function*>> staticMethods;
  };

  using Export = std::variant<Exports, const llvm::Function*, ClassExport>;

  struct Exports
  {
    std::map<std::string, Export> map;
    bool hasTypes = false;
  };

  enum struct FunctionType {
    MEMBER_FUNC,
    STATIC_FUNC,
    CONSTRUCTOR,
  };

  llvm::Module& module;
  ostream_proxy stream;
  llvm::StringRef makeModule;
  Exports exports;
  std::map<const llvm::Type*, std::string> exportedTypes;

  template<class T>
  static T& addExport(Exports& exports, T data, std::string name)
  {
    auto sep = name.find('.');

    if constexpr (std::is_same_v<std::decay_t<T>, ClassExport>)
      exports.hasTypes = true;

    if (sep == std::string::npos)
    {
      std::pair<std::string, T> pair(std::move(name), std::move(data));
      return std::get<T>(exports.map.emplace(std::move(pair)).first->second);
    }

    Exports& node = std::get<Exports>(exports.map[name.substr(0, sep)]);

    return addExport(node, std::move(data), name.substr(sep + 1));
  }

  std::string getTypeName(const llvm::Type* type) const;

  void declareFunction(const std::string& name, const llvm::Function* f, FunctionType type);
  void declareInterfaces(const Exports& exports);
  void declareModule(const Exports& exports);
  void declareGlobal(const Exports& exports);

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

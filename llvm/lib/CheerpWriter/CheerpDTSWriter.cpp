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
#include "llvm/Cheerp/JsExport.h"

using namespace llvm;
using namespace cheerp;

static const NewLineHandler NewLine;

// TODO: this function makes some unsafe assumptions
// - all function arguments are numbers
// - all return types are numbers or void
void CheerpDTSWriter::declareFunction(const std::string& name, const Function* f)
{
  stream << name << "(";

  auto begin = f->arg_begin();
  auto end = f->arg_end();

  for (auto arg = begin; arg != end; ++arg)
  {
    if (arg != begin)
      stream << ", ";

    stream << arg->getName() << ": number";
  }

  stream << ")";

  if (!f->getReturnType()->isVoidTy())
    stream << ": number";

  stream << ";" << NewLine;
}

void CheerpDTSWriter::declareModule(const Exports& exports)
{
  for (const auto& pair : exports.map)
  {
    const std::string& name = pair.first;
    const Export& ex = pair.second;

    std::visit([this, name](auto&& data) {
      using T = std::decay_t<decltype(data)>;

      if constexpr (std::is_same_v<T, const Function*>)
        declareFunction(name, data);
      else if constexpr (std::is_same_v<T, const StructType*>)
      {

      }
      else if constexpr (std::is_same_v<T, Exports>)
      {
        stream << name << ": {" << NewLine;
        declareModule(data);
        stream << "};" << NewLine;
      }
    }, ex);
  }
}

void CheerpDTSWriter::declareGlobal(const Exports& exports)
{
  for (const auto& pair : exports.map)
  {
    const std::string& name = pair.first;
    const Export& ex = pair.second;

    std::visit([this, name](auto&& data) {
      using T = std::decay_t<decltype(data)>;

      if constexpr (std::is_same_v<T, const Function*>)
      {
        stream << "function ";
        declareFunction(name, data);
        stream << "module " << name << " {" << NewLine;
        stream << "const promise: Promise<void>;" << NewLine;
        stream << "}" << NewLine;
      }
      else if constexpr (std::is_same_v<T, const StructType*>)
      {

      }
      else if constexpr (std::is_same_v<T, Exports>)
      {
        stream << "module " << name << " {" << NewLine;
        declareGlobal(data);
        stream << "}" << NewLine;
      }
    }, ex);
  }
}

void CheerpDTSWriter::makeDTS()
{
  auto processFunction = [this](const Function* f)
  {
    std::string name = TypeSupport::getNamespacedFunctionName(f->getName());
    addExport(exports, f, std::move(name));
  };

  auto processRecord = [this](const NamedMDNode& namedNode, const StringRef& name)
  {
    auto pair = TypeSupport::getJSExportedTypeFromMetadata(name, module);
    addExport<const StructType*>(exports, pair.first, std::move(pair.second));
  };

  iterateOverJsExportedMetadata(module, processFunction, processRecord);

  // TODO: use enum from CheerpWriter instead of string
  if (makeModule == "commonjs")
  {
    stream << "declare const __export: Promise<{" << NewLine;
    declareModule(exports);
    stream << "}>;" << NewLine;
    stream << "export = __export;" << NewLine;
  }
  else if (makeModule == "es6")
  {
    stream << "export default function(): Promise<{" << NewLine;
    declareModule(exports);
    stream << "}>;" << NewLine;
  }
  else if (makeModule == "closure")
  {
    stream << "declare global {" << NewLine;
    declareGlobal(exports);
    stream << "}" << NewLine;
    stream << "export {};" << NewLine;
  }
  else
  {
    // TODO: cannot make declarations if output is not a module
  }
}

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

std::string CheerpDTSWriter::getTypeName(const Type* type) const
{
  if (type->isVoidTy())
    return "void";

  if (!type->isPointerTy())
    return "number";

  if (!TypeSupport::isClientType(type->getPointerElementType()))
    return exportedTypes.at(type->getPointerElementType());

  StringRef name = type->getPointerElementType()->getStructName();

  if (name.startswith("class."))
    name = name.drop_front(6);
  else
    name = name.drop_front(7);

  demangler_iterator demangler(name);
  std::string jsName;

  while (demangler != demangler_iterator())
  {
    jsName += *demangler++;
    jsName += ".";
  }

  return jsName.substr(7, jsName.length() - 8);
}

void CheerpDTSWriter::declareFunction(const std::string& name, const Function* f, FunctionType type)
{
  stream << name << "(";

  auto begin = f->arg_begin();
  std::size_t index = 0;

  if (type == FunctionType::MEMBER_FUNC)
    ++begin;

  for (auto arg = begin; arg != f->arg_end(); ++arg)
  {
    if (arg != begin)
      stream << ", ";

    if (arg->getName().empty())
      stream << "_" << index++;
    else
      stream << arg->getName();

    stream << ": " << getTypeName(arg->getType());
  }

  if (f->isVarArg())
  {
    if (begin != f->arg_end())
      stream << ", ";

    stream << "...args";
  }

  stream << ")";

  if (type != FunctionType::CONSTRUCTOR)
    stream << ": " << getTypeName(f->getReturnType());

  stream << ";" << NewLine;
}

void CheerpDTSWriter::declareInterfaces(const Exports& exports)
{
  for (const auto& pair : exports.map)
  {
    const std::string& name = pair.first;
    const Export& ex = pair.second;

    std::visit([this, name](auto&& data) {
      using T = std::decay_t<decltype(data)>;

      if constexpr (std::is_same_v<T, ClassExport>)
      {
        stream << "export interface " << name << " {" << NewLine;

        for (const auto& [name, f] : data.instanceMethods)
          declareFunction(name, f, FunctionType::MEMBER_FUNC);

        stream << "}" << NewLine;
      }
      else if constexpr (std::is_same_v<T, Exports>)
      {
        stream << "export module " << name << " {" << NewLine;
        declareInterfaces(data);
        stream << "}" << NewLine;
      }
    }, ex);
  }
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
        declareFunction(name, data, FunctionType::STATIC_FUNC);
      else if constexpr (std::is_same_v<T, ClassExport>)
      {
        stream << name << ": {" << NewLine;

        if (data.constructor)
          declareFunction("new", data.constructor, FunctionType::STATIC_FUNC);

        for (const auto& [name, f] : data.staticMethods)
          declareFunction(name, f, FunctionType::STATIC_FUNC);

        stream << "};" << NewLine;
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
        declareFunction(name, data, FunctionType::STATIC_FUNC);
        stream << "module " << name << " {" << NewLine;
        stream << "const promise: Promise<void>;" << NewLine;
        stream << "}" << NewLine;
      }
      else if constexpr (std::is_same_v<T, ClassExport>)
      {
        stream << "class " << name << " {" << NewLine;

        if (data.constructor)
          declareFunction("constructor", data.constructor, FunctionType::CONSTRUCTOR);

        for (const auto& [name, f] : data.instanceMethods)
          declareFunction(name, f, FunctionType::MEMBER_FUNC);

        for (const auto& [name, f] : data.staticMethods)
        {
          stream << "static ";
          declareFunction(name, f, FunctionType::STATIC_FUNC);
        }

        stream << "}" << NewLine;
        stream << "module " << name << " {" << NewLine;
        stream << "const promise: Promise<void>;" << NewLine;
        stream << "}" << NewLine;
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
    ClassExport ex;
    auto pair = TypeSupport::getJSExportedTypeFromMetadata(name, module);

    ex.type = pair.first;
    ex.constructor = nullptr;

    for (auto it = namedNode.op_begin(); it != namedNode.op_end(); ++it)
    {
      const Function* f = cast<Function>(cast<ConstantAsMetadata>((*it)->getOperand(0))->getValue());
      std::string name = TypeSupport::getNamespacedFunctionName(f->getName());
      auto tail = name.find_last_of('.');

      if (tail != std::string::npos)
        name = name.substr(tail + 1);

      if (name == "new")
        ex.constructor = f;
      else if (isStatic(cast<ConstantInt>(cast<ConstantAsMetadata>((*it)->getOperand(1))->getValue())->getZExtValue()))
        ex.staticMethods.push_back({ std::move(name), f });
      else
        ex.instanceMethods.push_back({ std::move(name), f });
    }

    addExport(exports, std::move(ex), pair.second);
    exportedTypes.insert({ pair.first, pair.second });
  };

  iterateOverJsExportedMetadata(module, processFunction, processRecord);

  // TODO: use enum from CheerpWriter instead of string
  if (makeModule == "commonjs")
  {
    declareInterfaces(exports);
    stream << "declare const __export: Promise<{" << NewLine;
    declareModule(exports);
    stream << "}>;" << NewLine;
    stream << "export default __export;" << NewLine;
  }
  else if (makeModule == "es6")
  {
    declareInterfaces(exports);
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
}

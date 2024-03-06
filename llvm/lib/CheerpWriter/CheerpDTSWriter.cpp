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
#include "llvm/Demangle/ItaniumDemangle.h"

using namespace llvm;
using namespace llvm::itanium_demangle;
using namespace cheerp;

class Allocator final
{
  BumpPtrAllocator alloc;

public:
  void reset()
  {
    alloc.Reset();
  }

  template<class T, class... Args>
  T* makeNode(Args&&... args)
  {
    return new(alloc.Allocate(sizeof(T), alignof(T))) T(std::forward<Args>(args)...);
  }

  void* allocateNodeArray(size_t size)
  {
    return alloc.Allocate(sizeof(Node*) * size, alignof(Node*));
  }
};

class Parser final : public AbstractManglingParser<Parser, Allocator>
{
public:
  Parser(StringRef str) : AbstractManglingParser<Parser, Allocator>(str.begin(), str.end()) {}
};

static const NewLineHandler NewLine;

static const std::set<StringRef> TSReservedNames = {
  "break",
  "case",
  "catch",
  "class",
  "const",
  "continue",
  "debugger",
  "default",
  "delete",
  "do",
  "else",
  "enum",
  "export",
  "extends",
  "false",
  "finally",
  "for",
  "function",
  "if",
  "import",
  "in",
  "instanceof",
  "new",
  "null",
  "return",
  "super",
  "switch",
  "this",
  "throw",
  "true",
  "try",
  "typeof",
  "var",
  "void",
  "while",
  "with",
  "implements",
  "interface",
  "let",
  "package",
  "private",
  "protected",
  "public",
  "static",
  "yield",
  "abstract",
  "accessor",
  "as",
  "asserts",
  "assert",
  "any",
  "async",
  "await",
  "boolean",
  "constructor",
  "declare",
  "get",
  "infer",
  "instrinsic",
  "is",
  "keyof",
  "module",
  "namespace",
  "never",
  "out",
  "readonly",
  "require",
  "number",
  "object",
  "satisfies",
  "set",
  "string",
  "symbol",
  "type",
  "undefined",
  "unique",
  "unknown",
  "using",
  "from",
  "global",
  "bigint",
  "override",
  "of",
};

class Visitor final
{
  std::string* result;
  const char* delim;
  bool pointer;

  static StringRef getBaseName(const Node* node)
  {
    StringView name = node->getBaseName();
    return StringRef(name.begin(), name.size());
  }

  static std::string getFullName(const Node* node)
  {
    const NestedName* name;

    switch (node->getKind())
    {
    case Node::KNestedName:
      name = static_cast<const NestedName*>(node);
      return getFullName(name->Qual) + "." + getFullName(name->Name);
    default:
      return std::string(getBaseName(node));
    }
  }

  void handle(NameWithTemplateArgs* node)
  {
    std::string name = getFullName(node);

    if (name == "client._Union")
      accept(node->TemplateArgs, " | ", false);
    else if (name == "client._Function")
      accept(node->TemplateArgs, ", ", false);
    else
    {
      accept(node->Name);
      *result += "<";
      accept(node->TemplateArgs, ", ", false);
      *result += ">";
    }
  }

  void handle(TemplateArgs* node)
  {
    accept(node->getParams());
  }

  void handle(TemplateArgumentPack* node)
  {
    accept(node->getElements());
  }

  void handle(NestedName* node)
  {
    std::string name = getFullName(node);

    if (name == "client._Any")
      *result += "any";
    else if (name == "client.TArray")
      *result += "Array";
    else if (name == "client.Array")
      *result += "Array<any>";
    else if (name == "client.Object")
      *result += "object";
    else if (name == "client.String")
      *result += "string";
    else if (name == "client.Number")
      *result += "number";
    else if (name == "client.Boolean")
      *result += "boolean";
    else if (name == "client.Symbol")
      *result += "symbol";
    else if (name == "client.BigInt")
      *result += "bigint";
    else if (name.substr(0, 7) == "client.")
      *result += name.substr(7);
    else
      *result += name;
  }

  void handle(NameType* node)
  {
    StringRef name = getBaseName(node);

    if (pointer)
      *result += name;
    else if (name == "void")
      *result += "void";
    else if (name == "bool")
      *result += "boolean";
    else
      *result += "number";
  }

  void handle(itanium_demangle::PointerType* node)
  {
    acceptPointer(node->getPointee());
  }

  void handle(itanium_demangle::ReferenceType* node)
  {
    node->match([this](const Node* pointee, ReferenceKind rk) {
      acceptPointer(pointee);
    });
  }

  void handle(itanium_demangle::FunctionType* node)
  {
    node->match([this](const Node* ret, NodeArray params, Qualifiers cvQuals, FunctionRefQual refQual, const Node* exceptionSpec) {
      *result += "(";
      accept(params, "_: ");
      *result += ") => ";
      accept(ret);
    });
  }

  void handle(Node* node)
  {
    assert(false && "unhandled itanium demangler node in dts generator");
  }

public:
  Visitor(std::string* result, const char* delim = "", bool pointer = false) : result(result), delim(delim), pointer(pointer) {}

  template<class T>
  void operator()(const T* node)
  {
    handle(const_cast<T*>(node));
  }

  void accept(const Node* node, const char* delim, bool pointer)
  {
    node->visit(Visitor(result, delim, pointer));
  }

  void acceptPointer(const Node* node)
  {
    accept(node, delim, true);
  }

  void accept(const Node* node)
  {
    accept(node, delim, pointer);
  }

  void accept(NodeArray array, const char* prefix = "")
  {
    const char* tmp = "";

    for (const Node* node : array)
    {
      *result += tmp;
      *result += prefix;
      tmp = delim;
      accept(node);
    }
  }
};

std::string CheerpDTSWriter::getTypeName(const Type* type) const
{
  if (type->isVoidTy())
    return "void";

  if (!type->isPointerTy())
    return "number";

  StringRef name = type->getPointerElementType()->getStructName();
  Parser parser(name.drop_front(name.startswith("class.") ? 6 : 7));
  std::string result;
  Visitor visitor(&result);

  visitor.acceptPointer(parser.parse());

  return result;
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
    else if (TSReservedNames.find(arg->getName()) != TSReservedNames.end())
      stream << "_" << arg->getName();
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
        if (data.hasTypes)
        {
          stream << "export module " << name << " {" << NewLine;
          declareInterfaces(data);
          stream << "}" << NewLine;
        }
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

  if (makeModule == MODULE_TYPE::COMMONJS)
  {
    declareInterfaces(exports);
    stream << "declare const __export: Promise<{" << NewLine;
    declareModule(exports);
    stream << "}>;" << NewLine;
    stream << "export default __export;" << NewLine;
  }
  else if (makeModule == MODULE_TYPE::ES6)
  {
    declareInterfaces(exports);
    stream << "type __Options = { buffer: ArrayBuffer } | { absPath: String | URL };" << NewLine;
    stream << "export default function(options?: __Options): Promise<{" << NewLine;
    declareModule(exports);
    stream << "}>;" << NewLine;
  }
  else if (makeModule == MODULE_TYPE::CLOSURE)
  {
    stream << "declare global {" << NewLine;
    declareGlobal(exports);
    stream << "}" << NewLine;
    stream << "export {};" << NewLine;
  }
}

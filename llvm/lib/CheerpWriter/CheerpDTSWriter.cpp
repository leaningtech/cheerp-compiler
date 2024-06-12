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

std::string CheerpDTSWriter::getTypeName(StringRef name, bool pointer) const
{
  if (name.startswith("class."))
    name = name.drop_front(6);
  else if (name.startswith("struct."))
    name = name.drop_front(7);

  Parser parser(name);
  std::string result;
  Visitor visitor(&result, "", pointer);

  visitor.accept(parser.parse());

  return result;
}

std::string CheerpDTSWriter::getStructName(const llvm::StructType* type) const
{
  return getTypeName(type->getStructName(), true);
}

void CheerpDTSWriter::declareFunction(const JsExportFunction& func, FunctionType type)
{
  const llvm::Function* f = func.getFunction();
  const std::vector<StringRef>& args = func.getParamTypeStrings();

  if (type == FunctionType::CONSTRUCTOR)
    stream << "constructor(";
  else
    stream << func.getName().base() << "(";

  std::size_t begin = 0;

  if (type == FunctionType::MEMBER_FUNC)
    ++begin;

  for (std::size_t i = 0; i < args.size(); i++)
  {
    if (i != 0)
      stream << ", ";

    const auto* arg = f->getArg(begin + i);

    if (arg->getName().empty())
      stream << "_" << i;
    else if (TSReservedNames.find(arg->getName()) != TSReservedNames.end())
      stream << "_" << arg->getName();
    else
      stream << arg->getName();

    stream << ": " << getTypeName(args[i]);
  }

  if (f->isVarArg())
  {
    if (!args.empty())
      stream << ", ";

    stream << "...args";
  }

  stream << ")";

  if (type != FunctionType::CONSTRUCTOR)
    stream << ": " << getTypeName(func.getReturnTypeString());

  stream << ";" << NewLine;
}

void CheerpDTSWriter::declareProperty(const JsExportProperty& prop, PropertyType type)
{
  if (type == PropertyType::GLOBAL)
    stream << (prop.hasSetter() ? "var " : "const ");
  else if (!prop.hasSetter())
    stream << "readonly ";

  stream << prop.getName() << ": " << getTypeName(prop.getTypeString()) << ";" << NewLine;
}

void CheerpDTSWriter::declareInterfaces(const JsExportModule& exports)
{
  for (const auto& [name, value] : exports.getExports())
  {
    if (auto* ex = std::get_if<JsExportClass>(&value))
    {
      stream << "export interface " << name;

      if (auto* base = ex->getBase())
        stream << " extends " << getStructName(base);

      stream << " {" << NewLine;

      for (const auto& [_, func] : ex->getMethods())
        if (!func.isConstructor() && !func.isStatic())
          declareFunction(func, FunctionType::MEMBER_FUNC);

      for (const auto& [_, prop] : ex->getProperties())
        if (!prop.isStatic())
          declareProperty(prop, PropertyType::MEMBER);

      stream << "}" << NewLine;
    }
    else if (auto* ex = std::get_if<JsExportModule>(&value))
    {
      if (ex->hasTypes())
      {
        stream << "export module " << name << " {" << NewLine;
        declareInterfaces(*ex);
        stream << "}" << NewLine;
      }
    }
  }
}

void CheerpDTSWriter::declareModule(const JsExportModule& exports)
{
  for (const auto& [name, value] : exports.getExports())
  {
    if (auto* ex = std::get_if<JsExportFunction>(&value))
      declareFunction(*ex, FunctionType::STATIC_FUNC);
    else if (auto* ex = std::get_if<JsExportProperty>(&value))
      declareProperty(*ex, PropertyType::MEMBER);
    else if (auto* ex = std::get_if<JsExportClass>(&value))
    {
      stream << name << ": {" << NewLine;

      for (const auto& [_, func] : ex->getMethods())
        if (func.isConstructor() || func.isStatic())
          declareFunction(func, FunctionType::STATIC_FUNC);

      for (const auto& [_, prop] : ex->getProperties())
        if (prop.isStatic())
          declareProperty(prop, PropertyType::MEMBER);

      stream << "};" << NewLine;
    }
    else if (auto* ex = std::get_if<JsExportModule>(&value))
    {
      stream << name << ": {" << NewLine;
      declareModule(*ex);
      stream << "};" << NewLine;
    }
  }
}

void CheerpDTSWriter::declareGlobal(const JsExportModule& exports)
{
  for (const auto& [name, value] : exports.getExports())
  {
    if (auto* ex = std::get_if<JsExportFunction>(&value))
    {
      stream << "function ";
      declareFunction(*ex, FunctionType::STATIC_FUNC);
      stream << "module " << name << " {" << NewLine;
      stream << "const promise: Promise<void>;" << NewLine;
      stream << "}" << NewLine;
    }
    else if (auto* ex = std::get_if<JsExportProperty>(&value))
      declareProperty(*ex, PropertyType::GLOBAL);
    else if (auto* ex = std::get_if<JsExportClass>(&value))
    {
      stream << "class " << name;

      if (auto* base = ex->getBase())
        stream << " extends " << getStructName(base);

      stream << " {" << NewLine;

      for (const auto& [_, func] : ex->getMethods())
        if (func.isStatic())
        {
          stream << "static ";
          declareFunction(func, FunctionType::STATIC_FUNC);
        }
        else if (func.isConstructor())
          declareFunction(func, FunctionType::CONSTRUCTOR);
        else
          declareFunction(func, FunctionType::MEMBER_FUNC);

      for (const auto& [_, prop] : ex->getProperties())
      {
        if (prop.isStatic())
          stream << "static ";

        declareProperty(prop, PropertyType::MEMBER);
      }

      stream << "}" << NewLine;
      stream << "module " << name << " {" << NewLine;
      stream << "const promise: Promise<void>;" << NewLine;
      stream << "}" << NewLine;
    }
    else if (auto* ex = std::get_if<JsExportModule>(&value))
    {
      stream << "module " << name << " {" << NewLine;
      declareGlobal(*ex);
      stream << "}" << NewLine;
    }
  }
}

void CheerpDTSWriter::makeDTS()
{
  exports = getJsExportModule(module);

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

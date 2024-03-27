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

CheerpDTSWriter::Export& CheerpDTSWriter::Exports::insert(llvm::StringRef name, Export&& ex)
{
  auto sep = name.find('.');

  if (std::holds_alternative<ClassExport>(ex))
    hasTypes = true;

  if (sep == std::string::npos)
    return exports.try_emplace(name, std::move(ex)).first->second;

  Exports& node = std::get<Exports>(exports[name.substr(0, sep)]);
  return node.insert(name.substr(sep + 1), std::move(ex));
}

void CheerpDTSWriter::declareFunction(const JsExportFunction& func, FunctionType type)
{
  const llvm::Function* f = func.getFunction();

  if (type == FunctionType::CONSTRUCTOR)
    stream << "constructor(";
  else
    stream << func.getBaseName() << "(";

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

void CheerpDTSWriter::declareProperty(const Property& prop)
{
  assert(prop.getter.has_value());
  llvm::Type* type = prop.getter->getFunction()->getReturnType();
  stream << prop.getter->getPropertyName() << ": " << getTypeName(type) << ";" << NewLine;
}

void CheerpDTSWriter::declareInterfaces(const Exports& exports)
{
  for (const auto& pair : exports.exports)
  {
    llvm::StringRef name = pair.getKey();
    const Export* value = &pair.getValue();

    if (auto* ex = std::get_if<ClassExport>(value))
    {
      stream << "export interface " << name << " {" << NewLine;

      for (const auto& func : ex->methods)
        if (!func.isConstructor() && !func.isStatic())
          declareFunction(func, FunctionType::MEMBER_FUNC);

      for (const auto& prop : ex->properties)
        if (!prop.getValue().getter->isStatic())
        {
          if (!prop.getValue().setter)
            stream << "readonly ";

          declareProperty(prop.getValue());
        }

      stream << "}" << NewLine;
    }
    else if (auto* ex = std::get_if<Exports>(value))
    {
      if (ex->hasTypes)
      {
        stream << "export module " << name << " {" << NewLine;
        declareInterfaces(*ex);
        stream << "}" << NewLine;
      }
    }
  }
}

void CheerpDTSWriter::declareModule(const Exports& exports)
{
  for (const auto& pair : exports.exports)
  {
    llvm::StringRef name = pair.getKey();
    const Export* value = &pair.getValue();

    if (auto* ex = std::get_if<JsExportFunction>(value))
      declareFunction(*ex, FunctionType::STATIC_FUNC);
    else if (auto* ex = std::get_if<ClassExport>(value))
    {
      stream << name << ": {" << NewLine;

      for (const auto& func : ex->methods)
        if (func.isConstructor() || func.isStatic())
          declareFunction(func, FunctionType::STATIC_FUNC);

      for (const auto& prop : ex->properties)
        if (prop.getValue().getter->isStatic())
        {
          if (!prop.getValue().setter)
            stream << "readonly ";

          declareProperty(prop.getValue());
        }

      stream << "};" << NewLine;
    }
    else if (auto* ex = std::get_if<Exports>(value))
    {
      stream << name << ": {" << NewLine;
      declareModule(*ex);
      stream << "};" << NewLine;
    }
  }

  for (const auto& pair : exports.properties)
  {
    if (!pair.getValue().setter)
      stream << "readonly ";

    declareProperty(pair.getValue());
  }
}

void CheerpDTSWriter::declareGlobal(const Exports& exports)
{
  for (const auto& pair : exports.exports)
  {
    llvm::StringRef name = pair.getKey();
    const Export* value = &pair.getValue();

    if (auto* ex = std::get_if<JsExportFunction>(value))
    {
      stream << "function ";
      declareFunction(*ex, FunctionType::STATIC_FUNC);
      stream << "module " << name << " {" << NewLine;
      stream << "const promise: Promise<void>;" << NewLine;
      stream << "}" << NewLine;
    }
    else if (auto* ex = std::get_if<ClassExport>(value))
    {
      stream << "class " << name << " {" << NewLine;

      for (const auto& func : ex->methods)
        if (func.isStatic())
        {
          stream << "static ";
          declareFunction(func, FunctionType::STATIC_FUNC);
        }
        else if (func.isConstructor())
          declareFunction(func, FunctionType::CONSTRUCTOR);
        else
          declareFunction(func, FunctionType::MEMBER_FUNC);

      for (const auto& prop : ex->properties)
      {
        if (prop.getValue().getter->isStatic())
          stream << "static ";
        if (!prop.getValue().setter)
          stream << "readonly ";

        declareProperty(prop.getValue());
      }

      stream << "}" << NewLine;
      stream << "module " << name << " {" << NewLine;
      stream << "const promise: Promise<void>;" << NewLine;
      stream << "}" << NewLine;
    }
    else if (auto* ex = std::get_if<Exports>(value))
    {
      stream << "module " << name << " {" << NewLine;
      declareGlobal(*ex);
      stream << "}" << NewLine;
    }
  }

  for (const auto& pair : exports.properties)
  {
    if (!pair.getValue().setter)
      stream << "const ";
    else
      stream << "var ";

    declareProperty(pair.getValue());
  }
}

void CheerpDTSWriter::makeDTS()
{
  for (auto record : getJsExportRecords(module))
  {
    std::string name = record.getJsName();
    exports.insert(name, ClassExport());
  }

  for (auto function : getJsExportFunctions(module))
  {
    auto nameString = function.getJsName();
    llvm::StringRef name = nameString;
    auto tail = name.find_last_of('.');
    ExportRef parent = &exports;
    llvm::StringMap<Property>* properties = nullptr;

    if (tail != std::string::npos)
    {
      Export& ex = exports.insert(name.substr(0, tail), {});
      parent = std::visit([](auto& ex) -> ExportRef { return &ex; }, ex);
      name = name.substr(tail + 1);
    }

    if (auto* ex = std::get_if<ClassExport*>(&parent))
      properties = &(*ex)->properties;
    else if (auto* ex = std::get_if<Exports*>(&parent))
      properties = &(*ex)->properties;

    if (function.isGetter())
      (*properties)[function.getPropertyName()].getter = function;
    else if (function.isSetter())
      (*properties)[function.getPropertyName()].setter = function;
    else if (auto* ex = std::get_if<ClassExport*>(&parent))
      (*ex)->methods.push_back(function);
    else if (auto* ex = std::get_if<Exports*>(&parent))
      (*ex)->insert(name, function);
  }

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

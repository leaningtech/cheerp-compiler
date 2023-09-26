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
using namespace std;
using namespace cheerp;

static const NewLineHandler NewLine;

// TODO: this function makes some unsafe assumptions
// - all function arguments are numbers
// - all return types are numbers or void
// - functions are not namespaced
void CheerpDTSWriter::declareFunction(const Function* f)
{
  std::string name = TypeSupport::getNamespacedFunctionName(f->getName());

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

void CheerpDTSWriter::declareModule()
{
  for (const Function* f : exportedFunctions)
    declareFunction(f);
}

void CheerpDTSWriter::declareGlobal()
{
  for (const Function* f : exportedFunctions)
  {
    stream << "function ";
    declareFunction(f);
    stream << "module " << f->getName() << " {" << NewLine;
    stream << "const promise: Promise<void>;" << NewLine;
    stream << "}";
  }
}

void CheerpDTSWriter::makeDTS()
{
  auto processFunction = [this](const Function* f)
  {
    exportedFunctions.push_back(f);
  };

  auto processRecord = [this](const NamedMDNode& namedNode, const StringRef& name)
  {
    // TODO: export the types
  };

  iterateOverJsExportedMetadata(module, processFunction, processRecord);

  if (makeModule == "commonjs")
  {
    stream << "declare const __export: Promise<{" << NewLine;
    declareModule();
    stream << "}>;" << NewLine;
    stream << "export = __export;" << NewLine;
  }
  else if (makeModule == "es6")
  {
    stream << "export default function(): Promise<{" << NewLine;
    declareModule();
    stream << "}>;" << NewLine;
  }
  else if (makeModule == "closure")
  {
    stream << "declare global {" << NewLine;
    declareGlobal();
    stream << "}" << NewLine;
    stream << "export {};" << NewLine;
  }
  else
  {
    // TODO: cannot make declarations if output is not a module
  }
}

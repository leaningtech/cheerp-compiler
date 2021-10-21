//===--- CheerpToolChain.cpp - ToolChain Implementation for cheerp --------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "ToolChains/WebAssembly.h"

#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/Options.h"
#include "llvm/Config/config.h"
#include "llvm/Option/Arg.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/Path.h"

using namespace clang::driver;
using namespace clang::driver::toolchains;
using namespace clang;
using namespace llvm::opt;

Cheerp::Cheerp(const Driver &D, const llvm::Triple& Triple, const llvm::opt::ArgList &Args)
  : ToolChain(D, Triple, Args) {

  // InstalledDir should be LLVM_PREFIX/bin
  const std::string InstalledDir(getDriver().getInstalledDir());
  getProgramPaths().push_back(InstalledDir);

  path_list& filePaths = getFilePaths();

  // Add default paths
  if (Triple.getEnvironment() == llvm::Triple::WebAssembly) {
    filePaths.push_back(InstalledDir + "/../lib/asmjs");
  } else {
    filePaths.push_back(InstalledDir + "/../lib/genericjs");
  }
  filePaths.push_back(InstalledDir + "/../lib");

  // Add paths passed from the command line
  for (auto& it: Args.filtered(options::OPT_L)) {
    it->claim();
    filePaths.push_back(it->getValue());
  }
}

void Cheerp::AddClangSystemIncludeArgs(const llvm::opt::ArgList &DriverArgs,
                                        llvm::opt::ArgStringList &CC1Args) const {
  if (DriverArgs.hasArg(options::OPT_nostdinc))
    return;

  if (!DriverArgs.hasArg(options::OPT_nobuiltininc)) {
    SmallString<128> P(getDriver().ResourceDir);
    llvm::sys::path::append(P, "include");
    addSystemInclude(DriverArgs, CC1Args, P.str());
  }

  if (DriverArgs.hasArg(options::OPT_nostdlibinc))
    return;

  const std::string InstalledDir(getDriver().getInstalledDir());
  addExternCSystemInclude(DriverArgs, CC1Args,
		  InstalledDir + "/../include");
  addExternCSystemInclude(DriverArgs, CC1Args,
		  InstalledDir + "/../include/client");
}

void Cheerp::AddClangCXXStdlibIncludeArgs(const llvm::opt::ArgList &DriverArgs,
                                           llvm::opt::ArgStringList &CC1Args) const {
  if (DriverArgs.hasArg(options::OPT_nostdlibinc) ||
      DriverArgs.hasArg(options::OPT_nostdincxx))
    return;

  const std::string InstalledDir(getDriver().getInstalledDir());
  // Use the cheerp provided libc++
  addSystemInclude(DriverArgs, CC1Args,
		   InstalledDir + "/../include/c++/v1");
}

bool Cheerp::isPICDefault() const {
  return false;
}

bool Cheerp::isPIEDefault() const {
  return false;
}

bool Cheerp::isPICDefaultForced() const {
  return true;
}

Tool *Cheerp::buildLinker() const {
  return new tools::cheerp::Link(*this);
}

Tool *Cheerp::getTool(Action::ActionClass AC) const {
  switch (AC) {
  case Action::CheerpCompileJobClass:
    if (!CheerpCompiler)
      CheerpCompiler.reset(new tools::cheerp::CheerpCompiler(*this));
    return CheerpCompiler.get();
  case Action::CheerpOptimizeJobClass:
    if (!CheerpOptimizer)
      CheerpOptimizer.reset(new tools::cheerp::CheerpOptimizer(*this));
    return CheerpOptimizer.get();
  default:
    return ToolChain::getTool(AC);
  }
}

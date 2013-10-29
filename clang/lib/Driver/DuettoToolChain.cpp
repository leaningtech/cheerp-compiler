//===--- DuettoToolChain.cpp - ToolChain Implementation for duetto --------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "ToolChains.h"

#include "clang/Driver/Compilation.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/Options.h"
#include "llvm/Config/config.h"
#include "llvm/Option/ArgList.h"

using namespace clang::driver;
using namespace clang::driver::toolchains;
using namespace clang;

Duetto::Duetto(const Driver &D, const llvm::Triple& Triple, const llvm::opt::ArgList &Args)
  : ToolChain(D, Triple, Args) {
}

void Duetto::AddClangSystemIncludeArgs(const llvm::opt::ArgList &DriverArgs,
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

  addExternCSystemInclude(DriverArgs, CC1Args,
		  LLVM_PREFIX "/include");
}

void Duetto::AddClangCXXStdlibIncludeArgs(const llvm::opt::ArgList &DriverArgs,
                                           llvm::opt::ArgStringList &CC1Args) const {
  if (DriverArgs.hasArg(options::OPT_nostdlibinc) ||
      DriverArgs.hasArg(options::OPT_nostdincxx))
    return;

  // Use the duetto provided libc++
  addSystemInclude(DriverArgs, CC1Args,
		   LLVM_PREFIX "/include/c++/v1");
}

bool Duetto::IsUnwindTablesDefault() const {
  return false;
}

bool Duetto::isPICDefault() const {
  return false;
}

bool Duetto::isPIEDefault() const {
  return false;
}

bool Duetto::isPICDefaultForced() const {
  return true;
}

//===--- WebAssembly.cpp - WebAssembly ToolChain Implementation -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "WebAssembly.h"
#include "CommonArgs.h"
#include "Gnu.h"
#include "llvm/ADT/StringSwitch.h"
#include "clang/Basic/Version.h"
#include "clang/Config/config.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/DriverDiagnostic.h"
#include "clang/Driver/Driver.h"
#include "clang/Driver/DriverDiagnostic.h"
#include "clang/Driver/Options.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/VirtualFileSystem.h"
#include "llvm/Option/ArgList.h"

using namespace clang::driver;
using namespace clang::driver::tools;
using namespace clang::driver::toolchains;
using namespace clang;
using namespace llvm::opt;

/// Following the conventions in https://wiki.debian.org/Multiarch/Tuples,
/// we remove the vendor field to form the multiarch triple.
std::string WebAssembly::getMultiarchTriple(const Driver &D,
                                            const llvm::Triple &TargetTriple,
                                            StringRef SysRoot) const {
    return (TargetTriple.getArchName() + "-" +
            TargetTriple.getOSAndEnvironmentName()).str();
}

std::string wasm::Linker::getLinkerPath(const ArgList &Args) const {
  const ToolChain &ToolChain = getToolChain();
  if (const Arg* A = Args.getLastArg(options::OPT_fuse_ld_EQ)) {
    StringRef UseLinker = A->getValue();
    if (!UseLinker.empty()) {
      if (llvm::sys::path::is_absolute(UseLinker) &&
          llvm::sys::fs::can_execute(UseLinker))
        return std::string(UseLinker);

      // Accept 'lld', and 'ld' as aliases for the default linker
      if (UseLinker != "lld" && UseLinker != "ld")
        ToolChain.getDriver().Diag(diag::err_drv_invalid_linker_name)
            << A->getAsString(Args);
    }
  }

  return ToolChain.GetProgramPath(ToolChain.getDefaultLinker());
}

void wasm::Linker::ConstructJob(Compilation &C, const JobAction &JA,
                                const InputInfo &Output,
                                const InputInfoList &Inputs,
                                const ArgList &Args,
                                const char *LinkingOutput) const {

  const ToolChain &ToolChain = getToolChain();
  const char *Linker = Args.MakeArgString(getLinkerPath(Args));
  ArgStringList CmdArgs;

  CmdArgs.push_back("-m");
  if (ToolChain.getTriple().isArch64Bit())
    CmdArgs.push_back("wasm64");
  else
    CmdArgs.push_back("wasm32");

  if (Args.hasArg(options::OPT_s))
    CmdArgs.push_back("--strip-all");

  Args.AddAllArgs(CmdArgs, options::OPT_L);
  Args.AddAllArgs(CmdArgs, options::OPT_u);
  ToolChain.AddFilePathLibArgs(Args, CmdArgs);

  const char *Crt1 = "crt1.o";
  const char *Entry = nullptr;

  // If crt1-command.o exists, it supports new-style commands, so use it.
  // Otherwise, use the old crt1.o. This is a temporary transition measure.
  // Once WASI libc no longer needs to support LLVM versions which lack
  // support for new-style command, it can make crt1.o the same as
  // crt1-command.o. And once LLVM no longer needs to support WASI libc
  // versions before that, it can switch to using crt1-command.o.
  if (ToolChain.GetFilePath("crt1-command.o") != "crt1-command.o")
    Crt1 = "crt1-command.o";

  if (const Arg *A = Args.getLastArg(options::OPT_mexec_model_EQ)) {
    StringRef CM = A->getValue();
    if (CM == "command") {
      // Use default values.
    } else if (CM == "reactor") {
      Crt1 = "crt1-reactor.o";
      Entry = "_initialize";
    } else {
      ToolChain.getDriver().Diag(diag::err_drv_invalid_argument_to_option)
          << CM << A->getOption().getName();
    }
  }
  if (!Args.hasArg(options::OPT_nostdlib, options::OPT_nostartfiles))
    CmdArgs.push_back(Args.MakeArgString(ToolChain.GetFilePath(Crt1)));
  if (Entry) {
    CmdArgs.push_back(Args.MakeArgString("--entry"));
    CmdArgs.push_back(Args.MakeArgString(Entry));
  }

  AddLinkerInputs(ToolChain, Inputs, Args, CmdArgs, JA);

  if (!Args.hasArg(options::OPT_nostdlib, options::OPT_nodefaultlibs)) {
    if (ToolChain.ShouldLinkCXXStdlib(Args))
      ToolChain.AddCXXStdlibLibArgs(Args, CmdArgs);

    if (Args.hasArg(options::OPT_pthread)) {
      CmdArgs.push_back("-lpthread");
      CmdArgs.push_back("--shared-memory");
    }

    CmdArgs.push_back("-lc");
    AddRunTimeLibs(ToolChain, ToolChain.getDriver(), CmdArgs, Args);
  }

  CmdArgs.push_back("-o");
  CmdArgs.push_back(Output.getFilename());

  C.addCommand(std::make_unique<Command>(JA, *this,
                                         ResponseFileSupport::AtFileCurCP(),
                                         Linker, CmdArgs, Inputs, Output));

  // When optimizing, if wasm-opt is available, run it.
  if (Arg *A = Args.getLastArg(options::OPT_O_Group)) {
    auto WasmOptPath = ToolChain.GetProgramPath("wasm-opt");
    if (WasmOptPath != "wasm-opt") {
      StringRef OOpt = "s";
      if (A->getOption().matches(options::OPT_O4) ||
          A->getOption().matches(options::OPT_Ofast))
        OOpt = "4";
      else if (A->getOption().matches(options::OPT_O0))
        OOpt = "0";
      else if (A->getOption().matches(options::OPT_O))
        OOpt = A->getValue();

      if (OOpt != "0") {
        const char *WasmOpt = Args.MakeArgString(WasmOptPath);
        ArgStringList CmdArgs;
        CmdArgs.push_back(Output.getFilename());
        CmdArgs.push_back(Args.MakeArgString(llvm::Twine("-O") + OOpt));
        CmdArgs.push_back("-o");
        CmdArgs.push_back(Output.getFilename());
        C.addCommand(std::make_unique<Command>(
            JA, *this, ResponseFileSupport::AtFileCurCP(), WasmOpt, CmdArgs,
            Inputs, Output));
      }
    }
  }
}

/// Given a base library directory, append path components to form the
/// LTO directory.
static std::string AppendLTOLibDir(const std::string &Dir) {
    // The version allows the path to be keyed to the specific version of
    // LLVM in used, as the bitcode format is not stable.
    return Dir + "/llvm-lto/" LLVM_VERSION_STRING;
}

WebAssembly::WebAssembly(const Driver &D, const llvm::Triple &Triple,
                         const llvm::opt::ArgList &Args)
    : ToolChain(D, Triple, Args) {

  assert(Triple.isArch32Bit() != Triple.isArch64Bit());

  getProgramPaths().push_back(getDriver().getInstalledDir());

  auto SysRoot = getDriver().SysRoot;
  if (getTriple().getOS() == llvm::Triple::UnknownOS) {
    // Theoretically an "unknown" OS should mean no standard libraries, however
    // it could also mean that a custom set of libraries is in use, so just add
    // /lib to the search path. Disable multiarch in this case, to discourage
    // paths containing "unknown" from acquiring meanings.
    getFilePaths().push_back(SysRoot + "/lib");
  } else {
    const std::string MultiarchTriple =
        getMultiarchTriple(getDriver(), Triple, SysRoot);
    if (D.isUsingLTO()) {
      // For LTO, enable use of lto-enabled sysroot libraries too, if available.
      // Note that the directory is keyed to the LLVM revision, as LLVM's
      // bitcode format is not stable.
      auto Dir = AppendLTOLibDir(SysRoot + "/lib/" + MultiarchTriple);
      getFilePaths().push_back(Dir);
    }
    getFilePaths().push_back(SysRoot + "/lib/" + MultiarchTriple);
  }
}

bool WebAssembly::IsMathErrnoDefault() const { return false; }

bool WebAssembly::IsObjCNonFragileABIDefault() const { return true; }

bool WebAssembly::UseObjCMixedDispatch() const { return true; }

bool WebAssembly::isPICDefault() const { return false; }

bool WebAssembly::isPIEDefault(const llvm::opt::ArgList &Args) const {
  return false;
}

bool WebAssembly::isPICDefaultForced() const { return false; }

bool WebAssembly::IsIntegratedAssemblerDefault() const { return true; }

bool WebAssembly::hasBlocksRuntime() const { return false; }

// TODO: Support profiling.
bool WebAssembly::SupportsProfiling() const { return false; }

bool WebAssembly::HasNativeLLVMSupport() const { return true; }

void WebAssembly::addClangTargetOptions(const ArgList &DriverArgs,
                                        ArgStringList &CC1Args,
                                        Action::OffloadKind) const {
  if (!DriverArgs.hasFlag(clang::driver::options::OPT_fuse_init_array,
                          options::OPT_fno_use_init_array, true))
    CC1Args.push_back("-fno-use-init-array");

  // '-pthread' implies atomics, bulk-memory, mutable-globals, and sign-ext
  if (DriverArgs.hasFlag(options::OPT_pthread, options::OPT_no_pthread,
                         false)) {
    if (DriverArgs.hasFlag(options::OPT_mno_atomics, options::OPT_matomics,
                           false))
      getDriver().Diag(diag::err_drv_argument_not_allowed_with)
          << "-pthread"
          << "-mno-atomics";
    if (DriverArgs.hasFlag(options::OPT_mno_bulk_memory,
                           options::OPT_mbulk_memory, false))
      getDriver().Diag(diag::err_drv_argument_not_allowed_with)
          << "-pthread"
          << "-mno-bulk-memory";
    if (DriverArgs.hasFlag(options::OPT_mno_mutable_globals,
                           options::OPT_mmutable_globals, false))
      getDriver().Diag(diag::err_drv_argument_not_allowed_with)
          << "-pthread"
          << "-mno-mutable-globals";
    if (DriverArgs.hasFlag(options::OPT_mno_sign_ext, options::OPT_msign_ext,
                           false))
      getDriver().Diag(diag::err_drv_argument_not_allowed_with)
          << "-pthread"
          << "-mno-sign-ext";
    CC1Args.push_back("-target-feature");
    CC1Args.push_back("+atomics");
    CC1Args.push_back("-target-feature");
    CC1Args.push_back("+bulk-memory");
    CC1Args.push_back("-target-feature");
    CC1Args.push_back("+mutable-globals");
    CC1Args.push_back("-target-feature");
    CC1Args.push_back("+sign-ext");
  }

  if (!DriverArgs.hasFlag(options::OPT_mmutable_globals,
                          options::OPT_mno_mutable_globals, false)) {
    // -fPIC implies +mutable-globals because the PIC ABI used by the linker
    // depends on importing and exporting mutable globals.
    llvm::Reloc::Model RelocationModel;
    unsigned PICLevel;
    bool IsPIE;
    std::tie(RelocationModel, PICLevel, IsPIE) =
        ParsePICArgs(*this, DriverArgs);
    if (RelocationModel == llvm::Reloc::PIC_) {
      if (DriverArgs.hasFlag(options::OPT_mno_mutable_globals,
                             options::OPT_mmutable_globals, false)) {
        getDriver().Diag(diag::err_drv_argument_not_allowed_with)
            << "-fPIC"
            << "-mno-mutable-globals";
      }
      CC1Args.push_back("-target-feature");
      CC1Args.push_back("+mutable-globals");
    }
  }

  if (DriverArgs.getLastArg(options::OPT_fwasm_exceptions)) {
    // '-fwasm-exceptions' is not compatible with '-mno-exception-handling'
    if (DriverArgs.hasFlag(options::OPT_mno_exception_handing,
                           options::OPT_mexception_handing, false))
      getDriver().Diag(diag::err_drv_argument_not_allowed_with)
          << "-fwasm-exceptions"
          << "-mno-exception-handling";
    // '-fwasm-exceptions' is not compatible with
    // '-mllvm -enable-emscripten-cxx-exceptions'
    for (const Arg *A : DriverArgs.filtered(options::OPT_mllvm)) {
      if (StringRef(A->getValue(0)) == "-enable-emscripten-cxx-exceptions")
        getDriver().Diag(diag::err_drv_argument_not_allowed_with)
            << "-fwasm-exceptions"
            << "-mllvm -enable-emscripten-cxx-exceptions";
    }
    // '-fwasm-exceptions' implies exception-handling feature
    CC1Args.push_back("-target-feature");
    CC1Args.push_back("+exception-handling");
    // Backend needs -wasm-enable-eh to enable Wasm EH
    CC1Args.push_back("-mllvm");
    CC1Args.push_back("-wasm-enable-eh");
  }

  for (const Arg *A : DriverArgs.filtered(options::OPT_mllvm)) {
    StringRef Opt = A->getValue(0);
    if (Opt.startswith("-emscripten-cxx-exceptions-allowed")) {
      // '-mllvm -emscripten-cxx-exceptions-allowed' should be used with
      // '-mllvm -enable-emscripten-cxx-exceptions'
      bool EmEHArgExists = false;
      for (const Arg *A : DriverArgs.filtered(options::OPT_mllvm)) {
        if (StringRef(A->getValue(0)) == "-enable-emscripten-cxx-exceptions") {
          EmEHArgExists = true;
          break;
        }
      }
      if (!EmEHArgExists)
        getDriver().Diag(diag::err_drv_argument_only_allowed_with)
            << "-mllvm -emscripten-cxx-exceptions-allowed"
            << "-mllvm -enable-emscripten-cxx-exceptions";

      // Prevent functions specified in -emscripten-cxx-exceptions-allowed list
      // from being inlined before reaching the wasm backend.
      StringRef FuncNamesStr = Opt.split('=').second;
      SmallVector<StringRef, 4> FuncNames;
      FuncNamesStr.split(FuncNames, ',');
      for (auto Name : FuncNames) {
        CC1Args.push_back("-mllvm");
        CC1Args.push_back(DriverArgs.MakeArgString("--force-attribute=" + Name +
                                                   ":noinline"));
      }
    }

    if (Opt.startswith("-wasm-enable-sjlj")) {
      // '-mllvm -wasm-enable-sjlj' is not compatible with
      // '-mno-exception-handling'
      if (DriverArgs.hasFlag(options::OPT_mno_exception_handing,
                             options::OPT_mexception_handing, false))
        getDriver().Diag(diag::err_drv_argument_not_allowed_with)
            << "-mllvm -wasm-enable-sjlj"
            << "-mno-exception-handling";
      // '-mllvm -wasm-enable-sjlj' is not compatible with
      // '-mllvm -enable-emscripten-cxx-exceptions'
      // because we don't allow Emscripten EH + Wasm SjLj
      for (const Arg *A : DriverArgs.filtered(options::OPT_mllvm)) {
        if (StringRef(A->getValue(0)) == "-enable-emscripten-cxx-exceptions")
          getDriver().Diag(diag::err_drv_argument_not_allowed_with)
              << "-mllvm -wasm-enable-sjlj"
              << "-mllvm -enable-emscripten-cxx-exceptions";
      }
      // '-mllvm -wasm-enable-sjlj' is not compatible with
      // '-mllvm -enable-emscripten-sjlj'
      for (const Arg *A : DriverArgs.filtered(options::OPT_mllvm)) {
        if (StringRef(A->getValue(0)) == "-enable-emscripten-sjlj")
          getDriver().Diag(diag::err_drv_argument_not_allowed_with)
              << "-mllvm -wasm-enable-sjlj"
              << "-mllvm -enable-emscripten-sjlj";
      }
      // '-mllvm -wasm-enable-sjlj' implies exception-handling feature
      CC1Args.push_back("-target-feature");
      CC1Args.push_back("+exception-handling");
      // Backend needs '-exception-model=wasm' to use Wasm EH instructions
      CC1Args.push_back("-exception-model=wasm");
    }
  }
}

ToolChain::RuntimeLibType WebAssembly::GetDefaultRuntimeLibType() const {
  return ToolChain::RLT_CompilerRT;
}

ToolChain::CXXStdlibType
WebAssembly::GetCXXStdlibType(const ArgList &Args) const {
  if (Arg *A = Args.getLastArg(options::OPT_stdlib_EQ)) {
    StringRef Value = A->getValue();
    if (Value == "libc++")
      return ToolChain::CST_Libcxx;
    else if (Value == "libstdc++")
      return ToolChain::CST_Libstdcxx;
    else
      getDriver().Diag(diag::err_drv_invalid_stdlib_name)
          << A->getAsString(Args);
  }
  return ToolChain::CST_Libcxx;
}

void WebAssembly::AddClangSystemIncludeArgs(const ArgList &DriverArgs,
                                            ArgStringList &CC1Args) const {
  if (DriverArgs.hasArg(clang::driver::options::OPT_nostdinc))
    return;

  const Driver &D = getDriver();

  if (!DriverArgs.hasArg(options::OPT_nobuiltininc)) {
    SmallString<128> P(D.ResourceDir);
    llvm::sys::path::append(P, "include");
    addSystemInclude(DriverArgs, CC1Args, P);
  }

  if (DriverArgs.hasArg(options::OPT_nostdlibinc))
    return;

  // Check for configure-time C include directories.
  StringRef CIncludeDirs(C_INCLUDE_DIRS);
  if (CIncludeDirs != "") {
    SmallVector<StringRef, 5> dirs;
    CIncludeDirs.split(dirs, ":");
    for (StringRef dir : dirs) {
      StringRef Prefix =
          llvm::sys::path::is_absolute(dir) ? "" : StringRef(D.SysRoot);
      addExternCSystemInclude(DriverArgs, CC1Args, Prefix + dir);
    }
    return;
  }

  if (getTriple().getOS() != llvm::Triple::UnknownOS) {
    const std::string MultiarchTriple =
        getMultiarchTriple(D, getTriple(), D.SysRoot);
    addSystemInclude(DriverArgs, CC1Args, D.SysRoot + "/include/" + MultiarchTriple);
  }
  addSystemInclude(DriverArgs, CC1Args, D.SysRoot + "/include");
}

void WebAssembly::AddClangCXXStdlibIncludeArgs(const ArgList &DriverArgs,
                                               ArgStringList &CC1Args) const {

  if (DriverArgs.hasArg(options::OPT_nostdlibinc, options::OPT_nostdinc,
                        options::OPT_nostdincxx))
    return;

  switch (GetCXXStdlibType(DriverArgs)) {
  case ToolChain::CST_Libcxx:
    addLibCxxIncludePaths(DriverArgs, CC1Args);
    break;
  case ToolChain::CST_Libstdcxx:
    addLibStdCXXIncludePaths(DriverArgs, CC1Args);
    break;
  }
}

void WebAssembly::AddCXXStdlibLibArgs(const llvm::opt::ArgList &Args,
                                      llvm::opt::ArgStringList &CmdArgs) const {

  switch (GetCXXStdlibType(Args)) {
  case ToolChain::CST_Libcxx:
    CmdArgs.push_back("-lc++");
    if (Args.hasArg(options::OPT_fexperimental_library))
      CmdArgs.push_back("-lc++experimental");
    CmdArgs.push_back("-lc++abi");
    break;
  case ToolChain::CST_Libstdcxx:
    CmdArgs.push_back("-lstdc++");
    break;
  }
}

SanitizerMask WebAssembly::getSupportedSanitizers() const {
  SanitizerMask Res = ToolChain::getSupportedSanitizers();
  if (getTriple().isOSEmscripten()) {
    Res |= SanitizerKind::Vptr | SanitizerKind::Leak | SanitizerKind::Address;
  }
  return Res;
}

Tool *WebAssembly::buildLinker() const {
  return new tools::wasm::Linker(*this);
}

void WebAssembly::addLibCxxIncludePaths(
    const llvm::opt::ArgList &DriverArgs,
    llvm::opt::ArgStringList &CC1Args) const {
  const Driver &D = getDriver();
  std::string SysRoot = computeSysRoot();
  std::string LibPath = SysRoot + "/include";
  const std::string MultiarchTriple =
      getMultiarchTriple(D, getTriple(), SysRoot);
  bool IsKnownOs = (getTriple().getOS() != llvm::Triple::UnknownOS);

  std::string Version = detectLibcxxVersion(LibPath);
  if (Version.empty())
    return;

  // First add the per-target include path if the OS is known.
  if (IsKnownOs) {
    std::string TargetDir = LibPath + "/" + MultiarchTriple + "/c++/" + Version;
    addSystemInclude(DriverArgs, CC1Args, TargetDir);
  }

  // Second add the generic one.
  addSystemInclude(DriverArgs, CC1Args, LibPath + "/c++/" + Version);
}

void WebAssembly::addLibStdCXXIncludePaths(
    const llvm::opt::ArgList &DriverArgs,
    llvm::opt::ArgStringList &CC1Args) const {
  // We cannot use GCCInstallationDetector here as the sysroot usually does
  // not contain a full GCC installation.
  // Instead, we search the given sysroot for /usr/include/xx, similar
  // to how we do it for libc++.
  const Driver &D = getDriver();
  std::string SysRoot = computeSysRoot();
  std::string LibPath = SysRoot + "/include";
  const std::string MultiarchTriple =
      getMultiarchTriple(D, getTriple(), SysRoot);
  bool IsKnownOs = (getTriple().getOS() != llvm::Triple::UnknownOS);

  // This is similar to detectLibcxxVersion()
  std::string Version;
  {
    std::error_code EC;
    Generic_GCC::GCCVersion MaxVersion =
        Generic_GCC::GCCVersion::Parse("0.0.0");
    SmallString<128> Path(LibPath);
    llvm::sys::path::append(Path, "c++");
    for (llvm::vfs::directory_iterator LI = getVFS().dir_begin(Path, EC), LE;
         !EC && LI != LE; LI = LI.increment(EC)) {
      StringRef VersionText = llvm::sys::path::filename(LI->path());
      if (VersionText[0] != 'v') {
        auto Version = Generic_GCC::GCCVersion::Parse(VersionText);
        if (Version > MaxVersion)
          MaxVersion = Version;
      }
    }
    if (MaxVersion.Major > 0)
      Version = MaxVersion.Text;
  }

  if (Version.empty())
    return;

  // First add the per-target include path if the OS is known.
  if (IsKnownOs) {
    std::string TargetDir = LibPath + "/c++/" + Version + "/" + MultiarchTriple;
    addSystemInclude(DriverArgs, CC1Args, TargetDir);
  }

  // Second add the generic one.
  addSystemInclude(DriverArgs, CC1Args, LibPath + "/c++/" + Version);
  // Third the backward one.
  addSystemInclude(DriverArgs, CC1Args, LibPath + "/c++/" + Version + "/backward");
}

void cheerp::Link::ConstructJob(Compilation &C, const JobAction &JA,
                                const InputInfo &Output,
                                const InputInfoList &Inputs,
                                const ArgList &Args,
                                const char *LinkingOutput) const {
  ArgStringList CmdArgs;

  CmdArgs.push_back("-o");
  CmdArgs.push_back(Output.getFilename());

  for (InputInfoList::const_iterator
         it = Inputs.begin(), ie = Inputs.end(); it != ie; ++it) {
    const InputInfo &II = *it;
    if(II.isFilename())
      CmdArgs.push_back(II.getFilename());
  }

  const Driver &D = getToolChain().getDriver();
  bool hasUnalignedMemory = true;
  auto features = getWasmFeatures(D, Args);
  if(std::find(features.begin(), features.end(), UNALIGNEDMEM) == features.end())
    hasUnalignedMemory = false;

  // Add standard libraries
  if (!Args.hasArg(options::OPT_nostdlib) &&
      !Args.hasArg(options::OPT_nodefaultlibs)) {
    if (C.getDriver().CCCIsCXX()) {
      CmdArgs.push_back(Args.MakeArgString(getToolChain().GetFilePath("libstdlibs.bc")));
    } else {
      CmdArgs.push_back(Args.MakeArgString(getToolChain().GetFilePath("libc.bc")));
      CmdArgs.push_back(Args.MakeArgString(getToolChain().GetFilePath("crt1.bc")));
    }
    if (getToolChain().getTriple().getOS() == llvm::Triple::WASI)
      CmdArgs.push_back(Args.MakeArgString(getToolChain().GetFilePath("libwasi.bc")));
    else
      CmdArgs.push_back(Args.MakeArgString(getToolChain().GetFilePath("libsystem.bc")));

    // Add wasm helper if needed
    Arg *CheerpLinearOutput = Args.getLastArg(options::OPT_cheerp_linear_output_EQ);
    llvm::Triple::EnvironmentType env = getToolChain().getTriple().getEnvironment();
    if(((CheerpLinearOutput && CheerpLinearOutput->getValue() == StringRef("wasm")) ||
       (!CheerpLinearOutput && env == llvm::Triple::WebAssembly)) &&
	hasUnalignedMemory)
    {
      CmdArgs.push_back(Args.MakeArgString(getToolChain().GetFilePath("libwasm.bc")));
    }
  }
 
  // Do not add the same library more than once
  std::set<std::string> usedLibs;
  for (auto& it: Args.filtered(options::OPT_l)) {
    std::string libName("lib");
    libName += it->getValue();
    std::string bcLibName = libName + ".bc";
    std::string foundLib = getToolChain().GetFilePath(bcLibName.c_str());
    if (foundLib == bcLibName) {
      // Try again using .a, the internal format is still assumed to be BC
      std::string aLibName = libName + ".a";
      foundLib = getToolChain().GetFilePath(aLibName.c_str());
      if(foundLib == aLibName)
        foundLib = bcLibName;
    }
    if (usedLibs.count(foundLib))
      continue;
    usedLibs.insert(foundLib);
    CmdArgs.push_back(Args.MakeArgString(foundLib));
  }

  const char *Exec = Args.MakeArgString((getToolChain().GetProgramPath("llvm-link")));
  C.addCommand(std::make_unique<Command>(JA, *this, ResponseFileSupport::None(), Exec, CmdArgs, Inputs));
}

void cheerp::CheerpOptimizer::ConstructJob(Compilation &C, const JobAction &JA,
                                          const InputInfo &Output,
                                          const InputInfoList &Inputs,
                                          const ArgList &Args,
                                          const char *LinkingOutput) const {
  ArgStringList CmdArgs;
  const Driver &D = getToolChain().getDriver();

  std::string optPasses = "";
  auto addPass = [&optPasses](const std::string& passInvocation)->void{
	  if (optPasses.size())
		  optPasses += ",";
	  optPasses += passInvocation;
  };

  CmdArgs.push_back("-march=cheerp");
  if(Args.hasArg(options::OPT_cheerp_preexecute))
    addPass("PreExecute");
  if(Args.hasArg(options::OPT_cheerp_preexecute_main))
    CmdArgs.push_back("-cheerp-preexecute-main");
  if(Arg* cheerpFixFuncCasts = Args.getLastArg(options::OPT_cheerp_fix_wrong_func_casts))
    cheerpFixFuncCasts->render(Args, CmdArgs);
  if(Arg* cheerpUseBigInts = Args.getLastArg(options::OPT_cheerp_use_bigints))
    cheerpUseBigInts->render(Args, CmdArgs);
  else if (getToolChain().getTriple().getOS() == llvm::Triple::WASI)
    CmdArgs.push_back("-cheerp-use-bigints");

  // Malloc/Free are probably intercepted when using sanitizers, don't optimize
  // them out
  if(Args.hasArg(options::OPT_fsanitize_EQ))
    CmdArgs.push_back("-cheerp-preserve-free");

  if(Arg* cheerpStrictLinkingEq = Args.getLastArg(options::OPT_cheerp_strict_linking_EQ)) {
    if (cheerpStrictLinkingEq->getValue() != StringRef("warning") &&
        cheerpStrictLinkingEq->getValue() != StringRef("error")) {
      D.Diag(diag::err_drv_invalid_value)
      << cheerpStrictLinkingEq->getAsString(Args) << cheerpStrictLinkingEq->getValue();
    }
    cheerpStrictLinkingEq->render(Args, CmdArgs);
  }

  if(Arg* cheerpLinearOutput = Args.getLastArg(options::OPT_cheerp_linear_output_EQ))
    cheerpLinearOutput->render(Args, CmdArgs);
  else
  {
    std::string linearOut("-cheerp-linear-output=");
    llvm::Triple::EnvironmentType env = getToolChain().getTriple().getEnvironment();
    if (env == llvm::Triple::WebAssembly)
    {
      linearOut += "wasm";
    }
    else
    {
      // NOTE: we use "asmjs" also for -target cheerp
      linearOut += "asmjs";
    }
    CmdArgs.push_back(Args.MakeArgString(linearOut));
  }
  auto features = getWasmFeatures(D, Args);
  if(std::find(features.begin(), features.end(), EXPORTEDTABLE) != features.end())
    CmdArgs.push_back("-cheerp-wasm-exported-table");
  if(std::find(features.begin(), features.end(), EXPORTEDMEMORY) != features.end())
    CmdArgs.push_back("-cheerp-wasm-exported-memory");
  if(std::find(features.begin(), features.end(), SIMD) == features.end())
    CmdArgs.push_back("-cheerp-wasm-no-simd");
  if(std::find(features.begin(), features.end(), UNALIGNEDMEM) == features.end())
    CmdArgs.push_back("-cheerp-wasm-no-unaligned-mem");

  if (Args.hasArg(options::OPT_cheerp_no_icf))
    CmdArgs.push_back("-cheerp-no-icf");

  addPass("function(CheerpLowerInvoke)");
  if (Args.hasArg(options::OPT_fexceptions))
    CmdArgs.push_back("-cheerp-keep-invokes");
  addPass("function(simplifycfg)");

  addPass("CallConstructors");
  addPass("GlobalDepsAnalyzer");
  addPass("TypeOptimizer");
  addPass("function(CheerpLowerSwitch)");
  addPass("I64Lowering");
  addPass("function(ReplaceNopCastsAndByteSwaps)");

  if(!Args.hasArg(options::OPT_cheerp_no_lto))
  {
    addPass("FreeAndDeleteRemoval");
    CmdArgs.push_back("-cheerp-lto");
    addPass("default<Os>");
    addPass("PartialExecuter");
    // -Os converts loops to canonical form, which may causes empty forwarding branches, remove those
    // Also cleanup any constants instruced by PartialExecuter
    addPass("function(simplifycfg,instcombine)");
  }
  CmdArgs.push_back(Args.MakeArgString(std::string("-passes=")+optPasses));
  CmdArgs.push_back("-o");
  CmdArgs.push_back(Output.getFilename());

  const InputInfo &II = *Inputs.begin();
  CmdArgs.push_back(II.getFilename());

  // Honor -mllvm
  Args.AddAllArgValues(CmdArgs, options::OPT_mllvm);
  // Honor -cheerp-no-pointer-scev
  if (Arg *CheerpNoPointerSCEV = Args.getLastArg(options::OPT_cheerp_no_pointer_scev))
    CheerpNoPointerSCEV->render(Args, CmdArgs);

  const char *Exec = Args.MakeArgString((getToolChain().GetProgramPath("opt")));
  C.addCommand(std::make_unique<Command>(JA, *this, ResponseFileSupport::None(), Exec, CmdArgs, Inputs));
}

static cheerp::CheerpWasmOpt parseWasmOpt(StringRef opt)
{
  return llvm::StringSwitch<cheerp::CheerpWasmOpt>(opt)
    .Case("growmem", cheerp::GROWMEM)
    .Case("sharedmem", cheerp::SHAREDMEM)
    .Case("exportedtable", cheerp::EXPORTEDTABLE)
    .Case("exportedmemory", cheerp::EXPORTEDMEMORY)
    .Case("externref", cheerp::ANYREF)
    .Case("returncalls", cheerp::RETURNCALLS)
    .Case("branchhinting", cheerp::BRANCHHINTS)
    .Case("simd", cheerp::SIMD)
    .Case("globalization", cheerp::GLOBALIZATION)
    .Case("unalignedmem", cheerp::UNALIGNEDMEM)
    .Default(cheerp::INVALID);
}

std::vector<cheerp::CheerpWasmOpt> cheerp::getWasmFeatures(const Driver& D, const ArgList& Args)
{
  // Figure out which Wasm optional feature to enable/disable
  std::vector<CheerpWasmOpt> features;
  // We enable memory growth/globalization/unaligned memory accesses by default
  features.push_back(GROWMEM);
  features.push_back(GLOBALIZATION);
  features.push_back(UNALIGNEDMEM);
  if(Arg* cheerpWasmEnable = Args.getLastArg(options::OPT_cheerp_wasm_enable_EQ)) {
    for (StringRef opt: cheerpWasmEnable->getValues())
    {
      CheerpWasmOpt o = parseWasmOpt(opt);
      if (o == INVALID)
      {
        D.Diag(diag::err_drv_unsupported_option_argument)
          << cheerpWasmEnable->getOption().getName() << opt;
      }
      else
      {
        features.push_back(o);
      }
    }
  }
  if(Arg* cheerpWasmExternRef = Args.getLastArg(clang::driver::options::OPT_cheerp_wasm_anyref))
    features.push_back(CheerpWasmOpt(parseWasmOpt("externref")));

  // Sort and remove duplicates
  std::sort(features.begin(), features.end());
  auto last = std::unique(features.begin(), features.end());
  features.erase(last, features.end());

  if(Arg* cheerpWasmDisable = Args.getLastArg(options::OPT_cheerp_wasm_disable_EQ)) {
    for (StringRef opt: cheerpWasmDisable->getValues())
    {
      CheerpWasmOpt o = parseWasmOpt(opt);
      if (o == INVALID)
      {
        D.Diag(diag::err_drv_unsupported_option_argument)
          << cheerpWasmDisable->getOption().getName() << opt;
      }
      else
      {
        auto last = std::remove(features.begin(), features.end(), o);
        features.erase(last, features.end());
      }
    }
  }
  return features;
}

void cheerp::CheerpCompiler::ConstructJob(Compilation &C, const JobAction &JA,
                                          const InputInfo &Output,
                                          const InputInfoList &Inputs,
                                          const ArgList &Args,
                                          const char *LinkingOutput) const {
  const Driver &D = getToolChain().getDriver();

  ArgStringList CmdArgs;

  CmdArgs.push_back("-march=cheerp");

  {
    CmdArgs.push_back("-o");
    CmdArgs.push_back(Output.getFilename());
  }

  llvm::Triple::EnvironmentType env = getToolChain().getTriple().getEnvironment();
  llvm::Triple::OSType os = getToolChain().getTriple().getOS();
  Arg* cheerpLinearOutput = Args.getLastArg(options::OPT_cheerp_linear_output_EQ);
  if (cheerpLinearOutput)
    cheerpLinearOutput->render(Args, CmdArgs);
  else
  {
    std::string linearOut("-cheerp-linear-output=");
    if (env == llvm::Triple::WebAssembly)
    {
      linearOut += "wasm";
    }
    else
    {
      // NOTE: we use "asmjs" also for -target cheerp
      linearOut += "asmjs";
    }
    CmdArgs.push_back(Args.MakeArgString(linearOut));
  }

  // Malloc/Free are probably intercepted when using sanitizers, don't optimize
  // them out
  if(Args.hasArg(options::OPT_fsanitize_EQ))
    CmdArgs.push_back("-cheerp-preserve-free");

  if(Arg* cheerpSecondaryOutputFile = Args.getLastArg(options::OPT_cheerp_secondary_output_file_EQ))
    cheerpSecondaryOutputFile->render(Args, CmdArgs);
  else if(
      ((env == llvm::Triple::WebAssembly && os != llvm::Triple::WASI && !cheerpLinearOutput) ||
        (cheerpLinearOutput && cheerpLinearOutput->getValue() != StringRef("asmjs"))))
  {
    SmallString<64> path(Output.getFilename());
    StringRef ext = ".wasm";
    if (llvm::sys::path::extension(path) == ext)
    {
      path.append(ext);
    }
    else
      llvm::sys::path::replace_extension(path, ext);
    CmdArgs.push_back(Args.MakeArgString(Twine("-cheerp-secondary-output-file=")+path));
  }

  if(Arg* cheerpSecondaryOutputPath = Args.getLastArg(options::OPT_cheerp_secondary_output_path_EQ))
    cheerpSecondaryOutputPath->render(Args, CmdArgs);

  if(Arg* cheerpDTSOutputFile = Args.getLastArg(options::OPT_cheerp_dts_output_file_EQ))
    cheerpDTSOutputFile->render(Args, CmdArgs);
  else
  {
    SmallString<64> path(Output.getFilename());
    StringRef ext = ".d.ts";
    if (llvm::sys::path::extension(path) == ext)
      path.append(ext);
    else
      llvm::sys::path::replace_extension(path, ext);
    CmdArgs.push_back(Args.MakeArgString(Twine("-cheerp-dts-output-file=")+path));
  }

  if(Args.getLastArg(options::OPT_cheerp_make_module)) {
    CmdArgs.push_back("-cheerp-make-module=closure");
  }
  if(Arg* cheerpMakeModuleEq = Args.getLastArg(options::OPT_cheerp_make_module_EQ)) {
    if (cheerpMakeModuleEq->getValue() != StringRef("closure") &&
        cheerpMakeModuleEq->getValue() != StringRef("commonjs") &&
        cheerpMakeModuleEq->getValue() != StringRef("es6")) {
      D.Diag(diag::err_drv_invalid_value)
      << cheerpMakeModuleEq->getAsString(Args) << cheerpMakeModuleEq->getValue();
    }
    cheerpMakeModuleEq->render(Args, CmdArgs);
  }

  // Figure out which Wasm optional feature to enable/disable
  auto features = getWasmFeatures(D, Args);
  bool noGrowMem = true;
  bool noSIMD = true;
  bool noGlobalization = true;
  bool noUnalignedMem = true;
  for (CheerpWasmOpt o: features)
  {
    switch(o)
    {
      case SHAREDMEM:
        CmdArgs.push_back("-cheerp-wasm-shared-memory");
        break;
      case GROWMEM:
        noGrowMem = false;
        break;
      case EXPORTEDTABLE:
        CmdArgs.push_back("-cheerp-wasm-exported-table");
        break;
      case EXPORTEDMEMORY:
        CmdArgs.push_back("-cheerp-wasm-exported-memory");
        break;
      case ANYREF:
        CmdArgs.push_back("-cheerp-wasm-externref");
        break;
      case RETURNCALLS:
        CmdArgs.push_back("-cheerp-wasm-return-calls");
        break;
      case BRANCHHINTS:
	CmdArgs.push_back("-cheerp-wasm-branch-hinting");
	break;
      case SIMD:
        noSIMD = false;
        break;
      case GLOBALIZATION:
        noGlobalization = false;
        break;
      case UNALIGNEDMEM:
        noUnalignedMem = false;
        break;
      default:
        llvm_unreachable("invalid wasm option");
        break;
    }
  }
  if (noGrowMem)
    CmdArgs.push_back("-cheerp-wasm-no-grow-memory");
  if (noSIMD)
    CmdArgs.push_back("-cheerp-wasm-no-simd");
  if (noGlobalization)
    CmdArgs.push_back("-cheerp-wasm-no-globalization");
  if (noUnalignedMem)
    CmdArgs.push_back("-cheerp-wasm-no-unaligned-mem");

  if(Arg* cheerpSourceMap = Args.getLastArg(options::OPT_cheerp_sourcemap_EQ))
    cheerpSourceMap->render(Args, CmdArgs);
  if(Arg* cheerpSourceMapPrefix = Args.getLastArg(options::OPT_cheerp_sourcemap_prefix_EQ))
    cheerpSourceMapPrefix->render(Args, CmdArgs);
  if(Arg* cheerpSourceMapStandAlone = Args.getLastArg(options::OPT_cheerp_sourcemap_standalone))
    cheerpSourceMapStandAlone->render(Args, CmdArgs);
  if(Arg* cheerpPrettyCode = Args.getLastArg(options::OPT_cheerp_pretty_code))
    cheerpPrettyCode->render(Args, CmdArgs);
  if(Arg* cheerpAsmJSSymbolicGlobals = Args.getLastArg(options::OPT_cheerp_asmjs_symbolic_globals))
    cheerpAsmJSSymbolicGlobals->render(Args, CmdArgs);
  if(Arg* cheerpNoNativeMath = Args.getLastArg(options::OPT_cheerp_no_native_math))
    cheerpNoNativeMath->render(Args, CmdArgs);
  if(Arg* cheerpNoMathImul = Args.getLastArg(options::OPT_cheerp_no_math_imul))
    cheerpNoMathImul->render(Args, CmdArgs);
  if(Arg* cheerpNoMathFround = Args.getLastArg(options::OPT_cheerp_no_math_fround))
    cheerpNoMathFround->render(Args, CmdArgs);
  if(Arg* cheerpNoCredits = Args.getLastArg(options::OPT_cheerp_no_credits))
    cheerpNoCredits->render(Args, CmdArgs);
  if(Arg* cheerpForceTypedArrays = Args.getLastArg(options::OPT_cheerp_force_typed_arrays))
    cheerpForceTypedArrays->render(Args, CmdArgs);
  if(Arg* cheerpReservedNames = Args.getLastArg(options::OPT_cheerp_reserved_names_EQ))
    cheerpReservedNames->render(Args, CmdArgs);
  if(Arg* cheerpGlobalPrefix = Args.getLastArg(options::OPT_cheerp_global_prefix_EQ))
    cheerpGlobalPrefix->render(Args, CmdArgs);
  if(Arg *cheerpHeapSize = Args.getLastArg(options::OPT_cheerp_linear_heap_size))
    cheerpHeapSize->render(Args, CmdArgs);
  if(Arg *cheerpStackSize = Args.getLastArg(options::OPT_cheerp_linear_stack_size))
    cheerpStackSize->render(Args, CmdArgs);
  if(Arg* cheerpNoICF = Args.getLastArg(options::OPT_cheerp_no_icf))
    cheerpNoICF->render(Args, CmdArgs);
  if(Arg* cheerpBoundsCheck = Args.getLastArg(options::OPT_cheerp_bounds_check))
    cheerpBoundsCheck->render(Args, CmdArgs);
  if(Arg* cheerpAvoidWasmTraps = Args.getLastArg(options::OPT_cheerp_avoid_wasm_traps))
    cheerpAvoidWasmTraps->render(Args, CmdArgs);
  if(Arg* cheerpFixFuncCasts = Args.getLastArg(options::OPT_cheerp_fix_wrong_func_casts))
    cheerpFixFuncCasts->render(Args, CmdArgs);
  if(Arg* cheerpUseBigInts = Args.getLastArg(options::OPT_cheerp_use_bigints))
    cheerpUseBigInts->render(Args, CmdArgs);
  else if (getToolChain().getTriple().getOS() == llvm::Triple::WASI)
    CmdArgs.push_back("-cheerp-use-bigints");
  if(Arg* cheerpMakeDTS = Args.getLastArg(options::OPT_cheerp_make_dts))
    cheerpMakeDTS->render(Args, CmdArgs);

  // Set output to binary mode to avoid linefeed conversion on Windows.
  CmdArgs.push_back("-filetype");
  CmdArgs.push_back("obj");

  const InputInfo &II = *Inputs.begin();
  CmdArgs.push_back(II.getFilename());

  const char *Exec = Args.MakeArgString((getToolChain().GetProgramPath("llc")));
  C.addCommand(std::make_unique<Command>(JA, *this, ResponseFileSupport::None(), Exec, CmdArgs, Inputs));
}

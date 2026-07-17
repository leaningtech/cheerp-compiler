# This file sets up a CMakeCache for the second stage of a simple distribution
# bootstrap build.

set(CMAKE_INSTALL_PREFIX /opt/cheerp CACHE STRING "")
set(LLVM_DEFAULT_TARGET_TRIPLE cheerp-leaningtech-webbrowser-wasm CACHE STRING "")
set(LLVM_TARGETS_TO_BUILD X86;WebAssembly CACHE STRING "")
SET(CLANG_ENABLE_STATIC_ANALYZER OFF CACHE BOOL "")
SET(CLANG_ENABLE_ARCMT OFF CACHE BOOL "")

# setup toolchain
set(LLVM_INSTALL_TOOLCHAIN_ONLY ON CACHE BOOL "")
# llvm-dis is only a debugging aid; builds that are tight on space (e.g. the
# macOS DMG) can drop it by passing -DCHEERP_INSTALL_LLVM_DIS=OFF *before* the
# -C for this file (a -D after -C is applied too late to affect the lists below).
if(NOT DEFINED CHEERP_INSTALL_LLVM_DIS OR CHEERP_INSTALL_LLVM_DIS)
  set(CHEERP_LLVM_DIS llvm-dis)
endif()
set(LLVM_TOOLCHAIN_TOOLS
  ${CHEERP_LLVM_DIS}
  llvm-ar
  llvm-link
  opt
  llc
  CACHE STRING "")

if("${LLVM_ENABLE_PROJECTS}" MATCHES ".*clang-tools-extra.*")
  set(CHEERP_CLANGD clangd)
endif()

set(LLVM_DISTRIBUTION_COMPONENTS
  clang
  clang-resource-headers
  ${CHEERP_CLANGD}
  ${LLVM_TOOLCHAIN_TOOLS}
  CACHE STRING "")

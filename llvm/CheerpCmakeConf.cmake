# This file sets up a CMakeCache for the second stage of a simple distribution
# bootstrap build.

set(CMAKE_INSTALL_PREFIX /opt/cheerp CACHE STRING "")
set(LLVM_DEFAULT_TARGET_TRIPLE cheerp-leaningtech-webbrowser-wasm CACHE STRING "")
set(LLVM_TARGETS_TO_BUILD X86;CheerpBackend CACHE STRING "")
SET(CLANG_ENABLE_STATIC_ANALYZER OFF CACHE BOOL "")
SET(CLANG_ENABLE_ARCMT OFF CACHE BOOL "")
SET(LLVM_ENABLE_NEW_PASS_MANAGER OFF CACHE BOOL "")

# setup toolchain
set(LLVM_INSTALL_TOOLCHAIN_ONLY ON CACHE BOOL "")
set(LLVM_TOOLCHAIN_TOOLS
  llvm-nm
  llvm-as
  llvm-bcanalyzer
  llvm-extract
  llvm-diff
  llvm-rtdyld
  llvm-symbolizer
  llvm-dwarfdump
  llvm-mc
  llvm-profdata
  llvm-size
  llvm-objdump
  llvm-stress
  llvm-readobj
  llvm-dis
  llvm-ar
  llvm-config
  llvm-cov
  llvm-link
  opt
  llc
  CACHE STRING "")

set(LLVM_DISTRIBUTION_COMPONENTS
  clang
  clang-resource-headers
  ${LLVM_TOOLCHAIN_TOOLS}
  CACHE STRING "")


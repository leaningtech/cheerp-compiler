# Install script for directory: /home/c/Downloads/cheerp/cheerp-compiler/llvm/lib

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Debug")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/IR/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/FuzzMutate/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/FileCheck/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/InterfaceStub/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/IRPrinter/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/IRReader/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/CheerpUtils/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/CheerpWriter/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/CodeGen/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/BinaryFormat/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Bitcode/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Bitstream/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/DWARFLinker/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Extensions/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Frontend/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Transforms/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Linker/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Analysis/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/LTO/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/MC/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/MCA/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/ObjCopy/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Object/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/ObjectYAML/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Option/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Remarks/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Debuginfod/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/DebugInfo/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/DWP/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/ExecutionEngine/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Target/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/AsmParser/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/LineEditor/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/ProfileData/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Passes/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/TextAPI/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/ToolDrivers/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/XRay/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/Testing/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/WindowsDriver/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/lib/WindowsManifest/cmake_install.cmake")

endif()


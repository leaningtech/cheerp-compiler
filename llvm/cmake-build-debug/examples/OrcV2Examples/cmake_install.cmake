# Install script for directory: /home/c/Downloads/cheerp/cheerp-compiler/llvm/examples/OrcV2Examples

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
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/LLJITDumpObjects/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/LLJITRemovableCode/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/LLJITWithCustomObjectLinkingLayer/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/LLJITWithExecutorProcessControl/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/LLJITWithGDBRegistrationListener/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/LLJITWithInitializers/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/LLJITWithLazyReexports/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/LLJITWithObjectCache/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/LLJITWithObjectLinkingLayerPlugin/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/LLJITWithOptimizingIRTransform/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/LLJITWithThinLTOSummaries/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/OrcV2CBindingsAddObjectFile/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/OrcV2CBindingsBasicUsage/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/OrcV2CBindingsDumpObjects/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/OrcV2CBindingsIRTransforms/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/OrcV2CBindingsReflectProcessSymbols/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/OrcV2CBindingsRemovableCode/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/OrcV2CBindingsLazy/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/OrcV2CBindingsVeryLazy/cmake_install.cmake")
  include("/home/c/Downloads/cheerp/cheerp-compiler/llvm/cmake-build-debug/examples/OrcV2Examples/LLJITWithRemoteDebugging/cmake_install.cmake")

endif()


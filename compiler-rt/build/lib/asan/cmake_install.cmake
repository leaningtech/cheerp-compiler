# Install script for directory: /home/alex/cheerp/cheerp-compiler/compiler-rt/lib/asan

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/opt/cheerp")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "")
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

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "TRUE")
endif()

# Set path to fallback-tool for dependency-resolution.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "clang_rt.asan-Cheerp" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/asmjs" TYPE STATIC_LIBRARY FILES "/home/alex/cheerp/cheerp-compiler/compiler-rt/build/lib/cheerp/libclang_rt.asan-Cheerp.bc")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "clang_rt.asan_cxx-Cheerp" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/asmjs" TYPE STATIC_LIBRARY FILES "/home/alex/cheerp/cheerp-compiler/compiler-rt/build/lib/cheerp/libclang_rt.asan_cxx-Cheerp.bc")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "clang_rt.asan_static-Cheerp" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/asmjs" TYPE STATIC_LIBRARY FILES "/home/alex/cheerp/cheerp-compiler/compiler-rt/build/lib/cheerp/libclang_rt.asan_static-Cheerp.bc")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "clang_rt.asan-preinit-Cheerp" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/asmjs" TYPE STATIC_LIBRARY FILES "/home/alex/cheerp/cheerp-compiler/compiler-rt/build/lib/cheerp/libclang_rt.asan-preinit-Cheerp.bc")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "clang_rt.asan-dynamic-Cheerp" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/asmjs" TYPE STATIC_LIBRARY FILES "/home/alex/cheerp/cheerp-compiler/compiler-rt/build/lib/cheerp/libclang_rt.asan-Cheerp.bc")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "asan" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share" TYPE FILE FILES "/home/alex/cheerp/cheerp-compiler/compiler-rt/lib/asan/asan_ignorelist.txt")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/home/alex/cheerp/cheerp-compiler/compiler-rt/build/lib/asan/scripts/cmake_install.cmake")
  include("/home/alex/cheerp/cheerp-compiler/compiler-rt/build/lib/asan/tests/cmake_install.cmake")

endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
if(CMAKE_INSTALL_LOCAL_ONLY)
  file(WRITE "/home/alex/cheerp/cheerp-compiler/compiler-rt/build/lib/asan/install_local_manifest.txt"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
endif()

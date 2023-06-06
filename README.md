Cheerp: A C++ compiler for the Web
==================================

[![Discord server](https://img.shields.io/discord/988743885121548329?color=%235865F2&logo=discord&logoColor=%23fff)][discord-invite]

This directory and its sub-directories contain the source code for LLVM,
a toolkit for the construction of highly optimized compilers,
optimizers, and run-time environments.

Useful links
------------

Documentation and Tutorials:
https://docs.leaningtech.com/cheerp/

Please report bugs on the cheerp-meta repository:
https://github.com/leaningtech/cheerp-meta/issues

[Join the Discord server][discord-invite]

Or ask questions on stackoverflow:
https://stackoverflow.com/questions/ask?tags=cheerp

Cheerp compiler build instructions
----------------------------------

We advise doing an out of tree build, but it's not necessary

This is only one of the components of Cheerp. Please see https://docs.leaningtech.com/cheerp/Linux-build-instructions for instructions on how to build the whole suite.


### Build LLVM

```
cmake -S llvm -B build -C llvm/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS=clang -DCLANG_ENABLE_OPAQUE_POINTERS=OFF -G Ninja
ninja -C build
ninja -C build install
```
For development, consider to add the following option:
```
-DCMAKE_CXX_FLAGS_RELEASE="-DNDEBUG"
```

## External components

### Build utils

Go check the README.md of https://github.com/leaningtech/cheerp-utils

### Build newlib

Go check the README.md of https://github.com/leaningtech/cheerp-newlib

### Build libcxx and libcxxabi

```
cmake -S runtimes -B build_runtimes_genericjs -GNinja -C runtimes/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE="/opt/cheerp/share/cmake/Modules/CheerpToolchain.cmake"
ninja -C build_runtimes_genericjs
ninja -C build_runtimes_genericjs install

cmake -S runtimes -B build_runtimes_wasm -GNinja -C runtimes/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE="/opt/cheerp/share/cmake/Modules/CheerpWasmToolchain.cmake"
ninja -C build_runtimes_wasm
ninja -C build_runtimes_wasm install
```

### Build libs

Go check the README.md of https://github.com/leaningtech/cheerp-libs

[discord-invite]: https://discord.gg/3UfTSbWdYy

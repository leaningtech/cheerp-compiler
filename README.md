Cheerp: A C++ compiler for the Web
==================================

This is the repository for the llvm + clang components of the Cheerp compiler.

Useful links
------------

Documentation and tutorials available on github:
https://github.com/leaningtech/cheerp-meta/wiki

Please report bugs on the cheerp-meta repository:
https://github.com/leaningtech/cheerp-meta/issues

Developers chat on gitter:
https://gitter.im/leaningtech/cheerp

Or ask questions on stackoverflow:
https://stackoverflow.com/questions/ask?tags=cheerp

Cheerp compiler build instructions
----------------------------------

We advise doing an out of tree build, but it's not necessary

This is only one of the components of Cheerp. Please see https://github.com/leaningtech/cheerp-meta/wiki/Linux-build-instructions for instructions on how to build the whole suite.


### Build LLVM

```
cmake -S llvm -B build -C llvm/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS=clang -G Ninja
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
cmake -S runtimes -B build_runtimes_genericjs -GNinja -C runtimes/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER_TARGET="cheerp"
ninja -C build_runtimes_genericjs
sudo ninja -C build_runtimes_genericjs install

cmake -S runtimes -B build_runtimes_wasm -GNinja -C runtimes/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER_TARGET="cheerp-wasm"
ninja -C build_runtimes_wasm
sudo ninja -C build_runtimes_wasm install
```

### Build libs

Go check the README.md of https://github.com/leaningtech/cheerp-libs

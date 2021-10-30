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
mkdir build
cd build
cmake -C ../llvm/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS=clang -G Ninja ../llvm/
ninja
ninja install
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

### Build libcxx
Note that while building libcxx first both components have to be build (without the /opt/cheerp/include/c++/ headers being present), and then they have to be installed

```
rm -r /opt/cheerp/include/c++/
cd libcxx
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp   -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/  CheerpToolchain.cmake -DLIBCXX_ENABLE_SHARED=OFF -DLIBCXX_ENABLE_ASSERTIONS=OFF -DLIBCXX_INCLUDE_BENCHMARKS=OFF -DLIBCXX_CXX_ABI_INCLUDE_PATHS=$CHEERP_SRC/  cheerp-compiler/libcxxabi/include -DLIBCX  X_CXX_ABI=libcxxabi -DCMAKE_CXX_FLAGS="-f  exceptions" ..
make
cd ..
mkdir build_asmjs
cd build_asmjs
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp   -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/CheerpWasmToolchain.cmake -DLIBCXX_ENABLE_SHARED=OFF -DLIBCXX_ENABLE_ASSERTIONS=OFF -DLIBCXX_INCLUDE_BENCHMARKS=OFF -D  LIBCXX_CXX_ABI_INCLUDE_PATHS=$CHEERP_SRC/  cheerp-compiler/libcxxabi/include -DLIBCX  X_CXX_ABI=libcxxabi -DCMAKE_CXX_FLAGS="-f  exceptions" ..
make
cd ../build
make install
cd ../build_asmjs
make install
```

### Build libcxxabi

```
cd libcxxabi
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp   -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/  CheerpToolchain.cmake -DLIBCXXABI_ENABLE_SHARED=OFF -DLIBCXXABI_ENABLE_ASSERTIONS=  OFF -DLIBCXXABI_LIBCXX_PATH=$CHEERP_SRC/c  heerp-compiler/cheerp-libcxx/ -DLIBCXXABI  _LIBCXX_INCLUDES=$CHEERP_SRC/cheerp-compi  ler/libcxx/include -DLIBCXXABI_ENABLE_THR  EADS=0 -DLLVM_CONFIG=/opt/cheerp/bin/llvm  -config ..
make
make install
cd ..
mkdir build_asmjs
cd build_asmjs
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp   -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/  CheerpToolchain.cmake -DLIBCXXABI_ENABLE_SHARED=OFF -DLIBCXXABI_ENABLE_ASSERTIONS=  OFF -DLIBCXXABI_LIBCXX_PATH=$CHEERP_SRC/c  heerp-compiler/cheerp-libcxx/ -DLIBCXXABI  _LIBCXX_INCLUDES=$CHEERP_SRC/cheerp-compi  ler/libcxx/include -DLIBCXXABI_ENABLE_THR  EADS=0 -DLLVM_CONFIG=/opt/cheerp/bin/llvm  -config ..
make
make install
```

### Build libs

Go check the README.md of https://github.com/leaningtech/cheerp-libs

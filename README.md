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


### build LLVM

```
mkdir build
cd build
cmake -C ../llvm/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS=clang -G Ninja ../llvm/
ninja
ninja install
```

For development, consider to add the following option:
```
-DLLVM_ENABLE_ASSERTIONS=True
```

### build libcxx

This step is necessary to build the libs in https://github.com/leaningtech/cheerp-libs

```
cd libcxx
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp   -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCH  AIN_FILE=/opt/cheerp/share/cmake/Modules/  CheerpToolchain.cmake -DLIBCXX_ENABLE_SHA  RED=OFF -DLIBCXX_ENABLE_ASSERTIONS=OFF -D  LIBCXX_CXX_ABI_INCLUDE_PATHS=$CHEERP_SRC/  cheerp-compiler/libcxxabi/include -DLIBCX  X_CXX_ABI=libcxxabi -DCMAKE_CXX_FLAGS="-f  exceptions" ..
make
make install
```

### build libcxxabi

```
cd libcxxabi
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp   -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCH  AIN_FILE=/opt/cheerp/share/cmake/Modules/  CheerpToolchain.cmake -DLIBCXXABI_ENABLE_  SHARED=OFF -DLIBCXXABI_ENABLE_ASSERTIONS=  OFF -DLIBCXXABI_LIBCXX_PATH=$CHEERP_SRC/c  heerp-compiler/cheerp-libcxx/ -DLIBCXXABI  _LIBCXX_INCLUDES=$CHEERP_SRC/cheerp-compi  ler/libcxx/include -DLIBCXXABI_ENABLE_THR  EADS=0 -DLLVM_CONFIG=/opt/cheerp/bin/llvm  -config ..
make
make install
```

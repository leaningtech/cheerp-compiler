Cheerp: A C++ compiler for the Web
==================================

Please report bugs on launchpad:
https://bugs.launchpad.net/cheerp

Cheerp libcxxabi build instructions
-----------------------------------

Building the cheerp-enabled libcxx (C++ standard library) requires having the cheerp
compiler already installed in /opt/cheerp. It also require libcxx-cheerp headers.

```
git clone <libcxx-cheerp-repo> libcxx
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/CheerpToolchain.cmake -DLIBCXXABI_ENABLE_SHARED=OFF -DLIBCXXABI_ENABLE_ASSERTIONS=OFF -DLIBCXXABI_LIBCXX_PATH=$PWD/../libcxx/ -DLIBCXXABI_LIBCXX_INCLUDES=$PWD/../libcxx/include -DLIBCXXABI_ENABLE_THREADS=0 -DLLVM_CONFIG=/opt/cheerp/bin/llvm-config ..
make
make install
```

Cheerp libcxxabi build instructions (asm.js version)
-------------------------------------------------

It is recommended to create another build directory:

```
mkdir build_asmjs
cd build_asmjs
```

The rest of the instructions are the same, except for the cmake toolchain file,
which is `CheerpWasmToolchain.cmake`:

```
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/CheerpWasmToolchain.cmake -DLIBCXXABI_ENABLE_SHARED=OFF -DLIBCXXABI_ENABLE_ASSERTIONS=OFF -DLIBCXXABI_LIBCXX_PATH=$PWD/../libcxx/ -DLIBCXXABI_LIBCXX_INCLUDES=$PWD/../libcxx/include -DLIBCXXABI_ENABLE_THREADS=0 -DLLVM_CONFIG=/opt/cheerp/bin/llvm-config ..
make
make install
```

Cheerp: A C++ compiler for the Web
==================================

Please report bugs on launchpad:
https://bugs.launchpad.net/cheerp

Cheerp libcxx build instructions
--------------------------------

Building the cheerp-enabled libcxx (C++ standard library) requires having the cheerp
compiler already installed in /opt/cheerp. It also require libcxxabi-cheerp headers.

```
git clone <libcxxabi-cheerp-repo> libcxxabi
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/CheerpToolchain.cmake -DCHEERP_LINEAR_OUTPUT=asmjs -DLIBCXX_ENABLE_SHARED=OFF -DLIBCXX_ENABLE_ASSERTIONS=OFF -DLIBCXX_CXX_ABI_INCLUDE_PATHS=$PWD/../libcxxabi/include -DLIBCXX_CXX_ABI=libcxxabi -DCMAKE_CXX_FLAGS="-fexceptions" ..
make
make install
```

Cheerp libcxx build instructions (asm.js version)
-------------------------------------------------

It is recommended to create another build directory:

```
mkdir build_asmjs
cd build_asmjs
```

The rest of the instructions are the same, except for the cmake toolchain file,
which is `CheerpWasmToolchain.cmake`:

```
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/CheerpWasmToolchain.cmake -DCHEERP_LINEAR_OUTPUT=asmjs -DLIBCXX_ENABLE_SHARED=OFF -DLIBCXX_ENABLE_ASSERTIONS=OFF -DLIBCXX_CXX_ABI_INCLUDE_PATHS=$PWD/../libcxxabi/include -DLIBCXX_CXX_ABI=libcxxabi -DCMAKE_CXX_FLAGS="-fexceptions" ..
make
make install
```

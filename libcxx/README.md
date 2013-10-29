Duetto: A C++ compiler for the Web
==================================

Duetto libcxx build instructions
--------------------------------

Building the duetto-enabled libcxx (C++ standard library) requires having the duetto
compiler already installed in /opt/duetto. It also require libcxxabi-duetto headers.

```
git clone <libcxxabi-duetto-repo> libcxxabi
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/opt/duetto -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/duetto/share/cmake/Modules/DuettoToolchain.cmake -DLIBCXX_ENABLE_SHARED=OFF  -DLIBCXX_LIBCXXABI_INCLUDE_PATHS=$PWD/../libcxxabi/include -DLIBCXX_CXX_ABI=libcxxabi
make
make install
```

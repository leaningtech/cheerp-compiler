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

### Getting the Source Code and Building LLVM

The LLVM Getting Started documentation may be out of date.  The [Clang
Getting Started](http://clang.llvm.org/get_started.html) page might have more
accurate information.

This is an example work-flow and configuration to get and build the LLVM source:

1. Checkout LLVM (including related sub-projects like Clang):

     * ``git clone https://github.com/llvm/llvm-project.git``

     * Or, on windows, ``git clone --config core.autocrlf=false
    https://github.com/llvm/llvm-project.git``

2. Configure and build LLVM and Clang:

     * ``cd llvm-project``

     * ``cmake -S llvm -B build -G <generator> [options]``

        Some common build system generators are:

        * ``Ninja`` --- for generating [Ninja](https://ninja-build.org)
          build files. Most llvm developers use Ninja.
        * ``Unix Makefiles`` --- for generating make-compatible parallel makefiles.
        * ``Visual Studio`` --- for generating Visual Studio projects and
          solutions.
        * ``Xcode`` --- for generating Xcode projects.

        Some common options:

        * ``-DLLVM_ENABLE_PROJECTS='...'`` and ``-DLLVM_ENABLE_RUNTIMES='...'`` ---
          semicolon-separated list of the LLVM sub-projects and runtimes you'd like to
          additionally build. ``LLVM_ENABLE_PROJECTS`` can include any of: clang,
          clang-tools-extra, cross-project-tests, flang, libc, libclc, lld, lldb,
          mlir, openmp, polly, or pstl. ``LLVM_ENABLE_RUNTIMES`` can include any of
          libcxx, libcxxabi, libunwind, compiler-rt, libc or openmp. Some runtime
          projects can be specified either in ``LLVM_ENABLE_PROJECTS`` or in
          ``LLVM_ENABLE_RUNTIMES``.

## External components

### Build utils

Go check the README.md of https://github.com/leaningtech/cheerp-utils

### Build newlib

Go check the README.md of https://github.com/leaningtech/cheerp-newlib

### Build libcxx

        * CMake will generate targets for each tool and library, and most
          LLVM sub-projects generate their own ``check-<project>`` target.

        * Running a serial build will be **slow**.  To improve speed, try running a
          parallel build.  That's done by default in Ninja; for ``make``, use the option
          ``-j NNN``, where ``NNN`` is the number of parallel jobs to run.
          In most cases, you get the best performance if you specify the number of CPU threads you have.
          On some Unix systems, you can specify this with ``-j$(nproc)``.

      * For more information see [CMake](https://llvm.org/docs/CMake.html)

### Build libcxxabi

```
cd libcxxabi
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp   -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCH  AIN_FILE=/opt/cheerp/share/cmake/Modules/  CheerpToolchain.cmake -DLIBCXXABI_ENABLE_  SHARED=OFF -DLIBCXXABI_ENABLE_ASSERTIONS=  OFF -DLIBCXXABI_LIBCXX_PATH=$CHEERP_SRC/c  heerp-compiler/cheerp-libcxx/ -DLIBCXXABI  _LIBCXX_INCLUDES=$CHEERP_SRC/cheerp-compi  ler/libcxx/include -DLIBCXXABI_ENABLE_THR  EADS=0 -DLLVM_CONFIG=/opt/cheerp/bin/llvm  -config ..
make
make install
cd ..
mkdir build_asmjs
cd build_asmjs
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp   -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCH  AIN_FILE=/opt/cheerp/share/cmake/Modules/  CheerpToolchain.cmake -DLIBCXXABI_ENABLE_  SHARED=OFF -DLIBCXXABI_ENABLE_ASSERTIONS=  OFF -DLIBCXXABI_LIBCXX_PATH=$CHEERP_SRC/c  heerp-compiler/cheerp-libcxx/ -DLIBCXXABI  _LIBCXX_INCLUDES=$CHEERP_SRC/cheerp-compi  ler/libcxx/include -DLIBCXXABI_ENABLE_THR  EADS=0 -DLLVM_CONFIG=/opt/cheerp/bin/llvm  -config ..
make
make install
```

### Build libs

Go check the README.md of https://github.com/leaningtech/cheerp-libs

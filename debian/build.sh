#!/bin/bash

# This is an utility script for building and installing the entire Cheerp
# toolchain on Linux.
#
# This script expects that the Cheerp repositories have been placed like this:
# cheerp-compiler/
# cheerp-compiler/cheerp-musl
# cheerp-compiler/cheerp-utils
# cheerp-compiler/cheerp-libs
#
# Cheerp will be installed in $CHEERP_DEST/$CHEERP_PREFIX, you can change these
# variables to what you'd like.
#
# You can set CHEERP_DEST to an already installed Cheerp toolchain and use that
# to build parts of the toolchain separately.
#
# This script will by default try to use as many threads as possible for each
# step. If you're running out of resources while running this script, try
# setting the THREADS environment variable to something lower
#
# You'll probably just want to run this script like this to build and install
# the entire toolchain:
# ./build.sh all
# ./build.sh install

set -ex

FLAGS_RELEASE="-DNDEBUG -O2"
USE_CCACHE=Off
ENABLE_LLVM_ASSERTIONS=Off

BUILD_DIR="$PWD/build"
mkdir -p "$BUILD_DIR"

if [ -n "$CIRCLECI" ]; then
  USE_CCACHE=On
  if [ "$CIRCLE_BRANCH" != "master" ]; then
    ENABLE_LLVM_ASSERTIONS=On
  fi
  FLAGS_RELEASE="-O2"
fi

if [ -z "$CHEERP_DEST" ]; then
  export CHEERP_DEST=/
fi

if [ -z "$CHEERP_PREFIX" ]; then
  echo '$CHEERP_PREFIX not specified, defaulting to /opt/cheerp' >&2
  export CHEERP_PREFIX="/opt/cheerp"
fi

export TMP_INSTALL="$BUILD_DIR/$CHEERP_PREFIX"

if [ -z "$CHEERP_DIR" ]; then
  export CHEERP_DIR="$TMP_INSTALL"
fi

llvm_ninja_command() {
  JOBS_OPT=""
  if [ -n "$THREADS" ]; then
    JOBS_OPT="-j$THREADS"
  fi
  ninja $JOBS_OPT -v -C build_llvm "$1"
}

build_compiler() {
  cmake \
    -C llvm/CheerpCmakeConf.cmake \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_CXX_FLAGS_RELEASE="$FLAGS_RELEASE" \
    -DCLANG_VENDOR="Cheerp $deb_version" \
    -DLLVM_ENABLE_PROJECTS=clang \
    llvm/ \
    -GNinja \
    -DLLVM_ENABLE_ASSERTIONS="$ENABLE_LLVM_ASSERTIONS" \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_EXE_LINKER_FLAGS=-fuse-ld=ld \
    -DCMAKE_SHARED_LINKER_FLAGS=-fuse-ld=ld \
    -DCMAKE_MODULE_LINKER_FLAGS=-fuse-ld=ld \
    -DLLVM_CCACHE_BUILD="$USE_CCACHE" \
    -B build_llvm

  export DESTDIR="$BUILD_DIR"
  llvm_ninja_command install-distribution
  unset DESTDIR
}

prepare() {
  #This is temporary and will be overwritten after install
  cmake -B cheerp-utils/build -DCMAKE_INSTALL_PREFIX="$TMP_INSTALL" -DCHEERP_PREFIX="$CHEERP_DIR" cheerp-utils
  make -C cheerp-utils/build install
}

build_musl() {
  export RANLIB="$CHEERP_DIR/bin/llvm-ar s"
  export AR="$CHEERP_DIR/bin/llvm-ar"
  export LD="$CHEERP_DIR/bin/llvm-link"
  export CC_GENERICJS="$CHEERP_DIR/bin/clang -target cheerp"
  export CC_ASMJS="$CHEERP_DIR/bin/clang -target cheerp-wasm"
  export CFLAGS="-Wno-int-conversion"

  MUSL_GENERICJS_DIR=cheerp-musl/build_genericjs
  MUSL_ASMJS_DIR=cheerp-musl/build_asmjs

  mkdir -p "$MUSL_GENERICJS_DIR"
  (cd "$MUSL_GENERICJS_DIR" && CC="$CC_GENERICJS" ../configure --target=cheerp --disable-shared --prefix="$TMP_INSTALL" \
    --with-malloc=dlmalloc)
  make -C "$MUSL_GENERICJS_DIR" install-cheerp -j

  mkdir -p "$MUSL_ASMJS_DIR"
  (cd "$MUSL_ASMJS_DIR" && CC="$CC_ASMJS" ../configure --target=cheerp-wasm --disable-shared --prefix="$TMP_INSTALL" \
    --with-malloc=dlmalloc)
  make -C "$MUSL_ASMJS_DIR" install-bc -j

  unset CFLAGS
}

build_runtimes() {
  prepare

  LIBCXX_GENERICJS_DIR=build_runtimes_genericjs
  LIBCXX_ASMJS_DIR=build_runtimes_asmjs
  cmake -DCMAKE_INSTALL_PREFIX="$TMP_INSTALL" -S runtimes -B "$LIBCXX_GENERICJS_DIR" -GNinja -C \
    runtimes/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_TOOLCHAIN_FILE="$CHEERP_DIR/share/cmake/Modules/CheerpToolchain.cmake"

  cmake -DCMAKE_INSTALL_PREFIX="$TMP_INSTALL" -S runtimes -B "$LIBCXX_ASMJS_DIR" -GNinja -C \
    runtimes/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_TOOLCHAIN_FILE="$CHEERP_DIR/share/cmake/Modules/CheerpWasmToolchain.cmake"

  ninja -C "$LIBCXX_GENERICJS_DIR"
  ninja -C "$LIBCXX_ASMJS_DIR"

  # We can't install genericjs or asmjs before building the other, because it wouldn't then use the correct headers
  ninja -C "$LIBCXX_GENERICJS_DIR" install
  ninja -C "$LIBCXX_ASMJS_DIR" install
}


build_libraries() {
  prepare

  export INSTALL_PREFIX="$TMP_INSTALL"
  make -C cheerp-libs/webgles install CHEERP_PREFIX="$CHEERP_DIR"
  make -C cheerp-libs/wasm install CHEERP_PREFIX="$CHEERP_DIR"
  make -C cheerp-libs/stdlibs install CHEERP_PREFIX="$CHEERP_DIR"

  # SYSTEM
  SYS_GENERICJS_DIR="cheerp-libs/system/build_genericjs"
  SYS_ASMJS_DIR="cheerp-libs/system/build_asmjs"

  cmake -B "$SYS_GENERICJS_DIR" -DCMAKE_INSTALL_PREFIX="$TMP_INSTALL" \
    -DCMAKE_TOOLCHAIN_FILE="$CHEERP_DIR/share/cmake/Modules/CheerpToolchain.cmake" cheerp-libs/system
  cmake --build "$SYS_GENERICJS_DIR"
  cmake --install "$SYS_GENERICJS_DIR"

  cmake -B "$SYS_ASMJS_DIR" -DCMAKE_INSTALL_PREFIX="$TMP_INSTALL" \
    -DCMAKE_TOOLCHAIN_FILE="$CHEERP_DIR/share/cmake/Modules/CheerpWasmToolchain.cmake" cheerp-libs/system
  cmake --build "$SYS_ASMJS_DIR"
  cmake --install "$SYS_ASMJS_DIR"
}

build_compiler_rt() {
  prepare

  if [ -n "$CIRCLECI" ]; then
    LLVM_DIR_OPT="-DLLVM_DIR=$PWD/build_llvm/lib/cmake/llvm"
  fi
  LLVM_DIR_OPT="-DLLVM_DIR=$PWD/build_llvm/lib/cmake/llvm"

  cmake -S compiler-rt -B build_compiler_rt \
    -C compiler-rt/CheerpCmakeConf.cmake \
    -DCMAKE_BUILD_TYPE=Release $LLVM_DIR_OPT \
    -DCMAKE_INSTALL_PREFIX="$TMP_INSTALL" \
    -DCMAKE_TOOLCHAIN_FILE="$CHEERP_DIR/share/cmake/Modules/CheerpWasmToolchain.cmake"

  make -C build_compiler_rt install
}

install_all() {
  # cheerp-utils will install stuff with absolute paths. To fix this, during compilation we'll use a temporary cheerp-utils,
  # with the directories pointing to the build dirs, and just before installing, we'll change the paths to the actual install dir
  cmake -B cheerp-utils/build -DCMAKE_INSTALL_PREFIX="$TMP_INSTALL" -DCHEERP_PREFIX="$CHEERP_PREFIX" cheerp-utils
  make -C cheerp-utils/build install

  mkdir -p "$CHEERP_DEST"
  cp -r "$BUILD_DIR/." "$CHEERP_DEST"
  cp cheerp-utils/LICENSE.TXT "$CHEERP_DEST/$CHEERP_PREFIX/"
  cp cheerp-utils/README "$CHEERP_DEST/$CHEERP_PREFIX/"
  cp cheerp-utils/ChangeLog "$CHEERP_DEST/$CHEERP_PREFIX/"
}

build_all_libraries() {
  build_musl
  build_runtimes
  build_libraries
  build_compiler_rt
}

build_all() {
  build_compiler
  build_all_libraries
}

case "$1" in
  compiler)
    build_compiler
    ;;
  musl)
    build_musl
    ;;
  runtimes)
    build_runtimes
    ;;
  libs)
    build_libraries
    ;;
  compiler-rt)
    build_compiler_rt
    ;;
  all-libs)
    build_all_libraries
    ;;
  install)
    install_all
    ;;
  tar-compiler)
    tar -cvjf "$2" build_llvm debian/build.sh cmake third-party llvm clang .git/logs/HEAD
    ;;
  tar-install)
    tar -cvjf "$2" "$BUILD_DIR/."
    ;;
  run-command)
    llvm_ninja_command "$2"
    ;;
  all)
    build_all
    ;;
  *)
    echo "Unknown command $1" >&2
    exit 1
    ;;
esac

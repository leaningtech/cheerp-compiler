Name: cheerp-llvm-clang
Version: 2.6
Release:        1%{?dist}
Summary: A C++ compiler for the Web

License:  NCSA and MIT
URL: https://leaningtech.com/cheerp
Source0: %{NAME}_%{VERSION}.orig.tar.gz

BuildRequires: clang lld cmake ninja-build python3

%description
Cheerp is a tool to bring C++ programming to the Web. It can generate a seamless
combination of JavaScript, WebAssembly and Asm.js from a single C++ codebase.

%define debug_package %{nil}
%define circleci %{getenv:CIRCLECI}

%prep
%autosetup
mkdir -p build
cd build
cmake -C ../llvm/CheerpCmakeConf.cmake \
  -DCMAKE_INSTALL_RPATH:BOOL=";" \
  -DCMAKE_C_COMPILER=clang \
  -DCMAKE_CXX_COMPILER=clang++ \
  -DCMAKE_EXE_LINKER_FLAGS="-fuse-ld=lld" \
  -DCMAKE_SHARED_LINKER_FLAGS="-fuse-ld=lld" \
  -DCMAKE_MODULE_LINKER_FLAGS="-fuse-ld=lld" \
  -DCLANG_VENDOR="Cheerp %{VERSION}" \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_CXX_FLAGS_RELEASE="-DNDEBUG -O2" \
  -DLLVM_ENABLE_PROJECTS=clang \
  -GNinja \
  ../llvm/


%build
%if "%{circleci}" == "true"
NINJA_STATUS="[%u/%r/%f] " ninja -j${THREADS} -C build distribution
%else
NINJA_STATUS="[%u/%r/%f] " ninja -C build distribution
%endif

%check

%install
DESTDIR=%{buildroot} INSTALL="/usr/bin/install -p" ninja -C build install-distribution

%clean
rm -rf $RPM_BUILD_ROOT

%files
/opt/cheerp/

%changelog
* Wed Feb 03 2021 Yuri Iozzelli <yuri@leaningtech.com>
- Update to new monorepo
* Tue Dec 10 2019 Yuri Iozzelli <yuri@leaningtech.com>
- First RPM version

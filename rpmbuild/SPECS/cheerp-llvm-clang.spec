Name: cheerp-llvm-clang
Version: 2.6
Release:        1%{?dist}
Summary: A C++ compiler for the Web

License:  NCSA and MIT
URL: https://leaningtech.com/cheerp
Source0: %{NAME}_%{VERSION}.orig.tar.gz

BuildRequires: clang lld cmake make python3 perl

%description
Cheerp is a tool to bring C++ programming to the Web. It can generate a seamless
combination of JavaScript, WebAssembly and Asm.js from a single C++ codebase.

%define debug_package %{nil}

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
  -DCLANG_VENDOR="Cheerp %{VERSION}" \
  -DCMAKE_BUILD_TYPE=Release \
  -DLLVM_ENABLE_PROJECTS=clang \
  ../llvm/


%build
%make_build -C build

%check
%make_build -C build check
%make_build -C build check-clang-cheerp

%install
/usr/bin/make -C build install-distribution DESTDIR=%{buildroot} INSTALL="/usr/bin/install -p"

%clean
rm -rf $RPM_BUILD_ROOT

%files
/opt/cheerp/

%changelog
* Wed Feb 03 2021 Yuri Iozzelli <yuri@leaningtech.com>
- Update to new monorepo
* Tue Dec 10 2019 Yuri Iozzelli <yuri@leaningtech.com>
- First RPM version

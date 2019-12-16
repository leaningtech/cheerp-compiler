Name: cheerp-libcxx
Version: 2.1
Release:        1%{?dist}
Summary: A C++ compiler for the Web, C++ library implementation

License:  GPLv2
URL: https://leaningtech.com/cheerp
Source0: %{NAME}-%{VERSION}.tar.gz
Source1: cheerp-libcxxabi-%{VERSION}.tar.gz

BuildRequires: cmake make cheerp-llvm-clang = %{VERSION} cheerp-utils = %{VERSION} cheerp-newlib = %{VERSION}
Requires: cheerp-llvm-clang = %{VERSION} cheerp-utils = %{VERSION} cheerp-newlib = %{VERSION}

%description
Cheerp is a tool to bring C++ programming to the Web. It can generate a seamless
combination of JavaScript, WebAssembly and Asm.js from a single C++ codebase.

%define debug_package %{nil}

%prep
%autosetup
%setup -T -D -a 1

mkdir -p build_genericjs
cd build_genericjs
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/CheerpToolchain.cmake -DLIBCXX_ENABLE_SHARED=OFF -DLIBCXX_ENABLE_ASSERTIONS=OFF -DLIBCXX_CXX_ABI_INCLUDE_PATHS=$PWD/../cheerp-libcxxabi-%{VERSION}/include -DLIBCXX_CXX_ABI=libcxxabi ..

cd ..
mkdir -p build_asmjs
cd build_asmjs
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/CheerpWasmToolchain.cmake -DLIBCXX_ENABLE_SHARED=OFF -DLIBCXX_ENABLE_ASSERTIONS=OFF -DLIBCXX_CXX_ABI_INCLUDE_PATHS=$PWD/../cheerp-libcxxabi-%{VERSION}/include -DLIBCXX_CXX_ABI=libcxxabi ..

%build
%make_build -C build_genericjs
%make_build -C build_asmjs

%install
%make_install -C build_genericjs
%make_install -C build_asmjs

%clean
rm -rf $RPM_BUILD_ROOT

%files
/opt/cheerp/

%changelog
* Tue Dec 10 2019 Yuri Iozzelli <yuri@leaningtech.com>
- First RPM version

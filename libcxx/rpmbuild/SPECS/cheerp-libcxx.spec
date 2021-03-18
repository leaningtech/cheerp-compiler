Name: cheerp-libcxx
Version: 2.5
Release:        1%{?dist}
Summary: A C++ compiler for the Web, C++ library implementation

License:  GPLv2
URL: https://leaningtech.com/cheerp
Source0: %{NAME}_%{VERSION}.orig.tar.gz

BuildRequires: cmake make cheerp-llvm-clang = %{VERSION} cheerp-utils = %{VERSION} cheerp-newlib = %{VERSION} python3
Requires: cheerp-llvm-clang = %{VERSION} cheerp-utils = %{VERSION} cheerp-newlib = %{VERSION}

%description
Cheerp is a tool to bring C++ programming to the Web. It can generate a seamless
combination of JavaScript, WebAssembly and Asm.js from a single C++ codebase.

%define debug_package %{nil}

%prep
%autosetup
%setup -T -D

cd libcxx/
mkdir -p build_genericjs
cd build_genericjs
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/CheerpToolchain.cmake -DCHEERP_LINEAR_OUTPUT=asmjs -DLIBCXX_ENABLE_SHARED=OFF -DLIBCXX_ENABLE_ASSERTIONS=OFF -DLIBCXX_CXX_ABI_INCLUDE_PATHS=$PWD/../libcxxabi/include -DLIBCXX_CXX_ABI=libcxxabi -DCMAKE_CXX_FLAGS="-fexceptions" ..

cd ..
mkdir -p build_asmjs
cd build_asmjs
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/CheerpWasmToolchain.cmake -DCHEERP_LINEAR_OUTPUT=asmjs -DLIBCXX_ENABLE_SHARED=OFF -DLIBCXX_ENABLE_ASSERTIONS=OFF -DLIBCXX_CXX_ABI_INCLUDE_PATHS=$PWD/../libcxxabi/include -DLIBCXX_CXX_ABI=libcxxabi -DCMAKE_CXX_FLAGS="-fexceptions" ..

%build
%make_build -C libcxx/build_genericjs
%make_build -C libcxx/build_asmjs

%install
%make_install -C libcxx/build_genericjs
%make_install -C libcxx/build_asmjs

%clean
rm -rf $RPM_BUILD_ROOT

%files
/opt/cheerp/

%changelog
* Tue Dec 10 2019 Yuri Iozzelli <yuri@leaningtech.com>
- First RPM version

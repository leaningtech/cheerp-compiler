Name: cheerp-libcxxabi
Version: 2.5
Release:        1%{?dist}
Summary: A C++ compiler for the Web, C++ ABI implementation

License:  GPLv2
URL: https://leaningtech.com/cheerp
Source0: %{NAME}_%{VERSION}.orig.tar.gz
Source1: %{NAME}_%{VERSION}.orig-libcxx.tar.gz

BuildRequires: cmake make cheerp-llvm-clang = %{VERSION} cheerp-utils = %{VERSION} cheerp-newlib = %{VERSION} cheerp-libcxx = %{VERSION}
Requires: cheerp-llvm-clang = %{VERSION} cheerp-utils = %{VERSION} cheerp-newlib = %{VERSION} cheerp-libcxx = %{VERSION}

%description
Cheerp is a tool to bring C++ programming to the Web. It can generate a seamless
combination of JavaScript, WebAssembly and Asm.js from a single C++ codebase.

%define debug_package %{nil}

%prep
%autosetup
%setup -T -D -a 1

mkdir -p build_genericjs
cd build_genericjs
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/CheerpToolchain.cmake -DCHEERP_LINEAR_OUTPUT=asmjs -DLIBCXXABI_ENABLE_SHARED=OFF -DLIBCXXABI_ENABLE_ASSERTIONS=OFF -DLIBCXXABI_LIBCXX_PATH=$PWD/../libcxx/ -DLIBCXXABI_LIBCXX_INCLUDES=$PWD/../libcxx/include -DLIBCXXABI_ENABLE_THREADS=0 -DLLVM_CONFIG=/opt/cheerp/bin/llvm-config ..

cd ..
mkdir -p build_asmjs
cd build_asmjs
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp -DCMAKE_BUILD_TYPE=Release -DCMAKE_TOOLCHAIN_FILE=/opt/cheerp/share/cmake/Modules/CheerpWasmToolchain.cmake -DCHEERP_LINEAR_OUTPUT=asmjs -DLIBCXXABI_ENABLE_SHARED=OFF -DLIBCXXABI_ENABLE_ASSERTIONS=OFF -DLIBCXXABI_LIBCXX_PATH=$PWD/../libcxx/ -DLIBCXXABI_LIBCXX_INCLUDES=$PWD/../libcxx/include -DLIBCXXABI_ENABLE_THREADS=0 -DLLVM_CONFIG=/opt/cheerp/bin/llvm-config ..

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

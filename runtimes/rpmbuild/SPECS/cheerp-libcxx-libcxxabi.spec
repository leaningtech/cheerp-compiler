Name: cheerp-libcxx-libcxxabi
Version: 2.5
Release:        1%{?dist}
Summary: A C++ compiler for the Web, C++ library implementation

License:  GPLv2
URL: https://leaningtech.com/cheerp
Source0: %{NAME}_%{VERSION}.orig.tar.gz

BuildRequires: cmake make ninja-build cheerp-llvm-clang = %{VERSION} cheerp-utils = %{VERSION} cheerp-newlib = %{VERSION} python3
Requires: cheerp-llvm-clang = %{VERSION} cheerp-utils = %{VERSION} cheerp-newlib = %{VERSION}

%description
Cheerp is a tool to bring C++ programming to the Web. It can generate a seamless
combination of JavaScript, WebAssembly and Asm.js from a single C++ codebase.

%define debug_package %{nil}

%prep
%autosetup
%setup -T -D

cmake -S runtimes -B build_runtimes_genericjs -GNinja -C runtimes/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER_TARGET="cheerp"
cmake -S runtimes -B build_runtimes_wasm -GNinja -C runtimes/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER_TARGET="cheerp-wasm"

%build
ninja -C build_runtimes_genericjs 
ninja -C build_runtimes_wasm

%install
ninja -C build_runtimes_genericjs install
ninja -C build_runtimes_wasm install

%clean
rm -rf $RPM_BUILD_ROOT

%files
/opt/cheerp/

%changelog
* Tue Dec 10 2019 Yuri Iozzelli <yuri@leaningtech.com>
- First RPM version

Name: cheerp-core
Version: 3.0
Release: 1%{?dist}
Summary: A C++ compiler for the web

License: Apache-2.0 WITH LLVM-exception OR NCSA
URL: https://leaningtech.com/cheerp
Source0: %{NAME}_%{VERSION}.orig.tar.gz

BuildRequires: clang lld cmake ninja-build python3

%description
Cheerp is a tool to bring C++ programming to the Web. It can generate a seamless
combination of JavaScript, WebAssembly and Asm.js from a single C++ codebase.

%define debug_package %{nil}

%prep
%autosetup

%build
./debian/build.sh all

%install
echo %{buildroot}
CHEERP_DEST=%{buildroot} ./debian/build.sh install

%files
/opt/cheerp

%changelog
* Mon Jan 22 2024 Daan Meijer <daan@leaningtech.com>
- Merge packages into a single binary package using new build script
* Fri Mar 03 2023 Yuri Iozzelli <yuri@leaningtech.com>
- Update license
* Wed Feb 03 2021 Yuri Iozzelli <yuri@leaningtech.com>
- Update to new monorepo
* Tue Dec 10 2019 Yuri Iozzelli <yuri@leaningtech.com>
- First RPM version

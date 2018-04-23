Cheerp: A C++ compiler for the Web
==================================

Please report bugs on launchpad:
https://bugs.launchpad.net/cheerp

Cheerp compiler build instructions
----------------------------------

This repository is intended to be used together with the cheerp-clang
one. Please checkout cheerp-clang into the tools subdirectory, remember to
rename it as simply clang

```
cd tools
git clone <cheerp-clang-repo> clang
```

We advise doing an out of tree build, but it's not necessary

```
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/opt/cheerp -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD="CheerpBackend;CheerpWasmBackend;X86" ..
make
make install
```

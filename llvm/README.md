Duetto: A C++ compiler for the Web
==================================

Please report bugs on launchpad:
https://bugs.launchpad.net/duetto

Duetto compiler build instructions
----------------------------------

This repository is intended to be used together with the clang-duetto
one. Please checkout clang-duetto into the tools subdirectory, remember to
rename it as simply clang

```
cd tools
git clone <clang-duetto-repo> clang
```

We advise doing an out of tree build, but it's not necessary

```
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=/opt/duetto -DCMAKE_BUILD_TYPE=Release ..
make
make install
```

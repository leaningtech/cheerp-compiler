Cheerp: A C++ compiler for the Web
==================================

This is the repository for the llvm component of the Cheerp compiler.
The matching clang component is at https://github.com/leaningtech/cheerp-clang

Useful links
------------

Documentation and tutorials available on github:
https://github.com/leaningtech/cheerp-meta/wiki

Please report bugs on the cheerp-meta repository:
https://github.com/leaningtech/cheerp-meta/issues

Developers chat on gitter:
https://gitter.im/leaningtech/cheerp

Or ask questions on stackoverflow:
https://stackoverflow.com/questions/ask?tags=cheerp

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
cmake -C ../llvm/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS=clang ../llvm/
make
make install
```

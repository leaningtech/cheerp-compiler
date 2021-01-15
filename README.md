Cheerp: A C++ compiler for the Web
==================================

This is the repository for the llvm + clang components of the Cheerp compiler.

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

We advise doing an out of tree build, but it's not necessary

```
mkdir build
cd build
cmake -C ../llvm/CheerpCmakeConf.cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_ENABLE_PROJECTS=clang -G Ninja ../llvm/
ninja
ninja install
```

For development, consider to add the following option:
```
-DLLVM_ENABLE_ASSERTIONS=True
```

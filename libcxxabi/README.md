Duetto: A C++ compiler for the Web
==================================

Duetto libcxxabi build instructions
-----------------------------------

```
cd lib
export CXX="/opt/duetto/bin/clang -target duetto -emit-llvm"
./buildit
cp libcxxabi.bc /opt/duetto/lib/
```

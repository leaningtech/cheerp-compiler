Duetto: A C++ compiler for the Web
==================================

Please report bugs on launchpad:
https://bugs.launchpad.net/duetto

Duetto libcxxabi build instructions
-----------------------------------

```
cd lib
export CXX="/opt/duetto/bin/clang -target duetto"
./buildit
cp libcxxabi.bc /opt/duetto/lib/
```

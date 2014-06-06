Cheerp: A C++ compiler for the Web
==================================

Please report bugs on launchpad:
https://bugs.launchpad.net/cheerp

Cheerp libcxxabi build instructions
-----------------------------------

```
cd lib
export CXX="/opt/cheerp/bin/clang -target cheerp"
./buildit
cp libcxxabi.bc /opt/cheerp/lib/
```

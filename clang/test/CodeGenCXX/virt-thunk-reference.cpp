// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm-only %s

struct A { int a; virtual void aa(int&); };
struct B { int b; virtual void bb(int&); };
struct C : A,B { virtual void aa(int&), bb(int&); };
void C::aa(int&) {}
void C::bb(int&) {}

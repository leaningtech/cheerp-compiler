// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm -o - %s
struct A;
struct B;
extern A *f();
void a() { (B *) f(); }

// RUN: %clang_cc1 -triple %itanium_abi_triple %s -emit-llvm -o %t

extern void go(const void *p);
float v[2] = { 0.0, 1.0 };
void foo(void) { go(v); }


// RUN: %clang_cc1 -triple %itanium_abi_triple %s -emit-llvm -o - | opt -O3 | llc | \
// RUN:    not grep _foo2

void foo(void) __asm__("foo2");

void bar(void) {
  foo();
}

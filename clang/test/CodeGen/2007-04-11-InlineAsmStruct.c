// RUN: %clang_cc1 -triple %itanium_abi_triple %s -emit-llvm -o -

struct V { short X, Y; };
int bar(void) {
  struct V bar;
  __asm__ volatile("foo %0\n" : "=r"(bar));
  return bar.X;
}

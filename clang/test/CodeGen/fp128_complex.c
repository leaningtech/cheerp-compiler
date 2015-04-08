// RUN: %clang -target aarch64-linux-gnuabi %s -S -emit-llvm -o - | FileCheck %s

_Complex long double a, b, c, d;
void test_fp128_compound_assign(void) {
  // CHECK: call %complex._ZTSe @__multc3
  a *= b;
  // CHECK: call %complex._ZTSe @__divtc3
  c /= d;
}

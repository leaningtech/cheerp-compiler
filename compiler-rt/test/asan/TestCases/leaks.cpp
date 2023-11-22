// Test for LeakSanitizer+AddressSanitizer of different sizes.
// REQUIRES: leak-detection
//
// RUN: %clangxx_asan -DLEAK_AMOUNT=0 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DLEAK_AMOUNT=1 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DLEAK_AMOUNT=1000 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DLEAK_AMOUNT=1000000 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DLEAK_AMOUNT=0 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DLEAK_AMOUNT=1 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DLEAK_AMOUNT=1000 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DLEAK_AMOUNT=1000000 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// CHEERPASAN: TODO make test with 10000000 work

#include <cstdlib>
#include <stdio.h>
#include <thread>
int *t;

__attribute__((noopt)) void leak(int n) {
#if defined(__ANDROID__) || defined(__BIONIC__)
  // Bionic does not acutally allocate when n==0, hence
  // there would not be a leak.
  // Re-adjust n so the test can pass.
  if (n == 0)
    n = 1;
#endif

  // Repeat few times to make sure that at least one pointer is
  // not somewhere on the stack.
  for (int i = 0; i < 10; ++i) {
    t = new int[n];
    printf("t: %p\n", t);
    t = 0;
  }
}

int main(int argc, char **argv) {
  leak(LEAK_AMOUNT);
  exit(0);
}
// CHECK: LeakSanitizer: detected memory leaks

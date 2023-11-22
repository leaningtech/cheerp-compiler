// Sanity checking a test in pure C.
// RUN: %clang_asan -O2 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clang_asan -cheerp-linear-output=asmjs -O2 %s -o %t && not %run %t 2>&1 | FileCheck %s

// REQUIRES: stable-runtime

#include <stdlib.h>
int main() {
  char *x = (char*)malloc(10 * sizeof(char));
  free(x);
  return x[5];
  // CHECK: heap-use-after-free
  // CHECK: free
  // CHECK: main
  // CHECK: malloc
  // CHECK: main
}

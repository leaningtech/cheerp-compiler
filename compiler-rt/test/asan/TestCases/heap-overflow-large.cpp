// Regression test for
// https://code.google.com/p/address-sanitizer/issues/detail?id=183

// RUN: %clangxx_asan -DOFFSET=12 -O2 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DOFFSET=1000 -O2 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DOFFSET=100000 -O2 %s -o %t && not %run %t 2>&1 | FileCheck %s

// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DOFFSET=12 -O2 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DOFFSET=1000 -O2 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DOFFSET=100000 -O2 %s -o %t && not %run %t 2>&1 | FileCheck %s

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
  fprintf(stderr, "main\n");
  int *x = new int[5];
  memset(x, 0, sizeof(x[0]) * 5);
  unsigned res = x[OFFSET];
  // CHECK: main
  // CHECK-NOT: CHECK failed
  delete[] x;
  return (res % 10) + 1;
}

// RUN: %clangxx_asan -O0 -DHEAP=0 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -O0 -DHEAP=1 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O0 -DHEAP=0 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O0 -DHEAP=1 %s -o %t && not %run %t 2>&1 | FileCheck %s
#include <string.h>
char g[21];
char *x;

int main(int argc, char **argv) {
  if (HEAP)
    x = new char[21];
  else
    x = &g[0];
  memset(x, 0, 21);
  int *y = (int*)x;
  return y[5];
}
// CHECK: 0 bytes after

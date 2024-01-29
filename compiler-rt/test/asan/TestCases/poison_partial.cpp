// RUN: %clangxx_asan -O0 %s -o %t
// RUN: not %run %t 2>&1 | FileCheck %s
// RUN: not %run %t --cheerp-arg=heap 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O0 %s -o %t
// RUN: not %run %t 2>&1 | FileCheck %s
// RUN: not %run %t --cheerp-arg=heap 2>&1 | FileCheck %s
#include <string.h>
char g[21];
char *x;

int main(int argc, char **argv) {
  if (argc >= 1)
    x = new char[21];
  else
    x = &g[0];
  memset(x, 0, 21);
  int *y = (int*)x;
  return y[5];
}
// CHECK: 0 bytes after

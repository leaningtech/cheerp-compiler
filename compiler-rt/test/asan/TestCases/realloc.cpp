// Default is true (free on realloc to 0 size)
// RUN: %clangxx_asan -O0 %s -o %t && %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O0 %s -o %t && %run %t 2>&1 | FileCheck %s

#include <stdio.h>
#include <stdlib.h>

int main() {
  void *p = malloc(42);
  p = realloc(p, 0);
  if (p) {
    // NO-FREE: Allocated something on realloc(p, 0)
    fprintf(stderr, "Allocated something on realloc(p, 0)\n");
  } else {
    // CHECK: realloc(p, 0) returned nullptr
    fprintf(stderr, "realloc(p, 0) returned nullptr\n");
  }
  free(p);
}

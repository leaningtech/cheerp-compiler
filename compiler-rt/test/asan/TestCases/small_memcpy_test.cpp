// Test that small memcpy works correctly.

// RUN: %clangxx_asan %s -o %t
// RUN: not %run %t --cheerp-arg=8  --cheerp-arg=24 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: not %run %t --cheerp-arg=16 --cheerp-arg=32 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: not %run %t --cheerp-arg=24 --cheerp-arg=40 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: not %run %t --cheerp-arg=32 --cheerp-arg=48 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: not %run %t --cheerp-arg=40 --cheerp-arg=56 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: not %run %t --cheerp-arg=48 --cheerp-arg=64 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %clangxx_asan -cheerp-linear-output=asmjs %s -o %t
// RUN: not %run %t --cheerp-arg=8  --cheerp-arg=24 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: not %run %t --cheerp-arg=16 --cheerp-arg=32 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: not %run %t --cheerp-arg=24 --cheerp-arg=40 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: not %run %t --cheerp-arg=32 --cheerp-arg=48 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: not %run %t --cheerp-arg=40 --cheerp-arg=56 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: not %run %t --cheerp-arg=48 --cheerp-arg=64 2>&1 | FileCheck %s --check-prefix=CHECK
// REQUIRES: shadow-scale-3
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <sanitizer/asan_interface.h>

int main(int argc, char **argv) {
  assert(argc == 2);
  size_t poison_from = atoi(argv[0]);
  size_t poison_to = atoi(argv[1]);
  assert(poison_from <= poison_to);
  char A1[64], A2[64];
  fprintf(stderr, "%zd %zd\n", poison_from, poison_to - poison_from);
  __asan_poison_memory_region(&A1[0] + poison_from, poison_to - poison_from);
  memcpy(A1, A2, sizeof(A1));
// CHECK: AddressSanitizer: use-after-poison
  return 0;
}

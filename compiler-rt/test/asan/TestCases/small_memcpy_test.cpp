// Test that small memcpy works correctly.

// RUN: %clangxx_asan -DPOISON_FROM=8  -DPOISON_TO=24 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %clangxx_asan -DPOISON_FROM=16 -DPOISON_TO=32 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %clangxx_asan -DPOISON_FROM=24 -DPOISON_TO=40 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %clangxx_asan -DPOISON_FROM=32 -DPOISON_TO=48 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %clangxx_asan -DPOISON_FROM=40 -DPOISON_TO=56 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %clangxx_asan -DPOISON_FROM=48 -DPOISON_TO=64 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DPOISON_FROM=8  -DPOISON_TO=24 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DPOISON_FROM=16 -DPOISON_TO=32 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DPOISON_FROM=24 -DPOISON_TO=40 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DPOISON_FROM=32 -DPOISON_TO=48 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DPOISON_FROM=40 -DPOISON_TO=56 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DPOISON_FROM=48 -DPOISON_TO=64 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK
// REQUIRES: shadow-scale-3
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <sanitizer/asan_interface.h>

int main(int argc, char **argv) {
  volatile size_t poison_from = POISON_FROM;
  volatile size_t poison_to = POISON_TO;
  assert(poison_from <= poison_to);
  char A1[64], A2[64];
  fprintf(stderr, "%zd %zd\n", poison_from, poison_to - poison_from);
  __asan_poison_memory_region(&A1[0] + poison_from, poison_to - poison_from);
  memcpy(A1, A2, sizeof(A1));
// CHECK: AddressSanitizer: use-after-poison
  return 0;
}

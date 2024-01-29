// RUN: %clangxx_asan -O0 %s -o %t -mllvm -asan-detect-invalid-pointer-pair

// RUN: %run %t --cheerp-env=ASAN_OPTIONS=detect_invalid_pointer_pairs=2:halt_on_error=0 2>&1 | FileCheck %s

#include <assert.h>
#include <stdlib.h>

int foo(char *p, char *q) {
  return p - q;
}

char global1[100] = {}, global2[100] = {};

int main() {
  // Heap allocated memory.
  char *heap1 = (char *)malloc(42);
  char *heap2 = (char *)malloc(42);

  // CHECK: ERROR: AddressSanitizer: invalid-pointer-pair
  // CHECK: #{{[0-9]+ .*}} in {{.*}}main {{.*}}
  foo(heap1, heap2);

  // Global variables.
  // CHECK: ERROR: AddressSanitizer: invalid-pointer-pair
  // CHECK: #{{[0-9]+ .*}} in {{.*}}main {{.*}}
  foo(&global1[0], &global2[10]);

  // Stack variables.
  char stack1, stack2;
  // CHECK: ERROR: AddressSanitizer: invalid-pointer-pair
  // CHECK: #{{[0-9]+ .*}} in {{.*}}main {{.*}}
  foo(&stack1, &stack2);

  // Mixtures.
  // CHECK: ERROR: AddressSanitizer: invalid-pointer-pair
  // CHECK: #{{[0-9]+ .*}} in {{.*}}main {{.*}}
  foo(heap1, &stack1);
  // CHECK: ERROR: AddressSanitizer: invalid-pointer-pair
  // CHECK: #{{[0-9]+ .*}} in {{.*}}main {{.*}}
  foo(heap1, &global1[0]);
  // CHECK: ERROR: AddressSanitizer: invalid-pointer-pair
  // CHECK: #{{[0-9]+ .*}} in {{.*}}main {{.*}}
  foo(&stack1, &global1[0]);

  free(heap1);
  free(heap2);
  return 0;
}

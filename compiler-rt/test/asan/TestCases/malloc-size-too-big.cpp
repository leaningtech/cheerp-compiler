// RUN: %clangxx_asan -O0 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O0 %s -o %t && not %run %t 2>&1 | FileCheck %s

// REQUIRES: stable-runtime

#include <stdio.h>
#include <stdlib.h>

static const size_t kMaxAllowedMallocSizePlusOne =
#if __LP64__ || defined(_WIN64)
    (1ULL << 40) + 1;
#else
    (3UL << 30) + 1;
#endif

int main() {
  void *p = malloc(kMaxAllowedMallocSizePlusOne);
  // CHECK: {{ERROR: AddressSanitizer: requested allocation size .* \(.* after adjustments for alignment, red zones etc\.\) exceeds maximum supported size}}
  // CHECK: {{#0 0x.* in .*malloc}}
  // CHECK: {{#1 0x.* in .*main}}
  // CHECK: SUMMARY: AddressSanitizer: allocation-size-too-big

  printf("malloc returned: %zu\n", (size_t)p);
  // CHECK-NULL: malloc returned: 0

  return 0;
}

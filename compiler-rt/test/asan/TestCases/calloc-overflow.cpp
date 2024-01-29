// RUN: %clangxx_asan -O0 %s -o %t && not %run %t 2>&1 | FileCheck %s

// REQUIRES: stable-runtime

#include <stdio.h>
#include <stdlib.h>

int main() {
  void *p = calloc(-1, 1000);
  // CHECK: {{ERROR: AddressSanitizer: calloc parameters overflow: count \* size \(.* \* 1000\) cannot be represented in type size_t}}
  // CHECK: {{#0 0x.* in .*calloc}}
  // CHECK: {{#1 0x.* in .*main}}
  // CHECK: SUMMARY: AddressSanitizer: calloc-overflow

  printf("calloc returned: %zu\n", (size_t)p);

  return 0;
}

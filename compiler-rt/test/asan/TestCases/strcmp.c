// RUN: %clang_asan %s -o %t && not %run %t 2>&1 | FileCheck %s

#include <assert.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
  char s1[] = "abcd";
  char s2[] = "1234";
  assert(strcmp(s1, s2) > 0);
  assert(strcmp(s1 - 1, s2));

  // CHECK: {{.*ERROR: AddressSanitizer: stack-buffer-underflow on address}}
  // CHECK: READ of size 1
  return 0;
}

// Also works if no malloc context is available.
// RUN: %clangxx_asan -O0 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O0 %s -o %t && not %run %t 2>&1 | FileCheck %s
// REQUIRES: stable-runtime

#include <stdlib.h>
#include <string.h>
int main(int argc, char **argv) {
  ++argc;
  char *x = (char*)malloc(10 * sizeof(char));
  memset(x, 0, 10);
  int res = x[argc];
  free(x + 5);  // BOOM
  // CHECK: AddressSanitizer: attempting free on address{{.*}}in thread T0
  // CHECK: is located 5 bytes inside of 10-byte region
  // CHECK: allocated by thread T0 here:
  return res;
}

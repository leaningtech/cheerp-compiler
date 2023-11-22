// RUN: %clangxx_asan -O0 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -O1 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -O2 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -O3 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O0 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O1 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O3 %s -o %t && not %run %t 2>&1 | FileCheck %s

// FIXME: We fail to intercept strdup with the dynamic WinASan RTL, so it's not
// in the stack trace.
// XFAIL: win32-dynamic-asan

#include <string.h>

char kString[] = "foo";

int main(int argc, char **argv) {
  ++argc;
  char *copy = strdup(kString);
  int x = copy[4 + argc];  // BOOM
  // CHECK: AddressSanitizer: heap-buffer-overflow
  // CHECK: #0 {{.*}}main
  // CHECK-LABEL: allocated by thread T{{.*}} here:
  // CHECK: #{{[01]}} {{.*}}strdup
  // CHECK: #{{.*}}main
  // CHECK-LABEL: SUMMARY
  return x;
}

// RUN: %clangxx_asan -O0 -fno-builtin %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -O1 -fno-builtin %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -O2 -fno-builtin %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -O3 -fno-builtin %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O0 -fno-builtin %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O1 -fno-builtin %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -fno-builtin %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O3 -fno-builtin %s -o %t && not %run %t 2>&1 | FileCheck %s

// UNSUPPORTED: android

#include <string.h>


// Don't inline function otherwise stacktrace changes.
__attribute__((noinline)) void bad_function() {
  char buffer[] = "hello";
  // CHECK: strncpy-param-overlap: memory ranges
  // CHECK: [{{0x.*,[ ]*0x.*}}) and [{{0x.*,[ ]*0x.*}}) overlap
  // CHECK: {{#0 0x.* in .*strncpy}}
  // CHECK: {{#1 0x.* in .*bad_function}}
  // CHECK: {{#2 0x.* in .*main}}
  strncpy(buffer, buffer + 1, 5); // BOOM
}

int main(int argc, char **argv) {
  bad_function();
  return 0;
}

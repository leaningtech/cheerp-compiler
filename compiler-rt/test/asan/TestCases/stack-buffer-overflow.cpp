// RUN: %clangxx_asan -O0 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -O1 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -O2 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -O3 %s -o %t && not %run %t 2>&1 | FileCheck %s

#include <string.h>
int main(int argc, char **argv) {
  ++argc;
  volatile char x[10];
  memset((char*) x, 0, 10);
  int res = x[argc * 10];  // BOOOM
  // CHECK: {{READ of size 1 at 0x.* thread T0}}
  // CHECK: {{    #0 0x.* in .*main }}
  // CHECK: {{Address 0x.* is located in stack of thread T0 at offset}}
  // CHECK-NEXT: in{{.*}}main{{.*}}
  return res;
}

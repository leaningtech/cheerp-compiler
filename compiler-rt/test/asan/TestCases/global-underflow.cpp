// RUN: %clang_asan -O0 %s %p/Helpers/underflow.c -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clang_asan -O1 %s %p/Helpers/underflow.c -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clang_asan -O2 %s %p/Helpers/underflow.c -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clang_asan -O3 %s %p/Helpers/underflow.c -o %t && not %run %t 2>&1 | FileCheck %s

int XXX[2] = {2, 3};
extern int YYY[];
#include <string.h>
int main(int argc, char **argv) {
  memset(XXX, 0, 2*sizeof(int));
  // CHECK: {{READ of size 4 at 0x.* thread}}
  // CHECK: {{    #0 0x.* in .*main}}
  // CHECK: {{0x.* is located 4 bytes before global variable}}
  // CHECK:   {{.*YYY.* of size 12}}
  int res = YYY[-1];
  return res;
}

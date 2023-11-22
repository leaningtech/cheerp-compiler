// RUN: %clangxx_asan -O0 -DTEST=0 %s -o %t && not %run %t A 2>&1 | FileCheck --check-prefix=CHECK-A %s
// RUN: %clangxx_asan -O0 -DTEST=1 %s -o %t && not %run %t B 2>&1 | FileCheck --check-prefix=CHECK-B %s
// RUN: %clangxx_asan -O0 -DTEST=2 %s -o %t && not %run %t C 2>&1 | FileCheck --check-prefix=CHECK-C %s
// RUN: %clangxx_asan -O0 -DTEST=3 %s -o %t && not %run %t D 2>&1 | FileCheck --check-prefix=CHECK-D %s
// RUN: %clangxx_asan -O0 -DTEST=4 %s -o %t && not %run %t E 2>&1 | FileCheck --check-prefix=CHECK-E %s
// RUN: %clangxx_asan -O0 -DTEST=5 %s -o %t && not %run %t K 2>&1 | FileCheck --check-prefix=CHECK-K %s
// RUN: %clangxx_asan -O0 -DTEST=6 %s -o %t && not %run %t L 2>&1 | FileCheck --check-prefix=CHECK-L %s
// RUN: %clangxx_asan -O0 -DTEST=7 %s -o %t && not %run %t M 2>&1 | FileCheck --check-prefix=CHECK-M %s
// RUN: %clangxx_asan -O0 -DTEST=8 %s -o %t && not %run %t N 2>&1 | FileCheck --check-prefix=CHECK-N %s
// RUN: %clangxx_asan -O0 -DTEST=9 %s -o %t && not %run %t O 2>&1 | FileCheck --check-prefix=CHECK-O %s

#include <sanitizer/asan_interface.h>

#include <stdlib.h>
#include <string.h>
int main(int argc, char **argv) {
  char *x = new char[16];
  memset(x, 0xab, 16);
  int res = 1;
  switch (TEST) {
    case 0: res = __sanitizer_unaligned_load16(x + 15); break;
//  CHECK-A ERROR: AddressSanitizer: heap-buffer-overflow on address
//  CHECK-A: {{.*}}main{{.*}}
//  CHECK-A: is located 0 bytes after 16-byte region
    case 1: res = __sanitizer_unaligned_load32(x + 14); break;
//  CHECK-B: {{.*}}main{{.*}}
    case 2: res = __sanitizer_unaligned_load32(x + 13); break;
//  CHECK-C: {{.*}}main{{.*}}
    case 3: res = __sanitizer_unaligned_load64(x + 15); break;
//  CHECK-D: {{.*}}main{{.*}}
    case 4: res = __sanitizer_unaligned_load64(x + 9); break;
//  CHECK-E: {{.*}}main{{.*}}

    case 5: __sanitizer_unaligned_store16(x + 15, 0); break;
//  CHECK-K ERROR: AddressSanitizer: heap-buffer-overflow on address
//  CHECK-K: {{.*}}main{{.*}}
//  CHECK-K: is located 0 bytes after 16-byte region
    case 6: __sanitizer_unaligned_store32(x + 15, 0); break;
//  CHECK-L: {{.*}}main{{.*}}
    case 7: __sanitizer_unaligned_store32(x + 13, 0); break;
//  CHECK-M: {{.*}}main{{.*}}
    case 8: __sanitizer_unaligned_store64(x + 10, 0); break;
//  CHECK-N: {{.*}}main{{.*}}
    case 9: __sanitizer_unaligned_store64(x + 14, 0); break;
//  CHECK-O: {{.*}}main{{.*}}
  }
  delete[] x;
  return res;
}

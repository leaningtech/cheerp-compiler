// RUN: %clangxx_asan -O2 %s -o %t -DCHECK=0 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK --check-prefix=GLOB
// RUN: %clangxx_asan -O2 %s -o %t -DCHECK=1 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK --check-prefix=CLASS_STATIC
// RUN: %clangxx_asan -O2 %s -o %t -DCHECK=2 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK --check-prefix=FUNC_STATIC
// RUN: %clangxx_asan -O2 %s -o %t -DCHECK=3 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK --check-prefix=LITERAL
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 %s -o %t -DCHECK=0 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK --check-prefix=GLOB
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 %s -o %t -DCHECK=1 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK --check-prefix=CLASS_STATIC
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 %s -o %t -DCHECK=2 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK --check-prefix=FUNC_STATIC
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 %s -o %t -DCHECK=3 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK --check-prefix=LITERAL

// COFF doesn't support debuginfo for globals. For the non-debuginfo tests, see global-location-nodebug.cpp.
// XFAIL: windows-msvc

// atos doesn't show source line numbers for global variables.
// UNSUPPORTED: darwin, cheerp

// CHECK: AddressSanitizer: global-buffer-overflow

#include <string.h>

struct C {
  static int array[10];
  // CLASS_STATIC:      0x{{.*}} is located 4 bytes after global variable 'C::array' defined in '{{.*}}global-location.cpp:[[@LINE-1]]' {{.*}} of size 40
};

int global[10];
// GLOB:      0x{{.*}} is located 4 bytes after global variable 'global' defined in '{{.*}}global-location.cpp:[[@LINE-1]]' {{.*}} of size 40
int C::array[10];

int main(int argc, char **argv) {
  int one = argc + 1;
  volatile int check = CHECK;
  switch (check) {
  case 0: return global[one * 11];
  case 1: return C::array[one * 11];
  case 2:
    static int array[10];
    // FUNC_STATIC:      0x{{.*}} is located 4 bytes after global variable 'main::array' defined in '{{.*}}global-location.cpp:[[@LINE-1]]' {{.*}} of size 40
    memset(array, 0, 10);
    return array[one * 11];
  case 3:
    const char *str = "0123456789";
    // LITERAL:      0x{{.*}} is located 0 bytes after global variable {{.*}} defined in '{{.*}}global-location.cpp:[[@LINE-1]]' {{.*}} of size 11
    return str[one * 11];
  }
  return 0;
}

// CHECK: SUMMARY: AddressSanitizer: global-buffer-overflow

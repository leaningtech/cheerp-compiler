// RUN: %clangxx_asan -O0 %s -o %t
// RUN: not %run %t 2>&1 | FileCheck %s --check-prefix=SOURCE
// RUN: not %run %t --cheerp-env=ASAN_OPTIONS=symbolize=false 2>&1 | FileCheck %s --check-prefix=MODULE
// RUN: not %run %t --cheerp-env=ASAN_OPTIONS=print_summary=false 2>&1 | FileCheck %s --check-prefix=MISSING

int main() {
  char *x = new char[20];
  delete[] x;
  return x[0];
  // SOURCE: ERROR: AddressSanitizer: heap-use-after-free
  // SOURCE: SUMMARY: AddressSanitizer: heap-use-after-free {{.*}}
  // MODULE: ERROR: AddressSanitizer: heap-use-after-free
  // MODULE: SUMMARY: AddressSanitizer: heap-use-after-free ({{.*}}+0x{{.*}})
  // MISSING: ERROR: AddressSanitizer: heap-use-after-free
  // MISSING-NOT: SUMMARY
}

// RUN: %clangxx_asan -O0 %s -o %t
// RUN: %run %t --cheerp-env=ASAN_OPTIONS=help=1 2>&1 | FileCheck %s

int main() {
}

// CHECK: Available flags for AddressSanitizer:
// CHECK-DAG: handle_segv
// CHECK-DAG: check_initialization_order

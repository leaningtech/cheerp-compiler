// RUN: %clangxx_asan -O0 %s -o %t
// RUN: %env_asan_opts=help=1 %run %t 2>&1 | FileCheck %s

// UNSUPPORTED: cheerp

int main() {
}

// CHECK: Available flags for AddressSanitizer:
// CHECK-DAG: handle_segv
// CHECK-DAG: check_initialization_order

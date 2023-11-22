// RUN: %clangxx_asan -O2 -DCHECK=0 -fsanitize-address-outline-instrumentation %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_0_BYTES
// RUN: %clangxx_asan -O2 -DCHECK=1 -fsanitize-address-outline-instrumentation %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_0_BYTES
// RUN: %clangxx_asan -O2 -DCHECK=2 -fsanitize-address-outline-instrumentation %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_1_BYTES
// RUN: %clangxx_asan -O2 -DCHECK=3 -fsanitize-address-outline-instrumentation %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_1_BYTES
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -DCHECK=0 -fsanitize-address-outline-instrumentation %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_0_BYTES
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -DCHECK=1 -fsanitize-address-outline-instrumentation %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_0_BYTES
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -DCHECK=2 -fsanitize-address-outline-instrumentation %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_1_BYTES
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -DCHECK=3 -fsanitize-address-outline-instrumentation %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_1_BYTES

// RUN: %clangxx_asan -O2 -DCHECK=0 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-recover=1 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_0_BYTES
// RUN: %clangxx_asan -O2 -DCHECK=1 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-recover=1 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_0_BYTES
// RUN: %clangxx_asan -O2 -DCHECK=2 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-recover=1 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_1_BYTES
// RUN: %clangxx_asan -O2 -DCHECK=3 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-recover=1 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_1_BYTES
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -DCHECK=0 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-recover=1 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_0_BYTES
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -DCHECK=1 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-recover=1 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_0_BYTES
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -DCHECK=2 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-recover=1 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_1_BYTES
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -DCHECK=3 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-recover=1 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_1_BYTES

// RUN: %clangxx_asan -O2 -DCHECK=0 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-force-experiment=42 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_0_BYTES
// RUN: %clangxx_asan -O2 -DCHECK=1 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-force-experiment=42 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_0_BYTES
// RUN: %clangxx_asan -O2 -DCHECK=2 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-force-experiment=42 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_1_BYTES
// RUN: %clangxx_asan -O2 -DCHECK=3 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-force-experiment=42 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_1_BYTES
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -DCHECK=0 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-force-experiment=42 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_0_BYTES
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -DCHECK=1 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-force-experiment=42 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_0_BYTES
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -DCHECK=2 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-force-experiment=42 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_1_BYTES
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O2 -DCHECK=3 -fsanitize-address-outline-instrumentation %s -o %t -mllvm -asan-force-experiment=42 && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK_1_BYTES
// CHECK_0_BYTES: ERROR: AddressSanitizer: global-buffer-overflow on address [[ADDR:.*]] at
// CHECK_0_BYTES: [[ADDR]] is located 0 bytes after

// CHECK_1_BYTES: ERROR: AddressSanitizer: global-buffer-overflow on address [[ADDR:.*]] at
// CHECK_1_BYTES: [[ADDR]] is located 1 bytes after

#include <sanitizer/asan_interface.h>

#include <stdlib.h>
#include <string.h>

static int64_t mem = -1;
static int64_t *volatile G = &mem;

inline uint16_t UNALIGNED_LOAD(const void *p) {
  uint16_t data;
  memcpy(&data, p, sizeof data);
  return data;
}

inline void UNALIGNED_STORE(uint16_t data, void *p) {
  memcpy(p, &data, sizeof data);
}

int main(int argc, char **argv) {
  int res = 1;
  switch (CHECK) {
  case 0:
    res = UNALIGNED_LOAD(reinterpret_cast<char *>(G) + 7);
    break;
  case 1:
    UNALIGNED_STORE(0, reinterpret_cast<char *>(G) + 7);
    break;
  case 2:
    res = UNALIGNED_LOAD(reinterpret_cast<char *>(G) + 9);
    break;
  case 3:
    UNALIGNED_STORE(0, reinterpret_cast<char *>(G) + 9);
    break;
  }
  return res;
}

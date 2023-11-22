// RUN: %clangxx_asan -O1 -DFRAME=0 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK0
// RUN: %clangxx_asan -O1 -DFRAME=1 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK1
// RUN: %clangxx_asan -O1 -DFRAME=2 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK2
// RUN: %clangxx_asan -O1 -DFRAME=3 %s -o %t && not %run %t 2>&1 | FileCheck %s --check-prefix=CHECK3

#define NOINLINE __attribute__((noinline))
inline void break_optimization(void *arg) {
  __asm__ __volatile__("" : : "r"(reinterpret_cast<unsigned>(arg)) : "memory");
}

NOINLINE static void Frame0(int frame, char *a, char *b, char *c) {
  char s[4] = {0};
  char *d = s;
  break_optimization(&d);
  switch (frame) {
    case 3: a[5]++; break;
    case 2: b[5]++; break;
    case 1: c[5]++; break;
    case 0: d[5]++; break;
  }
}
NOINLINE static void Frame1(int frame, char *a, char *b) {
  char c[4] = {0}; Frame0(frame, a, b, c);
  break_optimization(0);
}
NOINLINE static void Frame2(int frame, char *a) {
  char b[4] = {0}; Frame1(frame, a, b);
  break_optimization(0);
}
NOINLINE static void Frame3(int frame) {
  char a[4] = {0}; Frame2(frame, a);
  break_optimization(0);
}

int main(int argc, char **argv) {
  Frame3(FRAME);
}

// CHECK0: AddressSanitizer: stack-buffer-overflow
// CHECK0: #0{{.*}}Frame0
// CHECK0: #1{{.*}}Frame1
// CHECK0: #2{{.*}}Frame2
// CHECK0: #3{{.*}}Frame3
// CHECK0: is located in stack of thread T0 at offset
//
// CHECK1: AddressSanitizer: stack-buffer-overflow
// CHECK1: is located in stack of thread T0 at offset
//
// CHECK2: AddressSanitizer: stack-buffer-overflow
// CHECK2: is located in stack of thread T0 at offset
//
// CHECK3: AddressSanitizer: stack-buffer-overflow
// CHECK3: is located in stack of thread T0 at offset

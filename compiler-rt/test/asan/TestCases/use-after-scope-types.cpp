// RUN: %clangxx_asan -DCOUNT=1 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DCOUNT=2 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DCOUNT=3 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DCOUNT=4 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DCOUNT=5 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DCOUNT=6 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DCOUNT=7 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DCOUNT=8 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DCOUNT=9 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -DCOUNT=10 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DCOUNT=1 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DCOUNT=2 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DCOUNT=3 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DCOUNT=4 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DCOUNT=5 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DCOUNT=6 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DCOUNT=7 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DCOUNT=8 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DCOUNT=9 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -DCOUNT=10 -O0 %s -o %t && not %run %t 0 2>&1 | FileCheck %s

#include <stdlib.h>
#include <string>
#include <vector>

template <class T> struct Ptr {
  void Store(T *ptr) { t = ptr; }

  void Access() { *t = {}; }

  T *t;
};

template <class T, size_t N> struct Ptr<T[N]> {
  using Type = T[N];
  void Store(Type *ptr) { t = *ptr; }

  void Access() { *t = {}; }

  T *t;
};

template <class T> __attribute__((noinline)) void test() {
  Ptr<T> ptr;
  {
    T x;
    ptr.Store(&x);
  }

  ptr.Access();
  // CHECK: ERROR: AddressSanitizer: stack-use-after-scope
  // CHECK:  #{{[0-9]+}} 0x{{.*}} in {{.*test.*}}
  // CHECK: Address 0x{{.*}} is located in stack of thread T{{.*}} at offset [[OFFSET:[^ ]+]] in frame
  // {{\[}}[[OFFSET]], {{[0-9]+}}) 'x'
}

int main() {
  using Tests = void (*)();
  Tests tests[] = {
    &test<bool>,
    &test<char>,
    &test<int>,
    &test<double>,
    &test<float>,
    &test<void*>,
    &test<std::vector<std::string>>,
    &test<int[3]>,
    &test<int[1000]>,
    &test<char[3]>,
    &test<char[1000]>,
  };

  int n = COUNT;
  if (n == sizeof(tests) / sizeof(tests[0])) {
    for (auto te : tests)
      te();
  } else {
    tests[n]();
  }

  return 0;
}

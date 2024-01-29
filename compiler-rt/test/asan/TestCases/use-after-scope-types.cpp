// RUN: %clangxx_asan -O0 %s -o %t
// RUN: not %run %t --cheerp-arg=1  2>&1 | FileCheck %s
// RUN: not %run %t --cheerp-arg=2  2>&1 | FileCheck %s
// RUN: not %run %t --cheerp-arg=3  2>&1 | FileCheck %s
// RUN: not %run %t --cheerp-arg=4  2>&1 | FileCheck %s
// RUN: not %run %t --cheerp-arg=5  2>&1 | FileCheck %s
// RUN: not %run %t --cheerp-arg=6  2>&1 | FileCheck %s
// RUN: not %run %t --cheerp-arg=7  2>&1 | FileCheck %s
// RUN: not %run %t --cheerp-arg=8  2>&1 | FileCheck %s
// RUN: not %run %t --cheerp-arg=9  2>&1 | FileCheck %s
// RUN: not %run %t --cheerp-arg=10 2>&1 | FileCheck %s

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

int main(int argc, char **argv) {
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

  int n = atoi(argv[0]);
  if (n == sizeof(tests) / sizeof(tests[0])) {
    for (auto te : tests)
      te();
  } else {
    tests[n]();
  }

  return 0;
}

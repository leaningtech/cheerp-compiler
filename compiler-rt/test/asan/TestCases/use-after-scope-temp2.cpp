// RUN: %clangxx_asan -O1 %s -o %t && not %run %t 2>&1 | FileCheck %s
// RUN: %clangxx_asan -cheerp-linear-output=asmjs -O1 %s -o %t && not %run %t 2>&1 | FileCheck %s

struct IntHolder {
  __attribute__((noinline)) const IntHolder &Self() const {
    return *this;
  }
  int val = 3;
};

const IntHolder *saved;

int main(int argc, char *argv[]) {
  saved = &IntHolder().Self();
  int x = saved->val;  // BOOM
  // CHECK: ERROR: AddressSanitizer: stack-use-after-scope
  // CHECK:  #0 0x{{.*}} in {{.*main}}
  return x;
}

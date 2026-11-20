// PR691
// RUN: %clang_cc1 %s -emit-llvm -o - | FileCheck %s
// CHECK: call ptr{{( addrspace\(.\))?}} @llvm.stacksave.p{{[0-9]+}}()

extern void external(int[*]);

void test(int N) {
  int i;
  for (i = 0; i < N; ++i) {
    int VLA[i];
    external(VLA);
  }
}

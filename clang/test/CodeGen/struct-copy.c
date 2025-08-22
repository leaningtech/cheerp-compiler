// RUN: %clang_cc1 -emit-llvm %s -o - | FileCheck %s
struct x { int a[100]; };


void foo(struct x *P, struct x *Q) {
// CHECK-LABEL: @foo(
// CHECK:    call void @llvm.memcpy.p{{.}}.p{{.}}
  *P = *Q;
}

// CHECK: declare void @llvm.memcpy.p{{.}}.p{{.}}{{.*}}(ptr{{( addrspace\(.\))?}} noalias nocapture writeonly, ptr{{( addrspace\(.\))?}} noalias nocapture readonly

void bar(struct x *P, struct x *Q) {
// CHECK-LABEL: @bar(
// CHECK:    call void @llvm.memcpy.p{{.}}.p{{.}}
  __builtin_memcpy(P, Q, sizeof(struct x));
}

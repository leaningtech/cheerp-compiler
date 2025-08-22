// RUN: %clang_cc1 -emit-llvm -o - %s | FileCheck %s

extern void abort(void) __attribute__((noreturn));

void f1(void) {
  abort();
}
// CHECK-LABEL: define {{.*}}void @f1()
// CHECK-NEXT: entry:
// CHECK-NEXT:   call{{( addrspace\(.\))?}} void @abort()
// CHECK-NEXT:   unreachable
// CHECK-NEXT: }

void *f2(void) {
  abort();
  return 0;
}
// CHECK-LABEL: define {{.*}}ptr{{( addrspace\(.\))?}} @f2()
// CHECK-NEXT: entry:
// CHECK-NEXT:   call{{( addrspace\(.\))?}} void @abort()
// CHECK-NEXT:   unreachable
// CHECK-NEXT: }


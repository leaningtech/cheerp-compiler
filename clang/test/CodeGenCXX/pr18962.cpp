// RUN: %clang_cc1 -triple i686-linux-gnu %s -emit-llvm -o - | FileCheck %s

class A {
  // append has to have the same prototype as fn1 to tickle the bug.
  void (*append)(A *);
};

class B {};
class D;

// C has to be non-C++98 POD with available tail padding, making the LLVM base
// type differ from the complete LLVM type.
class C {
  // This member creates a circular LLVM type reference to %class._Z1D.
  D *m_group;
  B changeListeners;
};
class D : C {};

void fn1(A *p1) {
}

void
fn2(C *) {
}

// CHECK: %class._Z1A = type { void (%class._Z1A*)* }
// CHECK: %class._Z1C = type <{ %class._Z1D*, %class._Z1B, [3 x i8] }>
// CHECK: %class._Z1D = type { %class._Z1C.base, [3 x i8] }
// CHECK: %class._Z1C.base = type <{ %class._Z1D*, %class._Z1B }>
// CHECK: %class._Z1B = type { i8 }

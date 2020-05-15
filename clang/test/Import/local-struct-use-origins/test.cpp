// RUN: clang-import-test -dump-ir -use-origins -import %S/Inputs/Callee.cpp -expression %s | FileCheck %s
// CHECK: %struct._ZZN3Bar3barEibE1S = type { i
// CHECK: %struct._ZZN3Bar3barEibE1S_0 = type { i

void foo() {
  return Bar().bar(3, true);
}

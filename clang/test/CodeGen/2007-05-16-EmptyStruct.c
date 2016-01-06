// PR 1417
// RUN: %clang_cc1   %s -emit-llvm -o - | FileCheck %s

// CHECK: global %"struct._Z3$_0"* null
struct { } *X;

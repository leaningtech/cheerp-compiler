// RUN: %clang_cc1 -triple %itanium_abi_triple -fwarn-stack-size=42 -emit-llvm -o - %s | FileCheck %s
void foo(void) {}
// CHECK: define {{.*}} @foo() [[ATTR:#[0-9]+]] {
// CHECK: attributes [[ATTR]] = {{.*}} "warn-stack-size"="42"

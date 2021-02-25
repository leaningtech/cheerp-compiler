// RUN: %clang_cc1 -triple %itanium_abi_triple %s -emit-llvm -o - | grep nounwind

void bar() { asm (""); }

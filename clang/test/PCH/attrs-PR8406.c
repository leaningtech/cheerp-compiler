// Test this without pch.
// RUN: %clang_cc1 -triple %itanium_abi_triple -include %s -emit-llvm -o - %s | FileCheck %s

// Test with pch.
// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-pch -o %t %s
// RUN: %clang_cc1 -triple %itanium_abi_triple -include-pch %t -emit-llvm -o - %s | FileCheck %s

#ifndef HEADER
#define HEADER

struct Bar
{
  // CHECK: align 512
  int buffer[123] __attribute__((__aligned__(512)));
};

#else

void foo(void) {
  struct Bar bar;
}

#endif

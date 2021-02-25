// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-pch -o %t.1.ast %S/Inputs/body1.c
// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-pch -o %t.2.ast %S/Inputs/body2.c
// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-obj -o /dev/null -ast-merge %t.1.ast -ast-merge %t.2.ast %s
// expected-no-diagnostics


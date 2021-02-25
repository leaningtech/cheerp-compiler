// RUN: %clang_cc1 -triple %itanium_abi_triple -fsyntax-only %s
// RUN: %clang_cc1 -triple %itanium_abi_triple -ast-print %s -o %t.1.c
// RUN: %clang_cc1 -triple %itanium_abi_triple -ast-print %t.1.c -o %t.2.c
// RUN: diff %t.1.c %t.2.c
// RUN: %clang_cc1 -triple %itanium_abi_triple -ast-dump %s
// RUN: %clang_cc1 -triple %itanium_abi_triple -ast-dump-all %s

#include "c-language-features.inc"

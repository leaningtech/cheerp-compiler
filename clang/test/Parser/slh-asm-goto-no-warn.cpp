// RUN: %clang_cc1 -triple %itanium_abi_triple -Wno-slh-asm-goto -mspeculative-load-hardening -fsyntax-only -verify %s

void f() {
  __asm goto("movl %ecx, %edx"); // expected-no-diagnostics
}

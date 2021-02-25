// RUN: %clang_cc1 -triple %itanium_abi_triple -mspeculative-load-hardening -fsyntax-only -verify %s

void f() {
  __asm goto("movl %ecx, %edx"); // expected-warning {{Speculative load hardening does not protect functions with asm goto}}
}

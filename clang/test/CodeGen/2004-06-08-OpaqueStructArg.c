// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm %s  -o /dev/null

   struct fu;
   void foo(struct fu);
   void bar() {
      foo;
   }

// RUN: %clang_cc1 -triple %itanium_abi_triple %s -emit-llvm -o -

struct Y {};
struct XXX {
  struct  Y F;
};

void test1(void) {
   (int)&((struct XXX*)(((void *)0)))->F;
}

void test2(void) {
   &((struct XXX*)(((void *)0)))->F;
}

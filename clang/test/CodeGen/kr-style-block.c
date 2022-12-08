// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm %s -o %t -fblocks

void foo (void(^)());

int main()
{
foo(
  ^() { }
);
}

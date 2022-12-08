// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm %s  -o /dev/null

typedef struct { } the_coolest_struct_in_the_world;
extern the_coolest_struct_in_the_world xyzzy;
void *foo(void) { return &xyzzy; }


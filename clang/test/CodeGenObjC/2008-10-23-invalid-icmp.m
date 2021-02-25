// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm -o %t %s

@protocol P @end

int f0(id<P> d) {
  return (d != ((void*) 0));
}

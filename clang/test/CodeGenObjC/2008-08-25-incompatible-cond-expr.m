// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm -o %t %s

@protocol P0
@end
@interface A <P0>
@end

id f0(int a, id<P0> x, A* p) {
  return a ? x : p;
}

// RUN: %clang_cc1 -triple %itanium_abi_triple %s -emit-llvm -o -

@interface I0 @end
@implementation I0
- (void) im0: (int (void)) a0 {
}
@end

void func(int pf(void)) {
}

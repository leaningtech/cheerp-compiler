// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm -o %t %s

@interface SUPER
+ (void)Meth;
@end

@interface CURRENT : SUPER
+ (void)Meth;
@end

@implementation CURRENT(CAT)
+ (void)Meth { [super Meth]; }
@end

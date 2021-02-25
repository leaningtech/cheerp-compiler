// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm  -debug-info-kind=limited %s -o - | FileCheck %s
// Radar 8494540

// CHECK: objc_selector
@interface MyClass {
}
- (id)init;
@end

@implementation MyClass
- (id) init
{
  return self;
}
@end

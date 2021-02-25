// RUN: %clang_cc1 -triple %itanium_abi_triple %s -emit-llvm -fsanitize=address -fblocks -o - | FileCheck %s

@interface I0 @end
@implementation I0
// CHECK-NOT: sanitize_address
- (void) im0: (int) a0 __attribute__((no_sanitize("address"))) {
  int (^blockName)() = ^int() { return 0; };
}
@end

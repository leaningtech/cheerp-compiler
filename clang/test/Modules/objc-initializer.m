// UNSUPPORTED: -zos, -aix
// RUN: rm -rf %t
// RUN: %clang_cc1 -triple %itanium_abi_triple -no-opaque-pointers -fmodules-cache-path=%t -fmodules -fimplicit-module-maps -I %S/Inputs/objc-initializer %s -emit-llvm -o - -fobjc-arc | FileCheck %s
// RUN: %clang_cc1 -triple %itanium_abi_triple -no-opaque-pointers -fmodules-cache-path=%t -fmodules -fimplicit-module-maps -I %S/Inputs/objc-initializer %s -emit-llvm -o - -fobjc-arc -DIMPORT_TOP | FileCheck %s
// CHECK: kSimDeviceIOGetInterface = internal constant {{.*}} bitcast

#ifdef IMPORT_TOP
@import X;
#else
#import <X.h>
#endif

void test2(const NSString*);
void test(void) {
  test2(kSimDeviceIOGetInterface);
}

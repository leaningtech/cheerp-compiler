// RUN: rm -rf %t
// RUN: %clang_cc1 -triple %itanium_abi_triple -fmodules-cache-path=%t -fmodules -fimplicit-module-maps -F %S/Inputs -include Module/Module.h %s -emit-llvm -o - | FileCheck %s

// CHECK: call {{.*}}i8* @getModuleVersion
const char* getVer(void) {
  return getModuleVersion();
}

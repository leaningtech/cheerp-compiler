// RUN: %clang -target x86_64-unknown-linux -### -Xlinker -Bsymbolic -emit-interface-stubs 2>&1 | FileCheck %s
// CHECK: Bsymbolic
// CHECK-NOT: Bsymbolic

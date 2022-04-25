// UNSUPPORTED: -zos
// REQUIRES: object-emission
// RUN: llvm-mc --arch=x86-64 %s | FileCheck %s

.file 1 "dir1/foo"

# CHECK-NOT: .file {{.*}} source

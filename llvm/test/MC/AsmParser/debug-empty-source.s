// XFAIL: -aix
// UNSUPPORTED: -zos
// REQUIRES: object-emission
// RUN: llvm-mc --arch=x86-64 %s -o -| FileCheck %s

.file 1 "dir1" "foo" source ""

# CHECK: .file {{.*}} source ""

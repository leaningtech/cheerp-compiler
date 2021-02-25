// RUN: %clang -target x86_64-unknown-linux -### -c -integrated-as -Wa,--mrelax-relocations=yes %s 2>&1 | FileCheck  %s

// CHECK: "-cc1"
// CHECK: "--mrelax-relocations"

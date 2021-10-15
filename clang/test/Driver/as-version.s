// Test version information.

// UNSUPPORTED: -zos
// RUN: %clang -target x86_64-unknown-none-none -Wa,--version -c -fintegrated-as %s -o /dev/null \
// RUN:   | FileCheck --check-prefix=IAS %s
// IAS: clang version

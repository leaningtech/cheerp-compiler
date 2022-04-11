// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-pch -o %t %S/debug-info-limited-struct.h
// RUN: %clang_cc1 -triple %itanium_abi_triple -include-pch %t -emit-llvm %s -debug-info-kind=limited -o - | FileCheck %s

// CHECK: !DICompositeType(tag: DW_TAG_structure_type, name: "foo"
// CHECK-NOT:              flags: {{[^,]*}}FlagFwdDecl
// CHECK-SAME:             {{$}}

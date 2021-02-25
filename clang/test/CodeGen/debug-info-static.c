// RUN: %clang_cc1 -triple %itanium_abi_triple  -debug-info-kind=limited -emit-llvm -o - %s | FileCheck %s

// CHECK: @f.xyzzy = internal global i32 0, align 4, !dbg [[XYZZY:![0-9]+]]

// CHECK: [[XYZZY]] = !DIGlobalVariableExpression(var: [[VAR:.*]], expr: !DIExpression())
// CHECK: [[VAR]] = distinct !DIGlobalVariable
void f(void)
{
   static int xyzzy;
   xyzzy += 3;
}

// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: {{\[\[}}cheerp::jsexport{{\]\]}} function 'doNothing' cannot have default arguments

[[cheerp::jsexport]]
void doNothing(int x = 23)
{
}

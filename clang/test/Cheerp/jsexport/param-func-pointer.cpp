// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: 'wasm' function can't take function pointer as parameter

[[cheerp::jsexport]][[cheerp::wasm]]
void otherFunc(void(*func)(int,int))
{
	(*func)(5, 7);
}

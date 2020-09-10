// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: A {{\[\[}}cheerp::jsexport{{\]\]}}-ed class/struct needs at least a public non-static member

class [[cheerp::genericjs]][[cheerp::jsexport]] A
{
	A() {}
	void someFunc()
	{
		x++;
	}
	int x;
};

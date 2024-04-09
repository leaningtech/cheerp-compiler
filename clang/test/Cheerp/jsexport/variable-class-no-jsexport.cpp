// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: Method tagged {{\[\[}}cheerp::jsexport{{\]\]}} should be part of jsexport-ed class/struct
// CHECK: error: Cheerp: Method tagged {{\[\[}}cheerp::jsexport{{\]\]}} should be part of jsexport-ed class/struct
// CHECK: error: Cheerp: Method tagged {{\[\[}}cheerp::jsexport{{\]\]}} should be part of jsexport-ed class/struct
// CHECK: error: Cheerp: Method tagged {{\[\[}}cheerp::jsexport{{\]\]}} should be part of jsexport-ed class/struct

struct Foo {
	[[cheerp::jsexport]]
	int memberVariable;

	[[cheerp::jsexport]]
	static int staticVariable;
};

int Foo::staticVariable;

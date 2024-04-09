// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: Functions or methods that needs to be {{\[\[}}cheerp::jsexport{{\]\]}}-ed can not have greater than 32bit integers as return type
// CHECK: error: Cheerp: Functions or methods that needs to be {{\[\[}}cheerp::jsexport{{\]\]}}-ed can not have greater than 32bit integers as parameter type
// CHECK: error: Cheerp: Functions or methods that needs to be {{\[\[}}cheerp::jsexport{{\]\]}}-ed can not have greater than 32bit integers as return type
// CHECK: error: Cheerp: Functions or methods that needs to be {{\[\[}}cheerp::jsexport{{\]\]}}-ed can not have greater than 32bit integers as parameter type
// CHECK: error: Cheerp: Functions or methods that needs to be {{\[\[}}cheerp::jsexport{{\]\]}}-ed can not have greater than 32bit integers as return type
// CHECK: error: Cheerp: Functions or methods that needs to be {{\[\[}}cheerp::jsexport{{\]\]}}-ed can not have greater than 32bit integers as parameter type

[[cheerp::jsexport]]
long long globalVariable;

struct [[cheerp::jsexport]] Foo {
	[[cheerp::jsexport]]
	long long memberVariable;

	[[cheerp::jsexport]]
	static long long staticVariable;
};

long long Foo::staticVariable;

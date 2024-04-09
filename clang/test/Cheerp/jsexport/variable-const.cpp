// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: use of undeclared identifier '__cheerp_set_globalVariable'
// CHECK: error: no member named '__cheerp_set_memberVariable' in 'Foo'
// CHECK: error: no member named '__cheerp_set_staticVariable' in 'Foo'

[[cheerp::jsexport]]
const int globalVariable = 0;

struct [[cheerp::jsexport]] Foo {
	[[cheerp::jsexport]]
	const int memberVariable;

	[[cheerp::jsexport]]
	static const int staticVariable;
};

const int Foo::staticVariable = 0;

int main() {
	Foo* foo = nullptr;

	__cheerp_set_globalVariable(0);
	foo->__cheerp_set_memberVariable(0);
	Foo::__cheerp_set_staticVariable(0);
}

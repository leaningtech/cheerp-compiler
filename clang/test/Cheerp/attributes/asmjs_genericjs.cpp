// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: 'asmjs' and 'genericjs' attributes are not compatible
//CHECK: error: 'genericjs' and 'asmjs' attributes are not compatible

class [[cheerp::genericjs]][[cheerp::asmjs]] SomeClass
{
public:
	SomeClass() {}
	void doStuff() {}
};

[[cheerp::asmjs]][[cheerp::genericjs]]
int main()
{
}

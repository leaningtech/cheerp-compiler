// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: 'bytelayout' and 'jsexport' attributes are not compatible

class [[cheerp::jsexport]][[cheerp::genericjs]][[cheerp::bytelayout]] SomeClass
{
public:
	SomeClass() {}
	void doStuff() {}
};

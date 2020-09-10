// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: Inherited classes can not be tagged with {{\[\[}}cheerp::jsexport{{\]\]}}

void someFunc(const char* ptr)
{
}

class [[cheerp::jsexport]] A
{
public:
	A()
	{
	}
	void f()
	{
		someFunc("A::f()");
	}
	void g()
	{
		someFunc("A::g");
	}
};

class [[cheerp::jsexport]] B : public A
{
public:
	B()
	{
	}
	void f()
	{
		someFunc("B::f");
	}
};

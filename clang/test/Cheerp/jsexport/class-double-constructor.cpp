// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: A class/struct should have no more than 1 user defined constructor tagged {{\[\[}}cheerp::jsexport{{\]\]}}

class [[cheerp::jsexport]] A
{
public:
	A() : y(234)
	{
	}
	A(int Y) : y(Y)
	{
	}
	int y;
};

int main()
{
	A a;
}

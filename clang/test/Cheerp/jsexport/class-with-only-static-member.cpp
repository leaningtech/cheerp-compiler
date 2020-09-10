// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: A {{\[\[}}cheerp::jsexport{{\]\]}}-ed class/struct needs at least a public non-static member

class [[cheerp::jsexport]] A
{
	A() : y(33)
	{
		y++;
	}
public:
	A(int X) : y(X)
	{
		y++;
	}
	[[cheerp::jsexport]] static void f(int x)
	{
		z+=x;
	}
private:
	int y;
	static int z;
};

int main()
{
	A a(23);
}

// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: A template function can not have the {{\[\[}}cheerp::jsexport{{\]\]}} attribute

class [[cheerp::jsexport]] A
{
public:
	A(int X) : y(X)
	{
		y++;
	}
	template<class T>
	static void f(T x)
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

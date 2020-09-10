// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: A class/struct with non trivial destructor cannot have the {{\[\[}}cheerp::jsexport{{\]\]}} attribute


class [[cheerp::jsexport]] A
{
public:
	A(int X) : y(X) 
	{
		y++;
	}
	~A()
	{
	}
private:
	int y;
};

int main()
{
	A a(23);
}

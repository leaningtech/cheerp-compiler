// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: Cheerp: Inner classes cannot be implicitly {{\[\[}}cheerp::jsexport{{\]\]}}-ed: either declare it private or tag the subset of methods you need with {{\[\[}}cheerp::jsexport{{\]\]}}

struct [[cheerp::jsexport]] A
{
	A()
	{
	}
	struct B{
		void f()
		{
			__asm__("console.log(123);");
		}
	private:
		int X;
		int cipolla;
		double a;
	};
private:
};

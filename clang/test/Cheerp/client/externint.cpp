// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: Cheerp: Taking the address of a namespace client object is forbidden

namespace [[cheerp::genericjs]] client
{
	extern int someInt;
}

void func(int* ptr)
{
}

int main()
{
	func(&client::someInt);
}

// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: Cheerp: Cannot take address of client namespace global variable

namespace [[cheerp::genericjs]] client
{
	extern int someInt;
}

void func(int& ptr)
{
}

int main()
{
	func(client::someInt);
}

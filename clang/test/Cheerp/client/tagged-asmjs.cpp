// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: Cheerp: Declarations in namespace client can not have attribute 'asmjs'

namespace [[cheerp::genericjs]] client
{

	void [[cheerp::asmjs]] some_func();
} //namespace client

void [[cheerp::asmjs]] some_func();

int main()
{
	client::some_func();
}

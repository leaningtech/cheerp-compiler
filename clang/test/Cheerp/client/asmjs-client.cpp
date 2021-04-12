// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: Cheerp: Namespace client has to be genericjs, so cannot be tagged with attribute 'asmjs'

namespace [[cheerp::asmjs]] client
{
	void some_func();
} //namespace client

int main()
{
	client::some_func();
}

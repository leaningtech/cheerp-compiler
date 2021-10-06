// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: no matching function for call to 'some_func'
//CHECK: note: candidate unavailable since it breaks Cheerp namespace client rules

namespace [[cheerp::genericjs]] client
{
	template <class C>
	C** some_func(C *);
} //namespace client

int main()
{
	int* K;
	client::some_func(K);
}

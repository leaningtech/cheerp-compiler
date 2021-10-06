// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: Cheerp: Functions or methods forward declared in namespace client can not have pointers to pointers as return type

namespace [[cheerp::genericjs]] client
{
	template <class C>
	int** some_func(C *);
} //namespace client

// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: Cheerp: Functions or methods forward declared in namespace client can not have pointers to base types as parameter type

namespace [[cheerp::genericjs]] client
{
	void some_func(int *);
} //namespace client

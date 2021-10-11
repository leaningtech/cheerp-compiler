// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-wasm %s 2>&1 | FileCheck %s

//CHECK: error: Cheerp: Namespace client records can not have fields

namespace [[cheerp::genericjs]] client
{
	class SomeClass
	{
		int a;
	};
}

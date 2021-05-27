// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: Function 'someFunc' tagged {{\[\[}}cheerp::jsexport{{\]\]}} should not be in namespace client
// CHECK: error: Cheerp: Record 'SomeStruct' tagged {{\[\[}}cheerp::jsexport{{\]\]}} should not be in namespace client

namespace [[cheerp::genericjs]] client
{
	void [[cheerp::jsexport]] someFunc();
	struct [[cheerp::jsexport]][[cheerp::client_layout]] SomeStruct
	{
		SomeStruct(int N);
		void doStuff();
	};
}

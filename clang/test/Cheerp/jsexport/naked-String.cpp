// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: Functions or methods that needs to be {{\[\[}}cheerp::jsexport{{\]\]}}-ed can not have naked Client types as parameter type

namespace [[cheerp::genericjs]] client
{
	class [[cheerp::client_layout]] Object
	{
	};

	class String : public client::Object
	{
	};
}

class [[cheerp::genericjs]] [[cheerp::jsexport]] SomeClass
{
public:
	SomeClass()
	{
	}
	void m(client::String G)
	{
	}
};

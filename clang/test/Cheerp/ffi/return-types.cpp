// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: Attribute 'genericjs' of function return 'builderOfClientClass' is incompatible with attribute 'wasm' of caller 'main'

namespace [[cheerp::genericjs]] client
{
	class [[cheerp::client_layout]] Object
	{
	};

	struct ClientClass : public client::Object
	{
		ClientClass(int,int);
	};
}

[[cheerp::genericjs]] client::ClientClass* builderOfClientClass(int a, int b)
{
	return new client::ClientClass(a, b);
}

[[cheerp::wasm]] int main()
{
	builderOfClientClass(3,5);
}

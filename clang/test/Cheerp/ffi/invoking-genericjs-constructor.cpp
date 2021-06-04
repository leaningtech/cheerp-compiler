// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: Attribute 'genericjs' of constructor 'ClientClass' is incompatible with attribute 'wasm' of caller function 'main'


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

[[cheerp::wasm]] int main()
{
	new client::ClientClass(3, -5);
}

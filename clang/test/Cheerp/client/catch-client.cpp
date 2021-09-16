// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs -fexceptions -fcxx-exceptions %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: Pointer to type 'client::Object' declared in the client namespace cannot be caught directly. Use 'cheerp::JSException' to capture foreign exceptions.

namespace [[cheerp::genericjs]] client
{
	class [[cheerp::client_layout]] Object {
	};
	void foo();
} //namespace client

int main()
{
	try {
		client::foo();
	} catch (client::Object* o) {
	}
}

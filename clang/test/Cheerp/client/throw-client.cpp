// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs -fexceptions -fcxx-exceptions %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: Pointer to type 'client::Object' declared in the client namespace cannot be thrown

namespace [[cheerp::genericjs]] client
{
	class [[cheerp::client_layout]] Object {
	};
} //namespace client

int main()
{
	throw new client::Object();
}

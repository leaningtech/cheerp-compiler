// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-wasm %s 2>&1 | FileCheck %s

//CHECK: error: Cheerp: Interface name should only be added to namespace client declarations
//CHECK: error: Cheerp: Interface name should only be added to namespace client declarations
//CHECK: error: Cheerp: Interface name should only be added to namespace client declarations
//CHECK: error: Cheerp: Interface name should only be added to namespace client declarations
//CHECK: error: Cheerp: Interface name should only be added to namespace client declarations
//CHECK-NOT: error: Cheerp: Interface name should only be added to namespace client declarations

namespace [[cheerp::genericjs]] client
{
	[[cheerp::interface_name("on_definition")]] void a() {};
	[[cheerp::interface_name("valid")]] void b();
	namespace inner
	{
		[[cheerp::interface_name("on_definition")]] void a() {};
		[[cheerp::interface_name("valid")]] void b();
	}

	struct [[cheerp::client_layout]] SomeStruct
	{
		SomeStruct();
		[[cheerp::interface_name("on_definition")]] void a() {};
		[[cheerp::interface_name("valid")]] void b();
	};
}

[[cheerp::interface_name("on_definition_non_client")]] void c() {};
[[cheerp::interface_name("on_non_client")]] void d();

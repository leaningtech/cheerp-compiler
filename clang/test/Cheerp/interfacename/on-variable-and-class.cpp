// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: 'interface_name' attribute only applies to functions
//CHECK: error: 'interface_name' attribute only applies to functions

namespace [[cheerp::genericjs]] client
{
	struct [[cheerp::interface_name("on_variable")]] SomeStruct;
	[[cheerp::interface_name("on_variable")]] int x;
}

// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: 'interface_name' attribute requires a string

namespace [[cheerp::genericjs]] client
{
	[[cheerp::interface_name(23)]] int func();
}

// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: 'interface_name' attribute takes one argument

namespace [[cheerp::genericjs]] client
{
	[[cheerp::interface_name]] int func();
}

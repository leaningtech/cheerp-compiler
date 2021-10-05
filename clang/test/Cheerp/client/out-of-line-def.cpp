// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: Cheerp: out-of-line definition are forbidden for namespace 'client' functions

namespace [[cheerp::genericjs]] client
{
	void some_func();
} //namespace client

void client::some_func()
{
}

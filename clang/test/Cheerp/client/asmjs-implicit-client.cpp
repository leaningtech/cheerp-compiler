// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s

//CHECK: error: Cheerp: Namespace client has to be genericjs, so cannot be implicitly tagged with attribute 'wasm'

namespace client
{
	void some_func();
} //namespace client

int main()
{
	client::some_func();
}

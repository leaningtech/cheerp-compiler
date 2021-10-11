// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-wasm %s 2>&1 | FileCheck %s

//CHECK: error: Cheerp: Namespace client globals should be external

struct SomeClass;

namespace [[cheerp::genericjs]] client
{
	SomeClass* someClass;
}

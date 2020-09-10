// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s
// CHECK: error: Cheerp: Cannot use inline asm in a 'wasm' function

// RUN: %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s

[[cheerp::asmjs]][[cheerp::jsexport]] int someFunc(int x)
{
	return x*x - x;
}

int main()
{
	int x = 23;
	__asm__("var assert = require('assert'); assert(someFunc(0) === 0);");
	return 0;
}

// RUN: not %clang_cc1 %s 2>&1 | FileCheck %s
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: Functions or methods that needs to be {{\[\[}}cheerp::jsexport{{\]\]}}-ed can not have pointers to unknown (neither client namespace nor jsexportable) types as parameter type

class Ciccio
{
};

[[cheerp::jsexport]]
int plippo(Ciccio& C)
{
	return 8;
}

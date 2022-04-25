// RUN: %clang_cc1 -triple cheerp-leaningtech-webbrowser-wasm %s -emit-llvm-bc -Werror=cheerp-unsafe -o /dev/null 2>&1
// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s -emit-llvm-bc -Werror=cheerp-unsafe -o /dev/null 2>&1 | FileCheck %s
// CHECK: error: Cheerp: Using values cast to unrelated types is undefined behaviour unless the destination type is the actual type of the value

int main()
{
        char a[12];
        *(((int*)a)+1) = 234223423;
        return 0;
}

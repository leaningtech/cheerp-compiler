// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

//CHECK: error: Cheerp: Interface name should only be added to namespace client declarations

[[cheerp::interface_name("on_variable")]] int someFunc();

// RUN: not %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs %s 2>&1 | FileCheck %s

// CHECK: error: Cheerp: {{\[\[}}cheerp::jsexport{{\]\]}} classes can only have one public non-virtual jsexported base class
// CHECK: error: Cheerp: {{\[\[}}cheerp::jsexport{{\]\]}} classes can only have one public non-virtual jsexported base class
// CHECK: error: Cheerp: {{\[\[}}cheerp::jsexport{{\]\]}} classes can only have one public non-virtual jsexported base class
// CHECK: error: Cheerp: {{\[\[}}cheerp::jsexport{{\]\]}} classes can only have one public non-virtual jsexported base class
// CHECK: error: Cheerp: {{\[\[}}cheerp::jsexport{{\]\]}} classes can only have one public non-virtual jsexported base class

struct [[cheerp::jsexport]] Foo {};
struct [[cheerp::jsexport]] Bar {};
struct Baz {};

struct [[cheerp::jsexport]] VirtualBase : virtual Foo {};
struct [[cheerp::jsexport]] MultipleInheritance : Foo, Bar {};
struct [[cheerp::jsexport]] NonJsExport : Baz {};
struct [[cheerp::jsexport]] ExplicitPrivateBase : private Foo {};
class [[cheerp::jsexport]] ImplicitPrivateBase : Foo {};

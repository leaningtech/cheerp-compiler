// RUN: %clang_cc1 -triple cheerp-leaningtech-webbrowser-wasm -no-opaque-pointers -emit-llvm -o - %s | FileCheck %s
// RUN: %clang_cc1 -triple cheerp-leaningtech-webbrowser-genericjs -no-opaque-pointers -emit-llvm -o - %s | FileCheck %s

// A pointer to a function that takes or returns a class by value has the
// same LLVM type whether the class is complete or not, here a template
// specialization not instantiated yet. With a void ()* placeholder Holder
// would get a different LLVM type in a translation unit that instantiates
// Local<Context> before Holder is laid out.

template <class T> struct Local { T *ptr; };
struct Context;

struct Holder {
  int (*take)(Local<Context>);
  Local<Context> (*give)(int);
  int value;
};
// CHECK-DAG: %struct._Z6Holder = type {{.*}}{ i32 (%struct._Z5LocalI7ContextE*)*, void (%struct._Z5LocalI7ContextE*, i32)*, i32 }

// The class is being laid out when its member is converted.
struct Node {
  void (*visit)(Node);
  Node *next;
};
// CHECK-DAG: %struct._Z4Node = type {{.*}}{ void (%struct._Z4Node*)*, %struct._Z4Node* }

int get(Holder *h) { return h->value; }
Node *next(Node *n) { return n->next; }

// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm %s -o -

struct Gfx {
  void opMoveSetShowText();
};

struct Operator {
  void (Gfx::*func)();
};

Operator opTab[] = {
  {&Gfx::opMoveSetShowText},
};

//===- LoadsTest.cpp - local load analysis unit tests ---------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/Analysis/Loads.h"
#include "llvm/AsmParser/Parser.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"

using namespace llvm;

static std::unique_ptr<Module> parseIR(LLVMContext &C, const char *IR) {
  SMDiagnostic Err;
  std::unique_ptr<Module> Mod = parseAssemblyString(IR, Err, C);
  if (!Mod)
    Err.print("AnalysisTests", errs());
  return Mod;
}

TEST(LoadsTest, CanReplacePointersIfEqual) {
  LLVMContext C;
  std::unique_ptr<Module> M = parseIR(C,
                                      R"IR(
@y = common global [1 x i32] zeroinitializer, align 4
@x = common global [1 x i32] zeroinitializer, align 4

declare void @use(i32*)

define void @f(i32* %p) {
  call void @use(i32* getelementptr inbounds ([1 x i32], [1 x i32]* @y, i64 0, i64 0))
  call void @use(i32* getelementptr inbounds (i32, i32* getelementptr inbounds ([1 x i32], [1 x i32]* @x, i64 0, i64 0), i64 1))
  ret void
}
)IR");
  const auto &DL = M->getDataLayout();
  auto *GV = M->getNamedValue("f");
  ASSERT_TRUE(GV);
  auto *F = dyn_cast<Function>(GV);
  ASSERT_TRUE(F);

  // NOTE: the implementation of canReplacePointersIfEqual is incomplete.
  // Currently the only the cases it returns false for are really sound and
  // returning true means unknown.
  Value *P = &*F->arg_begin();
  auto InstIter = F->front().begin();
  Value *ConstDerefPtr = *cast<CallInst>(&*InstIter)->arg_begin();
  // ConstDerefPtr is a constant pointer that is provably de-referenceable. We
  // can replace an arbitrary pointer with it.
  EXPECT_TRUE(canReplacePointersIfEqual(P, ConstDerefPtr, DL, nullptr));

  ++InstIter;
  Value *ConstUnDerefPtr = *cast<CallInst>(&*InstIter)->arg_begin();
  // ConstUndDerefPtr is a constant pointer that is provably not
  // de-referenceable. We cannot replace an arbitrary pointer with it.
  EXPECT_FALSE(
      canReplacePointersIfEqual(ConstDerefPtr, ConstUnDerefPtr, DL, nullptr));
}

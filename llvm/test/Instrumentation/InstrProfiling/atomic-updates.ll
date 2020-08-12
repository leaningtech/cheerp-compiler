; RUN: opt < %s -S -instrprof -instrprof-atomic-counter-update-all | FileCheck %s

target triple = "x86_64-apple-macosx10.10.0"

@__profn_foo = hidden constant [3 x i8] c"foo"

; CHECK-LABEL: define void @foo
; CHECK-NEXT: atomicrmw add i64* getelementptr inbounds ([1 x i64], [1 x i64]* @__profc_foo, i64 0, i64 0), i64 1 monotonic
define void @foo() {
  call void @llvm.instrprof.increment(i8* bitcast ([3 x i8]* @__profn_foo to i8*), i64 0, i32 1, i32 0)
  ret void
}

declare void @llvm.instrprof.increment(i8*, i64, i32, i32)

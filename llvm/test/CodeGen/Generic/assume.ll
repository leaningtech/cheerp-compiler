; RUN: llc < %s

target triple = "x86_64-unknown-linux"

define void @main() {
        call void @llvm.assume(i1 1)
        ret void
}

declare void @llvm.assume(i1) nounwind


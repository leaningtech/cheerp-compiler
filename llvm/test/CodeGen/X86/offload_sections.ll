; RUN: llc < %s -mtriple=x86_64-unknown-linux-gnu | FileCheck %s

@llvm.embedded.object = hidden constant [1 x i8] c"\00", section ".llvm.offloading.dummy"
@llvm.compiler.used = appending global [1 x i8*] [i8* bitcast ([1 x i8]* @llvm.embedded.object to i8*)], section "llvm.metadata"

; CHECK-DAG: .section	.llvm.offloading.dummy,""

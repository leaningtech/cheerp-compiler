; RUN: opt < %s -instcombine -S | FileCheck %s

target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:128:128"

; Test to check that instcombine doesn't drop the address space when optimizing
; memset.
%struct.Moves = type { [9 x i8], i8, i8, i8, [5 x i8] }

define i32 @test(%struct.Moves addrspace(1)* nocapture %moves) {
entry:
; CHECK: bitcast i8 addrspace(1)* %gep to i64 addrspace(1)*
	%gep = getelementptr inbounds %struct.Moves, %struct.Moves addrspace(1)* %moves, i32 1, i32 0, i32 9
	call void @llvm.memset.p1i8.i64(i8 addrspace(1)* %gep, i8 0, i64 8, i1 false)
	ret i32 0
}

declare void @llvm.memset.p1i8.i64(i8addrspace(1)* nocapture, i8, i64, i1) nounwind

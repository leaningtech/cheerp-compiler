; RUN: opt < %s -analyze -delinearize | FileCheck %s

target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.6.0"
; Derived from the following code:
;
; void foo(long n, long m, double A[n][m]) {
;   for (long i = 0; i < n; i++)
;     for (long j = 0; j < m; j++)
;       A[i][j] = 1.0;
; }

; Inst:  %val = load double, double* %arrayidx
; In Loop with Header: for.j
; AddRec: {{0,+,(%m * 8)}<%for.i>,+,8}<%for.j>
; Base offset: %A
; ArrayDecl[UnknownSize][%m] with elements of 8 bytes.
; ArrayRef[{0,+,1}<nuw><nsw><%for.i>][{0,+,1}<nuw><nsw><%for.j>]

; Inst:  store double %val, double* %arrayidx
; In Loop with Header: for.j
; AddRec: {{%A,+,(8 * %m)}<%for.i>,+,8}<%for.j>
; CHECK: Base offset: %A
; CHECK: ArrayDecl[UnknownSize][%m] with elements of 8 bytes.
; CHECK: ArrayRef[{0,+,1}<nuw><nsw><%for.i>][{0,+,1}<nuw><nsw><%for.j>]

define void @foo(i64 %n, i64 %m, double* %A) {
entry:
  br label %for.i

for.i:
  %i = phi i64 [ 0, %entry ], [ %i.inc, %for.i.inc ]
  %tmp = mul nsw i64 %i, %m
  br label %for.j

for.j:
  %j = phi i64 [ 0, %for.i ], [ %j.inc, %for.j ]
  %vlaarrayidx.sum = add i64 %j, %tmp
  %arrayidx = getelementptr inbounds double, double* %A, i64 %vlaarrayidx.sum
  %val = load double, double* %arrayidx
  store double %val, double* %arrayidx
  %j.inc = add nsw i64 %j, 1
  %j.exitcond = icmp eq i64 %j.inc, %m
  br i1 %j.exitcond, label %for.i.inc, label %for.j

for.i.inc:
  %i.inc = add nsw i64 %i, 1
  %i.exitcond = icmp eq i64 %i.inc, %n
  br i1 %i.exitcond, label %end, label %for.i

end:
  ret void
}

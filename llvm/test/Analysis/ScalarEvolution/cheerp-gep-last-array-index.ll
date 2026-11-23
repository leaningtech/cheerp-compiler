; RUN: opt < %s -disable-output "-passes=print<scalar-evolution>" 2>&1 | FileCheck %s
; RUN: opt < %s -S -passes='loop(indvars)' | FileCheck %s --check-prefix=INDVARS

; On a non-byte-addressable layout the last array index of a GEP is a byte
; offset from the GEP pointer of the indices before it, and must be counted once.
target datalayout = "b-e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-a:0:32-n8:16:32:64-S64"

%holder = type { i32, [5 x i32] }

@arr = global [7 x i32] [i32 5, i32 5, i32 9, i32 5, i32 8, i32 5, i32 50]
@matrix = global [3 x [4 x i32]] zeroinitializer
@h = global %holder zeroinitializer

; CHECK-LABEL: Classifying expressions for: @array
; CHECK: %p = phi
; CHECK-NEXT: -->  {(4 + @arr)<nuw><nsw>,+,4}<nuw><%loop>{{.*}}Exits: (24 + @arr)
; INDVARS-LABEL: @array(
; INDVARS: load i32, i32* getelementptr inbounds ([7 x i32], [7 x i32]* @arr, i32 0, i32 6)
define i32 @array() {
entry:
  br label %loop

loop:
  %p = phi i32* [ getelementptr inbounds ([7 x i32], [7 x i32]* @arr, i32 0, i32 1), %entry ], [ %p.next, %loop ]
  %i = phi i32 [ 0, %entry ], [ %i.next, %loop ]
  %p.next = getelementptr inbounds i32, i32* %p, i32 1
  %i.next = add nuw nsw i32 %i, 1
  %cmp = icmp ult i32 %i, 5
  br i1 %cmp, label %loop, label %exit

exit:
  %p.lcssa = phi i32* [ %p, %loop ]
  %v = load i32, i32* %p.lcssa, align 4
  ret i32 %v
}

; CHECK-LABEL: Classifying expressions for: @nested_array
; CHECK: %p = phi
; CHECK-NEXT: -->  {(4 + (@matrix, 1)),+,4}<nuw><%loop>{{.*}}Exits: (8 + (@matrix, 1))
; INDVARS-LABEL: @nested_array(
; INDVARS: load i32, i32* getelementptr inbounds ([3 x [4 x i32]], [3 x [4 x i32]]* @matrix, i32 0, i32 1, i32 2)
define i32 @nested_array() {
entry:
  br label %loop

loop:
  %p = phi i32* [ getelementptr inbounds ([3 x [4 x i32]], [3 x [4 x i32]]* @matrix, i32 0, i32 1, i32 1), %entry ], [ %p.next, %loop ]
  %i = phi i32 [ 0, %entry ], [ %i.next, %loop ]
  %p.next = getelementptr inbounds i32, i32* %p, i32 1
  %i.next = add nuw nsw i32 %i, 1
  %cmp = icmp ult i32 %i, 1
  br i1 %cmp, label %loop, label %exit

exit:
  %p.lcssa = phi i32* [ %p, %loop ]
  %v = load i32, i32* %p.lcssa, align 4
  ret i32 %v
}

; CHECK-LABEL: Classifying expressions for: @struct_array
; CHECK: %p = phi
; CHECK-NEXT: -->  {(8 + (@h, 1)),+,4}<nuw><%loop>{{.*}}Exits: (12 + (@h, 1))
; INDVARS-LABEL: @struct_array(
; INDVARS: load i32, i32* getelementptr inbounds (%holder, %holder* @h, i32 0, i32 1, i32 3)
define i32 @struct_array() {
entry:
  br label %loop

loop:
  %p = phi i32* [ getelementptr inbounds (%holder, %holder* @h, i32 0, i32 1, i32 2), %entry ], [ %p.next, %loop ]
  %i = phi i32 [ 0, %entry ], [ %i.next, %loop ]
  %p.next = getelementptr inbounds i32, i32* %p, i32 1
  %i.next = add nuw nsw i32 %i, 1
  %cmp = icmp ult i32 %i, 1
  br i1 %cmp, label %loop, label %exit

exit:
  %p.lcssa = phi i32* [ %p, %loop ]
  %v = load i32, i32* %p.lcssa, align 4
  ret i32 %v
}

; RUN: llc < %s

target triple = "x86_64-unknown-linux"

define void @test1(<8 x i32>* %ptr)
{
	%1 = load <8 x i32>, <8 x i32>* %ptr, align 32
	%2 = and <8 x i32> %1, <i32 0, i32 0, i32 0, i32 -1, i32 0, i32 0, i32 0, i32 -1>
	store <8 x i32> %2, <8 x i32>* %ptr, align 16
	ret void
}

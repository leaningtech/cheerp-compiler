; RUN: llc < %s
target triple = "x86_64-unknown-linux"

define i32 @test() noreturn nounwind  {
entry:
	tail call void @llvm.trap( )
	unreachable
}

declare void @llvm.trap() nounwind 


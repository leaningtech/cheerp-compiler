; RUN: llc -no-integrated-as < %s | grep "foo 0 0"

target triple = "x86_64-unknown-linux"

define void @bar() nounwind {
	tail call void asm sideeffect "foo ${:uid} ${:uid}", ""() nounwind
	ret void
}

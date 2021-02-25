; RUN: llc -no-integrated-as < %s | FileCheck %s

target triple = "x86_64-unknown-linux"

define void @bar() nounwind {
  ; CHECK: foo 0 0{{$}}
  tail call void asm sideeffect "foo ${:uid} ${:uid}", ""() nounwind
  ; CHECK: bar 1 x{{$}}
  tail call void asm sideeffect "bar $(${:uid} x$| ${:uid} x$)", ""() nounwind
  ret void
}

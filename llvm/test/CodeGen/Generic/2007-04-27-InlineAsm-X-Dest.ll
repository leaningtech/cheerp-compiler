; RUN: llc -no-integrated-as < %s

target triple = "x86_64-unknown-linux"

; Test that we can have an "X" output constraint.

define void @test(i16 * %t) {
        call void asm sideeffect "foo $0", "=*X,~{dirflag},~{fpsr},~{flags},~{memory}"( i16* elementtype( i16) %t )
        ret void
}

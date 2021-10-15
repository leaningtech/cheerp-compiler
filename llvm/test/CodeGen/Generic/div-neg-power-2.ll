; RUN: llc < %s

target triple = "x86_64-unknown-linux"

define i32 @test(i32 %X) {
        %Y = sdiv i32 %X, -2            ; <i32> [#uses=1]
        ret i32 %Y
}


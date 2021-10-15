; RUN: llc < %s
target triple = "x86_64-unknown-linux"

define i32 @foo(i32 %x) {
        ret i32 %x
}

define i32 @main() {
        %r = call i32 @foo( i32 0 )             ; <i32> [#uses=1]
        ret i32 %r
}


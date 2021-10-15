; RUN: llc < %s

target triple = "x86_64-unknown-linux"

@g = global i32 0               ; <i32*> [#uses=1]

define i32 @main() {
        %h = load i32, i32* @g               ; <i32> [#uses=1]
        ret i32 %h
}

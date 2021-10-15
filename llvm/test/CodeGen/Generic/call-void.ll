; RUN: llc < %s

target triple = "x86_64-unknown-linux"

define void @foo() {
        ret void
}

define i32 @main() {
        call void @foo( )
        ret i32 0
}


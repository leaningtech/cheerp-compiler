; RUN: llc < %s

target triple = "x86_64-unknown-linux"

define void @test() {
        %X = alloca {  }                ; <{  }*> [#uses=0]
        ret void
}

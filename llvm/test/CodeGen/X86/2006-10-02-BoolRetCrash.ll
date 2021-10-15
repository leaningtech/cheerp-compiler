; RUN: llc < %s 
; PR933
; REQUIRES: default_triple

target triple = "x86_64-unknown-linux"
define fastcc i1 @test() {
        ret i1 true
}


; RUN: llc < %s
target triple = "x86_64-unknown-linux"

define double @test(i1 %X) {
        %Y = uitofp i1 %X to double             ; <double> [#uses=1]
        ret double %Y
}


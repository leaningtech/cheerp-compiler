; RUN: llc < %s

target triple = "x86_64-unknown-linux"

define i32 @main() {  
  ret i32 42
}

; RUN:  llvm-dis < %s.bc

; The 2-field form @llvm.global_ctors will be upgraded when reading bitcode.
; CHECK: @llvm.global_ctors = appending global [0 x { i32, void ()*, i8* }] zeroinitializer

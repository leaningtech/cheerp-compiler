add_llvm_library(LLVMDuettoUtils
  AllocaMerging.cpp
  NativeRewriter.cpp
  )

add_dependencies(LLVMDuettoUtils intrinsics_gen)

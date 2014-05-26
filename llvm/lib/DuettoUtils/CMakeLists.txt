add_llvm_library(LLVMDuettoUtils
  AllocaMerging.cpp
  NativeRewriter.cpp
  ResolveAliases.cpp
  )

add_dependencies(LLVMDuettoUtils intrinsics_gen)

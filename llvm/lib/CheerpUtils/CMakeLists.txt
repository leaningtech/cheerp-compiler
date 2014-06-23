add_llvm_library(LLVMCheerpUtils
  AllocaMerging.cpp
  NativeRewriter.cpp
  ResolveAliases.cpp
  Utility.cpp
  )

add_dependencies(LLVMCheerpUtils intrinsics_gen)

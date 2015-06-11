add_llvm_library(LLVMCheerpUtils
  AllocaMerging.cpp
  NativeRewriter.cpp
  ResolveAliases.cpp
  )

add_dependencies(LLVMCheerpUtils intrinsics_gen)

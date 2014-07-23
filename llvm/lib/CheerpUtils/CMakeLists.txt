add_llvm_library(LLVMCheerpUtils
  AllocaMerging.cpp
  NativeRewriter.cpp
  PointerAnalyzer.cpp
  PointerPasses.cpp
  ResolveAliases.cpp
  Utility.cpp
  )

add_dependencies(LLVMCheerpUtils intrinsics_gen)

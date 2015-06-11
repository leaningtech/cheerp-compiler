add_llvm_library(LLVMCheerpWriter
  SourceMaps.cpp
  CheerpWriter.cpp
  JSInterop.cpp
  GlobalDepsAnalyzer.cpp
  NameGenerator.cpp
  PointerAnalyzer.cpp
  Relooper.cpp
  Types.cpp
  Utility.cpp
  Opcodes.cpp
  )

add_dependencies(LLVMCheerpWriter intrinsics_gen)

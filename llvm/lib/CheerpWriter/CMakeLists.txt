add_llvm_library(LLVMCheerpWriter
  SourceMaps.cpp
  CheerpWriter.cpp
  JSInterop.cpp
  NameGenerator.cpp
  Relooper.cpp
  Types.cpp
  Opcodes.cpp
  )

add_dependencies(LLVMCheerpWriter intrinsics_gen)

add_llvm_component_library(LLVMCheerpWriter
  SourceMaps.cpp
  CheerpWriter.cpp
  CheerpWasmWriter.cpp
  JSInterop.cpp
  NameGenerator.cpp
  Relooper.cpp
  Types.cpp
  Opcodes.cpp
  PreExecute.cpp
  CFGStackifier.cpp
  )

add_dependencies(LLVMCheerpWriter intrinsics_gen)

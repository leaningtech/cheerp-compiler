add_llvm_library(LLVMDuettoWriter
  SourceMaps.cpp
  DuettoWriter.cpp
  JSInterop.cpp
  NameGenerator.cpp
  PointerAnalyzer.cpp
  Relooper.cpp
  Types.cpp
  Utility.cpp
  Opcodes.cpp
  )

add_dependencies(LLVMDuettoWriter intrinsics_gen)

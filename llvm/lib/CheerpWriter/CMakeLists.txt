add_llvm_component_library(LLVMCheerpWriter
  SourceMaps.cpp
  CheerpBaseWriter.cpp
  CheerpWriter.cpp
  CheerpWasmWriter.cpp
  CheerpDTSWriter.cpp
  JSInterop.cpp
  NameGenerator.cpp
  Types.cpp
  Opcodes.cpp
  PreExecute.cpp
  CFGStackifier.cpp
  PartialExecuter.cpp

  ADDITIONAL_HEADER_DIRS
  ${LLVM_MAIN_SRC_DIR}/ExecutionEngine/
  )

add_dependencies(LLVMCheerpWriter intrinsics_gen LLVMExecutionEngine LLVMInterpreter)

add_llvm_library(LLVMCheerpUtils
  AllocaMerging.cpp
  AllocaLowering.cpp
  AllocateArrayLowering.cpp
  GlobalDepsAnalyzer.cpp
  IdenticalCodeFolding.cpp
  NativeRewriter.cpp
  PreExecute.cpp
  PointerAnalyzer.cpp
  PointerPasses.cpp
  CFGPasses.cpp
  ReplaceNopCastsAndByteSwaps.cpp
  ResolveAliases.cpp
  Registerize.cpp
  StructMemFuncLowering.cpp
  TypeOptimizer.cpp
  Utility.cpp
  ExpandStructRegs.cpp
  )

add_dependencies(LLVMCheerpUtils intrinsics_gen)

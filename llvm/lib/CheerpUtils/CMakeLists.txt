add_llvm_library(LLVMCheerpUtils
  AllocaMerging.cpp
  AllocaLowering.cpp
  CommandLine.cpp
  GlobalDepsAnalyzer.cpp
  IdenticalCodeFolding.cpp
  LinearMemoryHelper.cpp
  NativeRewriter.cpp
  PreExecute.cpp
  PointerAnalyzer.cpp
  PointerPasses.cpp
  GEPOptimizer.cpp
  CFGPasses.cpp
  ReplaceNopCastsAndByteSwaps.cpp
  Registerize.cpp
  StructMemFuncLowering.cpp
  TypeOptimizer.cpp
  Utility.cpp
  ExpandStructRegs.cpp
  FixIrreducibleControlFlow.cpp
  LowerSwitch.cpp
  FixFunctionCasts.cpp
  ByValLowering.cpp
  )

add_dependencies(LLVMCheerpUtils intrinsics_gen)

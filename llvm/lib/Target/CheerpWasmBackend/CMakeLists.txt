add_llvm_target(CheerpWasmBackendCodeGen
	CheerpWasmBackend.cpp
	CheerpWasmMCAsmInfo.cpp
  )

add_subdirectory(TargetInfo)

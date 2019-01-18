add_llvm_target(CheerpWasmBackendCodeGen
	CheerpWasmBackend.cpp
  )

add_subdirectory(MC)
add_subdirectory(TargetInfo)

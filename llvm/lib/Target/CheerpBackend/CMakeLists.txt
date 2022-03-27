add_llvm_component_group(CheerpBackend)

add_llvm_target(CheerpBackendCodeGen
	CheerpBackend.cpp

	LINK_COMPONENTS
	CheerpWriter
	CheerpUtils
	ExecutionEngine
	Interpreter
	SelectionDAG

	ADD_TO_COMPONENT
	CheerpBackend
  )

add_subdirectory(MC)
add_subdirectory(TargetInfo)

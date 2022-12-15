add_llvm_component_group(CheerpBackend)

set(LLVM_TARGET_DEFINITIONS Cheerp.td)

tablegen(LLVM CheerpBackendGenRegisterInfo.inc -gen-register-info)

add_public_tablegen_target(CheerpCommonTableGen)

add_llvm_target(CheerpBackendCodeGen
	CheerpBackend.cpp
	CheerpTargetTransformInfo.cpp
	CheerpRegisterInfo.cpp
	CheerpFrameLowering.cpp

	LINK_COMPONENTS
	CheerpWriter
	CheerpUtils
	ExecutionEngine
	Interpreter
	SelectionDAG
	Passes

	ADD_TO_COMPONENT
	CheerpBackend
  )

add_subdirectory(MC)
add_subdirectory(TargetInfo)

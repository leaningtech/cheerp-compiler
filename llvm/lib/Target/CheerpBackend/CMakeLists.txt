add_llvm_component_group(CheerpBackend)

add_llvm_target(CheerpBackendCodeGen
	CheerpBackend.cpp
	CheerpTargetTransformInfo.cpp
	WebAssemblyInstrInfo.cpp
	WebAssemblyInstrInfo.h
	WebAssemblySubtarget.cpp
	WebAssemblyRegisterInfo.cpp
	WebAssemblyRegisterInfo.h
	WebAssemblySubtarget.h
	WebAssemblyMachineFunctionInfo.cpp
	WebAssemblyMachineFunctionInfo.h
	WebAssemblyFrameLowering.cpp
	WebAssemblyFrameLowering.h
	WebAssemblyISelLowering.cpp
	WebAssemblyISelLowering.h
	WebAssemblySelectionDAGInfo.cpp
	WebAssemblySelectionDAGInfo.h
	WebAssembly.h

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

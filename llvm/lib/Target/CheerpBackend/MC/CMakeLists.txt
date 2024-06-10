add_llvm_library(LLVMCheerpBackendDesc
	CheerpMCAsmInfo.cpp
	WebAssemblyTargetStreamer.cpp
	WebAssemblyTargetStreamer.h
	WebAssemblyMCTargetDesc.h
	WebAssemblyTypeUtilities.cpp
	WebAssemblyTypeUtilities.h
	WebAssemblyInstPrinter.cpp
	WebAssemblyInstPrinter.h
	WebAssemblyUtilities.cpp
	WebAssemblyUtilities.h

	LINK_COMPONENTS
	MC
  )

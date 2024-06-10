add_llvm_library(LLVMCheerpBackendDesc
	CheerpMCAsmInfo.cpp
	WebAssemblyTargetStreamer.cpp
	WebAssemblyTargetStreamer.h
	WebAssemblyMCTargetDesc.cpp
	WebAssemblyMCTargetDesc.h
	WebAssemblyTypeUtilities.cpp
	WebAssemblyTypeUtilities.h
	WebAssemblyInstPrinter.cpp
	WebAssemblyInstPrinter.h
	WebAssemblyMCAsmInfo.cpp
	WebAssemblyMCAsmInfo.h
	WebAssemblyUtilities.cpp
	WebAssemblyUtilities.h

	LINK_COMPONENTS
	MC
  )

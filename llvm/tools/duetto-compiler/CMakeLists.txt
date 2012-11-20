set(LLVM_LINK_COMPONENTS ${LLVM_TARGETS_TO_BUILD} bitreader bitwriter asmparser irreader)

add_llvm_tool(duetto-compiler
  duetto-compiler.cpp
  )

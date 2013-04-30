set(LLVM_LINK_COMPONENTS bitreader bitwriter target asmparser duettowriter irreader)

add_llvm_tool(duetto-compiler
  duetto-compiler.cpp
  )

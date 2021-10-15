set(LLVM_LINK_COMPONENTS
  CheerpUtils
  Core
  IRReader
  )

add_llvm_unittest(CheerpTests
  CheerpPointerAnalyzerTest.cpp
  )

configure_file( test1.ll ${CMAKE_BINARY_DIR}/test/test1.ll COPYONLY )

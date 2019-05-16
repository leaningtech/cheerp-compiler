set(LLVM_LINK_COMPONENTS
  CheerpWriter
  Core
  IRReader
  )

add_llvm_unittest(CheerpTests
  CheerpPointerAnalyzerTest.cpp
  CheerpDeterministicPtrSetTest.cpp
  )

configure_file( test1.ll ${CMAKE_BINARY_DIR}/test/test1.ll COPYONLY )

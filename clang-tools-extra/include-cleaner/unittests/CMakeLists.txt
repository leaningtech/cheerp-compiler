set(LLVM_LINK_COMPONENTS
  Support
  TestingSupport
  )

add_custom_target(ClangIncludeCleanerUnitTests)
add_unittest(ClangIncludeCleanerUnitTests ClangIncludeCleanerTests
  AnalysisTest.cpp
  FindHeadersTest.cpp
  RecordTest.cpp
  TypesTest.cpp
  WalkASTTest.cpp
)

target_include_directories(ClangIncludeCleanerTests
  PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/../lib)

clang_target_link_libraries(ClangIncludeCleanerTests
  PRIVATE
  clangAST
  clangBasic
  clangFrontend
  clangLex
  clangToolingInclusionsStdlib
  )

target_link_libraries(ClangIncludeCleanerTests
  PRIVATE
  clangIncludeCleaner
  clangTesting
  LLVMTestingSupport
  )


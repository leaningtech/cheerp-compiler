# Needed by LLVM's CMake checks because this file defines multiple targets.
set(LLVM_OPTIONAL_SOURCES PluginsTest.cpp TestPlugin.cpp DoublerPlugin.cpp PassBuilderBindingsTest.cpp)

# If plugins are disabled, this test will disable itself at runtime. Otherwise,
# reconfiguring with plugins disabled will leave behind a stale executable.
if (LLVM_ENABLE_PLUGINS)
  add_definitions(-DLLVM_ENABLE_PLUGINS)
endif()

set(LLVM_LINK_COMPONENTS Support Passes Core Target native AllTargetsInfos)
add_llvm_unittest(PassesBindingsTests
	PassBuilderBindingsTest.cpp
	)
target_link_libraries(PassesBindingsTests PRIVATE LLVMTestingSupport)


# Needed by LLVM's CMake checks because this file defines multiple targets.
set(LLVM_OPTIONAL_SOURCES PluginsTest.cpp TestPlugin.cpp)

# If plugins are disabled, this test will disable itself at runtime. Otherwise,
# reconfiguring with plugins disabled will leave behind a stale executable.
if (LLVM_ENABLE_PLUGINS)
  add_definitions(-DLLVM_ENABLE_PLUGINS)
endif()

# The plugin expects to not link against the Support and Core libraries,
# but expects them to exist in the process loading the plugin. This doesn't
# work with DLLs on Windows (where a shared library can't have undefined
# references), so just skip this testcase on Windows.

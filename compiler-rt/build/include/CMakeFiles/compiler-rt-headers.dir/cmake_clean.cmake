file(REMOVE_RECURSE
  "CMakeFiles/compiler-rt-headers"
  "sanitizer/allocator_interface.h"
  "sanitizer/asan_interface.h"
  "sanitizer/common_interface_defs.h"
  "sanitizer/lsan_interface.h"
)

# Per-language clean rules from dependency scanning.
foreach(lang )
  include(CMakeFiles/compiler-rt-headers.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

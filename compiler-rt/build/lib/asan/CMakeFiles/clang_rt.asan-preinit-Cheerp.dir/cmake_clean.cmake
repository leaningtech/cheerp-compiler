file(REMOVE_RECURSE
  "../cheerp/libclang_rt.asan-preinit-Cheerp.bc"
  "../cheerp/libclang_rt.asan-preinit-Cheerp.pdb"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/clang_rt.asan-preinit-Cheerp.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

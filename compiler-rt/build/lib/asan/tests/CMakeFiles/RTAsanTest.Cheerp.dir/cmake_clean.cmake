file(REMOVE_RECURSE
  "libRTAsanTest.Cheerp.bc"
  "libRTAsanTest.Cheerp.pdb"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/RTAsanTest.Cheerp.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

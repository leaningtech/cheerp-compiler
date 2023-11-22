#ifndef SANITIZER_CHEERPWASM_MMAP_H
#define SANITIZER_CHEERPWASM_MMAP_H

#include "sanitizer_platform.h"

#if SANITIZER_CHEERPWASM
#  include "sanitizer_internal_defs.h"

namespace __sanitizer {

void UnmapFromTo(uptr from, uptr to);
void SetupMemoryMapping();

}  // namespace __sanitizer
#endif  // SANITIZER_CHEERPWASM
#endif  // SANITIZER_CHEERPWASM_MMAP_H

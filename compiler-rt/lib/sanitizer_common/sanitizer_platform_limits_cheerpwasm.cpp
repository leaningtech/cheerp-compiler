#include "sanitizer_platform_limits_cheerpwasm.h"

#if SANITIZER_CHEERPWASM
#  include <wchar.h>

unsigned mbstate_t_sz = sizeof(mbstate_t);
#endif  // SANITIZER_CHEERPWASM

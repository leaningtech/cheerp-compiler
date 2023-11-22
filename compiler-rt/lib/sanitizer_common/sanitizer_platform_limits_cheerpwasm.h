#ifndef SANITIZER_PLATFORM_LIMITS_CHEERPWASM_H
#define SANITIZER_PLATFORM_LIMITS_CHEERPWASM_H

#include "sanitizer_platform.h"

#if SANITIZER_CHEERPWASM

#  include <stdio.h>

typedef FILE __sanitizer_FILE;
extern unsigned mbstate_t_sz;

#endif  // SANITIZER_CHEERPWASM

#endif  // SANITIZER_PLATFORM_LIMITS_CHEERPWASM_H

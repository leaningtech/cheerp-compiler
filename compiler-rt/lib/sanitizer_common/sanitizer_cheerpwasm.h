#ifndef SANITIZER_CHEERPWASM_H
#define SANITIZER_CHEERPWASM_H

#include "sanitizer_platform.h"

#if SANITIZER_CHEERPWASM

#  include "sanitizer_internal_defs.h"
#  include "sanitizer_platform_limits_cheerpwasm.h"

namespace __sanitizer {
void InitEnv();
bool IsWasi();
uptr Utf16ToUtf8(char *dest, uptr dlen, const char16_t *src, uptr slen);
}  // namespace __sanitizer

#endif  // SANITIZER_CHEERPWASM

#endif  // SANITIZER_CHEERPWASM_H

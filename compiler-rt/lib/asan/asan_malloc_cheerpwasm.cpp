#include "sanitizer_common/sanitizer_platform.h"
#if SANITIZER_CHEERPWASM

#  include "asan_allocator.h"
#  include "asan_interceptors.h"
#  include "asan_internal.h"
#  include "asan_stack.h"
#  include "lsan/lsan_common.h"
#  include "sanitizer_common/sanitizer_allocator_checks.h"
#  include "sanitizer_common/sanitizer_allocator_dlsym.h"
#  include "sanitizer_common/sanitizer_errno.h"
#  include "sanitizer_common/sanitizer_tls_get_addr.h"

using namespace __asan;

INTERCEPTOR(void, free, void *ptr) {
  GET_STACK_TRACE_FREE;
  asan_free(ptr, &stack, FROM_MALLOC);
}

INTERCEPTOR(void *, malloc, uptr size) {
  ENSURE_ASAN_INITED();
  GET_STACK_TRACE_MALLOC;
  return asan_malloc(size, &stack);
}

INTERCEPTOR(void *, calloc, uptr nmemb, uptr size) {
  ENSURE_ASAN_INITED();
  GET_STACK_TRACE_MALLOC;
  return asan_calloc(nmemb, size, &stack);
}

INTERCEPTOR(void *, realloc, void *ptr, uptr size) {
  ENSURE_ASAN_INITED();
  GET_STACK_TRACE_MALLOC;
  return asan_realloc(ptr, size, &stack);
}

#endif  // SANITIZER_CHEERPWASM

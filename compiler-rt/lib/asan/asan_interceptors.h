//===-- asan_interceptors.h -------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file is a part of AddressSanitizer, an address sanity checker.
//
// ASan-private header for asan_interceptors.cpp
//===----------------------------------------------------------------------===//
#ifndef ASAN_INTERCEPTORS_H
#define ASAN_INTERCEPTORS_H

#include "asan_interceptors_memintrinsics.h"
#include "asan_internal.h"
#include "interception/interception.h"
#include "sanitizer_common/sanitizer_platform.h"
#include "sanitizer_common/sanitizer_platform_interceptors.h"

namespace __asan {

void InitializeAsanInterceptors();
void InitializePlatformInterceptors();

#define ENSURE_ASAN_INITED()      \
  do {                            \
    CHECK(!asan_init_is_running); \
    if (UNLIKELY(!asan_inited)) { \
      AsanInitFromRtl();          \
    }                             \
  } while (0)

}  // namespace __asan

// There is no general interception at all on Fuchsia.
// Only the functions in asan_interceptors_memintrinsics.h are
// really defined to replace libc functions.
#if !SANITIZER_FUCHSIA

// Use macro to describe if specific function should be
// intercepted on a given platform.
#if !SANITIZER_WINDOWS && !SANITIZER_CHEERPWASM
# define ASAN_INTERCEPT_ATOLL_AND_STRTOLL 1
# define ASAN_INTERCEPT__LONGJMP 1
# define ASAN_INTERCEPT_INDEX 1
# define ASAN_INTERCEPT_PTHREAD_CREATE 1
#else
# define ASAN_INTERCEPT_ATOLL_AND_STRTOLL 0
# define ASAN_INTERCEPT__LONGJMP 0
# define ASAN_INTERCEPT_INDEX 0
# define ASAN_INTERCEPT_PTHREAD_CREATE 0
#endif

#if SANITIZER_FREEBSD || SANITIZER_LINUX || SANITIZER_NETBSD || \
    SANITIZER_SOLARIS
# define ASAN_USE_ALIAS_ATTRIBUTE_FOR_INDEX 1
#else
# define ASAN_USE_ALIAS_ATTRIBUTE_FOR_INDEX 0
#endif

#if SANITIZER_GLIBC || SANITIZER_SOLARIS
# define ASAN_INTERCEPT_SWAPCONTEXT 1
#else
# define ASAN_INTERCEPT_SWAPCONTEXT 0
#endif

#if !SANITIZER_WINDOWS && !SANITIZER_CHEERPWASM
# define ASAN_INTERCEPT_SIGLONGJMP 1
#else
# define ASAN_INTERCEPT_SIGLONGJMP 0
#endif

#if SANITIZER_GLIBC
# define ASAN_INTERCEPT___LONGJMP_CHK 1
#else
# define ASAN_INTERCEPT___LONGJMP_CHK 0
#endif

#if ASAN_HAS_EXCEPTIONS && !SANITIZER_WINDOWS && !SANITIZER_SOLARIS && \
    !SANITIZER_NETBSD && !SANITIZER_CHEERPWASM
# define ASAN_INTERCEPT___CXA_THROW 1
# define ASAN_INTERCEPT___CXA_RETHROW_PRIMARY_EXCEPTION 1
# if defined(_GLIBCXX_SJLJ_EXCEPTIONS) || (SANITIZER_IOS && defined(__arm__))
#  define ASAN_INTERCEPT__UNWIND_SJLJ_RAISEEXCEPTION 1
# else
#  define ASAN_INTERCEPT__UNWIND_RAISEEXCEPTION 1
# endif
#elif SANITIZER_CHEERPWASM
# define ASAN_INTERCEPT___CXA_THROW 1
# define ASAN_INTERCEPT___CXA_RETHROW_PRIMARY_EXCEPTION 1
# define ASAN_INTERCEPT__UNWIND_RAISEEXCEPTION 0
# define ASAN_INTERCEPT__UNWIND_SJLJ_RAISEEXCEPTION 0
#else
# define ASAN_INTERCEPT___CXA_THROW 0
# define ASAN_INTERCEPT___CXA_RETHROW_PRIMARY_EXCEPTION 0
# define ASAN_INTERCEPT__UNWIND_RAISEEXCEPTION 0
# define ASAN_INTERCEPT__UNWIND_SJLJ_RAISEEXCEPTION 0
#endif

#if !SANITIZER_WINDOWS && !SANITIZER_CHEERPWASM //CHEERPASAN: TODO probably should be implemented at some point
# define ASAN_INTERCEPT___CXA_ATEXIT 1
#else
# define ASAN_INTERCEPT___CXA_ATEXIT 0
#endif

#if SANITIZER_NETBSD
# define ASAN_INTERCEPT_ATEXIT 1
#else
# define ASAN_INTERCEPT_ATEXIT 0
#endif

#if SANITIZER_GLIBC
# define ASAN_INTERCEPT___STRDUP 1
#else
# define ASAN_INTERCEPT___STRDUP 0
#endif

#if SANITIZER_LINUX &&                                                \
    (defined(__arm__) || defined(__aarch64__) || defined(__i386__) || \
     defined(__x86_64__) || SANITIZER_RISCV64 || SANITIZER_LOONGARCH64)
# define ASAN_INTERCEPT_VFORK 1
#else
# define ASAN_INTERCEPT_VFORK 0
#endif

#if SANITIZER_NETBSD
# define ASAN_INTERCEPT_PTHREAD_ATFORK 1
#else
# define ASAN_INTERCEPT_PTHREAD_ATFORK 0
#endif

DECLARE_REAL(int, memcmp, const void *a1, const void *a2, uptr size)
DECLARE_REAL(char*, strchr, const char *str, int c)
DECLARE_REAL(SIZE_T, strlen, const char *s)
DECLARE_REAL(char*, strncpy, char *to, const char *from, uptr size)
DECLARE_REAL(uptr, strnlen, const char *s, uptr maxlen)
DECLARE_REAL(char*, strstr, const char *s1, const char *s2)

#if SANITIZER_CHEERPWASM
#include <stdarg.h>

DECLARE_REAL(void*, memmove, void* dest, const void* src, uptr n)
DECLARE_REAL(char*, strcat, char* dst, const char* src)
DECLARE_REAL(char*, strncat, char* dst, const char* src, uptr sz)
DECLARE_REAL(char*, strcpy, char* dst, const char* src)
DECLARE_REAL(long, strtol, const char *nptr, char **endptr, int base)
DECLARE_REAL(int, atoi, const char* nptr)
DECLARE_REAL(long, atol, const char* nptr)
DECLARE_REAL(char*, strdup, const char* s)

DECLARE_REAL(int, printf, const char *format, ...)
DECLARE_REAL(int, sprintf, char *str, const char *format, ...)
DECLARE_REAL(int, snprintf, char *str, size_t size, const char *format, ...)
DECLARE_REAL(int, vprintf, const char *format, va_list ap)
DECLARE_REAL(int, vsprintf, char *str, const char *format, va_list ap)
DECLARE_REAL(int, vsnprintf, char *str, size_t size, const char *format, va_list ap)
DECLARE_REAL(int, asprintf, char **strp, const char *format, ...)
DECLARE_REAL(int, vasprintf, char **strp, const char *format, va_list ap)
DECLARE_REAL(int, fprintf, FILE *stream, const char *format, ...)
DECLARE_REAL(int, vfprintf, FILE *stream, const char *format, va_list ap)

DECLARE_REAL(size_t, fwrite, const void *ptr, size_t size, size_t nmemb, FILE *stream)

DECLARE_REAL(char*, strcasestr, const char *haystack, const char *needle)

DECLARE_REAL(int, strcmp, const char *s1, const char *s2)
DECLARE_REAL(int, strncmp, const char *s1, const char *s2, size_t n)

DECLARE_REAL(size_t, mbstowcs, wchar_t *dest, const char *src, size_t len);
DECLARE_REAL(size_t, mbsrtowcs, wchar_t *dest, const char **src, size_t len, void *ps);

extern "C" void __cheerp___cxa_throw(void *, void *, void *);
extern "C" void __cheerp___cxa_rethrow_primary_exception(void*);
#endif // SANITIZER_CHEERPWASM

#  if !SANITIZER_APPLE
#    define ASAN_INTERCEPT_FUNC(name)                                        \
      do {                                                                   \
        if (!INTERCEPT_FUNCTION(name))                                       \
          VReport(1, "AddressSanitizer: failed to intercept '%s'\n", #name); \
      } while (0)
#    define ASAN_INTERCEPT_FUNC_VER(name, ver)                           \
      do {                                                               \
        if (!INTERCEPT_FUNCTION_VER(name, ver))                          \
          VReport(1, "AddressSanitizer: failed to intercept '%s@@%s'\n", \
                  #name, ver);                                           \
      } while (0)
#    define ASAN_INTERCEPT_FUNC_VER_UNVERSIONED_FALLBACK(name, ver)           \
      do {                                                                    \
        if (!INTERCEPT_FUNCTION_VER(name, ver) && !INTERCEPT_FUNCTION(name))  \
          VReport(1,                                                          \
                  "AddressSanitizer: failed to intercept '%s@@%s' or '%s'\n", \
                  #name, ver, #name);                                         \
      } while (0)

#  else
// OS X interceptors don't need to be initialized with INTERCEPT_FUNCTION.
#    define ASAN_INTERCEPT_FUNC(name)
#  endif  // SANITIZER_APPLE

#endif  // !SANITIZER_FUCHSIA

#endif  // ASAN_INTERCEPTORS_H

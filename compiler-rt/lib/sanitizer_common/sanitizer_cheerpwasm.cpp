#include "sanitizer_cheerpwasm.h"

#if SANITIZER_CHEERPWASM

#  include <errno.h>
#  include <stdlib.h>

#  include "sanitizer_allocator_internal.h"
#  include "sanitizer_cheerpwasm_mmap.h"
#  include "sanitizer_common.h"
#  include "sanitizer_file.h"

#  define LEAN_CXX_LIB
#  include <client/cheerp/types.h>

#  define REPLACEMENT_CHARACTER 0xFFFD
#  define MAX_CODEPOINT 0x10FFFF
#  define INVALID_CODEPOINT -1

__attribute__((cheerp_asmjs)) char *volatile _stackBottom = (char *)0xdeadbeef;
__attribute__((cheerp_asmjs)) char *volatile _stackTop = (char *)0xdeadbeef;

namespace __sanitizer {

void ListOfModules::init() {
  modules_.Initialize(2);

  LoadedModule main_module;
  main_module.set("main", 0);

  // Cheerp represents program counters as offsets into WebAssembly
  // modules. For JavaScript code, the "program counter" is the line number
  // of the JavaScript code with the high bit set.
  // Therefore, PC values 0x80000000 and beyond represents JavaScript code.
  // As a result, 0x00000000 to 0x7FFFFFFF represents PC values for WASM code.
  // We consider WASM code as main_module.
  main_module.addAddressRange(0, 0x7FFFFFFF, /*executable*/ true,
                              /*writable*/ false);
  modules_.push_back(main_module);

  // The remaining PC values, 0x80000000 to 0xFFFFFFFF, are JavaScript,
  // and we consider it a separate module, js_module.
  LoadedModule js_module;
  js_module.set("JavaScript", 0x80000000);
  js_module.addAddressRange(0x80000000, 0xFFFFFFFF, /*executable*/ true,
                            /*writable*/ false);
  modules_.push_back(js_module);
}

int Atexit(void (*function)(void)) { return atexit(function); }

void ListOfModules::fallbackInit() { clear(); }

uptr ReadBinaryName(/*out*/ char *buf, uptr buf_len) {
  const char *default_binary_name = "default.binary.name";
  const size_t len = internal_strlen(default_binary_name);
  size_t n = buf_len < len ? buf_len : len;
  internal_memcpy(buf, default_binary_name, n);
  return n;
}

uptr ReadLongProcessName(/*out*/ char *buf, uptr buf_len) {
  return ReadBinaryName(buf, buf_len);
}

uptr Utf16ToUtf8(char *dest, uptr dlen, const char16_t *src, uptr slen) {
  uptr j = 0;
  for (uptr i = 0; i < slen; ++i) {
    uint32_t cp = src[i];

    if (cp >= 0xD800 && cp <= 0xDFFF) {
      if (i + 1 < slen) {
        uint32_t trail = src[++i];
        cp = 0x10000 + ((cp & 0x3FF) | (trail & 0x3FF));
      } else {
        // Missing lower surrogate
        cp = INVALID_CODEPOINT;
      }
    }

    if (cp > MAX_CODEPOINT)
      cp = INVALID_CODEPOINT;

    if (cp <= 0x7F) {
      if (j < dlen)
        *dest++ = static_cast<char>(cp);

      j += 1;
    } else if (cp <= 0x7FF) {
      if (j + 1 < dlen) {
        *dest++ = 0xC0 | (cp >> 6);
        *dest++ = 0x80 | (cp & 63);
      }

      j += 2;
    } else if (cp <= 0xFFFF) {
      if (j + 2 < dlen) {
        *dest++ = 0xE0 | (cp >> 12);
        *dest++ = 0x80 | ((cp >> 6) & 63);
        *dest++ = 0x80 | (cp & 63);
      }

      j += 3;
    } else {
      if (j + 3 < dlen) {
        *dest++ = 0xF0 | (cp >> 18);
        *dest++ = 0x80 | ((cp >> 12) & 63);
        *dest++ = 0x80 | ((cp >> 6) & 63);
        *dest++ = 0x80 | (cp & 63);
      }

      j += 4;
    }
  }
  return j;
}

extern "C" char **environ;

[[cheerp::genericjs]] static uptr GetEnvCount() {
  uptr count = 0;
  __asm__("typeof CHEERP_ENV == 'undefined' ? 0 : CHEERP_ENV.length"
          : "=r"(count));
  return count;
}

[[cheerp::genericjs]] static const client::String *GetJSEnv(uptr idx) {
  client::String *res = nullptr;
  __asm__("(CHEERP_ENV[%1][0] + '=' + CHEERP_ENV[%1][1])"
          : "=r"(res)
          : "r"(idx));
  return res;
}

[[cheerp::genericjs]] static uptr GetEnvLength(uptr idx) {
  return GetJSEnv(idx)->get_length();
}

[[cheerp::genericjs]] static void ReadEnv(char16_t *dest, uptr idx) {
  const client::String *env = GetJSEnv(idx);
  const uptr len = env->get_length();

  for (uptr i = 0; i < len; ++i) {
    dest[i] = env->charCodeAt(i);
  }
}

__attribute__((weak)) void InitEnv() {
  const uptr env_count = GetEnvCount();
  environ = reinterpret_cast<char **>(
      InternalAlloc(sizeof *environ * (env_count + 1)));
  CHECK(environ);

  char16_t cb_cap = 0;
  char16_t *cb = nullptr;

  for (uptr i = 0; i < env_count; ++i) {
    const uptr len16 = GetEnvLength(i);
    if (len16 > cb_cap) {
      cb = reinterpret_cast<char16_t *>(
          InternalRealloc(cb, sizeof(char16_t) * len16));
      cb_cap = len16;
      CHECK(cb);
    }

    ReadEnv(cb, i);

    const uptr len8 = Utf16ToUtf8(nullptr, 0, cb, len16);
    environ[i] = reinterpret_cast<char *>(InternalAlloc(len8 + 1));
    CHECK(environ[i]);

    Utf16ToUtf8(environ[i], len8, cb, len16);
    environ[i][len8] = 0;
  }

  environ[env_count] = nullptr;

  InternalFree(cb);
}

/* This function is overwritten in libwasi so to be able to detect if libasan is
 * running in wasi or not */
__attribute__((weak)) bool IsWasi() { return false; }

const char *GetEnv(const char *name) { return getenv(name); }

uptr GetPageSize() { return GetMmapGranularity(); }

tid_t GetTid() { return 0; }

uptr GetThreadSelf() { return 0; }
bool SupportsColoredOutput(fd_t fd) { return true; }

void ReportFile::Write(const char *buffer, uptr length) {
  // CHEERPASAN: TODO do not hardcode output stream
  fwrite(buffer, sizeof *buffer, length, stderr);
}

uptr MapDynamicShadow(uptr shadow_size_bytes, uptr shadow_scale,
                      uptr min_shadow_base_alignment,
                      UNUSED uptr &high_mem_end) {
  const uptr granularity = GetMmapGranularity();
  const uptr alignment =
      Max<uptr>(granularity << shadow_scale, 1ULL << min_shadow_base_alignment);
  const uptr left_padding =
      Max<uptr>(granularity, 1ULL << min_shadow_base_alignment);

  const uptr shadow_size = RoundUpTo(shadow_size_bytes, granularity);
  const uptr map_size = shadow_size + left_padding + alignment;

  const uptr map_start = (uptr)MmapNoAccess(map_size);
  CHECK_NE(map_start, ~(uptr)0);

  const uptr shadow_start = RoundUpTo(map_start + left_padding, alignment);

  UnmapFromTo(map_start, shadow_start - left_padding);
  UnmapFromTo(shadow_start + shadow_size, map_start + map_size);

  return shadow_start;
}

void InitializePlatformCommonFlags(CommonFlags *cf) {}
void InitializePlatformEarly() {}
void CheckASLR() {}
void DisableCoreDumperIfNecessary() {}
void InstallDeadlySignalHandlers(void (*)(int, void *, void *)) {}
void InitializeCoverage(bool, char const *) {}
void InitTlsSize() {}
uptr internal_getpid() { return 1; }
bool ErrorIsOOM(error_t err) { return err == ENOMEM; }
const char *SignalContext::Describe() const { UNIMPLEMENTED(); }
bool SignalContext::IsStackOverflow() const { UNIMPLEMENTED(); }
char **GetArgv() { UNIMPLEMENTED(); }
void Abort() { UNIMPLEMENTED(); }
void internal_usleep(u64) { UNIMPLEMENTED(); }
void DumpProcessMap() { UNIMPLEMENTED(); }
u64 MonotonicNanoTime() { UNIMPLEMENTED(); }
bool MprotectReadOnly(uptr, uptr) { UNIMPLEMENTED(); }
void FutexWake(atomic_uint32_t *, u32) { UNIMPLEMENTED(); }
void FutexWait(atomic_uint32_t *, u32) { UNIMPLEMENTED(); }
uptr internal_sched_yield() { return 0; }
void CloseFile(fd_t) { UNIMPLEMENTED(); }
fd_t OpenFile(const char *, FileAccessMode, error_t *) { UNIMPLEMENTED(); }
bool WriteToFile(fd_t, const void *, uptr, uptr *, error_t *) {
  UNIMPLEMENTED();
}
bool FileExists(const char *) { UNIMPLEMENTED(); }
bool IsAbsolutePath(const char *) { UNIMPLEMENTED(); }
bool ReadFromFile(fd_t, void *, uptr, uptr *, error_t *) { UNIMPLEMENTED(); }
void UnsetAlternateSignalStack() { UNIMPLEMENTED(); }
bool DontDumpShadowMemory(uptr addr, uptr length) { UNIMPLEMENTED(); }
bool IsPathSeparator(const char) { UNIMPLEMENTED(); }
bool DirExists(const char *) { UNIMPLEMENTED(); }
bool CreateDir(const char *) { UNIMPLEMENTED(); }
void *internal_start_thread(void *(*func)(void *arg), void *arg) {
  UNIMPLEMENTED();
}

void GetThreadStackAndTls(bool main, uptr *stk_addr, uptr *stk_size,
                          uptr *tls_addr, uptr *tls_size) {
  *stk_addr = (uptr)_stackTop;
  *stk_size = _stackBottom - _stackTop;
}

void SetAlternateSignalStack() {}

void internal__exit(int exitcode) { exit(exitcode); }

}  // namespace __sanitizer

#endif  // SANITIZER_CHEERPWASM

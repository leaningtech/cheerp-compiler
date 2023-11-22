#include "asan_interceptors.h"
#include "asan_internal.h"
#include "asan_mapping.h"
#include "asan_poisoning.h"
#include "asan_stack.h"
#include "asan_thread.h"

#if SANITIZER_CHEERPWASM
#  include <stdlib.h>

#  include "lsan/lsan_common.h"

namespace __asan {

uptr FindDynamicShadowStart() {
  uptr shadow_size_bytes = MemToShadowSize(kHighMemEnd);
  return MapDynamicShadow(shadow_size_bytes, ASAN_SHADOW_SCALE,
                          /*min_shadow_base_alignment*/ 0, kHighMemEnd);
}

void AsanCheckDynamicRTPrereqs() {}
void AsanCheckIncompatibleRT() {}
void InitializePlatformInterceptors() {}
void InitializePlatformExceptionHandlers() {}
bool IsSystemHeapAddress(uptr addr) { return false; }
void ReplaceSystemMalloc() {}
bool PlatformUnpoisonStacks() { return false; }
void AsanOnDeadlySignal(int, void *, void *) { UNIMPLEMENTED(); }

void *AsanDoesNotSupportStaticLinkage() {
  // On Linux, this is some magic that fails linking with -static.
  // On Cheerp, we have to do static linking, so we stub this out.
  return nullptr;
}

void InitializeAsanInterceptors() {}

void FlushUnneededASanShadowMemory(uptr p, uptr size) {}

static void (*tsd_destructor)(void *tsd) = nullptr;

struct tsd_key {
  tsd_key() : key(nullptr) {}
  ~tsd_key() {
    CHECK(tsd_destructor);
    if (key)
      (*tsd_destructor)(key);
  }
  void *key;
};

static /*thread_local*/ struct tsd_key key;

void AsanTSDInit(void (*destructor)(void *tsd)) {
  CHECK(!tsd_destructor);
  tsd_destructor = destructor;
}

void *AsanTSDGet() {
  CHECK(tsd_destructor);
  return key.key;
}

void AsanTSDSet(void *tsd) {
  CHECK(tsd_destructor);
  CHECK(tsd);
  CHECK(!key.key);
  key.key = tsd;
}

void PlatformTSDDtor(void *tsd) {
  CHECK(tsd_destructor);
  CHECK_EQ(key.key, tsd);
  key.key = nullptr;
  // Make sure that signal handler can not see a stale current thread pointer.
  atomic_signal_fence(memory_order_seq_cst);
  AsanThread::TSDDtor(tsd);
}

void InstallAtExitCheckLeaks() {
  if (CAN_SANITIZE_LEAKS) {
    if (common_flags()->detect_leaks && common_flags()->leak_check_at_exit) {
      if (flags()->halt_on_error)
        Atexit(__lsan::DoLeakCheck);
      else
        Atexit(__lsan::DoRecoverableLeakCheckVoid);
    }
  }
}

}  // namespace __asan

#endif  // SANITIZER_CHEERPWASM

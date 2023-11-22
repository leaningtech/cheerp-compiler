#include "lsan_common.h"

#if SANITIZER_CHEERPWASM && CAN_SANITIZE_LEAKS
#  include <stdlib.h>

#  include "lsan_thread.h"
#  include "sanitizer_common/sanitizer_common.h"
#  include "sanitizer_common/sanitizer_flags.h"
#  include "sanitizer_common/sanitizer_getauxval.h"
#  include "sanitizer_common/sanitizer_linux.h"
#  include "sanitizer_common/sanitizer_stackdepot.h"
#  include "sanitizer_common/sanitizer_thread_registry.h"

#  define LOG_THREADS(...)      \
    do {                        \
      if (flags()->log_threads) \
        Report(__VA_ARGS__);    \
    } while (0)

extern "C" {
extern char *volatile _heapStart;
extern char *volatile _stackTop;
}

namespace __lsan {
static LoadedModule *linker = nullptr;

void InitializePlatformSpecificModules() {}

static int disable_counter;
bool DisabledInThisThread() { return disable_counter > 0; }
void DisableInThisThread() { disable_counter++; }
void EnableInThisThread() {
  if (disable_counter == 0) {
    DisableCounterUnderflow();
  }
  disable_counter--;
}

u32 GetCurrentThread() { return 0; }
LoadedModule *GetLinker() { return linker; }

// Scans global variables for heap pointers.
void ProcessGlobalRegions(Frontier *frontier) {
  if (!flags()->use_globals)
    return;
  ScanGlobalRange((uptr)_stackTop, (uptr)_heapStart, frontier);
}

void ProcessPlatformSpecificAllocations(Frontier *frontier) {}

void LockStuffAndStopTheWorld(StopTheWorldCallback callback,
                              CheckForLeaksParam *argument) {
  // Currently, on Cheerp this does nothing and just calls the callback.
  // This works fine on a single-threaded environment.
  LockThreadRegistry();
  LockAllocator();
  StopTheWorld(callback, argument);
  UnlockAllocator();
  UnlockThreadRegistry();
}
// This is based on ProcessThreads in lsan_common.cc.
// We changed this to be a callback that gets called per thread by
// ThreadRegistry::RunCallbackForEachThreadLocked.
// We do not scan registers or DTLS since we do not have those.
// Finally, we can only obtain the stack pointer for the current thread,
// so we scan the full stack for other threads.
static void ProcessThreadsCallback(ThreadContextBase *tctx, void *arg) {
  if (tctx->status != ThreadStatusRunning)
    return;

  Frontier *frontier = reinterpret_cast<Frontier *>(arg);
  tid_t os_id = tctx->os_id;

  uptr stack_begin, stack_end, tls_begin, tls_end, cache_begin, cache_end;
  DTLS *dtls;
  bool thread_found =
      GetThreadRangesLocked(os_id, &stack_begin, &stack_end, &tls_begin,
                            &tls_end, &cache_begin, &cache_end, &dtls);
  if (!thread_found) {
    LOG_THREADS("Thread %llu not found in registry.\n", os_id);
    return;
  }

  if (flags()->use_stacks) {
    LOG_THREADS("Stack at %p-%p.\n", (void *)stack_begin, (void *)stack_end);

    // We can't get the SP for other threads to narrow down the range, but we
    // we can for the current thread.
    if (tctx->tid == GetCurrentThread()) {
      // uptr sp = (uptr) __builtin_frame_address(0);
      uptr sp = (uptr)alloca(
          0);  // CHEERPASAN: FIXME implement  __builtin_frame_address(0) (not
               // currently supported by cheerp)
      if (sp < stack_begin || sp >= stack_end) {
        // SP is outside the recorded stack range (e.g. the thread is running a
        // signal handler on alternate stack, or swapcontext was used).
        // Again, consider the entire stack range to be reachable.
        LOG_THREADS("WARNING: stack pointer not in stack range.\n");
      } else {
        // Shrink the stack range to ignore out-of-scope values.
        stack_begin = sp;
      }
    }

    ScanRangeForPointers(stack_begin, stack_end, frontier, "STACK", kReachable);
  }

  if (flags()->use_tls && tls_begin) {
    LOG_THREADS("TLS at %p-%p.\n", (void *)tls_begin, (void *)tls_end);
    // If the tls and cache ranges don't overlap, scan full tls range,
    // otherwise, only scan the non-overlapping portions
    if (cache_begin == cache_end || tls_end < cache_begin ||
        tls_begin > cache_end) {
      ScanRangeForPointers(tls_begin, tls_end, frontier, "TLS", kReachable);
    } else {
      if (tls_begin < cache_begin)
        ScanRangeForPointers(tls_begin, cache_begin, frontier, "TLS",
                             kReachable);
      if (tls_end > cache_end)
        ScanRangeForPointers(cache_end, tls_end, frontier, "TLS", kReachable);
    }
  }
}

void ProcessThreads(SuspendedThreadsList const &suspended_threads,
                    Frontier *frontier, tid_t caller_tid, uptr caller_sp) {
  GetThreadRegistryLocked()->RunCallbackForEachThreadLocked(
      ProcessThreadsCallback, frontier);
}

void HandleLeaks() {
  if (common_flags()->exitcode)
    Die();
}
}  // namespace __lsan

#endif  // SANITIZER_CHEERPWASM && CAN_SANITIZE_LEAKS

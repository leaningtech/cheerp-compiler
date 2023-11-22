#include "sanitizer_platform.h"

#if SANITIZER_CHEERPWASM
#  include "sanitizer_stoptheworld.h"

namespace __sanitizer {

class SuspendedThreadsListCheerp final : public SuspendedThreadsList {};

void StopTheWorld(StopTheWorldCallback callback, void *argument) {
  // TODO: have some workable alternative, since we can't just fork and
  // suspend the parent process. This does not matter when single thread.
  callback(SuspendedThreadsListCheerp(), argument);
}

}  // namespace __sanitizer

#endif  // SANITIZER_CHEERPWASM

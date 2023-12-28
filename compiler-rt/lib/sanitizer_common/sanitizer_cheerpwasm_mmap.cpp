#include "sanitizer_cheerpwasm_mmap.h"

#if SANITIZER_CHEERPWASM
#  include <assert.h>
#  include <cheerpintrin.h>
#  include <errno.h>
#  include <stdint.h>
#  include <sys/mman.h>

#  include "sanitizer_common.h"

extern "C" {
__attribute__((cheerp_asmjs, __weak__)) char *volatile _heapStart =
    (char *)0xdeadbeef;
__attribute__((cheerp_asmjs, __weak__)) char *volatile _heapEnd =
    (char *)0xdeadbeef;
}

namespace __sanitizer {

constexpr static uptr WASM_PAGESIZE = 64 * 1024;
constexpr static uptr MAX_VIRTUAL_ADDRESS = 2147483647;
constexpr static uptr MMAP_PAGESIZE = WASM_PAGESIZE;
constexpr static uptr MMAP_PAGECOUNT = MAX_VIRTUAL_ADDRESS / MMAP_PAGESIZE;

struct Page {
 private:
  constexpr static uint8_t FREE_PAGE = -1;
  constexpr static uint8_t FULLY_USED_PAGE = 0;

 public:
  uint8_t _start = FREE_PAGE;

  bool IsUsed() const { return _start != FREE_PAGE; }

  bool IsFree() const { return !IsUsed(); }

  void MakeUsed() { _start = FULLY_USED_PAGE; }

  void MakeFree() { _start = FREE_PAGE; }
};

static Page _pages[MMAP_PAGECOUNT];
static uptr _max_page_count = 0;
static uptr _mapped_pages = 0;
static bool _initialized = false;

static uptr PageCount() { return sizeof(_pages) / sizeof(_pages[0]); }

static uptr FindPages(uptr start, uptr page_count) {
  CHECK_LT(start, PageCount());

  uptr best = -1;
  uptr best_len = 0;

  for (uptr idx = start; idx < PageCount(); ++idx) {
    Page &page = _pages[idx];

    if (page.IsFree()) {
      if (!best_len)
        best = idx;
      ++best_len;
    } else {
      best_len = 0;
    }

    if (best_len == page_count)
      return best;
  }

  if (start)
    return FindPages(0, page_count);
  return -1;
}

static bool IsFree(uptr page, uptr page_count) {
  uptr end = page + page_count;
  for (uptr idx = page; idx < end; ++idx) {
    if (_pages[idx].IsUsed())
      return false;
  }
  return true;
}

// This function must not be inlined, as it is referenced in the PreExecutor
[[clang::noinline]] static int GrowMemory(int pages) {
  return __builtin_cheerp_grow_memory(pages);
}

static void ReservePages(uptr page, uptr page_count) {
  CHECK_LT(page, PageCount());

  uptr last_page = page + page_count;
  if (last_page > _mapped_pages) {
    uptr needed_pages = last_page - _mapped_pages;

    uptr wasm_pages =
        RoundUpTo(needed_pages * MMAP_PAGESIZE, WASM_PAGESIZE) / WASM_PAGESIZE;
    if (GrowMemory(wasm_pages) == -1) {
      assert(0 &&
             "If this triggered, you probably did not give enough memory to "
             "your program");
    }
    _mapped_pages = last_page;
  }

  uptr end = page + page_count;
  for (uptr idx = page; idx < end; ++idx) {
    _pages[idx].MakeUsed();
  }
}

static void FreePages(uptr page, uptr len) {
  CHECK_LT(page, PageCount());

  uptr end = page + ((len + MMAP_PAGESIZE - 1) / MMAP_PAGESIZE);
  for (uptr idx = page; idx < end; ++idx) {
    _pages[idx].MakeFree();
  }
}

uptr InternalMmap(uptr addr, uptr len, int prot, int flags, int fildes) {
  CHECK_EQ(0, addr % MMAP_PAGESIZE);
  CHECK_EQ(true, _initialized);
  CHECK_LT(addr, MAX_VIRTUAL_ADDRESS);

  len = RoundUpTo(len, MMAP_PAGESIZE);

  uptr page_count = len / MMAP_PAGESIZE;

  if (!(flags & (MAP_ANON | MAP_ANONYMOUS))) {
    errno = EINVAL;
    return -1;
  }

  uptr page = addr / MMAP_PAGESIZE;
  if (!(flags & MAP_FIXED)) {
    page = FindPages(addr / MMAP_PAGESIZE, page_count);
  }

  if (page == -1) {
    errno = ENOMEM;
    return -1;
  }

  ReservePages(page, page_count);
  uptr res = page * MMAP_PAGESIZE;
  internal_memset(reinterpret_cast<void *>(res), 0, len);
  return res;
}

void InternalMunmap(uptr addr, uptr len) {
  CHECK_EQ(0, addr % MMAP_PAGESIZE);
  FreePages(addr / MMAP_PAGESIZE, len);
}

void SetupMemoryMapping() {
  for (uptr idx = 0; idx < PageCount(); ++idx) {
    _pages[idx].MakeFree();
  }

  _max_page_count = reinterpret_cast<uptr>(_heapEnd) / MMAP_PAGESIZE;

  _mapped_pages = GrowMemory(0);
  if (_mapped_pages == -1) {
    _mapped_pages = _max_page_count;  // and hope for the best
  } else {
    _mapped_pages = (_mapped_pages * WASM_PAGESIZE) / MMAP_PAGESIZE;
  }

  uptr used_pages =
      RoundUpTo(reinterpret_cast<uptr>(_heapStart), MMAP_PAGESIZE) /
      MMAP_PAGESIZE;
  ReservePages(0, used_pages);
  _initialized = true;
}

uptr GetMaxUserVirtualAddress() { return MAX_VIRTUAL_ADDRESS; }

void *MmapOrDie(uptr size, const char *mem_type, bool raw_report) {
  size = RoundUpTo(size, GetPageSizeCached());
  uptr res =
      InternalMmap(0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1);
  if (UNLIKELY(res == -1))
    ReportMmapFailureAndDie(size, mem_type, "allocate", errno, raw_report);
  IncreaseTotalMmap(size);
  return (void *)res;
}

void UnmapOrDie(void *addr, uptr size) {
  InternalMunmap(reinterpret_cast<uptr>(addr), size);
  DecreaseTotalMmap(size);
}

bool MemoryRangeIsAvailable(uptr range_start, uptr range_end) {
  range_start = RoundDownTo(range_start, MMAP_PAGESIZE);
  range_end = RoundUpTo(range_start, MMAP_PAGESIZE);
  return IsFree(range_start, range_end);
}

uptr GetMmapGranularity() { return MMAP_PAGESIZE; }

void UnmapFromTo(uptr from, uptr to) {
  if (to == from)
    return;
  CHECK(to >= from);
  InternalMunmap(from, to - from);
}

// Doesn't actually protect memory on cheerp
void *MmapNoAccess(uptr size) {
  unsigned flags = MAP_PRIVATE | MAP_ANON | MAP_NORESERVE;
  return (void *)InternalMmap(0, size, PROT_NONE, flags, -1);
}

static bool MmapFixed(uptr fixed_addr, uptr size, int additional_flags,
                      const char *name) {
  size = RoundUpTo(size, GetPageSizeCached());
  fixed_addr = RoundDownTo(fixed_addr, GetPageSizeCached());
  uptr p =
      InternalMmap(fixed_addr, size, PROT_READ | PROT_WRITE,
                   MAP_PRIVATE | MAP_FIXED | additional_flags | MAP_ANON, -1);
  int reserrno = errno;
  if (p == -1) {
    Report(
        "ERROR: %s failed to "
        "allocate 0x%zx (%zd) bytes at address %zx (errno: %d)\n",
        SanitizerToolName, size, size, fixed_addr, reserrno);
    return false;
  }
  IncreaseTotalMmap(size);
  return true;
}

bool MmapFixedNoReserve(uptr fixed_addr, uptr size, const char *name) {
  return MmapFixed(fixed_addr, size, MAP_NORESERVE, name);
}

void *MmapOrDieOnFatalError(uptr size, const char *mem_type) {
  size = RoundUpTo(size, GetPageSizeCached());
  uptr res =
      InternalMmap(0, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1);
  int reserrno = errno;
  if (UNLIKELY(res == -1)) {
    if (reserrno == ENOMEM)
      return nullptr;
    ReportMmapFailureAndDie(size, mem_type, "allocate", reserrno);
  }
  IncreaseTotalMmap(size);
  return (void *)res;
}

// We want to map a chunk of address space aligned to 'alignment'.
// We do it by mapping a bit more and then unmapping redundant pieces.
// We probably can do it with fewer syscalls in some OS-dependent way.
void *MmapAlignedOrDieOnFatalError(uptr size, uptr alignment,
                                   const char *mem_type) {
  CHECK(IsPowerOfTwo(size));
  CHECK(IsPowerOfTwo(alignment));
  uptr map_size = size + alignment;
  uptr map_res = (uptr)MmapOrDieOnFatalError(map_size, mem_type);
  if (UNLIKELY(!map_res))
    return nullptr;
  uptr map_end = map_res + map_size;
  uptr res = map_res;
  if (!IsAligned(res, alignment)) {
    res = (map_res + alignment - 1) & ~(alignment - 1);
    UnmapOrDie((void *)map_res, res - map_res);
  }
  uptr end = res + size;
  end = RoundUpTo(end, GetPageSizeCached());
  if (end != map_end)
    UnmapOrDie((void *)end, map_end - end);
  return (void *)res;
}

void *MmapNoReserveOrDie(uptr size, const char *mem_type) {
  size = RoundUpTo(size, GetPageSizeCached());
  uptr p = InternalMmap(0, size, PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANON | MAP_NORESERVE, -1);
  int reserrno = errno;
  if (UNLIKELY(p == -1))
    ReportMmapFailureAndDie(size, mem_type, "allocate noreserve", reserrno);
  IncreaseTotalMmap(size);
  return (void *)p;
}
bool MmapFixedSuperNoReserve(uptr, uptr, const char *) { UNIMPLEMENTED(); }

}  // namespace __sanitizer

#endif  // SANITIZER_CHEERPWASM

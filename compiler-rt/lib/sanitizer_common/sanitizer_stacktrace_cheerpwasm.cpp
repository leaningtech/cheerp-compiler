#include "sanitizer_common.h"
#include "sanitizer_flags.h"
#include "sanitizer_placement_new.h"
#include "sanitizer_stacktrace.h"
#if SANITIZER_CHEERPWASM

#  include <uchar.h>

#  include "sanitizer_cheerpwasm.h"

// We have to define LEAN_CXX_LIB as client lib defines it own new operator
// which conflicts with the one that asan declares
#  define LEAN_CXX_LIB
#  include <client/cheerp/types.h>

// The sanitizer are able to output stacktraces to give more context of where
// something happened. One might expect that this would be implemented by
// calling the stack unwinder and explicitly tell it how many functions to skip.
// However, the sanitizer_common library doesn't work like that. It uses
// GET_CALLER_PC_BP_SP and GET_CURRENT_PC_BP_SP. Then is passes the PC, BP and
// SP values along until some other function uses those values to actually
// unwind the stack. On standard machines, the stack can be unwound by treating
// BP as a linked list. But on cheerp, BP does not exist. Therefore we must be
// able to unwind from a PC value that may no longer be on the execution stack,
// and thus are forced to cache the entire call stack.

namespace __sanitizer {

static uptr _prev_trace_len = 0;
static uptr _prev_trace[256];
static [[cheerp::genericjs]] client::Object* _symbols = nullptr;
static char _name_cache[256];
static uptr _name_len = 0;

[[cheerp::genericjs]] static uptr ConvertFrameToPc(client::String* frame) {
  if (client::TArray<client::String>* match =
          frame->match("\\bwasm-function\\[\\d+\\]:(0x[0-9a-f]+)")) {
    if (match->get_length() >= 2) {
      uptr pc = 0;
      __asm__("%1[1]" : "=r"(pc) : "r"(match));
      return pc;
    }
  }
  if (client::TArray<client::String>* match =
          frame->match(":(\\d+):\\d+(?:\\)|$)")) {
    if (match->get_length() >= 2) {
      uptr pc = 0;
      __asm__("%1[1]" : "=r"(pc) : "r"(match));
      return 0x80000000 | pc;
    }
  }
  return 0;
}

[[cheerp::genericjs]] static void CachePc(uptr pc, client::String* frame) {
  if (UNLIKELY(_symbols == nullptr))
    __asm__("{}" : "=r"(_symbols));

  __asm__("%0[%1]=%2" : : "r"(_symbols), "r"(pc), "r"(frame));
}

[[cheerp::genericjs]] __attribute__((weak)) uptr GetCallstack(uptr* dest,
                                                              uptr dest_len,
                                                              uptr skip) {
  client::TArray<client::String>* callstack = nullptr;
  __asm__("(new Error()).stack.toString().split('\\n')" : "=r"(callstack));

  uptr j = 0;
  for (uptr i = 0; j < dest_len && i < callstack->get_length(); ++i) {
    client::String* frame = (*callstack)[i];

    if (frame->startsWith("Error"))
      continue;

    if (skip > 0) {
      --skip;
      continue;
    }

    uptr pc = ConvertFrameToPc(frame);
    CachePc(pc, frame);
    dest[j++] = pc;

    if (pc == 0)
      break;
  }
  return j;
}

[[cheerp::genericjs]] static client::String* GetFunctionAtPc(uptr pc) {
  if (_symbols == nullptr)
    return nullptr;

  client::String* result = nullptr;
  __asm__("%1[%2]" : "=r"(result) : "r"(_symbols), "r"(pc));
  bool res = false;
  __asm__("!!%1" : "=r"(res) : "r"(result));
  if (res)
    return result;
  return 0;
}

[[cheerp::genericjs]] static uptr ClientStringToUtf16(const client::String* str,
                                                      char16_t* dest,
                                                      uptr len) {
  uptr i = 0;
  for (; i < len && i < str->get_length(); ++i) {
    dest[i] = str->charCodeAt(i);
  }
  return i;
}

[[cheerp::genericjs]] __attribute__((weak)) uptr GetUtf16FunctionNameAtPc(
    uptr pc, char16_t* dest, uptr len) {
  client::String* frame = GetFunctionAtPc(pc);

  if (frame) {
    if (client::TArray<client::String>* match =
            frame->match("^\\s+at (.*) \\(.*\\)$")) {
      return ClientStringToUtf16((*match)[1], dest, len);
    } else if (client::TArray<client::String>* match =
                   frame->match("^(.+?)@")) {
      return ClientStringToUtf16((*match)[1], dest, len);
    }
  }
  return 0;
}

const char* GetFunctionNameAtPc(uptr pc) {
  if (common_flags()->disable_traces)
    return "<stacktraces were disabled due to the 'disable_traces' flag>";
  char16_t buffer[256];
  uptr buffer_len =
      GetUtf16FunctionNameAtPc(pc, buffer, sizeof(buffer) / sizeof(buffer[0]));
  if (buffer_len == 0)
    return nullptr;

  _name_len = Utf16ToUtf8(_name_cache,
                          (sizeof(_name_cache) / sizeof(_name_cache[0])) - 1,
                          buffer, buffer_len);

  if (_name_len >= sizeof(_name_cache))
    _name_len = sizeof(_name_cache) - 1;  // string was truncated
  _name_cache[_name_len] = '\0';

  return _name_cache;
}

void BufferedStackTrace::UnwindFast(uptr pc, uptr bp, uptr stack_top,
                                    uptr stack_bottom, u32 max_depth) {
  size = _prev_trace_len > kStackTraceMax ? kStackTraceMax : _prev_trace_len;
  internal_memcpy(trace_buffer, _prev_trace, size * sizeof(_prev_trace[0]));
  trace_buffer[0] = pc;
}

[[clang::always_inline]] uptr GetReturnAddress(uptr idx) {
  if (common_flags()->disable_traces) {
    _prev_trace[0] = 1;
    _prev_trace_len = 1;
    return 1;
  }
  _prev_trace_len = GetCallstack(
      _prev_trace, sizeof(_prev_trace) / sizeof(_prev_trace[0]), idx + 1);

  if (_prev_trace_len)
    return _prev_trace[0];
  return 0;
}

[[clang::always_inline]] uptr StackTrace::GetCurrentPc() {
  return GetReturnAddress(0);
}

}  // namespace __sanitizer

#endif  // SANITIZER_CHEERPWASM

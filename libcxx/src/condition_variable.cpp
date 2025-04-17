//===----------------------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include <__config>

#if !defined(_LIBCPP_HAS_NO_THREADS) || defined(__CHEERP__)

#include <condition_variable>
#if defined(__CHEERP__) && !defined(__ASMJS__)
#include <cheerp/client.h>
#else
#include <thread>
#endif
#include <system_error>

#if defined(__ELF__) && defined(_LIBCPP_LINK_PTHREAD_LIB)
#  pragma comment(lib, "pthread")
#endif

_LIBCPP_PUSH_MACROS
#include <__undef_macros>

_LIBCPP_BEGIN_NAMESPACE_STD

// ~condition_variable is defined elsewhere.

void
condition_variable::notify_one() noexcept
{
#if !(defined(__CHEERP__) && !defined(__ASMJS__))
    __libcpp_condvar_signal(&__cv_);
#endif
}

void
condition_variable::notify_all() noexcept
{
#if !(defined(__CHEERP__) && !defined(__ASMJS__))
    __libcpp_condvar_broadcast(&__cv_);
#endif
}

void
condition_variable::wait(unique_lock<mutex>& lk) noexcept
{
    if (!lk.owns_lock())
        __throw_system_error(EPERM,
                                  "condition_variable::wait: mutex not locked");
#if defined(__CHEERP__) && !defined(__ASMJS__)
    cheerp::console_log("Cheerp: condition_variable::wait can't block");
#else
    int ec = __libcpp_condvar_wait(&__cv_, lk.mutex()->native_handle());
    if (ec)
        __throw_system_error(ec, "condition_variable wait failed");
#endif
}

void
condition_variable::__do_timed_wait(unique_lock<mutex>& lk,
     chrono::time_point<chrono::system_clock, chrono::nanoseconds> tp) noexcept
{
    using namespace chrono;
    if (!lk.owns_lock())
        __throw_system_error(EPERM,
                            "condition_variable::timed wait: mutex not locked");
#if defined(__CHEERP__) && !defined(__ASMJS__)
    cheerp::console_log("Cheerp: condition_variable::timed_wait can't block");
#else
    nanoseconds d = tp.time_since_epoch();
    if (d > nanoseconds(0x59682F000000E941))
        d = nanoseconds(0x59682F000000E941);
    __libcpp_timespec_t ts;
    seconds s = duration_cast<seconds>(d);
    typedef decltype(ts.tv_sec) ts_sec;
    _LIBCPP_CONSTEXPR ts_sec ts_sec_max = numeric_limits<ts_sec>::max();
    if (s.count() < ts_sec_max)
    {
        ts.tv_sec = static_cast<ts_sec>(s.count());
        ts.tv_nsec = static_cast<decltype(ts.tv_nsec)>((d - s).count());
    }
    else
    {
        ts.tv_sec = ts_sec_max;
        ts.tv_nsec = giga::num - 1;
    }
    int ec = __libcpp_condvar_timedwait(&__cv_, lk.mutex()->native_handle(), &ts);
    if (ec != 0 && ec != ETIMEDOUT)
        __throw_system_error(ec, "condition_variable timed_wait failed");
#endif
}

void
notify_all_at_thread_exit(condition_variable& cond, unique_lock<mutex> lk)
{
#if !(defined(__CHEERP__) && !defined(__ASMJS__))
    auto& tl_ptr = __thread_local_data();
    // If this thread was not created using std::thread then it will not have
    // previously allocated.
    if (tl_ptr.get() == nullptr) {
        tl_ptr.set_pointer(new __thread_struct);
    }
    __thread_local_data()->notify_all_at_thread_exit(&cond, lk.release());
#endif
}

_LIBCPP_END_NAMESPACE_STD

_LIBCPP_POP_MACROS

#endif // !_LIBCPP_HAS_NO_THREADS

//===------------------------- mutex.cpp ----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "mutex"
#include "limits"
#include "system_error"
#include "include/atomic_support.h"
#include "__undef_macros"

#ifndef _LIBCPP_HAS_NO_THREADS
#if defined(__ELF__) && defined(_LIBCPP_LINK_PTHREAD_LIB)
#pragma comment(lib, "pthread")
#endif
#endif

_LIBCPP_BEGIN_NAMESPACE_STD
#if !defined(_LIBCPP_HAS_NO_THREADS) || defined(__CHEERP__)

const defer_lock_t  defer_lock{};
const try_to_lock_t try_to_lock{};
const adopt_lock_t  adopt_lock{};

// ~mutex is defined elsewhere

void
mutex::lock()
{
#ifdef __CHEERP__
    if(__m_)
      client::console.log("Cheerp: mutex::lock can't block");
    else
      __m_++;
#else
    int ec = __libcpp_mutex_lock(&__m_);
    if (ec)
        __throw_system_error(ec, "mutex lock failed");
#endif
}

bool
mutex::try_lock() _NOEXCEPT
{
#ifdef __CHEERP__
    if(__m_)
      return false;
    else
    {
      __m_++;
      return true;
    }
#else
    return __libcpp_mutex_trylock(&__m_);
#endif
}

void
mutex::unlock() _NOEXCEPT
{
#ifdef __CHEERP__
    __m_--;
#else
    int ec = __libcpp_mutex_unlock(&__m_);
    (void)ec;
    _LIBCPP_ASSERT(ec == 0, "call to mutex::unlock failed");
#endif
}

// recursive_mutex

recursive_mutex::recursive_mutex()
{
#ifdef __CHEERP__
    __m_ = 0;
#else
    int ec = __libcpp_recursive_mutex_init(&__m_);
    if (ec)
        __throw_system_error(ec, "recursive_mutex constructor failed");
#endif
}

recursive_mutex::~recursive_mutex()
{
#ifndef __CHEERP__
    int e = __libcpp_recursive_mutex_destroy(&__m_);
    (void)e;
    _LIBCPP_ASSERT(e == 0, "call to ~recursive_mutex() failed");
#endif
}

void
recursive_mutex::lock()
{
#ifdef __CHEERP__
    __m_++;
#else
    int ec = __libcpp_recursive_mutex_lock(&__m_);
    if (ec)
        __throw_system_error(ec, "recursive_mutex lock failed");
#endif
}

void
recursive_mutex::unlock() _NOEXCEPT
{
#ifdef __CHEERP__
    __m_--;
#else
    int e = __libcpp_recursive_mutex_unlock(&__m_);
    (void)e;
    _LIBCPP_ASSERT(e == 0, "call to recursive_mutex::unlock() failed");
#endif
}

bool
recursive_mutex::try_lock() _NOEXCEPT
{
#ifdef __CHEERP__
    __m_++;
    return true;
#else
    return __libcpp_recursive_mutex_trylock(&__m_);
#endif
}

// timed_mutex

timed_mutex::timed_mutex()
    : __locked_(false)
{
}

timed_mutex::~timed_mutex()
{
    lock_guard<mutex> _(__m_);
}

void
timed_mutex::lock()
{
    unique_lock<mutex> lk(__m_);
#ifdef __CHEERP__
    if (__locked_)
        client::console.log("Cheerp: timed_mutex::lock can't block");
#else
    while (__locked_)
        __cv_.wait(lk);
#endif
    __locked_ = true;
}

bool
timed_mutex::try_lock() _NOEXCEPT
{
    unique_lock<mutex> lk(__m_, try_to_lock);
    if (lk.owns_lock() && !__locked_)
    {
        __locked_ = true;
        return true;
    }
    return false;
}

void
timed_mutex::unlock() _NOEXCEPT
{
    lock_guard<mutex> _(__m_);
    __locked_ = false;
#ifndef __CHEERP__
    __cv_.notify_one();
#endif
}

// recursive_timed_mutex

recursive_timed_mutex::recursive_timed_mutex()
    : __count_(0)
#ifndef __CHEERP__
      ,__id_{}
#endif
{
}

recursive_timed_mutex::~recursive_timed_mutex()
{
    lock_guard<mutex> _(__m_);
}

void
recursive_timed_mutex::lock()
{
#ifndef __CHEERP__
    __thread_id id = this_thread::get_id();
#endif
    unique_lock<mutex> lk(__m_);
#ifndef __CHEERP__
    if (id ==__id_)
#endif
    {
        if (__count_ == numeric_limits<size_t>::max())
            __throw_system_error(EAGAIN, "recursive_timed_mutex lock limit reached");
        ++__count_;
        return;
    }
#ifndef __CHEERP__
    while (__count_ != 0)
        __cv_.wait(lk);
    __count_ = 1;
    __id_ = id;
#endif
}

bool
recursive_timed_mutex::try_lock() _NOEXCEPT
{
#ifndef __CHEERP__
    __thread_id id = this_thread::get_id();
#endif
    unique_lock<mutex> lk(__m_, try_to_lock);
#ifdef __CHEERP__
    return true;
#else
    if (lk.owns_lock() && (__count_ == 0 || id == __id_))
    {
        if (__count_ == numeric_limits<size_t>::max())
            return false;
        ++__count_;
        __id_ = id;
        return true;
    }
    return false;
#endif
}

void
recursive_timed_mutex::unlock() _NOEXCEPT
{
    unique_lock<mutex> lk(__m_);
    if (--__count_ == 0)
    {
#ifndef __CHEERP__
        __id_.__reset();
#endif
        lk.unlock();
#ifndef __CHEERP__
        __cv_.notify_one();
#endif
    }
}

#endif // !_LIBCPP_HAS_NO_THREADS

// If dispatch_once_f ever handles C++ exceptions, and if one can get to it
// without illegal macros (unexpected macros not beginning with _UpperCase or
// __lowercase), and if it stops spinning waiting threads, then call_once should
// call into dispatch_once_f instead of here. Relevant radar this code needs to
// keep in sync with:  7741191.

#ifndef _LIBCPP_HAS_NO_THREADS
_LIBCPP_SAFE_STATIC static __libcpp_mutex_t mut = _LIBCPP_MUTEX_INITIALIZER;
_LIBCPP_SAFE_STATIC static __libcpp_condvar_t cv = _LIBCPP_CONDVAR_INITIALIZER;
#endif

void __call_once(volatile once_flag::_State_type& flag, void* arg,
                 void (*func)(void*))
{
#if defined(_LIBCPP_HAS_NO_THREADS)
    if (flag == 0)
    {
#ifndef _LIBCPP_NO_EXCEPTIONS
        try
        {
#endif  // _LIBCPP_NO_EXCEPTIONS
            flag = 1;
            func(arg);
            flag = ~once_flag::_State_type(0);
#ifndef _LIBCPP_NO_EXCEPTIONS
        }
        catch (...)
        {
            flag = 0;
            throw;
        }
#endif  // _LIBCPP_NO_EXCEPTIONS
    }
#else // !_LIBCPP_HAS_NO_THREADS
    __libcpp_mutex_lock(&mut);
    while (flag == 1)
        __libcpp_condvar_wait(&cv, &mut);
    if (flag == 0)
    {
#ifndef _LIBCPP_NO_EXCEPTIONS
        try
        {
#endif  // _LIBCPP_NO_EXCEPTIONS
            __libcpp_relaxed_store(&flag, once_flag::_State_type(1));
            __libcpp_mutex_unlock(&mut);
            func(arg);
            __libcpp_mutex_lock(&mut);
            __libcpp_atomic_store(&flag, ~once_flag::_State_type(0),
                                  _AO_Release);
            __libcpp_mutex_unlock(&mut);
            __libcpp_condvar_broadcast(&cv);
#ifndef _LIBCPP_NO_EXCEPTIONS
        }
        catch (...)
        {
            __libcpp_mutex_lock(&mut);
            __libcpp_relaxed_store(&flag, once_flag::_State_type(0));
            __libcpp_mutex_unlock(&mut);
            __libcpp_condvar_broadcast(&cv);
            throw;
        }
#endif  // _LIBCPP_NO_EXCEPTIONS
    }
    else
        __libcpp_mutex_unlock(&mut);
#endif // !_LIBCPP_HAS_NO_THREADS
}
_LIBCPP_END_NAMESPACE_STD

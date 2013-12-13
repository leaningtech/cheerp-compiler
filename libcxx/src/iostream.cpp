//===------------------------ iostream.cpp --------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "__std_stream"
#include "__locale"
#include "string"
#include "new"

#define _str(s) #s
#define str(s) _str(s)
#define _LIBCPP_ABI_NAMESPACE_STR str(_LIBCPP_ABI_NAMESPACE)

_LIBCPP_BEGIN_NAMESPACE_STD

#ifndef _LIBCPP_HAS_NO_STDIN
_ALIGNAS_TYPE (istream) _LIBCPP_FUNC_VIS istream cin [[noinit]]
#if defined(_LIBCPP_ABI_MICROSOFT) && defined(__clang__)
__asm__("?cin@" _LIBCPP_ABI_NAMESPACE_STR "@std@@3V?$basic_istream@DU?$char_traits@D@" _LIBCPP_ABI_NAMESPACE_STR "@std@@@12@A")
#endif
;
_ALIGNAS_TYPE (__stdinbuf<char> ) static __stdinbuf<char> __cin [[noinit]];
static mbstate_t mb_cin;
_ALIGNAS_TYPE (wistream) _LIBCPP_FUNC_VIS wistream wcin [[noinit]]
#if defined(_LIBCPP_ABI_MICROSOFT) && defined(__clang__)
__asm__("?wcin@" _LIBCPP_ABI_NAMESPACE_STR "@std@@3V?$basic_istream@_WU?$char_traits@_W@" _LIBCPP_ABI_NAMESPACE_STR "@std@@@12@A")
#endif
;
_ALIGNAS_TYPE (__stdinbuf<wchar_t> ) static __stdinbuf<wchar_t> __wcin [[noinit]];
static mbstate_t mb_wcin;
#endif

#ifndef _LIBCPP_HAS_NO_STDOUT
_ALIGNAS_TYPE (ostream) _LIBCPP_FUNC_VIS ostream cout [[noinit]]
#if defined(_LIBCPP_ABI_MICROSOFT) && defined(__clang__)
__asm__("?cout@" _LIBCPP_ABI_NAMESPACE_STR "@std@@3V?$basic_ostream@DU?$char_traits@D@" _LIBCPP_ABI_NAMESPACE_STR "@std@@@12@A")
#endif
;
_ALIGNAS_TYPE (__stdoutbuf<char>) static __stdoutbuf<char> __cout [[noinit]];
static mbstate_t mb_cout;
_ALIGNAS_TYPE (wostream) _LIBCPP_FUNC_VIS wostream wcout [[noinit]]
#if defined(_LIBCPP_ABI_MICROSOFT) && defined(__clang__)
__asm__("?wcout@" _LIBCPP_ABI_NAMESPACE_STR "@std@@3V?$basic_ostream@_WU?$char_traits@_W@" _LIBCPP_ABI_NAMESPACE_STR "@std@@@12@A")
#endif
;
_ALIGNAS_TYPE (__stdoutbuf<wchar_t>) static __stdoutbuf<wchar_t> __wcout [[noinit]];
static mbstate_t mb_wcout;
#endif

_ALIGNAS_TYPE (ostream) _LIBCPP_FUNC_VIS ostream cerr [[noinit]]
#if defined(_LIBCPP_ABI_MICROSOFT) && defined(__clang__)
__asm__("?cerr@" _LIBCPP_ABI_NAMESPACE_STR "@std@@3V?$basic_ostream@DU?$char_traits@D@" _LIBCPP_ABI_NAMESPACE_STR "@std@@@12@A")
#endif
;
_ALIGNAS_TYPE (__stdoutbuf<char>) static __stdoutbuf<char> __cerr [[noinit]];
static mbstate_t mb_cerr;
_ALIGNAS_TYPE (wostream) _LIBCPP_FUNC_VIS wostream wcerr [[noinit]];
#if defined(_LIBCPP_ABI_MICROSOFT) && defined(__clang__)
__asm__("?wcerr@" _LIBCPP_ABI_NAMESPACE_STR "@std@@3V?$basic_ostream@_WU?$char_traits@_W@" _LIBCPP_ABI_NAMESPACE_STR "@std@@@12@A")
#endif
;
_ALIGNAS_TYPE (__stdoutbuf<wchar_t>) static __stdoutbuf<wchar_t> __wcerr [[noinit]];
static mbstate_t mb_wcerr;

_ALIGNAS_TYPE (ostream) _LIBCPP_FUNC_VIS ostream clog [[noinit]]
#if defined(_LIBCPP_ABI_MICROSOFT) && defined(__clang__)
__asm__("?clog@" _LIBCPP_ABI_NAMESPACE_STR "@std@@3V?$basic_ostream@DU?$char_traits@D@" _LIBCPP_ABI_NAMESPACE_STR "@std@@@12@A")
#endif
;
_ALIGNAS_TYPE (wostream) _LIBCPP_FUNC_VIS wostream wclog [[noinit]]
#if defined(_LIBCPP_ABI_MICROSOFT) && defined(__clang__)
__asm__("?wclog@" _LIBCPP_ABI_NAMESPACE_STR "@std@@3V?$basic_ostream@_WU?$char_traits@_W@" _LIBCPP_ABI_NAMESPACE_STR "@std@@@12@A")
#endif
;

_LIBCPP_HIDDEN ios_base::Init __start_std_streams;

// On Windows the TLS storage for locales needs to be initialized before we create
// the standard streams, otherwise it may not be alive during program termination
// when we flush the streams.
static void force_locale_initialization() {
#if defined(_LIBCPP_MSVCRT_LIKE)
  static bool once = []() {
    auto loc = newlocale(LC_ALL_MASK, "C", 0);
    {
        __libcpp_locale_guard g(loc); // forces initialization of locale TLS
        ((void)g);
    }
    freelocale(loc);
    return true;
  }();
  ((void)once);
#endif
}

class DoIOSInit {
public:
	DoIOSInit();
	~DoIOSInit();
};

DoIOSInit::DoIOSInit()
{
    force_locale_initialization();

#ifndef _LIBCPP_HAS_NO_STDIN
    istream* cin_ptr  = ::new(&cin)  istream(::new(&__cin)  __stdinbuf <char>(stdin, &mb_cin));
    wistream* wcin_ptr  = ::new(&wcin)  wistream(::new(&__wcin)  __stdinbuf <wchar_t>(stdin, &mb_wcin));
#endif
#ifndef _LIBCPP_HAS_NO_STDOUT
    ostream* cout_ptr = ::new(&cout) ostream(::new(&__cout) __stdoutbuf<char>(stdout, &mb_cout));
    wostream* wcout_ptr = ::new(&wcout) wostream(::new(&__wcout) __stdoutbuf<wchar_t>(stdout, &mb_wcout));
#endif
    ostream* cerr_ptr = ::new(&cerr) ostream(::new(&__cerr) __stdoutbuf<char>(stderr, &mb_cerr));
                        ::new(&clog) ostream(cerr_ptr->rdbuf());
    wostream* wcerr_ptr = ::new(&wcerr) wostream(::new(&__wcerr) __stdoutbuf<wchar_t>(stderr, &mb_wcerr));
                          ::new(&wclog) wostream(wcerr_ptr->rdbuf());

#if !defined(_LIBCPP_HAS_NO_STDIN) && !defined(_LIBCPP_HAS_NO_STDOUT)
    cin_ptr->tie(cout_ptr);
    wcin_ptr->tie(wcout_ptr);
#endif
    _VSTD::unitbuf(*cerr_ptr);
    _VSTD::unitbuf(*wcerr_ptr);
#ifndef _LIBCPP_HAS_NO_STDOUT
    cerr_ptr->tie(cout_ptr);
    wcerr_ptr->tie(wcout_ptr);
#endif
}

DoIOSInit::~DoIOSInit()
{
#ifndef _LIBCPP_HAS_NO_STDOUT
    ostream* cout_ptr = reinterpret_cast<ostream*>(&cout);
    wostream* wcout_ptr = reinterpret_cast<wostream*>(&wcout);
    cout_ptr->flush();
    wcout_ptr->flush();
#endif

    ostream* clog_ptr = reinterpret_cast<ostream*>(&clog);
    wostream* wclog_ptr = reinterpret_cast<wostream*>(&wclog);
    clog_ptr->flush();
    wclog_ptr->flush();
}

ios_base::Init::Init()
{
    static DoIOSInit init_the_streams; // gets initialized once
}

ios_base::Init::~Init()
{
}

_LIBCPP_END_NAMESPACE_STD

//===---- cheerpintrin.h - Cheerp intrinsics ------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2013-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef __CHEERPINTRIN_H
#define __CHEERPINTRIN_H

namespace [[cheerp::genericjs]] {

template<class R, class P>
R* __builtin_cheerp_pointer_base(const P* ptr);

template<class P>
size_t __builtin_cheerp_pointer_offset(const P* ptr);

template<class P>
bool __builtin_cheerp_is_linear_heap(const P* ptr);
/* This method returns a closure. When it is invoked it will execute func with obj as the first argument
   and its own argument as the second one
*/
template<class R,class T,class O>
R* __builtin_cheerp_create_closure(T* func, O* obj);

template<class R,class T>
R* __builtin_cheerp_make_complete_object(T*);

template<class R,class T>
R* __builtin_cheerp_make_regular(T*, int);

template<class P>
size_t __builtin_cheerp_pointer_kind(const P* ptr);

int __builtin_cheerp_grow_memory(int bytes);

void* __buitin_cheerp_stack_save();

void __buitin_cheerp_stack_restore(void*);

template<class R, class P>
R* __builtin_cheerp_downcast(const P* ptr, int offset);

template<class P>
int __builtin_cheerp_downcast_current(const P* ptr);

template<class P>
[[noreturn]]
void __builtin_cheerp_throw(const P* ptr);

}
#endif /* __CHEERPINTRIN_H */

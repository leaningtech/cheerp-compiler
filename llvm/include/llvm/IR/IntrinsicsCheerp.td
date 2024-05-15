//===- IntrinsicsCheerp.td - Defines Cheerp intrinsics -----*- tablegen -*-===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2013-2016 Leaning Technologies
//
//
//===----------------------------------------------------------------------===//
//
// This file defines all of the Cheerp-specific intrinsics.
//
//===----------------------------------------------------------------------===//


//===-------------------------- Cheerp Intrinsics --------------------------===//
//
// Casting helpers
def int_cheerp_downcast  : Intrinsic<[llvm_anyptr_ty],
                             [llvm_anyptr_ty, llvm_i32_ty],
                             [NoCapture<ArgIndex<0>>]>;

def int_cheerp_downcast_current  : Intrinsic<[llvm_i32_ty],
                             [llvm_anyptr_ty],
                             [NoCapture<ArgIndex<0>>]>;

def int_cheerp_upcast_collapsed  : Intrinsic<[llvm_anyptr_ty],
                             [llvm_anyptr_ty],
                             []>;
def int_cheerp_virtualcast  : Intrinsic<[llvm_anyptr_ty],
                             [llvm_anyptr_ty, llvm_i32_ty],
                             [NoCapture<ArgIndex<0>>]>;

def int_cheerp_cast_user  : Intrinsic<[llvm_anyptr_ty],
                             [llvm_anyptr_ty],
                             []>;

def int_cheerp_typed_ptrcast : Intrinsic<[llvm_anyptr_ty],
                                      [llvm_anyptr_ty],
                                      [IntrNoMem]>;


// Type safe memory allocation
def int_cheerp_allocate  : Intrinsic<[llvm_anyptr_ty],
                             [llvm_anyptr_ty,llvm_i32_ty]>;
def int_cheerp_allocate_array  : Intrinsic<[llvm_anyptr_ty],
                             [llvm_anyptr_ty,llvm_i32_ty]>;
def int_cheerp_get_array_len  : Intrinsic<[llvm_i32_ty],
                             [llvm_anyptr_ty]>;
def int_cheerp_reallocate  : Intrinsic<[llvm_anyptr_ty],
                             [llvm_anyptr_ty,llvm_i32_ty]>;
def int_cheerp_deallocate  : Intrinsic<[],
                             [llvm_anyptr_ty]>;
def int_cheerp_coro_alloc  : Intrinsic<[llvm_ptr_ty],
                             [llvm_i32_ty]>;

// Access to pointer components
def int_cheerp_pointer_base : Intrinsic<[llvm_anyptr_ty],
                                [llvm_anyptr_ty],
                                [IntrNoMem]>;
def int_cheerp_pointer_offset : Intrinsic<[llvm_i32_ty],
                                [llvm_anyptr_ty],
                                [IntrNoMem]>;
def int_cheerp_is_linear_heap : Intrinsic<[llvm_i1_ty],
                                [llvm_anyptr_ty],
                                [IntrNoMem]>;
def int_cheerp_pointer_elem_size : Intrinsic<[llvm_i32_ty],
                                [llvm_anyptr_ty],
                                [IntrNoMem]>;

// Closure creation for callbacks
def int_cheerp_create_closure : Intrinsic<[llvm_anyptr_ty],
                                [llvm_anyptr_ty, llvm_anyptr_ty],
                                [IntrNoMem]>;

// Forging pointers of specific kinds
def int_cheerp_make_complete_object : Intrinsic<[llvm_anyptr_ty],
                                      [llvm_anyptr_ty],
                                      [IntrNoMem]>;
def int_cheerp_make_regular : Intrinsic<[llvm_anyptr_ty],
                                      [llvm_anyptr_ty, llvm_i32_ty],
                                      [IntrNoMem]>;

// Access to pointer information
def int_cheerp_pointer_kind : Intrinsic<[llvm_i32_ty],
                                [llvm_anyptr_ty],
                                [IntrNoMem]>;

def int_cheerp_grow_memory : Intrinsic<[llvm_i32_ty],
                                [llvm_i32_ty]>;

def int_cheerp_throw : Intrinsic<[],
                                [llvm_anyptr_ty],
                               [Throws, IntrNoReturn]>;

def int_cheerp_memory_init : Intrinsic<[llvm_i32_ty],
                            [llvm_i32_ty, llvm_i32_ty, llvm_i32_ty, llvm_i32_ty]>;

def int_cheerp_data_drop : Intrinsic<[llvm_i32_ty],
                            [llvm_i32_ty]>;

def int_cheerp_get_threading_object : Intrinsic<[llvm_anyptr_ty]>;

def int_cheerp_get_threading_blob : Intrinsic<[llvm_anyptr_ty]>;

def int_cheerp_get_thread_pointer : Intrinsic<[llvm_i32_ty]>;

def int_cheerp_set_thread_pointer : Intrinsic<[],
                                      [llvm_i32_ty]>;

def int_cheerp_atomic_wait : Intrinsic<[llvm_i32_ty],
                              [llvm_ptr_ty, llvm_i32_ty, llvm_i64_ty]>,
                              ClangBuiltin<"__builtin_cheerp_atomic_wait">;

def int_cheerp_atomic_notify : Intrinsic<[llvm_i32_ty],
                                [llvm_ptr_ty, llvm_i32_ty]>,
                                ClangBuiltin<"__builtin_cheerp_atomic_notify">;

def int_cheerp_environ : Intrinsic<[llvm_anyptr_ty]>;

def int_cheerp_argv : Intrinsic<[llvm_anyptr_ty]>;

// SIMD left shift
def int_cheerp_wasm_shl : Intrinsic<[llvm_anyvector_ty],
                            [llvm_anyvector_ty, llvm_anyint_ty],
                            [IntrNoMem, IntrSpeculatable]>;
// SIMD right shift signed
def int_cheerp_wasm_shr_s : Intrinsic<[llvm_anyvector_ty],
                            [llvm_anyvector_ty, llvm_anyint_ty],
                            [IntrNoMem, IntrSpeculatable]>;
// SIMD right shift unsigned
def int_cheerp_wasm_shr_u : Intrinsic<[llvm_anyvector_ty],
                            [llvm_anyvector_ty, llvm_anyint_ty],
                            [IntrNoMem, IntrSpeculatable]>;
// SIMD splat
def int_cheerp_wasm_splat : Intrinsic<[llvm_anyvector_ty],
                            [llvm_any_ty],
                            [IntrNoMem, IntrSpeculatable]>;

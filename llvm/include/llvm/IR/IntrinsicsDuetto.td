//===- IntrinsicsARM.td - Defines ARM intrinsics -----------*- tablegen -*-===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2013 Leaning Technologies
//
//
//===----------------------------------------------------------------------===//
//
// This file defines all of the ARM-specific intrinsics.
//
//===----------------------------------------------------------------------===//


//===-------------------------- Duetto Intrinsics --------------------------===//
//
def int_duetto_downcast  : Intrinsic<[llvm_ptr_ty],
                             [llvm_ptr_ty, llvm_i32_ty],
                             [NoCapture<0>]>;

def int_duetto_upcast_collapsed  : Intrinsic<[llvm_ptr_ty],
                             [llvm_ptr_ty],
                             [IntrNoMem, IntrIsCast]>;

def int_duetto_cast_user  : Intrinsic<[llvm_anyptr_ty],
                             [llvm_anyptr_ty],
                             [IntrNoMem, IntrIsCast]>;

// Ok, it's not really a cast but we use this to signal intrinsics that may have non parsable names
def int_duetto_allocate  : Intrinsic<[llvm_anyptr_ty],
                             [llvm_i32_ty],
                             [IntrIsCast]>;

// Access to pointer components
def int_duetto_pointer_base : Intrinsic<[llvm_ptr_ty],
                                [llvm_ptr_ty],
                                [IntrNoMem]>;
def int_duetto_pointer_offset : Intrinsic<[llvm_i32_ty],
                                [llvm_ptr_ty],
                                [IntrNoMem]>;

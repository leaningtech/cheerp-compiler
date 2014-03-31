//===-- Duetto/Utility.h - Duetto common routines -------------------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _DUETTO_UTILITY_H
#define _DUETTO_UTILITY_H

#include <set>
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"

namespace duetto
{

bool isValidTypeCast(const llvm::Value* cast, const llvm::Value* castOp, llvm::Type* src, llvm::Type* dst);
bool isClientType(const llvm::Type* t);
bool isClientArrayType(const llvm::Type* t);
bool isClientGlobal(const char* mangledName);
bool isI32Type(llvm::Type* t);
bool isTypedArrayType(llvm::Type* t);
bool isComingFromAllocation(const llvm::Value* val);
bool isNopCast(const llvm::Value* val);
bool isValidVoidPtrSource(const llvm::Value* val);
bool isValidVoidPtrSource(const llvm::Value* val, std::set<const llvm::PHINode*>& visitedPhis);
bool isInlineable(const llvm::Instruction& I);
bool isBitCast(const llvm::Value* v);
bool isGEP(const llvm::Value* v);
bool isImmutableType(const llvm::Type* t);
bool isUnion(const llvm::Type* t);
bool safeUsagesForNewedMemory(const llvm::Value* v);
bool safeCallForNewedMemory(const llvm::CallInst* ci);

}

#endif //_DUETTO_UTILITY_H


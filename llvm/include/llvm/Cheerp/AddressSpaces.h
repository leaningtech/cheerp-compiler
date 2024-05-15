//===-- Cheerp/AddressSpaces.h - Cheerp-specific address spaces -----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2011-2025 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_ADDRESS_SPACES_H
#define _CHEERP_ADDRESS_SPACES_H

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Value.h"
#include <cassert>

namespace cheerp {

enum class CheerpAS {
	Default = 0,
	Client = 1,
	GenericJS = 2,
	ByteLayout = 3,
	Wasm = 4,
};

inline CheerpAS getCheerpAS(const llvm::PointerType* t) {
	unsigned AS = t->getAddressSpace();
	assert(AS <= unsigned(CheerpAS::Wasm));
	return static_cast<CheerpAS>(AS);
}
inline CheerpAS getCheerpAS(const llvm::Value* v) {
	return getCheerpAS(llvm::cast<llvm::PointerType>(v->getType()));
}


} // namespace cheerp

#endif // _CHEERP_ADDRESS_SPACES_H

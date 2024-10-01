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

namespace cheerp {

enum class CheerpAS {
	Default = 0,
	Client = 1,
	GenericJS = 2,
	ByteLayout = 3,
	Wasm = 4,
};

inline CheerpAS getCheerpFunctionAS(CheerpAS AS) {
	switch (AS) {
	case CheerpAS::Default:
		return AS;
	case CheerpAS::GenericJS:
		return CheerpAS::Client;
	case CheerpAS::Wasm:
		return AS;
	default:
		assert(false);
	}
}
inline unsigned getCheerpFunctionAS(unsigned AS) {
	return static_cast<unsigned>(getCheerpFunctionAS(static_cast<CheerpAS>(AS)));
}

} // namespace cheerp

#endif // _CHEERP_ADDRESS_SPACES_H

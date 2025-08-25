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
	ByteLayout = 2,
	Wasm = 4,
};

inline CheerpAS getCheerpFunctionAS(CheerpAS DataAS) {
	switch (DataAS) {
	case CheerpAS::GenericJS:
		return CheerpAS::Client;
	default:
		return DataAS;
	}
}
inline unsigned getCheerpFunctionAS(unsigned DataAS) {
	return static_cast<unsigned>(getCheerpFunctionAS(static_cast<CheerpAS>(DataAS)));
}

inline CheerpAS getCheerpDataAS(CheerpAS FnAS) {
	switch (FnAS) {
	case CheerpAS::Client:
		return CheerpAS::GenericJS;
	default:
		return FnAS;
	}
}
inline unsigned getCheerpDataAS(unsigned FnAS) {
	return static_cast<unsigned>(getCheerpDataAS(static_cast<CheerpAS>(FnAS)));
}

} // namespace cheerp

#endif // _CHEERP_ADDRESS_SPACES_H

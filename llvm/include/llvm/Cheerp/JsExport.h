//===-- Cheerp/JsExport.h - Cheerp common routines -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_JSEXPORT_H
#define _CHEERP_JSEXPORT_H

namespace cheerp
{

enum class MemberKind : uint32_t
{
	Constructor, Destructor, Method, Field
};

inline uint32_t getRepresentation(const MemberKind& kind, const bool isStatic, const bool isConst)
{
	return ((uint8_t)kind << 2) + (isStatic << 1) + isConst;
}
inline MemberKind getMemberKind(const uint32_t value)
{
	return MemberKind(value >> 2);
}
inline bool isConstructor(const uint32_t value)
{
	return getMemberKind(value) == MemberKind::Constructor;
}
inline bool isStatic(const uint32_t value)
{
	return value & 2;
}
inline bool isConst(const uint32_t value)
{
	return value & 1;
}

}//namespace cheerp

#endif //_CHEERP_JSEXPORT_H

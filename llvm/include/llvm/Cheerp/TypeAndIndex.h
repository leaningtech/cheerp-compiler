//===-- Cheerp/TypeAndIndex.h - Helper class for canonicalization ---------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_TYPE_AND_INDEX_H
#define _CHEERP_TYPE_AND_INDEX_H

struct TypeAndIndex
{
	llvm::Type* type;
	uint32_t index;
	enum CANONICALIZE_TYPE { STRUCT_MEMBER = 0, ARGUMENT };
	TypeAndIndex(llvm::Type* t, uint32_t i, CANONICALIZE_TYPE c):type(t),index(i)
	{
		if(!t)
			return;
		assert(c == STRUCT_MEMBER || c == ARGUMENT);
		if (c == STRUCT_MEMBER)
		{
			// Find if a direct base is the actual owner of the field
			if(llvm::StructType* st=llvm::dyn_cast<llvm::StructType>(t))
			{
				while(st->getDirectBase() && st->getDirectBase()->getNumElements() > i)
					st = st->getDirectBase();
				type = st;
			}
		}
		else if(c == ARGUMENT)
		{
			// Collapse every argument to the least derived one
			if(llvm::StructType* st=llvm::dyn_cast<llvm::StructType>(t))
			{
				while(st->getDirectBase())
					st = st->getDirectBase();
				type = st;
			}
		}
	}
	struct Hash
	{
		uint32_t operator()(const TypeAndIndex& x) const
		{
			return std::hash<llvm::Type*>()(x.type) ^ std::hash<uint32_t>()(x.index);
		}
	};
	bool operator<(const TypeAndIndex& rhs) const
	{
		if(type==rhs.type)
			return index < rhs.index;
		else
			return type < rhs.type;
	}
	operator bool() const
	{
		return type != NULL;
	}
};

#endif

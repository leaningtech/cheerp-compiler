//===-- Cheerp/JsExport.h - Cheerp JsExport utilities routines -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019-2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_JSEXPORT_H
#define _CHEERP_JSEXPORT_H

#include "llvm/Cheerp/Writer.h"


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

template <typename FuncOnFunctions, typename FuncOnRecord>
void iterateOverJsExportedMetadata(const llvm::Module& M, FuncOnFunctions funcOnFunctions, FuncOnRecord funcOnRecord)
{
	for( const llvm::NamedMDNode& namedNode: M.named_metadata())
	{
		llvm::StringRef name = namedNode.getName();

		if(name == "jsexported_free_functions")
		{
			for ( llvm::NamedMDNode::const_op_iterator it = namedNode.op_begin(); it != namedNode.op_end(); ++ it )
			{
				const llvm::MDNode * node = *it;
				const llvm::Function * f = llvm::cast<llvm::Function>(llvm::cast<llvm::ConstantAsMetadata>(node->getOperand(0))->getValue());
				funcOnFunctions(f);
			}
			continue;
		}
		if (!name.endswith("_methods") || !(name.startswith("class.") || name.startswith("struct.")) )
			continue;

		funcOnRecord(namedNode, name);
	}
}

}//namespace cheerp

#endif //_CHEERP_JSEXPORT_H

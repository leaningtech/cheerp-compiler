//===-- Cheerp/BaseWriter.h - The Cheerp base generator -----------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2022-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_BASE_WRITER_H
#define _CHEERP_BASE_WRITER_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include <unordered_map>

namespace cheerp
{

class CheerpBaseWriter
{
public:
	bool needsUnsignedTruncation(const llvm::Value* v, bool asmjs) const;

	virtual bool isGlobalized(const llvm::Value* v) const =0;
	virtual ~CheerpBaseWriter() = 0;

	// Utility function to iterate all the elements of a value. Elements include the
	// struct elements if the type is a StructType, and the two parts of a SPLIT_REGULAR
	// pointer if the type (or an element type inside a StructType) is a SPLIT_REGULAR.
	// The signature of cb is:
	// void(const Value* v, Type* Ty, StructType* STy, POINTER_KIND elemPtrKind, bool isOffset, Registerize::REGISTER_KIND elemRegKind, uint32_t elemIdx, uint32_t structElemIdx, bool asmjs)
	template<typename F>
	void forEachElem(const llvm::Value* v, llvm::Type* Ty, const PointerAnalyzer& PA, const Registerize& registerize, bool asmjs, F cb)
	{
		//TODO support struct types
		assert(!Ty->isStructTy());

		POINTER_KIND valKind = Ty->isPointerTy()? PA.getPointerKind(v) : COMPLETE_OBJECT;
		Registerize::REGISTER_KIND regKind = registerize.getRegKindFromType(Ty, asmjs);
		cb(v, Ty, nullptr, valKind, /*isOffset*/false, regKind, /*elemIdx*/0, /*structElemIdx*/0, asmjs);
		if(valKind == SPLIT_REGULAR && !PA.getConstantOffsetForPointer(v))
		{
			cb(v, Ty, nullptr, valKind, true, Registerize::INTEGER, 1, 0, asmjs);
		}
		return;
	}

private:
	bool needsUnsignedTruncation(std::unordered_map<const llvm::Value*, bool>& visited, const llvm::Value* v, bool asmjs) const;
	bool needsUnsignedTruncationImpl(std::unordered_map<const llvm::Value*, bool>& visited, const llvm::Value* v, bool asmjs) const;

};


} // cheerp

#endif

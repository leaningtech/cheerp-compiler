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
private:
	bool needsUnsignedTruncation(std::unordered_map<const llvm::Value*, bool>& visited, const llvm::Value* v, bool asmjs) const;
	bool needsUnsignedTruncationImpl(std::unordered_map<const llvm::Value*, bool>& visited, const llvm::Value* v, bool asmjs) const;

};


} // cheerp

#endif

//===-- Cheerp/LinearMemoryHelper.h - The Cheerp JavaScript generator -----===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_LINEAR_MEMORY_HELPER_H
#define _CHEERP_LINEAR_MEMORY_HELPER_H

#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include <map>

namespace cheerp
{

class LinearMemoryHelper
{
private:
	const llvm::DataLayout& targetData;
	const GlobalDepsAnalyzer& globalDeps;
	const int32_t functionAddrStart;
	// map global variables to their addresses
	std::map<const llvm::GlobalVariable*,uint32_t> gVarsAddr;
	// The next address available to allocate global variables.
	// The heap space will start after the last global variable allocation
	uint32_t heapStart{8};
public:
	LinearMemoryHelper(const llvm::DataLayout& targetData, const GlobalDepsAnalyzer& globalDeps, int32_t functionAddrStart):
				targetData(targetData),globalDeps(globalDeps),functionAddrStart(functionAddrStart)
	{
	}
	// Returns the newly assigned address
	uint32_t addGlobalVariable(const llvm::GlobalVariable* G);
	uint32_t getGlobalVariableAddress(const llvm::GlobalVariable* G) const;
	struct ByteListener
	{
		virtual void addByte(uint8_t b) = 0;
		virtual ~ByteListener()
		{
		}
	};
	void compileConstantAsBytes(const llvm::Constant* c, bool asmjs, ByteListener* listener);
	struct GepListener
	{
		virtual void addValue(const llvm::Value* v, uint32_t size) = 0;
		virtual void addConst(uint32_t v) = 0;
		virtual ~GepListener()
		{
		}
	};
	// Returns the base of the compiled expression
	const llvm::Value* compileGEP(const llvm::Value* p, GepListener* listener);
	uint8_t getTotalMemory() const
	{
		return heapStart;
	}
};

}

#endif

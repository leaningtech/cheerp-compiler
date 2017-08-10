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

#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include <map>
#include <unordered_map>

namespace cheerp
{
class LinearMemoryHelper
{
public:
	/**
	 * NOTE: function addresses are different in asmjs and wasm: in asmjs
	 * they are offsets in the corresponding table, while in wasm they are
	 * offsets in a big table that is built concatenating all the tables
	 * together. In order to have a linear address space for functions also
	 * in asmjs, we will reserve the tpo 16 bits of the address to indicate
	 * the table number. This effectively limits the maximum number of tables
	 * and functions per table to 2^16 in asmjs.
	 */
	enum FunctionAddressMode {
		AsmJS = 0,
		Wasm
	};
	/**
	 * Used to compile asm.js indirect function calls
	 */
	struct FunctionTableInfo {
		std::string name;
		std::vector<const llvm::Function*> functions;
	};
	/**
	 * Custom hash and compare functions for the FunctionTableInfoMap
	 * Two types are considered equal if they are both void, floating point,
	 * or (integer or pointer). Two function types are considered equal if
	 * they return equivalent types and they have parameters with equivalent
	 * types in the same order
	 */
	struct FunctionSignatureHash
	{
		std::size_t operator()(const llvm::FunctionType* const& fTy) const 
		{
			const llvm::Type* retTy = fTy->getReturnType();
			size_t hash = 31;
			if (retTy->isVoidTy())
				hash = hash*31 + std::hash<size_t>()(0);
			else if (retTy->isPointerTy() || retTy->isIntegerTy())
				hash = hash*31 + std::hash<size_t>()(1);
			else if (retTy->isFloatingPointTy())
				hash = hash*31 + std::hash<size_t>()(2);
			for (const auto& pTy: fTy->params())
			{
				if (pTy->isPointerTy() || pTy->isIntegerTy())
					hash = hash*31 + std::hash<size_t>()(1);
				else if (pTy->isFloatingPointTy())
					hash = hash*31 + std::hash<size_t>()(2);
			}
			return hash;
		}
	};
	struct FunctionSignatureCmp
	{
		bool operator()(const llvm::FunctionType* const& lhs, const llvm::FunctionType* const& rhs) const
		{
			size_t r1 = 0, r2 = 0;
			const llvm::Type* retTy = lhs->getReturnType();
			if (retTy->isPointerTy() || retTy->isIntegerTy())
				r1 = 1;
			else if (retTy->isFloatingPointTy())
				r1 = 2;
			retTy = rhs->getReturnType();
			if (retTy->isPointerTy() || retTy->isIntegerTy())
				r2 = 1;
			else if (retTy->isFloatingPointTy())
				r2 = 2;
			if (r1 != r2) return false;
			if (lhs->getNumParams() != rhs->getNumParams()) return false;
			auto lit = lhs->param_begin();
			auto rit = rhs->param_begin();
			for (;lit != lhs->param_end(); lit++,rit++)
			{
				if ((*lit)->isPointerTy() || (*lit)->isIntegerTy())
					r1 = 1;
				else if ((*lit)->isFloatingPointTy())
					r1 = 2;
				if ((*rit)->isPointerTy() || (*rit)->isIntegerTy())
					r2 = 1;
				else if ((*rit)->isFloatingPointTy())
					r2 = 2;
				if (r1 != r2) return false;
			}
			return true;
		}
	};
	/**
	 * Used to store the information needed to compile and use the asm.js
	 * function tables for indirect calls
	 */
	typedef std::unordered_map<const llvm::FunctionType*,FunctionTableInfo,
		  FunctionSignatureHash,FunctionSignatureCmp> FunctionTableInfoMap;
	/**
	 * Used to assign asm.js function pointers
	 */
	typedef std::unordered_map<const llvm::Function*, int32_t> FunctionAddressesMap;
	/**
	 * Used to assign asm.js globals
	 */
	typedef std::unordered_map<const llvm::GlobalVariable*, int32_t> GlobalAddressesMap;

	LinearMemoryHelper(llvm::Module& module, FunctionAddressMode mode):
		module(module), mode(mode)
	{
		addFunctions();
		addGlobals();
		addHeapStart();
	}

	uint32_t getGlobalVariableAddress(const llvm::GlobalVariable* G) const;
	uint32_t getFunctionAddress(const llvm::Function* F) const;
	bool functionHasAddress(const llvm::Function* F) const;
	uint32_t getFunctionAddressMask(const llvm::FunctionType* Fty) const;
	const FunctionTableInfoMap& getFunctionTables() const
	{
		return functionTables;
	}

	struct ByteListener
	{
		virtual void addByte(uint8_t b) = 0;
		virtual ~ByteListener()
		{
		}
	};
	void compileConstantAsBytes(const llvm::Constant* c, bool asmjs, ByteListener* listener, int32_t offset=0) const;
	struct GepListener
	{
		virtual void addValue(const llvm::Value* v, uint32_t size) = 0;
		virtual void addConst(uint32_t v) = 0;
		virtual ~GepListener()
		{
		}
	};
	// Returns the base of the compiled expression
	const llvm::Value* compileGEP(const llvm::Value* p, GepListener* listener) const;
private:
	void addGlobals();
	void addFunctions();
	void addHeapStart();

	llvm::Module& module;
	FunctionAddressMode mode;

	FunctionTableInfoMap functionTables;
	FunctionAddressesMap functionAddresses;
	GlobalAddressesMap globalAddresses;
	// The next address available to allocate global variables.
	// The heap space will start after the last global variable allocation
	uint32_t heapStart{8};
};

}

#endif

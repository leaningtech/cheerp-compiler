//===-- Cheerp/LinearMemoryHelper.h - The Cheerp JavaScript generator -----===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_LINEAR_MEMORY_HELPER_H
#define _CHEERP_LINEAR_MEMORY_HELPER_H

#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Pass.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/BuiltinInstructions.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include <map>
#include <unordered_map>

namespace cheerp
{
class LinearMemoryHelper: public llvm::ModulePass
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
		llvm::SmallString<4> name;
		std::vector<const llvm::Function*> functions;
		size_t offset;
		size_t typeIndex;
	};
	/**
	 * Custom hash and compare functions for the FunctionTableInfoMap
	 * Two types are considered equal if they are both void, floating point,
	 * or (integer or pointer). Two function types are considered equal if
	 * they return equivalent types and they have parameters with equivalent
	 * types in the same order
	 */
	template<bool isStrict = false>
	struct FunctionSignatureHash
	{
		std::size_t operator()(const llvm::FunctionType* const& fTy) const 
		{
			const llvm::Type* retTy = fTy->getReturnType();
			size_t hash = 31;
			hash = hash*31 + std::hash<size_t>()(typeKindOf<isStrict>(retTy));
			for (const auto& pTy: fTy->params())
			{
				hash = hash*31 + std::hash<size_t>()(typeKindOf<isStrict>(pTy));
			}
			return hash;
		}
	};
	template<bool isStrict = false>
	struct FunctionSignatureCmp
	{
	public:
		bool operator()(const llvm::FunctionType* const& lhs, const llvm::FunctionType* const& rhs) const
		{
			if (isStrict && lhs->isVarArg() != rhs->isVarArg())
				return false;

			size_t r1 = typeKindOf<isStrict>(lhs->getReturnType());
			size_t r2 = typeKindOf<isStrict>(rhs->getReturnType());
			if (r1 != r2)
				return false;
			if (lhs->getNumParams() != rhs->getNumParams())
				return false;
			auto lit = lhs->param_begin();
			auto rit = rhs->param_begin();
			for (;lit != lhs->param_end(); lit++,rit++)
			{
				r1 = typeKindOf<isStrict>(*lit);
				r2 = typeKindOf<isStrict>(*rit);
				if (r1 != r2)
					return false;
			}
			return true;
		}
	};

	static std::string getFunctionTableName(const llvm::FunctionType* ft)
	{
		auto getTypeKindChar = [](const llvm::Type* ty)
		{
			switch (typeKindOf(ty))
			{
				case TypeKind::Void:
					return 'v';
				case TypeKind::Integer:
				case TypeKind::RawPointer:
					return 'i';
				case TypeKind::Integer64:
					return 'j';
				case TypeKind::RefPointer:
					return 'r';
				case TypeKind::Double:
					return 'd';
				case TypeKind::Float:
					return 'f';
			}
		};
		std::string table_name;
		llvm::Type* ret = ft->getReturnType();
		table_name += getTypeKindChar(ret);
		for (const auto& param : ft->params())
		{
			table_name += getTypeKindChar(param);
		}
		return table_name;
	}

	/**
	 * Used to store the information needed to compile and use the asm.js
	 * function tables for indirect calls
	 */
	typedef std::unordered_map<const llvm::FunctionType*,FunctionTableInfo,
		  FunctionSignatureHash<>,FunctionSignatureCmp<>> FunctionTableInfoMap;
	typedef std::vector<const llvm::FunctionType*> FunctionTableOrder;
	/**
	 * Used to assign asm.js function pointers
	 */
	typedef std::unordered_map<const llvm::Function*, int32_t> FunctionAddressesMap;
	/**
	 * Used to assign asm.js globals
	 */
	typedef std::unordered_map<const llvm::GlobalVariable*, int32_t> GlobalAddressesMap;

	typedef std::unordered_map<const llvm::FunctionType*, size_t,
		FunctionSignatureHash<>,FunctionSignatureCmp<>> FunctionTypeIndicesMap;

	static char ID;

	LinearMemoryHelper(FunctionAddressMode mode, uint32_t memorySize,
		uint32_t stackSize, bool wasmOnly, bool growMem):
		llvm::ModulePass(ID), module(nullptr), globalDeps(nullptr),
		mode(mode), maxFunctionId(0), memorySize(memorySize*1024*1024),
		stackSize(stackSize*1024*1024), wasmOnly(wasmOnly), growMem(growMem)
	{
	}
	virtual bool runOnModule(llvm::Module& module) override
	{
		this->module = &module;
		globalDeps = &getAnalysis<GlobalDepsAnalyzer>();
		builtinIds.fill(std::numeric_limits<uint32_t>::max());
		addFunctions();
		addStack();
		addGlobals();
		checkMemorySize();
		addHeapStartAndEnd();

		return false;
	}

	void getAnalysisUsage(llvm::AnalysisUsage & AU) const override;

	uint32_t getGlobalVariableAddress(const llvm::GlobalVariable* G) const;
	uint32_t getFunctionAddress(const llvm::Function* F) const;
	bool functionHasAddress(const llvm::Function* F) const;
	uint32_t getFunctionAddressMask(const llvm::FunctionType* Fty) const;
	FunctionTableInfoMap& getFunctionTables()
	{
		return functionTables;
	}
	const FunctionTableInfoMap& getFunctionTables() const
	{
		return functionTables;
	}
	const FunctionTableOrder& getFunctionTableOrder() const
	{
		return functionTableOrder;
	}

	/**
	 * Get a list of the asm.js functions. This list excludes functions without
	 * an "asmjs" section. This list does also exclude native WebAssembly
	 * intrinsics (like sqrt, abs, copysign, etc.), when cheerp-mode is set to
	 * WebAssembly.
	 */
	const std::vector<const llvm::Function*> & functions() const {
		return asmjsFunctions_;
	}

	const FunctionTypeIndicesMap& getFunctionTypeIndices() const {
		return functionTypeIndices;
	}

	const std::unordered_map<const llvm::Function*, uint32_t>& getFunctionIds() const {
		return functionIds;
	}

	uint32_t getStackStart() const {
		return stackStart;
	}
	uint32_t getHeapStart() const {
		return heapStart;
	}

	/**
	 * Vector of distinct function types that corresponds to the function list,
	 * and are ordered by the appearence in that list.
	 */
	const std::vector<const llvm::FunctionType*> getFunctionTypes() const {
		return functionTypes;
	}

	/**
	 * Get a list of the asm.js global variables. This list excludes global
	 * variables without an "asmjs" section.
	 */
	const std::vector<const llvm::GlobalVariable*> & globals() const {
		return asmjsGlobals;
	}

	const std::vector<const llvm::GlobalVariable*> & addressableGlobals() const {
		return asmjsAddressableGlobals;
	}

	typedef llvm::DenseMap<const llvm::GlobalVariable*, uint32_t> GlobalUsageMap;
	const GlobalUsageMap& getGlobalizedGlobalUsage() const
	{
		return globalizedGlobalsUsage;
	}

	struct ByteListener
	{
		virtual void addByte(uint8_t b) = 0;
		virtual ~ByteListener()
		{
		}
	};
	void compileConstantAsBytes(const llvm::Constant* c, bool asmjs, ByteListener* listener, int32_t offset=0) const;
	bool isZeroInitializer(const llvm::Constant* c) const;
	bool hasNonZeroInitialiser(const llvm::GlobalVariable* G) const;
	struct GepListener
	{
		virtual void addValue(const llvm::Value* v, uint32_t size) = 0;
		virtual void subValue(const llvm::Value* v, uint32_t size)
		{
			assert(false);
		}
		virtual void addConst(int64_t v) = 0;
		virtual bool isInlineable(const llvm::Value* p) = 0;
		virtual bool hasSubValue() const
		{
			return false;
		}
		virtual ~GepListener()
		{
		}
	};
	class LinearGepListener: public GepListener
	{
	private:
		const PointerAnalyzer& PA;
	public:
		LinearGepListener(const PointerAnalyzer& PA):PA(PA)
		{
		}
		bool isInlineable(const llvm::Value* p) override;
	};
	static int64_t compileGEPOperand(const llvm::Value* idxVal, uint32_t size, GepListener* listener, bool invert);
	// Returns the base of the compiled expression
	const llvm::Value* compileGEP(const llvm::Value* p, GepListener* listener, const PointerAnalyzer* PA) const;
	static const llvm::Value* compileGEP(const llvm::Module* module, const llvm::Value* p, GepListener* listener, const PointerAnalyzer* PA);

	uint32_t getBuiltinId(BuiltinInstr::BUILTIN b) const
	{
		assert(builtinIds[b] != std::numeric_limits<uint32_t>::max());
		return builtinIds[b];
	}

	bool canGrowMemory() const {
		return growMem;
	}
private:
	// Different kind of types for the purpose of comparing function signatures
	enum TypeKind {
		Void = 0,
		Integer,
		Integer64,
		Double,
		Float,
		RawPointer,
		RefPointer,
	};
	template<bool isStrict = false>
	static TypeKind typeKindOf(const llvm::Type* type)
	{
		if (type->isIntegerTy(64))
			return TypeKind::Integer64;
		if (type->isIntegerTy())
			return TypeKind::Integer;
		if (type->isDoubleTy())
			return TypeKind::Double;
		if (type->isFloatTy())
			return TypeKind::Float;
		if (type->isPointerTy() && TypeSupport::isRawPointer(type, true))
			return (isStrict ? TypeKind::RawPointer : TypeKind::Integer);
		if (type->isPointerTy())
			return TypeKind::RefPointer;
		if (type->isVoidTy())
			return TypeKind::Void;
		llvm_unreachable("unrecognized type kind");
	}

	void addGlobals();
	void addFunctions();
	void addStack();
	void addHeapStartAndEnd();
	void checkMemorySize();

	llvm::Module* module;
	GlobalDepsAnalyzer* globalDeps;

	FunctionAddressMode mode;

	FunctionTableInfoMap functionTables;
	FunctionTableOrder functionTableOrder;
	std::vector<const llvm::Function*> asmjsFunctions_;

	std::unordered_map<const llvm::Function*, uint32_t> functionIds;
	std::array<uint32_t, BuiltinInstr::numGenericBuiltins()> builtinIds;
	uint32_t maxFunctionId;
	std::vector<const llvm::FunctionType*> functionTypes;
	FunctionTypeIndicesMap functionTypeIndices;

	std::vector<const llvm::GlobalVariable*> asmjsGlobals;
	std::vector<const llvm::GlobalVariable*> asmjsAddressableGlobals;
	GlobalUsageMap globalizedGlobalsUsage;
	void generateGlobalizedGlobalsUsage();

	FunctionAddressesMap functionAddresses;
	GlobalAddressesMap globalAddresses;
	// The next address available to allocate global variables.
	// The heap space will start after the last global variable allocation
	uint32_t heapStart{8};
	// Total memory size
	uint32_t memorySize;
	// Stack size
	uint32_t stackSize;
	// Stack start (it grows downwards)
	uint32_t stackStart;
	// We are producing a standalone wasm module
	bool wasmOnly;
	// Whether memory can grow at runtime or not
	bool growMem;
};

llvm::ModulePass *createLinearMemoryHelperPass(LinearMemoryHelper::FunctionAddressMode mode,
		uint32_t memorySize,uint32_t stackSize, bool wasmOnly, bool growMem);

}

#endif

//===-- Cheerp/TypeOptimizer.h - Cheerp utility code --------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2015-2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef CHEERP_TYPE_OPTIMIZER_H
#define CHEERP_TYPE_OPTIMIZER_H

#include "llvm/Cheerp/TypeAndIndex.h"
#include "llvm/Cheerp/DeterministicUnorderedMap.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include <unordered_map>
#include <unordered_set>
#include <set>

namespace cheerp
{

class TypeOptimizer: public llvm::ModulePass
{
private:
	struct TypeMappingInfo
	{
		enum MAPPING_KIND { IDENTICAL, COLLAPSED, COLLAPSING, COLLAPSING_BUT_USED, BYTE_LAYOUT_TO_ARRAY, POINTER_FROM_ARRAY, FLATTENED_ARRAY,
						MERGED_MEMBER_ARRAYS, MERGED_MEMBER_ARRAYS_AND_COLLAPSED };
		llvm::Type* mappedType;
		MAPPING_KIND elementMappingKind;
		TypeMappingInfo():mappedType(NULL),elementMappingKind(IDENTICAL)
		{
		}
		TypeMappingInfo(llvm::Type* t, MAPPING_KIND k):mappedType(t),elementMappingKind(k)
		{
		}
		operator llvm::Type*() const
		{
			return mappedType;
		}
		static bool isCollapsedStruct(MAPPING_KIND k)
		{
			return k == COLLAPSED || k == MERGED_MEMBER_ARRAYS_AND_COLLAPSED;
		}
	};
	class LocalTypeMapping {
	public:
		llvm::Type* getOriginalOperandType(llvm::Value* v)
		{
			auto it = localTypeMapping.find(v);
			if(it != localTypeMapping.end())
				return it->second;
			else if(llvm::GlobalValue* GV=llvm::dyn_cast<llvm::GlobalValue>(v))
			{
				auto it=globalTypeMapping.find(GV);
				if(it==globalTypeMapping.end())
					return GV->getType();
				else
					return it->second;
			}
			else
				return v->getType();
		}
		void setOriginalOperandType(llvm::Value* v, llvm::Type* t)
		{
			localTypeMapping[v] = t;
		}

		LocalTypeMapping(std::unordered_map<llvm::GlobalValue*, llvm::Type*>& globalTypeMapping)
			: globalTypeMapping(globalTypeMapping)
		{
		}
	private:
		std::unordered_map<llvm::Value*, llvm::Type*> localTypeMapping;
		std::unordered_map<llvm::GlobalValue*, llvm::Type*>& globalTypeMapping;
	};


	class LocalInstMapping
	{
	public:
		LocalInstMapping(TypeOptimizer& TO): TO(TO)
		{}
		std::pair<llvm::Value*, uint8_t> getMappedOperand(llvm::Value* v) 
		{
			assert(v);
			if(llvm::Constant* C=llvm::dyn_cast<llvm::Constant>(v))
				return TO.rewriteConstant(C, false);
			auto it = localInstMapping.find(v);
			if(it != localInstMapping.end())
				return it->second;
			if(auto A = llvm::dyn_cast<llvm::Argument>(v))
			{
				auto it = TO.globalsMapping.find(A->getParent());
				assert(it == TO.globalsMapping.end() || it->second == it->first);
			}
			return std::make_pair(v, 0);
		};
		void setMappedOperand(llvm::Value* v, llvm::Value* m, uint8_t o)
		{
			localInstMapping[v] = std::make_pair(m, o);
		};
		DeterministicUnorderedMap<llvm::Value*, std::pair<llvm::Value*, uint8_t>, RestrictionsLifted::NoErasure>::iterator begin()
		{
			return localInstMapping.begin();
		}
		DeterministicUnorderedMap<llvm::Value*, std::pair<llvm::Value*, uint8_t>, RestrictionsLifted::NoErasure>::iterator end()
		{
			return localInstMapping.end();
		}

	private:
		DeterministicUnorderedMap<llvm::Value*, std::pair<llvm::Value*, uint8_t>, RestrictionsLifted::NoErasure> localInstMapping;
		TypeOptimizer& TO;
	};

	llvm::Module* module;
	const llvm::DataLayout* DL;
	std::unordered_map<const llvm::StructType*,std::set<llvm::StructType*>> downcastSourceToDestinationsMapping;
	std::unordered_map<const llvm::StructType*, std::vector<std::pair<uint32_t, uint32_t>>> membersMappingData;
	std::unordered_map<llvm::GlobalValue*, llvm::Constant*> globalsMapping;
	std::unordered_map<llvm::GlobalValue*, llvm::Type*> globalTypeMapping;
	std::unordered_map<const llvm::StructType*, llvm::Type*> baseTypesForByteLayout;
	std::unordered_map<const llvm::Type*, TypeMappingInfo> typesMapping;
	std::unordered_set<llvm::Function*> pendingFunctions;
	std::vector<llvm::Function*> emptiedFunctions;
	// In this context a field "escapes" if it has any use which is not just a load/store
	std::set<TypeAndIndex> escapingFields;
	// Wasm functions used only by wasm. We can keep i64 in the ABI for these
	std::set<const llvm::Function*> onlyCalledByWasmFuncs;
#ifndef NDEBUG
	std::unordered_set<llvm::Type*> newStructTypes;
#endif
	llvm::Constant* rewriteGlobal(llvm::GlobalVariable* GV);
	void rewriteGlobalInit(llvm::GlobalVariable* GV);
	TypeMappingInfo rewriteType(llvm::Type* t);
	llvm::FunctionType* rewriteFunctionType(llvm::FunctionType* t, bool keepI64);
	llvm::Function* rewriteFunctionSignature(llvm::Function* F);
	void rewriteUses(llvm::Value* V, llvm::Value* NewV);
	std::pair<llvm::Constant*, uint8_t> rewriteConstant(llvm::Constant* C, bool rewriteI64);
	void rewriteFunction(llvm::Function* F);
	llvm::Function* rewriteIntrinsic(llvm::Function* F, llvm::FunctionType* FT);
	void gatherAllTypesInfo(const llvm::Module& M);
	uint8_t rewriteGEPIndexes(llvm::SmallVector<llvm::Value*, 4>& newIndexes, llvm::Type* ptrType, llvm::ArrayRef<llvm::Value*> idxs,
				llvm::Type* targetType, llvm::Instruction* insertionPoint);
	bool isUnsafeDowncastSource(llvm::StructType* st);
	void addAllBaseTypesForByteLayout(llvm::StructType* st, llvm::Type* base);
	bool canCollapseStruct(llvm::StructType* st, llvm::StructType* newStruct, llvm::Type* newType);
	bool isI64ToRewrite(llvm::Type* t);
	static void pushAllBaseConstantElements(llvm::SmallVector<llvm::Constant*, 4>& newElements, llvm::Constant* C, llvm::Type* baseType);
	// Helper function to handle the various kind of arrays in constants
	static void pushAllArrayConstantElements(llvm::SmallVector<llvm::Constant*, 4>& newElements, llvm::Constant* array);
	static llvm::StructType* isEscapingStructGEP(const llvm::User* GEP);
public:
	static char ID;
	explicit TypeOptimizer() : ModulePass(ID) { }
	bool runOnModule(llvm::Module &M) override;
	llvm::StringRef getPassName() const override;
};

//===----------------------------------------------------------------------===//
//
// TypeOptimizer - Change the layout of structs and arrays to reduce the number of allocated objects at runtime
//
inline llvm::ModulePass *createTypeOptimizerPass()
{
	return new TypeOptimizer();
}

}

#endif

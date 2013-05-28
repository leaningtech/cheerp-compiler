//===-- Duetto/Writer.h - The Duetto JavaScript generator -------------===//
//
//	Copyright 2011-2013 Leaning Technlogies
//===----------------------------------------------------------------------===//

#ifndef _DUETTO_WRITER_H
#define _DUETTO_WRITER_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Support/FormattedStream.h"
#include <set>
#include <map>

namespace duetto
{

class DuettoWriter
{
private:
	llvm::Module& module;
	llvm::DataLayout targetData;
	std::set<llvm::StructType*> classesNeeded;
	uint32_t getIntFromValue(const llvm::Value* v) const;
	bool isValidTypeCast(const llvm::Value* cast, const llvm::Value* castOp, llvm::Type* src, llvm::Type* dst) const;
	bool isClientType(llvm::Type* t) const;
	bool isClientGlobal(const char* mangledName) const;
	bool isI32Type(llvm::Type* t) const;
	bool isComingFromAllocation(const llvm::Value* val) const;
	bool isComingFromAllocation(const llvm::Value* val, std::set<const llvm::PHINode*>& visitedPhis) const;
	bool isInlineable(const llvm::Instruction& I) const;
	bool isBitCast(const llvm::Value* v) const;
	bool isGEP(const llvm::Value* v) const;
	void compileTerminatorInstruction(const llvm::TerminatorInst& I);
	void compileTerminatorInstruction(const llvm::TerminatorInst& I,
			const std::map<const llvm::BasicBlock*, uint32_t>& blocksMap);
	bool compileInlineableInstruction(const llvm::Instruction& I);
	bool compileNotInlineableInstruction(const llvm::Instruction& I);
	const llvm::Type* compileRecursiveAccessToGEP(const llvm::Type* curType, const llvm::Use* it,
			const llvm::Use* const itE);
	void compilePredicate(llvm::CmpInst::Predicate p);
	void compileOperandForIntegerPredicate(const llvm::Value* v, llvm::CmpInst::Predicate p);
	void compileType(llvm::Type* t);
	void compileTypeImpl(llvm::Type* t);
	enum POINTER_KIND { COMPLETE_OBJECT = 0, COMPLETE_ARRAY, REGULAR };
	POINTER_KIND getPointerKind(const llvm::Value* v, std::map<const llvm::PHINode*, POINTER_KIND>& visitedPhis);
	POINTER_KIND getPointerKind(const llvm::Value* v);
	bool isDowncast(const llvm::Value* val) const;
	/*
	 * \param v The pointer to dereference, it may be a regular pointer, a complete obj or a complete array
	 * \param offset An offset coming from code, which may be also NULL
	 * \param namedOffset An offset that will be added verbatim to the code
	 */
	void compileDereferencePointer(const llvm::Value* v, const llvm::Value* offset, const char* namedOffset = NULL);
	void compileFastGEPDereference(const llvm::Value* operand, const llvm::Use* idx_begin, const llvm::Use* idx_end);
	void compileGEP(const llvm::Value* val, const llvm::Use* it, const llvm::Use* const itE);
	const llvm::Type* compileObjectForPointerGEP(const llvm::Value* val, const llvm::Use* it, const llvm::Use* const itE);
	bool compileOffsetForPointerGEP(const llvm::Value* val, const llvm::Use* it, const llvm::Use* const itE,
			const llvm::Type* lastType);
	const llvm::Type* compileObjectForPointer(const llvm::Value* val);
	/*
	 * Returns true if anything is printed
	 */
	bool compileOffsetForPointer(const llvm::Value* val, const llvm::Type* lastType);
	llvm::Type* findRealType(const llvm::Value* v, std::set<const llvm::PHINode*>& visitedPhis) const;
	void compileCopy(const llvm::Value* dest, const llvm::Value* src, const llvm::Value* size);
	void compileCopyRecursive(const std::string& baseName, const llvm::Value* baseDest,
		const llvm::Value* baseSrc, const llvm::Type* currentType, const char* namedOffset);
	void compileReset(const llvm::Value* dest, uint8_t resetValue, const llvm::Value* size);
	void compileResetRecursive(const std::string& baseName, const llvm::Value* baseDest,
		uint8_t resetValue, const llvm::Type* currentType, const char* namedOffset);
	void compileDowncast(const llvm::Value* src, uint32_t baseOffset);
	void compileAllocation(const llvm::Value* callV, const llvm::Value* size);
	void compileFree(const llvm::Value* obj);
	void compilePointer(const llvm::Value* v, POINTER_KIND acceptedKind);
	void compileOperandImpl(const llvm::Value* v);
	void printLLVMName(const llvm::StringRef& s) const;
	void printVarName(const llvm::Value* v);
	void handleBuiltinNamespace(const char* ident, llvm::User::const_op_iterator it,
			llvm::User::const_op_iterator itE);
	bool handleBuiltinCall(const char* ident, const llvm::Value* callV,
			llvm::User::const_op_iterator it, llvm::User::const_op_iterator itE);
	bool safeUsagesForNewedMemory(const llvm::Value* v) const;
	bool safeCallForNewedMemory(const llvm::CallInst* ci) const;
	uint32_t getUniqueIndexForValue(const llvm::Value* v);
	std::map<const llvm::Value*, uint32_t> unnamedValueMap;
	void compileMethod(llvm::Function& F);
	void compileGlobal(llvm::GlobalVariable& G);
	uint32_t compileClassTypeRecursive(const std::string& baseName, llvm::StructType* currentType, uint32_t baseCount);
	void compileClassType(llvm::StructType* T);
	enum OperandFix{ OPERAND_NO_FIX = 0, OPERAND_EXPAND_COMPLETE_OBJECTS };
	void compileConstantExpr(const llvm::ConstantExpr* ce);
public:
	llvm::raw_ostream& stream;
	DuettoWriter(llvm::Module& m, llvm::raw_ostream& s):module(m),targetData(&m),stream(s)
	{
	}
	void makeJS();
	void compileBB(llvm::BasicBlock& BB, const std::map<const llvm::BasicBlock*, uint32_t>& blocksMap);
	void compileOperand(const llvm::Value* v, OperandFix fix = OPERAND_NO_FIX);
	void compileConstant(const llvm::Constant* c);
	void compilePHIOfBlockFromOtherBlock(const llvm::BasicBlock* to, const llvm::BasicBlock* from);
};

}
#endif

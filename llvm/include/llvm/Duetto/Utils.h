//===-- Duetto/Utils.h - Duetto utility code -----------------------------===//
//
//	Copyright 2011-2013 Leaning Technlogies
//===----------------------------------------------------------------------===//

#ifndef DUETTO_UTILS_H
#define DUETTO_UTILS_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include <string>

class DuettoUtils
{
private:
	static bool isBuiltinConstructor(const char* s, const std::string& typeName);
	static bool isBuiltinType(const char* typeName, std::string& builtinName);
	static void baseSubstitutionForBuiltin(llvm::User* i, llvm::Instruction* old, llvm::AllocaInst* source);
	static bool findMangledClassName(const char* const s, const char* &className, int& classLen);
public:
	static void rewriteNativeObjectsConstructors(llvm::Module& M, llvm::Function& F);
	/*
	 * Return true if callInst has been rewritten and it must be deleted
	 */
	static bool rewriteIfNativeConstructorCall(llvm::Module& M, llvm::Instruction* i, llvm::AllocaInst* newI,
					    llvm::Instruction* callInst, llvm::Function* called,
					    const std::string& builtinTypeName,
					    llvm::SmallVector<llvm::Value*, 4>& initialArgs);
	static void rewriteNativeAllocationUsers(llvm::Module& M,llvm::SmallVector<llvm::Instruction*,4>& toRemove,
							llvm::Instruction* allocation, llvm::Type* t,
							const std::string& builtinTypeName);
};

#endif

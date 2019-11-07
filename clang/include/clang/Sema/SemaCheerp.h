//===-- Sema/SemaCheerp.h - Cheerp Sema utilities -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_SEMA_CHEERP_H
#define _CHEERP_SEMA_CHEERP_H

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Cheerp/DeterministicUnorderedSet.h"
#include "llvm/Cheerp/DeterministicUnorderedMap.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "clang/AST/DeclCXX.h"
#include "clang/Sema/AttributeList.h"
#include <string>

namespace cheerp{

enum class TypeKind
{
	Void, IntMax32Bit, IntGreater32Bit, FloatingPoint, NamespaceClient, Pointer, Function, FunctionPointer, Reference, JsExportable, Other
};

TypeKind classifyType(const clang::Type* Ty);

bool isInAnyNamespace(const clang::Decl* decl);

bool couldBeJsExported(clang::CXXRecordDecl* Record, clang::Sema& sema);

bool couldReturnBeJsExported(const clang::Type* Ty, clang::FunctionDecl* FD, clang::Sema& sema);
bool couldParameterBeJsExported(const clang::Type* Ty, clang::FunctionDecl* FD, clang::Sema& sema, const bool isParameter = true);

bool checkParameters(clang::FunctionDecl* Method, clang::Sema& sema);

void checkFunction(clang::FunctionDecl* FD, clang::Sema& sema);

bool isTemplate(clang::FunctionDecl* FD);
void checkFunctionToBeJsExported(clang::FunctionDecl* FD, bool isMethod, clang::Sema& sema);

class CheerpSemaClassData
{
public:
	CheerpSemaClassData(clang::CXXRecordDecl* record, clang::Sema& sema) : recordDecl(record), sema(sema)
	{
	}
	void addMethod(clang::CXXMethodDecl* FD);
	void checkRecord();
private:
	typedef cheerp::DeterministicUnorderedSet<clang::CXXMethodDecl*> MethodSet;
	struct Interface
	{
		bool isPublicInterface{true};
		MethodSet methods;
		void clear()
		{
			assert(isPublicInterface);
			methods.clear();
		}
		bool empty() const
		{
			return methods.empty();
		}
		void insert(clang::CXXMethodDecl* method)
		{
			methods.insert(method);
		}
		template <class T>
		void addToInterface (T* item, clang::Sema& sema);
	};
	clang::CXXRecordDecl* recordDecl;
	//Set of methods declared in a class/struct
	MethodSet declared_methods;
	clang::Sema& sema;
};

class CheerpSemaData
{
public:
	CheerpSemaData(clang::Sema& sema) : sema(sema)
	{
	}
	void addFunction(clang::FunctionDecl* FD);
	void checkRecord(clang::CXXRecordDecl* record);
private:
	void addMethod(clang::CXXMethodDecl* method, const bool isJsExport);

	cheerp::DeterministicUnorderedMap<clang::CXXRecordDecl*, CheerpSemaClassData, RestrictionsLifted::NoErasure | RestrictionsLifted::NoDeterminism> classData;
	clang::Sema& sema;
};

class JsExportContext
{
public:
	explicit JsExportContext(llvm::Module& module, llvm::LLVMContext& context, llvm::IntegerType* type)
		: module(module), context(context), intType(type)
	{
	}
	void addFreeFunctionJsExportMetadata(llvm::Function* F);
	void addRecordJsExportMetadata(const clang::CXXMethodDecl *method, llvm::Function* F, const std::string& className);
private:
	llvm::Module& module;
	llvm::LLVMContext& context;
	llvm::IntegerType* intType;
};

bool shouldBeJsExported(const clang::Decl *D, const bool isMethod);

}  //end namespace cheerp
#endif //_CHEERP_SEMA_CHEERP_H

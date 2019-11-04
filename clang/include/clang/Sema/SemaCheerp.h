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

#include "llvm/ADT/iterator_range.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "clang/AST/DeclCXX.h"
#include "clang/Sema/AttributeList.h"
#include <set>
#include <unordered_map>
#include <string>

namespace cheerp{

enum class TypeKind
{
	Void, IntMax32Bit, IntGreater32Bit, FloatingPoint, NamespaceClient, Pointer, Function, FunctionPointer, Reference, JsExportable, Other
};

TypeKind classifyType(const clang::Type* Ty);

template <typename T>
unsigned int getNumUserDefinedMethods(const llvm::iterator_range<T>& range)
{
	auto it = range.begin();
	const auto& end = range.end();
	unsigned int count = 0;
	while (it != end)
	{
		if (it->isUserProvided())
			++count;
		++it;
	}
	return count;
}

bool isInAnyNamespace(const clang::Decl* decl);

bool couldBeJsExported(clang::CXXRecordDecl* Record, clang::Sema& sema);

bool couldReturnBeJsExported(const clang::Type* Ty, clang::FunctionDecl* FD, clang::Sema& sema);
bool couldParameterBeJsExported(const clang::Type* Ty, clang::FunctionDecl* FD, clang::Sema& sema, const bool isParamether = true);

bool checkParameters(clang::FunctionDecl* Method, clang::Sema& sema);
bool couldBeJsExported(clang::CXXMethodDecl* Method, clang::Sema& sema);

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
	typedef std::set<clang::CXXMethodDecl*> MethodSet;
	struct Interface
	{
		MethodSet methods;
		bool empty() const
		{
			return methods.empty();
		}
		void insert(clang::CXXMethodDecl* method)
		{
			methods.insert(method);
		}
	};
	template <class T>
	void insertIntoInterfaces(T* item, Interface& tagged_, Interface& public_);
	clang::CXXRecordDecl* recordDecl;
	MethodSet methods;
	MethodSet toBeJsExportedMethods;
	void buildToBeJsExportedMethods();
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
	void checkFreeJsExportedFunction(clang::FunctionDecl* FD);
	void addMethod(clang::CXXMethodDecl* method);

	std::unordered_map<clang::CXXRecordDecl*, CheerpSemaClassData> classData;
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

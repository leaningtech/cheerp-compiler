//===-- Sema/SemaCheerp.h - Cheerp Sema utilities -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019-2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_SEMA_CHEERP_H
#define _CHEERP_SEMA_CHEERP_H

#include <map>
#include "llvm/ADT/iterator_range.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Cheerp/DeterministicUnorderedSet.h"
#include "llvm/Cheerp/DeterministicUnorderedMap.h"
#include "clang/AST/DeclCXX.h"
#include "clang/Sema/ParsedAttr.h"

namespace cheerp{

enum class TypeKind
{
	Void, Boolean, IntLess32Bit, UnsignedInt32Bit, SignedInt32Bit, IntGreater32Bit, FloatingPoint, NamespaceClient, Pointer, Function, FunctionPointer, Reference, JsExportable, Other
};

enum class SpecialFunctionClassify
{
	GenericGetter, Getter, GenericSetter, Setter, Other
};

TypeKind classifyType(const clang::QualType& Qy, const clang::Sema& sema);

void checkCouldBeJsExported(const clang::CXXRecordDecl* Record, clang::Sema& sema, bool& shouldContinue);

void checkCouldReturnBeJsExported(const clang::QualType& Ty, const clang::FunctionDecl* FD, clang::Sema& sema);
void checkCouldBeParameterOfJsExported(const clang::QualType& Ty, const clang::Decl* FD, clang::Sema& sema, const clang::Attr* asmJSAttr, const bool isParameter = true);

void checkParameters(const clang::FunctionDecl* Method, clang::Sema& sema);

SpecialFunctionClassify classifyNamedFunction(const clang::StringRef name);

void checkDestructor(const clang::CXXRecordDecl* Record, clang::Sema& sema, bool& shouldContinue);
void checkFunctionOnDefinition(clang::FunctionDecl* FD, clang::Sema& sema);
void checkFunctionOnDeclaration(clang::FunctionDecl* FD, clang::Sema& sema);

bool isTemplate(const clang::FunctionDecl* FD);

class CheerpSemaData;

class CheerpSemaClassData
{
public:
	CheerpSemaClassData(const clang::CXXRecordDecl* record, cheerp::CheerpSemaData* cheerpSema) : recordDecl(record), cheerpSema(cheerpSema)
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
	clang::Sema& get_sema() const;
	const clang::CXXRecordDecl* recordDecl;
	//Set of methods declared in a class/struct
	MethodSet declared_methods;
	cheerp::CheerpSemaData* cheerpSema;
};


class CheerpSemaData
{
public:
	CheerpSemaData(clang::Sema& sema) : sema(sema)
	{
	}
	void addFunction(clang::FunctionDecl* FD);
	void checkRecord(const clang::CXXRecordDecl* record);
	clang::Sema& get_sema() const
	{
		return sema;
	}
	void checkFunctionToBeJsExported(const clang::FunctionDecl* FD, bool isMethod);
private:
	void addMethod(clang::CXXMethodDecl* method, const bool isJsExport);
	void checkNamespaceLevelName(const clang::NamedDecl* FD);

	cheerp::DeterministicUnorderedMap<const clang::CXXRecordDecl*, CheerpSemaClassData, RestrictionsLifted::NoErasure | RestrictionsLifted::NoDeterminism> classData;
	std::map<std::pair<const clang::DeclContext*, std::string>, const clang::NamedDecl*> namedDecl;
	clang::Sema& sema;
};

bool shouldBeJsExported(const clang::Decl *D, const bool isMethod);

}  //end namespace cheerp
#endif //_CHEERP_SEMA_CHEERP_H

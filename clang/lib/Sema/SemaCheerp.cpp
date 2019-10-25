//===-- Sema/SemaCheerp.cpp - Cheerp Sema utilities -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "clang/Analysis/AnalysisDeclContext.h"
#include "clang/AST/Type.h"
#include "clang/Sema/SemaCheerp.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Sema/SemaInternal.h"

bool cheerp::couldBeJsExported(clang::CXXRecordDecl* Record, clang::Sema& sema)
{
	//class or struct
	using namespace clang;

	if (Record->isDynamicClass())
	{
		sema.Diag(Record->getLocation(), diag::err_cheerp_attribute_on_virtual_class) << Record->getAttr<JsExportAttr>();
		return false;
	}

	uint32_t numUserDeclared = cheerp::getNumUserDefinedMethods<CapturedDecl::specific_decl_iterator< CXXConstructorDecl > >(Record->ctors());
	if (numUserDeclared != 1)
	{
		if (numUserDeclared == 0)
		{
			assert(!Record->hasUserDeclaredConstructor());
			sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_on_class_without_constructor);
		}
		else
		{
			sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_on_class_with_multiple_user_defined_constructor);
		}
		return false;
	}

	if (Record->hasNonTrivialDestructor())
	{
		sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_with_non_trivial_destructor);
		return false;
	}

	//TODO: templated classes should be checked elsewhere, double check that all error here are also catched for template

	for (auto method : Record->methods())
	{
		if (method->getAccess() != AS_public)
			continue;
		if (isa<CXXConstructorDecl>(method))
			continue;
		couldBeJsExported(method, sema);
	}

	//TODO: Check for any public data or static member
	return true;
}

bool cheerp::couldBeJsExported(clang::CXXMethodDecl* method, clang::Sema& sema)
{
	using namespace clang;

	if (method->isOverloadedOperator())
	{
		sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_with_operators);
		return false;
	}

	bool could = couldReturnBeJsExported(method->getReturnType().getTypePtr(), method, sema);

//TODO: introduce check also for paramethers of methods (more tricky, since passing functions should be ok)
//	for (auto it : method->parameters())
//	{
//		could &= couldBeUsedJsExported(it->getOriginalType().getTypePtr(), method, sema);
//	}

	return could;
}

bool cheerp::couldReturnBeJsExported(const clang::Type* Ty, clang::CXXMethodDecl* method, clang::Sema& sema)
{
	using namespace cheerp;
	using namespace clang;

	switch (classifyType(Ty))
	{
		case TypeKind::Void:
		case TypeKind::IntMax32Bit:
		case TypeKind::FloatingPoint:
		{
			return true;
		}
		case TypeKind::NamespaceClient:
		{
			sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_on_naked_client);
			return false;
		}
		case TypeKind::Other:
		{
			sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_unknow_type);
			return false;
		//TODO: If Ty is Other, it may be ok as long as it's already tagged as JsExportable or could be tagged JsExportable
			//(possibly by voiding some restrictions, for example existence of public constructor since it's already created)
			//but both loop detection and tagging of classes should be first implemented
		//	CXXRecordDecl* Record = Ty->getAsCXXRecordDecl();
		//	return (couldBeJsExported(Record, sema) && hasJsExportedAttr(Record));
		}
		case TypeKind::IntGreater32Bit:
		{
			sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_longlong);
			return false;
		}
		case TypeKind::Pointer:
		{
			break;
		}
		case TypeKind::Reference:
		{
			sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_on_reference);
			return false;
		}
		case TypeKind::FunctionPointer:
		{
			sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_on_function_pointer);
			return false;
		}
		default:
		{
			llvm_unreachable("Should have been catched earlier");
		}
	}

	assert(Ty->isPointerType());
	const clang::Type* Ty2 = Ty->getPointeeType().getTypePtr();

	switch (classifyType(Ty2))
	{
		case TypeKind::Void:
		{
			sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_on_void_pointer);
			return false;
		}
		case TypeKind::IntMax32Bit:
		case TypeKind::FloatingPoint:
		case TypeKind::NamespaceClient:
		{
			return true;
		}
		case TypeKind::Other:
		{
			sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_unknow_type);
			return false;
		}
		case TypeKind::IntGreater32Bit:
		{
			sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_TODO);
			return false;
		}
		case TypeKind::Pointer:
		{
			sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_on_double_pointer);
			return false;
		}
		default:
		{
			llvm_unreachable("Should have been catched earlier");
		}
	}
	llvm_unreachable("Should have been catched earlier");
}

cheerp::TypeKind cheerp::classifyType(const clang::Type* Ty)
{
	using namespace cheerp;
	if (Ty->isReferenceType())
	{
		return TypeKind::Reference;
	}
	if (Ty->isIntegerType())
	{
		const clang::BuiltinType* builtin = (const clang::BuiltinType*) Ty;
		switch (builtin->getKind())
		{
			case clang::BuiltinType::LongLong:
			case clang::BuiltinType::ULongLong:
				return TypeKind::IntGreater32Bit;
			default:
				return TypeKind::IntMax32Bit;
		}
	}
	if (Ty->isRealFloatingType())
	{
		return TypeKind::FloatingPoint;
	}
	if (Ty->isFunctionType())
	{
		return TypeKind::Function;
	}
	if (Ty->isFunctionPointerType())
	{
		return TypeKind::FunctionPointer;
	}
	if (Ty->isPointerType())
	{
		return TypeKind::Pointer;
	}

	const clang::CXXRecordDecl* Record = Ty->getAsCXXRecordDecl();
	if (Record == NULL)
	{
		return TypeKind::Void;
	}
	if (clang::AnalysisDeclContext::isInClientNamespace(Record))
	{
		return TypeKind::NamespaceClient;
	}
	return TypeKind::Other;
}

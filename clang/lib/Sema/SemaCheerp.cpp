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

	if (Record->hasNonTrivialDestructor())
	{
		sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_with_non_trivial_destructor);
		return false;
	}

	//TODO: templated classes should be checked elsewhere, double check that all error here are also catched for template

	int publicConstructors = 0;
	std::set<std::string> publicMethodNames;
	std::set<std::string> staticPublicMethodNames;
	bool could = true;

	for (auto method : Record->methods())
	{
		if (method->getAccess() != AS_public)
			continue;
		if (isa<CXXDestructorDecl>(method))
		{
			continue;
		}
		if (isa<CXXConstructorDecl>(method))
		{
			could &= checkParameters(method, sema);
			++publicConstructors;
			continue;
		}
		could &= couldBeJsExported(method, sema);
		const auto& name = method->getNameInfo().getName().getAsString();
		const auto pair = method->isStatic() ?
			staticPublicMethodNames.insert(name) :
			publicMethodNames.insert(name);
		if (pair.second == false)
		{
			sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_same_name_methods) << method;
			could = false;
		}
	}

	if (publicConstructors != 1)
	{
		if (publicConstructors == 0)
			sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_on_class_without_constructor);
		else
			sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_on_class_with_multiple_user_defined_constructor);
		could = false;
	}

	//TODO: Check for any public data or static member
	return could;
}

bool cheerp::checkParameters(clang::FunctionDecl* FD, clang::Sema& sema)
{
	bool could = true;

	for (auto it : FD->parameters())
	{
		if (it->hasDefaultArg())
		{
			sema.Diag(it->getLocation(), clang::diag::err_cheerp_jsexport_with_default_arg) << FD->getParent();
			could = false;
		}
		could &= couldParameterBeJsExported(it->getOriginalType().getTypePtr(), FD, sema);
	}

	return could;
}

bool cheerp::couldBeJsExported(clang::CXXMethodDecl* method, clang::Sema& sema)
{
	using namespace clang;

	if (method->isOverloadedOperator())
	{
		sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_with_operators);
		return false;
	}
	//TODO: Template public methods should be checked in SemaTemplate

	bool could = couldReturnBeJsExported(method->getReturnType().getTypePtr(), method, sema);

	could &= checkParameters(method, sema);

	return could;
}

bool cheerp::couldParameterBeJsExported(const clang::Type* Ty, clang::FunctionDecl* FD, clang::Sema& sema)
{
	using namespace cheerp;
	using namespace clang;

	//TODO: have to be checked again, may be possible to be more restrictive on some things while permitting others

	switch (classifyType(Ty))
	{
		case TypeKind::Void:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_TODO);
			return false;
		}
		case TypeKind::FunctionPointer:
		case TypeKind::IntMax32Bit:
		case TypeKind::FloatingPoint:
		{
			return true;
		}
		case TypeKind::NamespaceClient:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_naked_client);
			return false;
		}
		case TypeKind::Other:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_unknow_type);
			return false;
		//TODO: If Ty is Other, it may be ok as long as it's already tagged as JsExportable or could be tagged JsExportable
			//(possibly by voiding some restrictions, for example existence of public constructor since it's already created)
			//but both loop detection and tagging of classes should be first implemented
		//	CXXRecordDecl* Record = Ty->getAsCXXRecordDecl();
		//	return (couldBeJsExported(Record, sema) && hasJsExportedAttr(Record));
		}
		case TypeKind::IntGreater32Bit:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_longlong);
			return false;
		}
		case TypeKind::Pointer:
		{
			break;
		}
		case TypeKind::Reference:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_reference);
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
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_void_pointer);
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
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_unknow_type);
			return false;
		}
		case TypeKind::IntGreater32Bit:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_TODO);
			return false;
		}
		case TypeKind::Pointer:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_double_pointer);
			return false;
		}
		default:
		{
			llvm_unreachable("Should have been catched earlier");
		}
	}
	llvm_unreachable("Should have been catched earlier");
}

bool cheerp::couldReturnBeJsExported(const clang::Type* Ty, clang::FunctionDecl* method, clang::Sema& sema)
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

void cheerp::checkFunction(clang::FunctionDecl* FD, clang::Sema& sema)
{
	sema.cheerpSemaData.addFunction(FD);
}

void cheerp::CheerpSemaData::addFunction(clang::FunctionDecl* FD)
{
	using namespace cheerp;
	using namespace clang;
	if (isa<CXXMethodDecl>(FD))
	{
		return;
	}
	if (FD->hasAttr<JsExportAttr>())
		checkFreeJsExportedFunction(FD);
}

void cheerp::CheerpSemaData::checkFreeJsExportedFunction(clang::FunctionDecl* FD)
{
	using namespace cheerp;
	using namespace clang;
	const bool isTemplate = (FD->getTemplatedKind() != FunctionDecl::TemplatedKind::TK_NonTemplate);
	if (isTemplate)
	{
		sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_function_template);
		return;
	}

	couldReturnBeJsExported(FD->getReturnType().getTypePtr(), FD, sema);
	checkParameters(FD, sema);

	std::string name = FD->getNameInfo().getName().getAsString();
	auto pair = jsexportedFreeFunction.insert(name);
	if (!pair.second)
		sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_same_name_methods) << name;
}

void cheerp::CheerpSemaData::checkMethod(clang::CXXMethodDecl* method)
{
	using namespace cheerp;
	using namespace clang;

	//TODO: add this method to the class, then decide whether this makes any sense

	const bool isTemplate = (method->getTemplatedKind() != FunctionDecl::TemplatedKind::TK_NonTemplate);
	if (isTemplate)
	{
		sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_on_method_template);
		return;
	}
}

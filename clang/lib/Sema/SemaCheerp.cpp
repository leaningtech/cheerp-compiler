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
#include "llvm/Cheerp/JsExport.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include <string>
#include <unordered_set>

bool cheerp::isInAnyNamespace(const clang::Decl* decl)
{
	auto context = decl->getDeclContext();
	while (context->isTransparentContext())
	{
		//This serves to skip things like: extern "C" that are a context but that are transparent for namespace rules
		context = context->getParent();
	}
	return (context->getParent() != NULL);
}

bool cheerp::couldBeJsExported(clang::CXXRecordDecl* Record, clang::Sema& sema)
{
	//class or struct
	using namespace clang;

	if (isInAnyNamespace(Record))
	{
		sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_on_namespace);
		return false;
	}

	if (Record->isDynamicClass())
	{
		sema.Diag(Record->getLocation(), diag::err_cheerp_attribute_on_virtual_class) << Record->getAttr<JsExportAttr>();
		return false;
	}

	if (Record->getNumBases())
	{
		sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_inheritance);
		return false;
	}
	assert(Record->getNumVBases() == 0);

	if (Record->hasNonTrivialDestructor())
	{
		sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_with_non_trivial_destructor);
		return false;
	}

	return true;
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
		case TypeKind::JsExportable:
		case TypeKind::NamespaceClient:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_naked_client);
			return false;
		}
		case TypeKind::Other:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_unknow_type);
			return false;
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
		case TypeKind::JsExportable:
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
		case TypeKind::JsExportable:
		case TypeKind::NamespaceClient:
		{
			sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_on_naked_client);
			return false;
		}
		case TypeKind::Other:
		{
			sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_unknow_type);
			return false;
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
		case TypeKind::JsExportable:
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
	using namespace clang;
	if (Record->hasAttr<JsExportAttr>())
	{
		//The actual check about whether is actually JsExporable is done somewhere else (TODO: it is?)
		return TypeKind::JsExportable;
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
		addMethod((CXXMethodDecl*)FD);
	}
	else if (FD->hasAttr<JsExportAttr>())
	{
		checkFreeJsExportedFunction(FD);
	}
}

void cheerp::CheerpSemaData::checkFreeJsExportedFunction(clang::FunctionDecl* FD)
{
	using namespace cheerp;
	using namespace clang;

	checkFunctionToBeJsExported(FD, /*isMethod*/false, sema);

	//TODO: implement on the backend the check that every jsexported/class has to have a different name
}

bool cheerp::isTemplate(clang::FunctionDecl* FD)
{
	return FD->getTemplatedKind() != clang::FunctionDecl::TemplatedKind::TK_NonTemplate;
}

void cheerp::checkFunctionToBeJsExported(clang::FunctionDecl* FD, bool isMethod, clang::Sema& sema)
{
	using namespace cheerp;
	using namespace clang;
	if (isMethod && FD->hasAttr<AsmJSAttr>())
	{
		const std::string kind = isMethod ? "method" : "free function";
		sema.Diag(FD->getLocation(), diag::err_cheerp_incompatible_attributes) << FD->getAttr<AsmJSAttr>() << kind << FD <<
				"[[cheerp::jsexport]]" << kind << FD;
	}

	if (!isMethod)
	{
		if (isInAnyNamespace(FD))
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_namespace);
			return;
		}
	}


	if (isTemplate(FD))
	{
		sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_function_template);
		return;
	}

	if (!isa<CXXConstructorDecl>(FD))
	{
		couldReturnBeJsExported(FD->getReturnType().getTypePtr(), FD, sema);
	}
	checkParameters(FD, sema);
}

void cheerp::CheerpSemaData::addMethod(clang::CXXMethodDecl* method)
{
	clang::CXXRecordDecl* record = method->getParent();
	classData.emplace(record, CheerpSemaClassData(record, sema)).first->second.addMethod(method);
}

void cheerp::CheerpSemaData::checkRecord(clang::CXXRecordDecl* record)
{
	//Here all checks about external feasibility of jsexporting a class/struct have to be perfomed (eg. checking for name clashes against other functions)

	classData.emplace(record, CheerpSemaClassData(record, sema)).first->second.checkRecord();
}

template <class T>
void cheerp::CheerpSemaClassData::insertIntoInterfaces(T* item, Interface& tagged, Interface& implicit)
{
	using namespace clang;
	const bool isJsExport = item->template hasAttr<JsExportAttr>();
	const bool isPublic = item->getAccess() == AS_public;

	if (isJsExport && !isPublic)
	{
		sema.Diag(recordDecl->getLocation(), diag::err_cheerp_jsexport_on_non_public_member);
		return;
	}

	if (isJsExport)
		tagged.insert(item);
	else if (isPublic)
		implicit.insert(item);
};

void cheerp::CheerpSemaClassData::checkRecord()
{
	using namespace std;
	using namespace clang;
	//Here all checks regarding internal feasibility of jsexporting a class/struct have to be performed
	couldBeJsExported(recordDecl, sema);

	//There are 2 possible interfaces: the implicit one, based on the public member, and the explicit one, based on the member/fields tagged [[cheerp::jsexport]]
	Interface taggedInterface;
	Interface implicitInterface;

	for (auto method : recordDecl->methods())
	{
		insertIntoInterfaces<CXXMethodDecl>(method, taggedInterface, implicitInterface);
	}

	for (auto field : recordDecl->fields())
	{
		insertIntoInterfaces<FieldDecl>(field, taggedInterface, implicitInterface);
	}

	const bool isPublicInterface = taggedInterface.empty();
	Interface& toBeJsExported =
		isPublicInterface ?
		implicitInterface :
		taggedInterface;

	if (!toBeJsExported.fields.empty())
	{
		//Currently not supported jsexporting fields, so fail here
		//TODO: support getter and setter on methods
		sema.Diag(recordDecl->getLocation(), diag::err_cheerp_jsexport_with_public_fields) << *toBeJsExported.fields.begin() << recordDecl;
		return;
	}

	int JsExportedConstructors = 0;
	std::unordered_set<std::string> JsExportedMethodNames;
	std::unordered_set<std::string> staticJsExportedMethodNames;

	staticJsExportedMethodNames.insert("promise");

	for (auto method : toBeJsExported.methods)
	{
		checkFunctionToBeJsExported(method, /*isMethod*/true, sema);

		if (isa<CXXConstructorDecl>(method))
		{
			++JsExportedConstructors;
			continue;
		}

		const auto& name = method->getNameInfo().getAsString();
		const auto pair = method->isStatic() ?
			staticJsExportedMethodNames.insert(name) :
			JsExportedMethodNames.insert(name);
		if (pair.second == false)
		{
			if (name == "promise" && method->isStatic())
				sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_promise_static_method);
			else
				sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_same_name_methods) << method;
		}
	}

	if (JsExportedConstructors != 1)
	{
		if (JsExportedConstructors == 0 && isPublicInterface)
			sema.Diag(recordDecl->getLocation(), diag::err_cheerp_jsexport_on_class_without_constructor);
		else if (JsExportedConstructors > 1)
			sema.Diag(recordDecl->getLocation(), diag::err_cheerp_jsexport_on_class_with_multiple_user_defined_constructor);
	}

	for (auto method : toBeJsExported.methods)
	{
		method->addAttr(JsExportAttr::CreateImplicit(sema.Context));
	}

	//TODO: Implement more checks (eg. whether template functions names clashes) and add logic to jsexport only a subset of the public methods
}

void cheerp::CheerpSemaClassData::addMethod(clang::CXXMethodDecl* method)
{
	//Methods here are only added to the classes/struct, and only later they are checked
	//They have to be added here since this is the only moment that will capture templated methods (that possibly will never be instantiated)
	//But the actual checks have to be performed later when all methods are known
	methods.insert(method);
}

void cheerp::JsExportContext::addFreeFunctionJsExportMetadata(llvm::Function* F)
{
       llvm::NamedMDNode* namedNode = module.getOrInsertNamedMetadata("jsexported_methods");
       llvm::SmallVector<llvm::Metadata*,1> values;
       values.push_back(llvm::ConstantAsMetadata::get(F));
       llvm::MDNode* node = llvm::MDNode::get(context,values);
       namedNode->addOperand(node);
}

void cheerp::JsExportContext::addRecordJsExportMetadata(const clang::CXXMethodDecl *method, llvm::Function* F, const std::string& className)
{
       llvm::NamedMDNode* namedNode = module.getOrInsertNamedMetadata(llvm::Twine(className,"_methods").str());
       llvm::SmallVector<llvm::Metadata*,2> values;
       values.push_back(llvm::ConstantAsMetadata::get(F));

       const MemberKind kind = clang::isa<clang::CXXConstructorDecl>(method) ? MemberKind::Constructor : MemberKind::Method;
       const uint32_t representation = cheerp::getRepresentation(kind, method->isStatic(), method->isConst());
       values.push_back(llvm::ConstantAsMetadata::get(llvm::ConstantInt::get(intType, representation)));

       llvm::MDNode* node = llvm::MDNode::get(context,values);
       namedNode->addOperand(node);
}

bool cheerp::shouldBeJsExported(const clang::Decl *D, const bool isMethod)
{
	return D->hasAttr<clang::JsExportAttr>() && (clang::isa<clang::CXXMethodDecl>(D) == isMethod);
}


//===-- Sema/SemaCheerp.cpp - Cheerp Sema utilities -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2019-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "clang/Analysis/AnalysisDeclContext.h"
#include "clang/AST/ASTContext.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/AST/Type.h"
#include "clang/Sema/SemaCheerp.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Sema/SemaCheerp.h"
#include "clang/Sema/SemaInternal.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "llvm/Cheerp/ForbiddenIdentifiers.h"
#include <string>
#include <unordered_map>

static const clang::DeclContext* getCanonicalContext(const clang::Decl* decl)
{
	auto context = decl->getDeclContext();
	while (context && context->isTransparentContext())
	{
		//This serves to skip things like: extern "C" that are a context but that are transparent for namespace rules
		context = context->getParent();
	}
	return context;
}

static bool isInsideClass(const clang::Decl* decl)
{
	auto context = getCanonicalContext(decl);

	return (context && context->isRecord());
}

static bool isInsideAnonymousNamespace(const clang::Decl* decl)
{
	auto context = decl->getDeclContext();

	while (context)
	{
		if (auto ns = clang::dyn_cast<clang::NamespaceDecl>(context))
			if (ns->isAnonymousNamespace())
				return true;

		context = context->getParent();
	}

	return false;
}

static void checkName(const clang::Decl* decl, const llvm::StringRef& name, clang::Sema& sema)
{
	if (cheerp::isForbiddenJSIdentifier(name))
		sema.Diag(decl->getLocation(), clang::diag::err_cheerp_jsexport_forbidden_identifier) << name;
}

void cheerp::checkCouldBeJsExported(const clang::CXXRecordDecl* Record, clang::Sema& sema, bool& shouldContinue)
{
	//class or struct
	using namespace clang;

	const bool isClient = clang::AnalysisDeclContext::isInClientNamespace(Record);
	if (isClient)
		sema.Diag(Record->getLocation(), diag::err_cheerp_incompatible_jsexport_client) << "Record" << Record;

	if (isInsideAnonymousNamespace(Record))
		sema.Diag(Record->getLocation(), diag::warn_cheerp_anonymous_jsexport);

	if (isInsideClass(Record))
		sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_on_namespace);

	if (Record->getNumBases())
	{
		auto* base = Record->bases_begin();
		auto* baseClass = base->getType()->getAsCXXRecordDecl();

		if (Record->getNumBases() > 1 || base->isVirtual() || base->getAccessSpecifier() != AS_public || !baseClass->hasAttr<JsExportAttr>())
			sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_inheritance);
	}
}

bool cheerp::isNamespaceClientDisabledDecl(clang::FunctionDecl* FD, clang::Sema& sema)
{
      const bool isClient = clang::AnalysisDeclContext::isInClientNamespace(FD);

      clang::FunctionDecl* CheckFD = FD->getTemplateInstantiationPattern();

      if (!CheckFD)
              CheckFD = FD;

      if (!isClient || CheckFD->isDefined())
              return false;

      bool doesWork = TypeChecker::checkSignature<TypeChecker::NamespaceClient, TypeChecker::ReturnValue>(FD, sema);

      return !doesWork;
}


template <cheerp::TypeChecker::KindOfFunction kindOfFunction, cheerp::TypeChecker::FailureMode failureMode>
bool cheerp::TypeChecker::checkSignature(const clang::FunctionDecl* FD, clang::Sema& sema)
{
	bool doesWork = true;
	const clang::JsExportAttr* jsexportAttr = FD->getAttr<clang::JsExportAttr>();
	const bool jsexportUnsafe = jsexportAttr &&
		(jsexportAttr->getSemanticSpelling() == clang::JsExportAttr::CXX11_cheerp_jsexport_unsafe ||
		 jsexportAttr->getSemanticSpelling() == clang::JsExportAttr::GNU_cheerp_jsexport_unsafe);

	if (jsexportUnsafe)
		return true;

	for (auto it : FD->parameters())
	{
		if (kindOfFunction == JSExported && it->hasDefaultArg())
		{
			assert(failureMode == Diagnostic);
			sema.Diag(it->getLocation(), clang::diag::err_cheerp_jsexport_with_default_arg) << FD;
		}

		doesWork &= TypeChecker::checkType<TypeChecker::KindOfValue::Parameter, kindOfFunction, failureMode> (it->getOriginalType(), it->getLocation(), sema, FD->getAttr<clang::AsmJSAttr>());
	}

	if (!clang::isa<clang::CXXConstructorDecl>(FD))
                doesWork &= TypeChecker::checkType<TypeChecker::KindOfValue::Return, kindOfFunction, failureMode>(FD->getReturnType(), FD->getLocation(), sema, FD->getAttr<clang::AsmJSAttr>());

	return doesWork;
}

template <cheerp::TypeChecker::KindOfValue kindOfValue, cheerp::TypeChecker::KindOfFunction kindOfFunction, cheerp::TypeChecker::FailureMode failureMode>
bool cheerp::TypeChecker::checkType(const clang::QualType& Ty, clang::SourceLocation Loc, clang::Sema& sema, const clang::Attr* asmJSAttr)
{
	bool doesWork = true;
	checkTypeImpl<kindOfValue, kindOfFunction, failureMode>(Ty, Loc, sema, asmJSAttr, doesWork);
	return doesWork;
}

template <cheerp::TypeChecker::KindOfValue kindOfValue, cheerp::TypeChecker::KindOfFunction kindOfFunction, cheerp::TypeChecker::FailureMode failureMode>
void cheerp::TypeChecker::checkTypeImpl(const clang::QualType& Ty, clang::SourceLocation Loc, clang::Sema& sema, const clang::Attr* asmJSAttr, bool& doesWork)
{
	//Nothing to say if the type dependes on a Template
	if (Ty->isDependentType())
		return;

	using namespace cheerp;
	using namespace clang;

	assert(kindOfFunction == JSExported || kindOfFunction == NamespaceClient);

	auto trackError = [&sema, &Loc, &doesWork](const char* kind)
	{
		const llvm::StringRef where = (kindOfValue == Parameter) ? "parameter" : "return";
		const llvm::StringRef message = (kindOfFunction == JSExported) ? "that needs to be [[cheerp::jsexport]]-ed" :
			(kindOfFunction == NamespaceClient) ? "forward declared in namespace client" : "declared and defined in namespace client";

		if (failureMode == Diagnostic)
			sema.Diag(Loc, diag::err_cheerp_at_interface_native_js) << message << kind << where;

		doesWork = false;
	};

	const auto type = classifyType(Ty, sema);

	if (kindOfValue == Return)
		if (type == TypeKind::Void)
		{
			//No return is fine (while no parameter is not)
			return;
		}

	if (kindOfFunction == JSExported &&
		kindOfValue == Return)
	{
		if (type == TypeKind::Function ||
			type == TypeKind::FunctionPointer)
		{
			//Returning a function pointer could be OK in certain cases, but we have to check whether the function itself could be jsexported
			//In general the answer is no
			//TODO: possibly relax this check (or maybe not since it's actually complex, will require checking that every possible return value is jsExported
			trackError("function pointers");
		}
		if (type == TypeKind::UnsignedInt32Bit)
		{
			//Unsigned integer can't be represented in JavaScript
			trackError("unsigned interger");
		}
		if (type == TypeKind::IntLess32Bit)
		{
			//Interger shorter than 32 bit would need to carry metatada around to specify whether signed or unsigned
			trackError("interger smaller than 32bit");
		}
	}

	switch (type)
	{
		case TypeKind::Function:
		case TypeKind::FunctionPointer:
		{
			if (asmJSAttr)
			{
				//asmjs / wasm functions can't take (yet) functions pointers as parameters
				sema.Diag(Loc, diag::err_cheerp_jsexport_on_parameter_amsjs_function) << asmJSAttr;
				return;
			}
			else
			{
				//genericjs works!
				return;
			}
		}
		case TypeKind::Boolean:
		case TypeKind::IntLess32Bit:
		case TypeKind::UnsignedInt32Bit:
		case TypeKind::SignedInt32Bit:
		case TypeKind::FloatingPoint:
		case TypeKind::NullPtr:
		{
			//Good!
			return;
		}
		case TypeKind::Pointer:
		case TypeKind::Reference:
		{
			break;
		}
		case TypeKind::Void:
		{
			trackError("void");
			return;
		}
		case TypeKind::JsExportable:
		{
			trackError("naked JsExportable types");
			return;
		}
		case TypeKind::NamespaceClient:
		{
			trackError("naked Client types");
			return;
		}
		case TypeKind::Other:
		{
			trackError("unknown types");
			return;
		}
		case TypeKind::IntGreater32Bit:
		{
			trackError("greater than 32bit integers");
			return;
		}
		default:
		{
			Ty->dump();
			llvm_unreachable("Should have been caught earlier");
		}
	}

	//TODO: have to be checked again, may be possible to be more restrictive on some things while permitting others
	const clang::QualType& PointedTy = Ty.getTypePtr()->getPointeeType();

	if (kindOfValue == Return && PointedTy.isConstQualified() && kindOfFunction == JSExported)
	{
		//TODO: is possible in practice to have const-jsexported elements, but for now keep it at no
		trackError("const-qualified pointer or reference");
	}

	switch (classifyType(PointedTy, sema))
	{
		case TypeKind::Void:
		{
			trackError("void*");
			return;
		}
		case TypeKind::NamespaceClient:
		case TypeKind::JsExportable:
		{
			//Good!
			return;
		}
		case TypeKind::Other:
		{
			trackError("pointers to unknown (neither client namespace nor jsexportable) types");
			return;
		}
		case TypeKind::Boolean:
		case TypeKind::IntLess32Bit:
		case TypeKind::UnsignedInt32Bit:
		case TypeKind::SignedInt32Bit:
		case TypeKind::IntGreater32Bit:
		case TypeKind::FloatingPoint:
		{
			trackError("pointers to base types");
			return;
		}
		case TypeKind::Pointer:
		case TypeKind::FunctionPointer:
		{
			trackError("pointers to pointers");
			return;
		}
		case TypeKind::Reference:
		{
			trackError("pointers to references");
			return;
		}
		default:
		{
			llvm_unreachable("Should have been caught earlier");
		}
	}
}

cheerp::TypeKind cheerp::TypeChecker::classifyType(const clang::QualType& Qy, const clang::Sema& sema)
{
	const clang::QualType& Desugared = Qy.getDesugaredType(sema.Context);
	const clang::Type* Ty = Desugared.getTypePtr();
	using namespace cheerp;
	if (Ty->isReferenceType())
	{
		if (clang::cast<clang::ReferenceType>(Ty)->getPointeeType()->isFunctionType())
			return TypeKind::FunctionPointer;
		return TypeKind::Reference;
	}
	if (Ty->isIntegerType())
	{
		if (sema.Context.getIntWidth(Desugared) > 32)
			return TypeKind::IntGreater32Bit;
		else if (sema.Context.getIntWidth(Desugared) == 1)
			return TypeKind::Boolean;
		else if (sema.Context.getIntWidth(Desugared) == 32)
		{
			if (Ty->isSignedIntegerType())
				return TypeKind::SignedInt32Bit;
			else
				return TypeKind::UnsignedInt32Bit;
		}
		else
			return TypeKind::IntLess32Bit;
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
	if (Ty->isNullPtrType())
	{
		return TypeKind::NullPtr;
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
		//The actual check about whether it is actually JsExportable is done somewhere else
		return TypeKind::JsExportable;
	}
	return TypeKind::Other;
}

static clang::StringRef kinfOfFuncionDecl(const clang::FunctionDecl* FD)
{
		const bool isMethod = clang::isa<clang::CXXMethodDecl>(FD);
		const bool isStatic = (!isMethod) || (clang::dyn_cast<clang::CXXMethodDecl>(FD)->isStatic());

		auto kind = "Free function";
		if (isMethod)
			kind = isStatic ? "Static method" : "Method";

		return kind;
}

cheerp::SpecialFunctionClassify cheerp::classifyNamedFunction(const clang::StringRef name)
{
	if (name.startswith("get_"))
	{
		if (name.size() == 4)
			return SpecialFunctionClassify::GenericGetter;
		return SpecialFunctionClassify::Getter;
	}

	if (name.startswith("set_"))
	{
		if (name.size() == 4)
			return SpecialFunctionClassify::GenericSetter;
		return SpecialFunctionClassify::Setter;
	}

	return SpecialFunctionClassify::Other;
}

static clang::StringRef getName(const cheerp::SpecialFunctionClassify& classification)
{
	using namespace cheerp;

	switch (classification)
	{
	case SpecialFunctionClassify::GenericGetter:
		return "generic setter ('get_')";
	case SpecialFunctionClassify::Getter:
		return "getter ('get_*')";
	case SpecialFunctionClassify::GenericSetter:
		return "generic setter ('set_')";
	case SpecialFunctionClassify::Setter:
		return "setter ('set_*')";
	default:
		return "non special";
	}
}

void cheerp::checkFunctionOnDefinition(clang::FunctionDecl* FD, clang::Sema& sema)
{
	const bool isClient = clang::AnalysisDeclContext::isInClientNamespace(FD);

	if (isClient)
	{
		if (FD->hasAttr<clang::CheerpInterfaceNameAttr>())
			sema.Diag(FD->getLocation(), clang::diag::err_cheerp_interface_name_non_client);

		if (FD->isOutOfLine())
			sema.Diag(FD->getLocation(), clang::diag::err_cheerp_client_out_of_line);

		if (FD->getIdentifier())
		{
			auto kind = kinfOfFuncionDecl(FD);
			const auto name = FD->getName();
			const auto classification = classifyNamedFunction(name);

			if (classification != SpecialFunctionClassify::Other)
				sema.Diag(FD->getLocation(), clang::diag::err_cheerp_client_special_has_definition) << kind << name << getName(classification);
		}
	}
}

void cheerp::checkFunctionOnDeclaration(clang::FunctionDecl* FD, clang::Sema& sema, const bool isAlsoDefinition)
{
	const bool isClient = clang::AnalysisDeclContext::isInClientNamespace(FD);

	if (FD->hasAttr<clang::CheerpInterfaceNameAttr>())
		if (!isClient)
			sema.Diag(FD->getLocation(), clang::diag::err_cheerp_interface_name_non_client);

	if (isClient)
	{
		//Check naming of namespace client forward declared functions
		if (FD->getIdentifier())
		{
			const auto name = FD->getName();
			const auto classification = classifyNamedFunction(name);
			auto kind = kinfOfFuncionDecl(FD);;

			uint32_t expectedNumOfParameters = -1;
			bool isGetterOrSetter = true;
			switch (classification)
			{
			case SpecialFunctionClassify::GenericGetter:
				expectedNumOfParameters = 1;
				break;
			case SpecialFunctionClassify::Getter:
				expectedNumOfParameters = 0;
				break;
			case SpecialFunctionClassify::GenericSetter:
				expectedNumOfParameters = 2;
				break;
			case SpecialFunctionClassify::Setter:
				expectedNumOfParameters = 1;
				break;
			default:
				isGetterOrSetter = false;
				break;
			}

			if (isGetterOrSetter)
				sema.cheerpSemaData.checkTopLevelSpecialFunctions(FD);

			const uint32_t numArgs = FD->getNumParams();

			if (expectedNumOfParameters != -1u && numArgs != expectedNumOfParameters)
				sema.Diag(FD->getLocation(), clang::diag::err_cheerp_client_special_wrong_num_parameters) << kind << name << getName(classification) << expectedNumOfParameters;

		}
		//In line definition means it's codegenerated by Cheerp, so we can relax checks
		if (!isAlsoDefinition)
			TypeChecker::checkSignature<cheerp::TypeChecker::KindOfFunction::NamespaceClient, cheerp::TypeChecker::FailureMode::Diagnostic>(FD, sema);
	}
	if (FD->hasAttr<clang::GenericJSAttr>()) {
		for (clang::ParmVarDecl* P: FD->parameters())
		{
			// Check if any parameters to a function with genericJS attribute are vectors.
			if (P->getType()->isVectorType())
				sema.Diag(P->getLocation(), clang::diag::err_cheerp_vector_from_genericjs) << "have parameter" << P << FD << FD->getAttr<clang::GenericJSAttr>();
			if (auto* PD = P->getType()->getAsTagDecl()) {
				if (clang::AnalysisDeclContext::isInClientNamespace(PD)) {
					sema.Diag(P->getLocation(), clang::diag::err_cheerp_client_layout_lvalue);
				}
			}
		}
	}

	sema.cheerpSemaData.addFunction(FD, isClient);
}

void cheerp::CheerpSemaData::addFunction(clang::FunctionDecl* FD, const bool isClient)
{
	using namespace cheerp;
	using namespace clang;
	const bool isJsExport = FD->hasAttr<JsExportAttr>();

	if (isa<CXXMethodDecl>(FD))
	{
		addMethod((CXXMethodDecl*)FD, isJsExport);
	}
	else if (isJsExport)
	{
		checkFunctionToBeJsExported(FD, /*isMethod*/false, isClient);
	}
}

bool cheerp::isTemplate(const clang::FunctionDecl* FD)
{
	return FD->getTemplatedKind() != clang::FunctionDecl::TemplatedKind::TK_NonTemplate;
}

void cheerp::CheerpSemaData::checkFunctionToBeJsExported(const clang::FunctionDecl* FD, const bool isMethod, const bool isClient)
{
	using namespace cheerp;
	using namespace clang;
	clang::Sema& sema = get_sema();

	if (isMethod)
	{
		//Method specific checks
		if (FD->isOverloadedOperator())
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_with_operators);
	}
	else
	{
		//Free function specific checks
		checkNamespaceLevelName(FD);

		if (isInsideClass(FD))
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_namespace);
	}

	if (isClient)
		sema.Diag(FD->getLocation(), diag::err_cheerp_incompatible_jsexport_client) << "Function" << FD;

	if (isInsideAnonymousNamespace(FD))
		sema.Diag(FD->getLocation(), diag::warn_cheerp_anonymous_jsexport);

	if (isTemplate(FD))
		sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_function_template);

	TypeChecker::checkSignature<TypeChecker::JSExported, TypeChecker::Diagnostic>(FD, sema);

	checkName(FD, FD->getNameInfo().getAsString(), sema);
}

void cheerp::CheerpSemaData::checkTopLevelSpecialFunctions(const clang::FunctionDecl* FD)
{
	using namespace clang;

	auto context = getCanonicalContext(FD);

	if (context->isActualClientNamespace())
	{
		sema.Diag(FD->getLocation(), diag::err_cheerp_client_top_level_get_set) << FD->getName();
	}
}

void cheerp::CheerpSemaData::checkNamespaceLevelName(const clang::NamedDecl* ND)
{
	using namespace clang;
	const auto& name = ND->getName();
	auto context = getCanonicalContext(ND);

	const auto pair = namedDecl.emplace(std::pair<const clang::DeclContext*, std::string>(context, name), ND);

	if (!pair.second && pair.first->second->getCanonicalDecl() != ND->getCanonicalDecl())
	{
		sema.Diag(ND->getLocation(), diag::err_cheerp_jsexport_same_name) << name;
		sema.Diag(pair.first->second->getLocation(), diag::note_previous_definition);
	}
}

void cheerp::CheerpSemaData::addMethod(clang::CXXMethodDecl* method, const bool isJsExport)
{
	using namespace clang;
	const clang::CXXRecordDecl* record = method->getParent();

	if (isJsExport)
	{
		if (!record->hasAttr<JsExportAttr>())
		{
			sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_on_method_of_not_jsexported_class) << record->getLocation();
			return;
		}
	}

	classData.emplace(record, CheerpSemaClassData(record, this)).first->second.addMethod(method);
}

void cheerp::CheerpSemaData::checkRecord(const clang::CXXRecordDecl* record)
{
	checkNamespaceLevelName(record);
	//Here all checks about external feasibility of jsexporting a class/struct have to be performed (eg. checking for name clashes against other functions)
	checkName(record, record->getName(), sema);

	//TODO: name check against known function/classes with the same name

	classData.emplace(record, CheerpSemaClassData(record, this)).first->second.checkRecord();
}

clang::Sema& cheerp::CheerpSemaClassData::get_sema() const
{
	return cheerpSema->get_sema();
}

template <class T>
void cheerp::CheerpSemaClassData::Interface::addToInterface(T* item, clang::Sema& sema)
{
	using namespace clang;
	const bool isJsExport = item->template hasAttr<JsExportAttr>();
	const bool isPublic = item->getAccess() == AS_public;

	if (isJsExport && !isPublic)
	{
		sema.Diag(item->getLocation(), diag::err_cheerp_jsexport_on_non_public_member);
		return;
	}

	if (isJsExport)
	{
		if (isPublicInterface)
			clear();
		isPublicInterface = false;
		insert(item);
	}
	else if (isPublic && isPublicInterface)
	{
		insert(item);
	}
}

void cheerp::CheerpSemaClassData::Interface::addDestructor(clang::CXXDestructorDecl* decl, clang::Sema& sema)
{
	if (decl)
		insert(decl);
}

void cheerp::CheerpSemaClassData::checkRecord()
{
	using namespace std;
	using namespace clang;

	clang::Sema& sema = get_sema();

	assert(recordDecl->hasAttr<JsExportAttr>());
	auto attr = recordDecl->getAttr<JsExportAttr>();

	bool shouldContinue = true;
	bool unsafe = attr->getSemanticSpelling() == clang::JsExportAttr::CXX11_cheerp_jsexport_unsafe ||
		attr->getSemanticSpelling() == clang::JsExportAttr::GNU_cheerp_jsexport_unsafe;
	//Here all checks regarding internal feasibility of jsexporting a class/struct have to be performed
	if(!unsafe)
		checkCouldBeJsExported(recordDecl, sema, shouldContinue);

	if (!shouldContinue)
		return;

	//Add all the methods. This is needed since recordDecl->methods() would skip templated methods non instantiated, but we already collected them earlier
	for (auto method : recordDecl->methods())
	{
		addMethod(method);
	}

	//There are 2 possible interfaces: the implicit one, based on the public member, and the explicit one, based on the member tagged [[cheerp::jsexport]]
	Interface interface;

	for (auto method : declared_methods)
	{
		interface.addToInterface<CXXMethodDecl>(method, sema);
	}

	const bool isPublicInterface = interface.isPublicInterface;

	if (isPublicInterface)
	{
		for (auto decl : recordDecl->decls())
		{
			if (decl->isImplicit())
				continue;
			if (decl->getAccess() == AS_public && isa<RecordDecl>(decl))
				sema.Diag(decl->getLocation(), diag::err_cheerp_jsexport_inner_RecordDecl);
		}
	}

	interface.addDestructor(recordDecl->getDestructor(), sema);

	std::vector<const CXXConstructorDecl*> JsExportedConstructors;
	std::unordered_map<std::string, const clang::CXXMethodDecl*> JsExportedMethodNames;
	std::unordered_map<std::string, const clang::CXXMethodDecl*> staticJsExportedMethodNames;

	staticJsExportedMethodNames.emplace("promise", nullptr);

	for (auto method : interface.methods)
	{
		cheerpSema->checkFunctionToBeJsExported(method, /*isMethod*/true, /*isClient*/false);
		//isClient is set to false since membership of namespace client are already checked in checkCouldBeJsExported

		if (const CXXConstructorDecl* constructor = clang::dyn_cast<CXXConstructorDecl>(method))
		{
			JsExportedConstructors.push_back(constructor);
			continue;
		}
		if (CXXDestructorDecl* destructor = clang::dyn_cast<CXXDestructorDecl>(method))
		{
			destructor->setImplicit(false);
			continue;
		}

		const auto& name = method->getNameInfo().getAsString();
		const auto pair = method->isStatic() ?
			staticJsExportedMethodNames.emplace(name, method) :
			JsExportedMethodNames.emplace(name, method);
		if (pair.second == false)
		{
			if (name == "promise" && method->isStatic())
				sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_promise_static_method);
			else
			{
				sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_same_name_methods) << method;
				sema.Diag(pair.first->second->getLocation(), diag::note_previous_definition);
			}
		}
	}

	if (JsExportedConstructors.size() > 1)
	{
		sema.Diag(JsExportedConstructors[0]->getLocation(), diag::err_cheerp_jsexport_on_class_with_multiple_user_defined_constructor);
		for (unsigned int i = 1; i<JsExportedConstructors.size(); i++)
			sema.Diag(JsExportedConstructors[i]->getLocation(), diag::note_previous_definition);
	}

	for (auto method : interface.methods)
	{
		method->addAttr(JsExportAttr::CreateImplicit(sema.Context));
	}
}

void cheerp::CheerpSemaClassData::addMethod(clang::CXXMethodDecl* method)
{
	//Methods here are only added to the classes/struct, and only later they are checked
	//They have to be added here since this is the only moment that will capture templated methods (that possibly will never be instantiated)
	//But the actual checks have to be performed later when all methods are known
	if (method->isImplicit())
		return;
	declared_methods.insert(method);
}

cheerp::CheerpAttributeToAdd cheerp::getCheerpAttributeToAdd(clang::Sema& S, const clang::Decl*& decl)
{
	// If already present, return the current decl's attribute
	if (decl->hasAttr<clang::AsmJSAttr>())
		return CheerpAttributeToAdd::AsmJSLike;
	else if (decl->hasAttr<clang::GenericJSAttr>())
		return CheerpAttributeToAdd::GenericJS;

	// Inherit from context (possibly walking up in the tree until a DeclContext is tagged or no DeclContext exists)
	while (const clang::DeclContext* ctx = decl->getDeclContext())
	{
		decl = clang::cast<clang::Decl>(ctx);

		if (decl->hasAttr<clang::AsmJSAttr>())
			return CheerpAttributeToAdd::AsmJSLike;
		else if (decl->hasAttr<clang::GenericJSAttr>())
			return CheerpAttributeToAdd::GenericJS;
	}

	// Or set default attr based on default
	if (S.CurCheerpFallbackAS == clang::LangAS::cheerp_wasm)
		return CheerpAttributeToAdd::AsmJSLike;
	else if (S.CurCheerpFallbackAS == clang::LangAS::cheerp_genericjs)
		return CheerpAttributeToAdd::GenericJS;
	else
		return CheerpAttributeToAdd::None;

	assert(false);
}

void cheerp::checksOnAsmJSAttributeInjection(clang::Sema& sema, const clang::Decl* decl)
{
	using namespace clang;
	if (decl->hasAttr<PackedAttr>() && (sema.LangOpts.getCheerpLinearOutput() == LangOptions::CheerpLinearOutputTy::CHEERP_LINEAR_OUTPUT_AsmJs))
		sema.Diag(decl->getBeginLoc(), diag::err_attributes_are_not_compatible)
			<< "'asmjs'"
			<< decl->getAttr<PackedAttr>();
}

bool cheerp::canAddressOfClientBeTaken(const clang::VarDecl* VD, const clang::Sema& sema)
{
	using namespace clang;
	QualType Ty = VD->getType();
	while (true)
	{
		const auto type = cheerp::TypeChecker::classifyType(Ty, sema);

		switch (type)
		{
			case TypeKind::Boolean:
			case TypeKind::IntLess32Bit:
			case TypeKind::UnsignedInt32Bit:
			case TypeKind::SignedInt32Bit:
			case TypeKind::FloatingPoint:
			{
				return false;
			}
			case TypeKind::Pointer:
			{
				return Ty->getPointeeType().getAddressSpace() != LangAS::cheerp_client;
			}
			case TypeKind::Reference:
			{
				break;
			}
			default:
			{
				return true;
			}

		}

		Ty = Ty.getTypePtr()->getPointeeType();
	}
}

bool cheerp::isBuiltinOrClientPtr(const clang::QualType& Ty, const clang::Sema& sema)
{
	const auto type = cheerp::TypeChecker::classifyType(Ty, sema);

	switch (type)
	{
		case TypeKind::Boolean:
		case TypeKind::IntLess32Bit:
		case TypeKind::UnsignedInt32Bit:
		case TypeKind::SignedInt32Bit:
		case TypeKind::FloatingPoint:
		{
			return true;
		}
		case TypeKind::Pointer:
		{
			break;
		}
		default:
		{
			return false;
		}

	}

	const clang::QualType PtrTy = Ty.getTypePtr()->getPointeeType();
	const auto ptrType = cheerp::TypeChecker::classifyType(PtrTy, sema);

	return (ptrType == TypeKind::NamespaceClient);
}

//===-- Sema/SemaCheerp.cpp - Cheerp Sema utilities -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019-2021 Leaning Technologies
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

void cheerp::checkDestructor(const clang::CXXRecordDecl* Record, clang::Sema& sema, bool& shouldContinue)
{
	using namespace clang;
	//TODO: expand this check
	if (Record->hasNonTrivialDestructor())
	{
		sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_with_non_trivial_destructor);
		shouldContinue = false;
	}
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

	if (isInsideClass(Record))
		sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_on_namespace);

	if (Record->isDynamicClass())
		sema.Diag(Record->getLocation(), diag::err_cheerp_attribute_on_virtual_class) << Record->getAttr<JsExportAttr>();

	if (Record->getNumBases())
		sema.Diag(Record->getLocation(), diag::err_cheerp_jsexport_inheritance);
	else
		assert(Record->getNumVBases() == 0);

	checkDestructor(Record, sema, shouldContinue);
}

void cheerp::checkParameters(const clang::FunctionDecl* FD, clang::Sema& sema)
{
	for (auto it : FD->parameters())
	{
		if (it->hasDefaultArg())
			sema.Diag(it->getLocation(), clang::diag::err_cheerp_jsexport_with_default_arg) << FD;

		checkCouldBeParameterOfJsExported(it->getOriginalType(), &*it, sema, FD->getAttr<clang::AsmJSAttr>());
	}
}

void cheerp::checkCouldBeParameterOfJsExported(const clang::QualType& Ty, const clang::Decl* FD, clang::Sema& sema, const clang::Attr* asmJSAttr, const bool isParameter)
{
	using namespace cheerp;
	using namespace clang;

	const llvm::StringRef where = isParameter ? "parameter" : "return";

	//TODO: have to be checked again, may be possible to be more restrictive on some things while permitting others

	switch (classifyType(Ty, sema))
	{
		case TypeKind::Function:
		case TypeKind::FunctionPointer:
		{
			if (asmJSAttr)
			{
				//asmjs / wasm functions can't take (yet) functions pointers as parameters
				sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_amsjs_function) << asmJSAttr;
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
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "void" << where;
			return;
		}
		case TypeKind::JsExportable:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "naked JsExportable types" << where;
			return;
		}
		case TypeKind::NamespaceClient:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "naked Client types" << where;
			return;
		}
		case TypeKind::Other:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "unknown types" << where;
			return;
		}
		case TypeKind::IntGreater32Bit:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "greater than 32bit integers" << where;
			return;
		}
		default:
		{
			Ty->dump();
			llvm_unreachable("Should have been caught earlier");
		}
	}

	const clang::QualType& Ty2 = Ty.getTypePtr()->getPointeeType();

	if (!isParameter && Ty2.isConstQualified())
	{
		//TODO: is possible in practice to have const-jsexported elements, but for now keep it at no
		sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "const-qualified pointer or reference" << where;
	}

	switch (classifyType(Ty2, sema))
	{
		case TypeKind::Void:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "void*" << where;
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
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) <<
				"pointers to unknown (neither client namespace nor jsexportable) types" << where;
			return;
		}
		case TypeKind::Boolean:
		case TypeKind::IntLess32Bit:
		case TypeKind::UnsignedInt32Bit:
		case TypeKind::SignedInt32Bit:
		case TypeKind::IntGreater32Bit:
		case TypeKind::FloatingPoint:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "pointers to base type" << where;
			return;
		}
		case TypeKind::Pointer:
		case TypeKind::FunctionPointer:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "pointers to pointer" << where;
			return;
		}
		case TypeKind::Reference:
		{
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "pointers to references" << where;
			return;
		}
		default:
		{
			llvm_unreachable("Should have been caught earlier");
		}
	}
	llvm_unreachable("Should have been caught earlier");
}

void cheerp::checkCouldReturnBeJsExported(const clang::QualType& Ty, const clang::FunctionDecl* FD, clang::Sema& sema)
{
	using namespace cheerp;
	using namespace clang;

	switch (classifyType(Ty, sema))
	{
		case TypeKind::Void:
		{
			//No return is fine (while no parameter is not)
			return;
		}
		case TypeKind::Function:
		case TypeKind::FunctionPointer:
		{
			//Returning a function pointer could be OK in certain cases, but we have to check whether the function itself could be jsexported
			//In general the answer is no
			//TODO: possibly relax this check (or maybe not since it's actually complex, will require checking that every possible return value is jsExported
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "function pointers" << "return";
			break;
		}
		case TypeKind::UnsignedInt32Bit:
		{
			//Unsigned integer can't be represented in JavaScript
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "unsigned interger" << "return";
			break;
		}
		case TypeKind::IntLess32Bit:
		{
			//Interger shorter than 32 bit would need to carry metatada around to specify whether signed or unsigned
			sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_parameter_or_return) << "integer smaller than 32bit" << "return";
			break;
		}
		default:
			break;
	}

	//In all other cases, if something could be a parameter it could also work as return
	checkCouldBeParameterOfJsExported(Ty, FD, sema, FD->getAttr<AsmJSAttr>(), /*isParameter*/false);
}

cheerp::TypeKind cheerp::classifyType(const clang::QualType& Qy, const clang::Sema& sema)
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
		const clang::BuiltinType* builtin = clang::cast<clang::BuiltinType>(Ty);

		if (sema.Context.getIntWidth(Desugared) > 32)
			return TypeKind::IntGreater32Bit;
		else if (sema.Context.getIntWidth(Desugared) == 1)
			return TypeKind::Boolean;
		else if (sema.Context.getIntWidth(Desugared) == 32)
		{
			if (builtin->isSignedInteger())
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

void cheerp::checkFunctionOnDeclaration(clang::FunctionDecl* FD, clang::Sema& sema)
{
	const bool isClient = clang::AnalysisDeclContext::isInClientNamespace(FD);

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
	}

	sema.cheerpSemaData.addFunction(FD);
}

void cheerp::CheerpSemaData::addFunction(clang::FunctionDecl* FD)
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
		checkFunctionToBeJsExported(FD, /*isMethod*/false);
	}
}

bool cheerp::isTemplate(const clang::FunctionDecl* FD)
{
	return FD->getTemplatedKind() != clang::FunctionDecl::TemplatedKind::TK_NonTemplate;
}

void cheerp::CheerpSemaData::checkFunctionToBeJsExported(const clang::FunctionDecl* FD, bool isMethod)
{
	using namespace cheerp;
	using namespace clang;
	clang::Sema& sema = get_sema();

	if (isMethod)
	{
		//Method specific checks
		if (FD->hasAttr<AsmJSAttr>())
			sema.Diag(FD->getLocation(), diag::err_cheerp_incompatible_attributes) << FD->getAttr<AsmJSAttr>() << "method" << FD <<
					"[[cheerp::jsexport]]" << "method" << FD;

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


	if (isTemplate(FD))
		sema.Diag(FD->getLocation(), diag::err_cheerp_jsexport_on_function_template);

	if (!isa<CXXConstructorDecl>(FD))
		checkCouldReturnBeJsExported(FD->getReturnType(), FD, sema);

	checkParameters(FD, sema);

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

void cheerp::CheerpSemaClassData::checkRecord()
{
	using namespace std;
	using namespace clang;

	clang::Sema& sema = get_sema();

	assert(recordDecl->hasAttr<JsExportAttr>());

	bool shouldContinue = true;
	//Here all checks regarding internal feasibility of jsexporting a class/struct have to be performed
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

	std::vector<const CXXConstructorDecl*> JsExportedConstructors;
	std::unordered_map<std::string, const clang::CXXMethodDecl*> JsExportedMethodNames;
	std::unordered_map<std::string, const clang::CXXMethodDecl*> staticJsExportedMethodNames;

	staticJsExportedMethodNames.emplace("promise", nullptr);
	staticJsExportedMethodNames.emplace("valueOf", nullptr);
	bool isAnyNonStatic = false;

	for (auto method : interface.methods)
	{
		cheerpSema->checkFunctionToBeJsExported(method, /*isMethod*/true);

		if (const CXXConstructorDecl* constructor = clang::dyn_cast<CXXConstructorDecl>(method))
		{
			isAnyNonStatic = true;
			JsExportedConstructors.push_back(constructor);
			continue;
		}
		if (!method->isStatic())
		{
			isAnyNonStatic = true;
		}

		const auto& name = method->getNameInfo().getAsString();
		const auto pair = method->isStatic() ?
			staticJsExportedMethodNames.emplace(name, method) :
			JsExportedMethodNames.emplace(name, method);
		if (pair.second == false)
		{
			if (name == "promise" && method->isStatic())
				sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_promise_static_method);
			if (name == "valueOf" && method->isStatic())
				sema.Diag(method->getLocation(), diag::err_cheerp_jsexport_valueOf_static_method);
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

	if (!isAnyNonStatic)
	{
		sema.Diag(recordDecl->getLocation(), diag::err_cheerp_jsexport_only_static);
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
	declared_methods.insert(method);
}

bool cheerp::shouldBeJsExported(const clang::Decl *D, const bool isMethod)
{
	return D->hasAttr<clang::JsExportAttr>() && (clang::isa<clang::CXXMethodDecl>(D) == isMethod);
}

cheerp::CheerpAttributeToAdd cheerp::getCheerpAttributeToAdd(const clang::Decl* decl, clang::ASTContext& Context)
{
	// Inherit from context (possibly walking up in the tree until a DeclContext is tagged or no DeclContext exists)
	while (const clang::DeclContext* ctx = decl->getDeclContext())
	{
		decl = clang::cast<clang::Decl>(ctx);

		if (decl->hasAttr<clang::AsmJSAttr>())
			return CheerpAttributeToAdd::AsmJSLike;
		else if (decl->hasAttr<clang::GenericJSAttr>())
			return CheerpAttributeToAdd::GenericJS;
	}

	// Or set default attr based on Triple Environment component
	if (Context.getTargetInfo().getTriple().getEnvironment() == llvm::Triple::WebAssembly)
		return CheerpAttributeToAdd::AsmJSLike;
	else if (Context.getTargetInfo().getTriple().getEnvironment() == llvm::Triple::GenericJs)
		return CheerpAttributeToAdd::GenericJS;
	else
		return CheerpAttributeToAdd::None;

	assert(false);
}

void cheerp::checksOnAsmJSAttributeInjection(clang::Sema& sema, const clang::Decl* decl)
{
	using namespace clang;
	if (decl->hasAttr<JsExportAttr>() && isa<CXXRecordDecl>(decl))
		sema.Diag(decl->getBeginLoc(), diag::err_attributes_are_not_compatible)
			<< (sema.LangOpts.getCheerpLinearOutput() == LangOptions::CheerpLinearOutputTy::CHEERP_LINEAR_OUTPUT_AsmJs ? "'asmjs'" : "'wasm'")
			<< decl->getAttr<JsExportAttr>();
	else if (decl->hasAttr<PackedAttr>() && (sema.LangOpts.getCheerpLinearOutput() == LangOptions::CheerpLinearOutputTy::CHEERP_LINEAR_OUTPUT_AsmJs))
		sema.Diag(decl->getBeginLoc(), diag::err_attributes_are_not_compatible)
			<< "'asmjs'"
			<< decl->getAttr<PackedAttr>();
}

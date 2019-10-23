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

#include "clang/Sema/SemaCheerp.h"
#include "clang/Basic/PartialDiagnostic.h"
#include "clang/Sema/SemaInternal.h"

bool cheerp::couldBeJsExported(clang::CXXRecordDecl* Record, clang::Sema& sema)
{
	//class or struct
	using namespace clang;
	if (!Record->hasAttr<JsExportAttr>())
		return false;

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


	//TODO: Check for any public data or static member
	return true;
}

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
#include "clang/AST/DeclCXX.h"
#include "clang/Sema/Sema.h"

namespace cheerp{

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

bool couldBeJsExported(clang::CXXRecordDecl* Record, clang::Sema& sema);

bool couldBeJsExported(clang::CXXMethodDecl* Method, clang::Sema& sema);

}  //end namespace cheerp
#endif //_CHEERP_SEMA_CHEERP_H

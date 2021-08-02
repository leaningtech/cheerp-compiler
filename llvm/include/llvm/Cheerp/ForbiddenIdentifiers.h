//===-- Cheerp/ForbiddenIdentifiers.h - Cheerp for forbidden identifiers (in JavaScript) -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_FORBIDDEN_IDENTIFIERS_H
#define _CHEERP_FORBIDDEN_IDENTIFIERS_H

#include <algorithm>
#include "llvm/ADT/StringRef.h"

namespace cheerp
{

inline bool isForbiddenJSIdentifier(const llvm::StringRef& identifier)
{
	//From either: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords
	//or https://mathiasbynens.be/notes/javascript-identifiers#reserved-words
	const char* reserved_names[] = {
		"Infinity",
		"NaN",
		"abstract",
		"arguments",
		"await",
		"boolean",
		"break",
		"byte",
		"case",
		"catch",
		"char",
		"class",
		"const",
		"continue",
		"debugger",
		"default",
		"delete",
		"do",
		"double",
		"else",
		"enum",
		"eval",
		"export",
		"extends",
		"false",
		"final",
		"finally",
		"float",
		"for",
		"function",
		"goto",
		"if",
		"implements",
		"import",
		"in",
		"instanceof",
		"int",
		"interface",
		"let",
		"long",
		"native",
		"new",
		"null",
		"package",
		"private",
		"protected",
		"public",
		"return",
		"short",
		"static",
		"super",
		"switch",
		"synchronized",
		"this",
		"throw",
		"throws",
		"transient",
		"true",
		"try",
		"typeof",
		"undefined",
		"var",
		"void",
		"volatile",
		"while",
		"with",
		"yield",
	};
	return std::binary_search(reserved_names, reserved_names+(sizeof(reserved_names)/sizeof(const char*)), identifier);
}

}//namespace cheerp

#endif //_CHEERP_FORBIDDEN_IDENTIFIERS_H

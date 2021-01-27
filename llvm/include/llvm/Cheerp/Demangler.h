//===-- Cheerp/Demangler.h - Cheerp demangler utilities -------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2021 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_DEMANGLER_H
#define _CHEERP_DEMANGLER_H

#include <string>
#include <vector>
#include <cstring>
#include <cassert>
#include "llvm/Demangle/Demangle.h"

namespace cheerp
{

class Demangler
{
public:
	Demangler(const char* s)
	{
		if(strncmp(s,"_ZN",3)!=0)
			return;

		mangled = true;

		const size_t OriginalSize = 4;
		char *Buf = static_cast<char *>(std::malloc(OriginalSize));

		llvm::ItaniumPartialDemangler demangler;
		demangler.partialDemangle(s);
		size_t N = OriginalSize;

		char* res = NULL;
		if (demangler.isFunction())
		{
			function = true;
			res = demangler.getFunctionDeclContextName(Buf, &N);
			constructor = demangler.isCtorOrDtor();
		}
		else
		{
			res = demangler.finishDemangle(Buf, &N);
		}

		char * iter = res;
		std::string curr = "";

		while (*iter != 0)
		{
			curr += *iter;
			iter++;

			if (curr[curr.size()-2] == ':')
				if (curr[curr.size()-1] == ':')
				{
					scopes.push_back(std::string(curr.begin(), curr.end()-2));
					curr = "";
				}
		}

		assert(!curr.empty());

		scopes.push_back(curr);

		std::free(res);

		if (isFunction() && !isConstructor())
		{
			Buf = static_cast<char *>(std::malloc(OriginalSize));
			N = OriginalSize;
			res = demangler.getFunctionBaseName(Buf, &N);

			scopes.push_back(std::string(res));
			std::free(res);
		}

	}
	std::string getJSMangling(const bool doCleanup) const
	{
		std::string res = "";

		for (uint32_t i=(doCleanup && isNamespaceClient())?1:0; i<scopes.size(); i++)
		{
			if (doCleanup)
				res += cleanupTemplates(scopes[i]);
			else
				res += scopes[i];
			res += '.';
		}

		//remove last dot
		res.pop_back();

		return res;
	}
	bool isNamespaceClient() const
	{
		return scopes.front() == "client";
	}
	bool isMangled() const
	{
		return mangled;
	}
	bool isConstructor() const
	{
		assert(isMangled() && isFunction());
		return constructor;
	}
	bool isDestructor() const
	{
		assert(isMangled() && isFunction());
		return destructor;
	}
	bool isFunction() const
	{
		assert(isMangled());
		return function;
	}
	const std::vector<std::string>& getScopes() const
	{
		assert(isMangled());

		return scopes;
	}
	const std::string& getFunctionName() const
	{
		assert(isMangled() && isFunction());
		return scopes.back();
	}
private:
	std::string cleanupTemplates(const std::string& name) const
	{
		const char close = '>';
		const char open = '<';

		int balance = 0;
		uint32_t lastBalanced = name.size();

		for (int i=name.size()-1; i>=0; i--)
		{
			bool isSpecial = false;
			if (name[i] == close)
			{
				balance++;
				isSpecial = true;
			}
			else if (name[i] == open)
			{
				balance--;
				isSpecial = true;
			}

			if (isSpecial && balance == 0)
			{
				lastBalanced = i;
			}
		}

		return std::string(name, 0, lastBalanced);
	}
	std::vector<std::string> scopes;
	std::string functionName{""};
	bool function{false};
	bool constructor{false};
	bool destructor{false};
	bool mangled{false};
};


} //namespace cheerp

#endif //_CHEERP_DEMANGLER_H


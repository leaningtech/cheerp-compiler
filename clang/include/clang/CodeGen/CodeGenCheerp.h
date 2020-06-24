//===-- CodeGen/CodeGenCheerp.h - Cheerp CodeGen utilities -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_CODEGEN_CHEERP_H
#define _CHEERP_CODEGEN_CHEERP_H

#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Function.h"
#include "clang/AST/DeclCXX.h"

namespace cheerp{

class JsExportContext
{
public:
	explicit JsExportContext(llvm::Module& module, llvm::LLVMContext& context, llvm::IntegerType* type)
		: module(module), context(context), intType(type)
	{
	}
	void addFreeFunctionJsExportMetadata(llvm::Function* F);
	void addRecordJsExportMetadata(const clang::CXXMethodDecl *method, llvm::Function* F, const llvm::StringRef className);
private:
	llvm::Module& module;
	llvm::LLVMContext& context;
	llvm::IntegerType* intType;
};

}  //end namespace cheerp
#endif //_CHEERP_CODEGEN_CHEERP_H

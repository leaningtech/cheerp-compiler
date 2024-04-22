//===-- CodeGen/CodeGenCheerp.cpp - Cheerp CodeGen utilities -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2020-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "clang/CodeGen/CodeGenCheerp.h"
#include "llvm/Cheerp/JsExport.h"

using namespace llvm;
using namespace clang;

static void emitMetadata(CodeGen::CodeGenModule& CGM, StringRef name, ArrayRef<Metadata*> MDs)
{
	auto* node = CGM.getModule().getOrInsertNamedMetadata(name);
	node->addOperand(MDNode::get(CGM.getLLVMContext(), MDs));
}

static uint32_t getFunctionFlags(const FunctionDecl* FD)
{
	auto* CMD = dyn_cast<CXXMethodDecl>(FD);
	return CMD && CMD->isStatic() ? 1 : 0;
}

void cheerp::emitFunctionJsExportMetadata(CodeGen::CodeGenModule& CGM, const FunctionDecl* FD, Function* F)
{
	auto* flags = ConstantInt::get(CGM.Int32Ty, getFunctionFlags(FD));
	auto* funcMD = ConstantAsMetadata::get(F);
	auto* flagsMD = ConstantAsMetadata::get(flags);
	emitMetadata(CGM, "jsexport_functions", { funcMD, flagsMD });
}

void cheerp::emitRecordJsExportMetadata(CodeGen::CodeGenModule& CGM, const CXXRecordDecl* CRD)
{
	std::vector<Metadata*> bases;

	for (const auto& base : CRD->bases())
	{
		const auto* baseCRD = base.getType()->getAsCXXRecordDecl();

		if (baseCRD->hasAttr<JsExportAttr>())
		{
			auto* type = CGM.getTypes().ConvertType(CGM.getContext().getTypeDeclType(baseCRD));
			bases.push_back(MDString::get(CGM.getLLVMContext(), cast<StructType>(type)->getName()));
		}
	}

	auto* type = CGM.getTypes().ConvertType(CGM.getContext().getTypeDeclType(CRD));
	auto* nameMD = MDString::get(CGM.getLLVMContext(), cast<StructType>(type)->getName());
	auto* baseMD = MDTuple::get(CGM.getLLVMContext(), bases);
	emitMetadata(CGM, "jsexport_records", { nameMD, baseMD });
}

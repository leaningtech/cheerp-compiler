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
#include "llvm/Support/raw_ostream.h"
#include "CGCXXABI.h"

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

static uint32_t getRecordFlags(const CXXRecordDecl* CRD)
{
	return CRD->isAbstract() ? 1 : 0;
}

static MDString* getTypeString(CodeGen::CodeGenModule& CGM, QualType T)
{
	std::string name;
	raw_string_ostream os(name);
	ItaniumMangleContext& Mangler = cast<ItaniumMangleContext>(CGM.getCXXABI().getMangleContext());
	Mangler.mangleType(T, os);
	return MDString::get(CGM.getLLVMContext(), name);
}

static Attribute getTypeAttr(CodeGen::CodeGenModule& CGM, const CXXRecordDecl* CRD)
{
	auto* type = CGM.getTypes().ConvertType(CGM.getContext().getTypeDeclType(CRD));
	return Attribute::get(CGM.getLLVMContext(), Attribute::JsExportType, type);
}

void cheerp::emitFunctionJsExportMetadata(CodeGen::CodeGenModule& CGM, const FunctionDecl* FD, Function* F)
{
	std::vector<Metadata*> params;

	for (const auto* param : FD->parameters())
		params.push_back(getTypeString(CGM, param->getType()));

	auto* flags = ConstantInt::get(CGM.Int32Ty, getFunctionFlags(FD));
	auto* funcMD = ConstantAsMetadata::get(F);
	auto* flagsMD = ConstantAsMetadata::get(flags);
	auto* returnTypeMD = getTypeString(CGM, FD->getReturnType());
	auto* paramsMD = MDTuple::get(CGM.getLLVMContext(), params);
	emitMetadata(CGM, "jsexport_functions", { funcMD, flagsMD, returnTypeMD, paramsMD });
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
	auto* flags = ConstantInt::get(CGM.Int32Ty, getRecordFlags(CRD));
	auto* nameMD = MDString::get(CGM.getLLVMContext(), cast<StructType>(type)->getName());
	auto* baseMD = MDTuple::get(CGM.getLLVMContext(), bases);
	auto* flagsMD = ConstantAsMetadata::get(flags);
	emitMetadata(CGM, "jsexport_records", { nameMD, baseMD, flagsMD });
}

void cheerp::setFunctionJsExportAttributes(clang::CodeGen::CodeGenModule& CGM, const clang::FunctionDecl* FD, llvm::Function* F)
{
	std::size_t begin = 0;

	if (const auto* CMD = dyn_cast<CXXMethodDecl>(FD))
	{
		if (!CMD->isStatic())
		{
			const auto* CRD = CMD->getParent();

			if (CRD->hasAttr<JsExportAttr>())
				F->addParamAttr(0, getTypeAttr(CGM, CRD));

			begin += 1;
		}
	}

	for (std::size_t i = 0; i < FD->param_size(); i++)
	{
		const auto* CRD = FD->getParamDecl(i)->getType()->getPointeeCXXRecordDecl();

		if (CRD && CRD->hasAttr<JsExportAttr>())
			F->addParamAttr(begin + i, getTypeAttr(CGM, CRD));
	}

	const auto* CRD = FD->getReturnType()->getPointeeCXXRecordDecl();

	if (CRD && CRD->hasAttr<JsExportAttr>())
		F->addRetAttr(getTypeAttr(CGM, CRD));
}

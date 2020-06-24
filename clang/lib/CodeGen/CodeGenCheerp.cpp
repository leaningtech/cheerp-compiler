//===-- CodeGen/CodeGenCheerp.cpp - Cheerp CodeGen utilities -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "clang/CodeGen/CodeGenCheerp.h"
#include "llvm/Cheerp/JsExport.h"

void cheerp::JsExportContext::addFreeFunctionJsExportMetadata(llvm::Function* F)
{
       llvm::NamedMDNode* namedNode = module.getOrInsertNamedMetadata("jsexported_methods");
       llvm::SmallVector<llvm::Metadata*,1> values;
       values.push_back(llvm::ConstantAsMetadata::get(F));
       llvm::MDNode* node = llvm::MDNode::get(context,values);
       namedNode->addOperand(node);
}

void cheerp::JsExportContext::addRecordJsExportMetadata(const clang::CXXMethodDecl *method, llvm::Function* F, const llvm::StringRef className)
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

//===-- CodeGen/CodeGenCheerp.h - Cheerp CodeGen utilities -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2020-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_CODEGEN_CHEERP_H
#define _CHEERP_CODEGEN_CHEERP_H

#include "CodeGenModule.h"

namespace cheerp{

void emitFunctionJsExportMetadata(clang::CodeGen::CodeGenModule& CGM, const clang::FunctionDecl* FD, llvm::Function* F);
void emitRecordJsExportMetadata(clang::CodeGen::CodeGenModule& CGM, const clang::CXXRecordDecl* CRD);

}  //end namespace cheerp
#endif //_CHEERP_CODEGEN_CHEERP_H

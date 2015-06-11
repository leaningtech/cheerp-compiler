//===-- Cheerp/SourceMaps.h - Cheerp source maps generation code-----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_SOURCE_MAPS_H
#define _CHEERP_SOURCE_MAPS_H

#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Metadata.h"
#include <map>

namespace cheerp
{

class SourceMapGenerator
{
private:
	llvm::raw_ostream* sourceMap;
	llvm::LLVMContext& Ctx;
	std::map<llvm::MDString*, uint32_t> fileMap;
	uint32_t lastFile;
	uint32_t lastLine;
	uint32_t lastColoumn;
	bool validInfo;
	bool lineStart;
	void writeBase64VLQInt(int32_t i);
public:
	SourceMapGenerator(llvm::raw_ostream* s, llvm::LLVMContext& C);
	void setDebugLoc(const llvm::DebugLoc& debugLoc);
	void clearDebugInfo()
	{
		validInfo = false;
	}
	void beginFile();
	void finishLine();
	void endFile();
};

}
#endif

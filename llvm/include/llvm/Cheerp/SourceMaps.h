//===-- Cheerp/SourceMaps.h - Cheerp source maps generation code-----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_SOURCE_MAPS_H
#define _CHEERP_SOURCE_MAPS_H

#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/Support/ToolOutputFile.h"
#include <map>

namespace cheerp
{

class SourceMapGenerator
{
private:
	llvm::tool_output_file sourceMap;
	const std::string& sourceMapName;
	const std::string& sourceMapPrefix;
	llvm::LLVMContext& Ctx;
	std::map<llvm::StringRef, uint32_t> fileMap;
	std::map<llvm::StringRef, uint32_t> functionNameMap;
	uint32_t lastFile;
	uint32_t lastLine;
	uint32_t lastColumn;
	uint32_t lastOffset;
	uint32_t lineOffset;
	uint32_t lastName;
	const llvm::DebugLoc* currentDebugLoc;
	bool standAlone;
	bool lineBegin;
	void writeBase64VLQInt(int32_t i);
public:
	// sourceMapName and sourceMapPrefix life spans should be longer than the one of the SourceMapGenerator
	SourceMapGenerator(const std::string& sourceMapName, const std::string& sourceMapPrefix, bool standAlone, llvm::LLVMContext& C, std::error_code& ErrorCode);
	void setFunctionName(const llvm::DISubprogram &method);
	void setDebugLoc(const llvm::DebugLoc* debugLoc);
	const llvm::DebugLoc* getDebugLoc() const
	{
		return currentDebugLoc;
	}
	void beginFile();
	void finishLine();
	// TODO: It's not clear if the line offset in encoded in bytes or charathers
	void addLineOffset(uint32_t o) { lineOffset+=o; }
	void endFile();
	std::string getSourceMapName() const;
};

}
#endif

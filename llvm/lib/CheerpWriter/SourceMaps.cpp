//===-- SourceMaps.cpp - The Cheerp JavaScript generator ------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2015 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/SourceMaps.h"
#include "llvm/IR/Metadata.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

using namespace llvm;

namespace cheerp
{

SourceMapGenerator::SourceMapGenerator(const std::string& sourceMapName, const std::string& sourceMapPrefix, llvm::LLVMContext& C, std::error_code& ErrorCode):
	sourceMap(sourceMapName.c_str(), ErrorCode, sys::fs::F_None), sourceMapName(sourceMapName), sourceMapPrefix(sourceMapPrefix),
	Ctx(C), lastFile(0), lastLine(0), lastColoumn(0), lastOffset(0), lineOffset(0)
{
}

static char base64Chars[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

void SourceMapGenerator::writeBase64VLQInt(int i)
{
	// The sign is encoded as the least significant bit
	if (i < 0)
		i = ((-i) << 1) | 1;
	else
		i = i << 1;
	do
	{
		// 5 bit of data, 1 of continuation
		int base64Char = i & 0x1f;
		i >>= 5;
		if(i)
			base64Char |= 0x20;
		sourceMap.os() << base64Chars[base64Char];
	}
	while(i);
}

void SourceMapGenerator::setDebugLoc(const llvm::DebugLoc& debugLoc)
{
	MDNode* file = debugLoc.getScope(Ctx);
	assert(file->getNumOperands()>=2);
	MDNode* fileNamePath = cast<MDNode>(file->getOperand(1));
	assert(fileNamePath->getNumOperands()==2);
	MDString* fileNameString = cast<MDString>(fileNamePath->getOperand(0));

	auto fileMapIt = fileMap.find(fileNameString);
	if (fileMapIt == fileMap.end())
		fileMapIt = fileMap.insert(std::make_pair(fileNameString, fileMap.size())).first;
	uint32_t currentFile = fileMapIt->second;
	uint32_t currentLine = debugLoc.getLine() - 1;
	uint32_t currentColoumn = debugLoc.getCol() - 1;
	if(lastOffset != 0)
		sourceMap.os() << ',';
	// Starting coloumn in the generated code
	writeBase64VLQInt(lineOffset - lastOffset);
	// Other fields are encoded as difference from the previous one in the file
	// We can use the last value directly because it is initialized as 0
	// File index
	writeBase64VLQInt(currentFile - lastFile);
	// Line index
	writeBase64VLQInt(currentLine - lastLine);
	// Coloumn index
	writeBase64VLQInt(currentColoumn - lastColoumn);
	lastFile = currentFile;
	lastLine = currentLine;
	lastColoumn = currentColoumn;
	lastOffset = lineOffset;
}

void SourceMapGenerator::beginFile()
{
	// Output the prologue of the file
	sourceMap.os() << "{\n";
	sourceMap.os() << "\"version\": 3,\n";
	sourceMap.os() << "\"names\": [],\n";
	sourceMap.os() << "\"mappings\": \"";
}

void SourceMapGenerator::finishLine()
{
	sourceMap.os() << ";";
	lastOffset = 0;
	lineOffset = 0;
}

void SourceMapGenerator::endFile()
{
	// Output the prologue of the file
	sourceMap.os() << "\",\n";
	// Output file names
	SmallVector<MDString*, 10> files(fileMap.size(), NULL);
	for(auto mapItem: fileMap)
		files[mapItem.second] = mapItem.first;
	sourceMap.os() << "\"sources\": [";
	for(uint32_t i=0;i<files.size();i++)
	{
		if(i!=0)
			sourceMap.os() << ',';
		// Fix slashes in the file path
		std::string tmp;
		StringRef string=files[i]->getString();
		unsigned start=0;
		if(string.startswith(sourceMapPrefix))
			start=sourceMapPrefix.size();
		tmp.reserve(string.size());
		for(unsigned i=start;i<string.size();i++)
		{
			char c=string[i];
			if(c=='\\')
				c='/';
			tmp.push_back(c);
		}
		sourceMap.os() << '"' << tmp << '"';
	}
	sourceMap.os() << "]\n";
	sourceMap.os() << "}\n";
	sourceMap.keep();
}

std::string SourceMapGenerator::getSourceMapName() const
{
	return llvm::sys::path::filename(sourceMapName);
}

}

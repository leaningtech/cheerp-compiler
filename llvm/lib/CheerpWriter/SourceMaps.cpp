//===-- SourceMaps.cpp - The Cheerp JavaScript generator ------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/SourceMaps.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/IR/Metadata.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Path.h"

using namespace llvm;

namespace cheerp
{

SourceMapGenerator::SourceMapGenerator(const std::string& sourceMapName, const std::string& sourceMapPrefix, bool standAlone, std::error_code& ErrorCode):
	sourceMap(sourceMapName.c_str(), ErrorCode, sys::fs::F_None), sourceMapName(sourceMapName), sourceMapPrefix(sourceMapPrefix),
	lastFile(0), lastLine(0), lastColumn(0), lastOffset(0), lineOffset(0), lastName(0), currentDebugLoc(nullptr), standAlone(standAlone), lineBegin(true)
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

void SourceMapGenerator::setFunctionName(const llvm::DISubprogram *method) {
	StringRef fileName = method->getFilename();
	unsigned lineNumber = method->getLine();
	StringRef functionName = method->getLinkageName();
	if (functionName.empty())
		functionName = method->getName();

	auto fileMapIt = fileMap.find(fileName);
	if (fileMapIt == fileMap.end()) {
		auto pair = std::make_pair(fileName, fileMap.size());
		fileMapIt = fileMap.insert(pair).first;
	}

	auto functionNameMapIt = functionNameMap.find(functionName);
	if (functionNameMapIt == functionNameMap.end()) {
		auto pair = std::make_pair(functionName, functionNameMap.size());
		functionNameMapIt = functionNameMap.insert(pair).first;
	}

	uint32_t currentFile = fileMapIt->second;
	uint32_t currentLine = lineNumber - 1;
	uint32_t currentName = functionNameMapIt->second;
	uint32_t currentColumn = 0;

	if(!lineBegin)
		sourceMap.os() << ',';
	lineBegin = false;

	// Starting column in the generated code
	writeBase64VLQInt(lineOffset - lastOffset);
	// Other fields are encoded as difference from the previous one in the file
	// We can use the last value directly because it is initialized as 0
	// File index
	writeBase64VLQInt(currentFile - lastFile);
	// Line index
	writeBase64VLQInt(currentLine - lastLine);
	// Column index
	writeBase64VLQInt(currentColumn - lastColumn);
	// Name index
	writeBase64VLQInt(currentName - lastName);
	lastFile = currentFile;
	lastLine = currentLine;
	lastColumn = currentColumn;
	lastOffset = lineOffset;
	lastName = currentName;
}

void SourceMapGenerator::setDebugLoc(const llvm::DebugLoc* debugLoc)
{
	currentDebugLoc = debugLoc;
	if(debugLoc == nullptr)
		return;
	MDNode* file = debugLoc->getScope();
	assert(file->getNumOperands()>=2);
	DIScope* fileNamePath = cast<DIScope>(file->getOperand(1));
	StringRef fileName = fileNamePath->getFilename();

	auto fileMapIt = fileMap.find(fileName);
	if (fileMapIt == fileMap.end())
		fileMapIt = fileMap.insert(std::make_pair(fileName, fileMap.size())).first;
	uint32_t currentFile = fileMapIt->second;
	uint32_t currentLine = debugLoc->getLine() - 1;
	uint32_t currentColumn = debugLoc->getCol() - 1;
	if(!lineBegin)
		sourceMap.os() << ',';
	lineBegin = false;
	// Starting column in the generated code
	writeBase64VLQInt(lineOffset - lastOffset);
	// Other fields are encoded as difference from the previous one in the file
	// We can use the last value directly because it is initialized as 0
	// File index
	writeBase64VLQInt(currentFile - lastFile);
	// Line index
	writeBase64VLQInt(currentLine - lastLine);
	// Column index
	writeBase64VLQInt(currentColumn - lastColumn);
	lastFile = currentFile;
	lastLine = currentLine;
	lastColumn = currentColumn;
	lastOffset = lineOffset;
}

void SourceMapGenerator::beginFile()
{
	// Output the prologue of the file
	sourceMap.os() << "{\n";
	sourceMap.os() << "\"version\": 3,\n";
	sourceMap.os() << "\"mappings\": \"";
}

void SourceMapGenerator::finishLine()
{
	sourceMap.os() << ";";
	lastOffset = 0;
	lineOffset = 0;
	lineBegin = true;
	// Repeat the last known debugLoc, if any
	if(currentDebugLoc)
		setDebugLoc(currentDebugLoc);
}

void SourceMapGenerator::endFile()
{
	// Output the prologue of the file
	sourceMap.os() << "\",\n";
	// Output file names
	SmallVector<StringRef, 10> files(fileMap.size());
	for(auto mapItem: fileMap)
		files[mapItem.second] = mapItem.first;
	sourceMap.os() << "\"sources\": [";
	for(uint32_t i=0;i<files.size();i++)
	{
		if(i!=0)
			sourceMap.os() << ',';
		// Fix slashes in the file path
		std::string tmp;
		StringRef string = files[i];
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
	sourceMap.os() << "],\n";
	// Output the contents of source files, if required
	sourceMap.os() << "\"sourcesContent\": [";
	for(uint32_t i=0;i<files.size();i++)
	{
		if(i!=0)
			sourceMap.os() << ',';
		if(files[i][0] != '/' && !standAlone)
		{
			sourceMap.os() << "null";
			continue;
		}
		llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> Buf = llvm::MemoryBuffer::getFile(files[i], -1, false);
		std::error_code EC = Buf.getError();
		if (EC)
		{
			sourceMap.os() << "null";
			llvm::errs() << "warning: Could not open source file " << files[i] << "\n";
			continue;
		}
		sourceMap.os() << '"';
		CheerpWriter::compileEscapedString(sourceMap.os(), Buf.get()->getBuffer(), /*forJSON*/true);
		sourceMap.os() << '"';
	}
	sourceMap.os() << "],\n";
	// Output the symbol names
	SmallVector<StringRef, 10> functions(functionNameMap.size());
	for(auto mapItem: functionNameMap)
		functions[mapItem.second] = mapItem.first;
	sourceMap.os() << "\"names\": [";
	for(uint32_t i=0; i < functions.size(); i++)
	{
		if (i != 0)
			sourceMap.os() << ',';
		// Add an underscore to the function name to match the generated symbol
		// names in the JavaScript file.
		sourceMap.os() << '"' << functions[i] << '"';
	}
	sourceMap.os() << "]\n";
	sourceMap.os() << "}\n";
	sourceMap.keep();
}

std::string SourceMapGenerator::getSourceMapName() const
{
	return std::string(llvm::sys::path::filename(sourceMapName));
}

}

//===-- CheerpWasmWriter.cpp - The Cheerp JavaScript generator ------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <algorithm>
#include <limits>

#include "Relooper.h"
#include "CFGStackifier.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Cheerp/BuiltinInstructions.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/PHIHandler.h"
#include "llvm/Cheerp/NameGenerator.h"
#include "llvm/Cheerp/WasmWriter.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/LEB128.h"

using namespace cheerp;
using namespace llvm;
using namespace std;

//#define WASM_DUMP_SECTIONS 1
//#define WASM_DUMP_SECTION_DATA 1
//#define WASM_DUMP_METHODS 1
//#define WASM_DUMP_METHOD_DATA 1
//#define STRESS_DEFERRED 1

static uint32_t COMPILE_METHOD_LIMIT = 100000;

enum BLOCK_TYPE { WHILE1 = 0, DO, SWITCH, CASE, LABEL_FOR_SWITCH, IF, LOOP };

class BlockType
{
public:
	BLOCK_TYPE type;
	uint32_t depth;
	int32_t label;

	BlockType(BLOCK_TYPE bt, uint32_t depth = 0, int32_t label = 0)
	 :

		type(bt),
		depth(depth),
		label(label)
	{}
};

BlockType* findSwitchBlockType(std::vector<BlockType>& blocks)
{
	for (size_t i = blocks.size(); i;)
	{
		BlockType* block = &blocks[--i];
		if (block->type == SWITCH)
			return block;
	}

	llvm_unreachable("switch render block not found");
}

static inline void encodeF32(float f, WasmBuffer& stream)
{
	stream.write(reinterpret_cast<const char*>(&f), sizeof(float));
}

static inline void encodeF64(double f, WasmBuffer& stream)
{
	stream.write(reinterpret_cast<const char*>(&f), sizeof(double));
}

static inline void encodeRegisterKind(Registerize::REGISTER_KIND regKind, WasmBuffer& stream)
{
	switch(regKind)
	{
		case Registerize::DOUBLE:
			encodeULEB128(0x7c, stream);
			break;
		case Registerize::FLOAT:
			encodeULEB128(0x7d, stream);
			break;
		case Registerize::INTEGER:
			encodeULEB128(0x7f, stream);
			break;
		case Registerize::INTEGER64:
			encodeULEB128(0x7e, stream);
			break;
		case Registerize::OBJECT:
			encodeULEB128(0x6f, stream);
			break;
	}
}

static uint32_t getValType(const Type* t)
{
	if (t->isIntegerTy(64))
		return 0x7e;
	else if (t->isIntegerTy() || TypeSupport::isRawPointer(t, true))
		return 0x7f;
	else if (t->isFloatTy())
		return 0x7d;
	else if (t->isDoubleTy())
		return 0x7c;
	else if (t->isPointerTy())
		return 0x6f;
	else
	{
#ifndef NDEBUG
		llvm::errs() << "Unsupported type ";
		t->dump();
#endif
		llvm_unreachable("Unsuppored type");
	}
}

static void encodeValType(const Type* t, WasmBuffer& stream)
{
	encodeULEB128(getValType(t), stream);
}

static void encodeLiteralType(const Type* t, WasmBuffer& stream)
{
	if (t->isIntegerTy(64))
		encodeULEB128(0x42, stream);
	else if (t->isIntegerTy() || TypeSupport::isRawPointer(t, true))
		encodeULEB128(0x41, stream);
	else if(t->isFloatTy())
		encodeULEB128(0x43, stream);
	else if(t->isDoubleTy())
		encodeULEB128(0x44, stream);
	else
	{
#ifndef NDEBUG
		llvm::errs() << "Unsupported type: ";
		t->dump();
#endif
		llvm_unreachable("Unsuppored type");
	}
}

std::string string_to_hex(StringRef input)
{
	static const char* const lut = "0123456789abcdef";
	size_t len = input.size();

	std::string output;
	output.reserve(2 * len);
	for (size_t i = 0; i < len; ++i)
	{
		const unsigned char c = input[i];
		output.push_back(lut[c >> 4]);
		output.push_back(lut[c & 15]);
		if ((i & 1) == 1 && (i + 1) < len)
			output.push_back(' ');
	}
	return output;
}

Section::Section(uint32_t sectionId, const char* sectionName, CheerpWasmWriter* writer)
	: writer(writer)
{
	encodeULEB128(sectionId, writer->stream);

	// Custom sections have a section name.
	if (!sectionId) {
		encodeULEB128(strlen(sectionName), *this);
		(*this) << sectionName;
	}
}
Section::~Section()
{
#if WASM_DUMP_SECTIONS
	uint64_t start = writer->stream.tell();
	fprintf(stderr, "%10s id=0x%x start=0x%08lx end=0x%08lx size=0x%08lx\n",
			sectionName, sectionId, start, start + tell(), tell());
#if WASM_DUMP_SECTION_DATA
	llvm::errs() << "section: " << string_to_hex(str()) << '\n';
#endif
#endif

	encodeULEB128(tell(), writer->stream);
	writer->stream << str();
}

enum ConditionRenderMode {
	NormalCondition = 0,
	InvertCondition
};

class CheerpWasmRenderInterface: public RenderInterface
{
private:
	CheerpWasmWriter* writer;
	WasmBuffer& code;
	std::vector<BlockType> blockTypes;
	uint32_t labelLocal;
	void renderCondition(const BasicBlock* B, const std::vector<int>& branchIds,
			ConditionRenderMode mode);
	int findIndexFromLabel(int labelId);
	int findClosestBlockIndex();
public:
	const BasicBlock* lastDepth0Block;
	CheerpWasmRenderInterface(CheerpWasmWriter* w, WasmBuffer& code, uint32_t labelLocal)
	 :
		writer(w),
		code(code),
		labelLocal(labelLocal),
		lastDepth0Block(nullptr)
	{ }
	void renderBlock(const BasicBlock* BB);
	void renderLabelForSwitch(int labelId);
	void renderSwitchOnLabel(IdShapeMap& idShapeMap);
	void renderCaseOnLabel(int labelId);
	void renderSwitchBlockBegin(const SwitchInst* switchInst, BlockBranchMap& branchesOut);
	void renderCaseBlockBegin(const BasicBlock* caseBlock, int branchId);
	void renderDefaultBlockBegin(bool empty = false);
	void renderIfBlockBegin(const BasicBlock* condBlock, int branchId, bool first, int labelId = 0);
	void renderIfBlockBegin(const BasicBlock* condBlock, const vector<int>& branchId, bool first, int labelId = 0);
	void renderElseBlockBegin();
	void renderIfBlockEnd(bool labelled = false);
	void renderBlockEnd(bool empty = false);
	void renderBlockPrologue(const BasicBlock* blockTo, const BasicBlock* blockFrom);
	void renderWhileBlockBegin();
	void renderWhileBlockBegin(int labelId);
	void renderDoBlockBegin();
	void renderDoBlockBegin(int labelId);
	void renderDoBlockEnd();
	void renderBlockBegin(int labelId);
	void renderBreak();
	void renderBreak(int labelId);
	void renderContinue();
	void renderContinue(int labelId);
	void renderLabel(int labelId);
	void renderIfOnLabel(int labelId, bool first);
};

void CheerpWasmRenderInterface::renderBlock(const BasicBlock* bb)
{
	if (blockTypes.empty())
		lastDepth0Block = bb;
	else
		lastDepth0Block = nullptr;

	writer->compileBB(code, *bb);

	if (!lastDepth0Block && isa<ReturnInst>(bb->getTerminator()))
	{
		writer->encodeInst(WasmOpcode::RETURN, code);
	}
}

void CheerpWasmRenderInterface::renderCondition(const BasicBlock* bb,
		const std::vector<int>& branchIds,
		ConditionRenderMode mode)
{
	assert(!branchIds.empty());
	const Instruction* term=bb->getTerminator();

	if(isa<BranchInst>(term))
	{
		assert(branchIds.size() == 1);
		int branchId = branchIds[0];
		(void)branchId;
		const BranchInst* bi=cast<BranchInst>(term);
		assert(bi->isConditional());
		//The second branch is the default
		assert(branchId==0);

		const Value* cond = bi->getCondition();
		bool canInvertCond = isa<Instruction>(cond) && writer->isInlineable(*cast<Instruction>(cond));

		if(canInvertCond && isa<ICmpInst>(cond))
		{
			const ICmpInst* ci = cast<ICmpInst>(cond);
			CmpInst::Predicate p = ci->getPredicate();
			if(mode == InvertCondition)
				p = CmpInst::getInversePredicate(p);
			// Optimize "if (a != 0)" to "if (a)" and "if (a == 0)" to "if (!a)".
			if ((p == CmpInst::ICMP_NE || p == CmpInst::ICMP_EQ) &&
					isa<Constant>(ci->getOperand(1)) &&
					cast<Constant>(ci->getOperand(1))->isNullValue())
			{
				if(ci->getOperand(0)->getType()->isPointerTy())
					writer->compileOperand(code, ci->getOperand(0));
				else if(ci->getOperand(0)->getType()->isIntegerTy(32))
					writer->compileSignedInteger(code, ci->getOperand(0), /*forComparison*/true);
				else
					writer->compileUnsignedInteger(code, ci->getOperand(0));
				if(p == CmpInst::ICMP_EQ)
					writer->encodeInst(WasmOpcode::I32_EQZ, code);
				return;
			}
			writer->compileICmp(*ci, p, code);
		}
		else if(canInvertCond && isa<FCmpInst>(cond))
		{
			const CmpInst* ci = cast<CmpInst>(cond);
			CmpInst::Predicate p = ci->getPredicate();
			if(mode == InvertCondition)
				p = CmpInst::getInversePredicate(p);
			writer->compileFCmp(ci->getOperand(0), ci->getOperand(1), p, code);
		}
		else
		{
			writer->compileOperand(code, bi->getCondition());
			if (mode == InvertCondition) {
				// Invert result
				writer->encodeInst(WasmOpcode::I32_EQZ, code);
			}
		}
	}
	else if(isa<SwitchInst>(term))
	{
		const SwitchInst* si=cast<SwitchInst>(term);
		bool first = true;
		for (int branchId: branchIds)
		{
			SwitchInst::ConstCaseIt it=si->case_begin();
			for(int i=1;i<branchId;i++)
				++it;
			const BasicBlock* dest=it->getCaseSuccessor();
			writer->compileOperand(code, si->getCondition());
			writer->compileOperand(code, it->getCaseValue());
			if(mode == InvertCondition)
				writer->encodeInst(WasmOpcode::I32_NE, code);
			else
				writer->encodeInst(WasmOpcode::I32_EQ, code);
			//We found the destination, there may be more cases for the same
			//destination though
			for(++it;it!=si->case_end();++it)
			{
				if(it->getCaseSuccessor()==dest)
				{
					//Also add this condition
					writer->compileOperand(code, si->getCondition());
					writer->compileOperand(code, it->getCaseValue());
					if(mode == InvertCondition)
					{
						writer->encodeInst(WasmOpcode::I32_NE, code);
						writer->encodeInst(WasmOpcode::I32_AND, code);
					}
					else
					{
						writer->encodeInst(WasmOpcode::I32_EQ, code);
						writer->encodeInst(WasmOpcode::I32_OR, code);
					}
				}
			}
			if (!first)
			{
				if(mode == InvertCondition)
					writer->encodeInst(WasmOpcode::I32_AND, code);
				else
					writer->encodeInst(WasmOpcode::I32_OR, code);
			}
			first = false;
		}
	}
	else
	{
#ifndef NDEBUG
		term->dump();
#endif
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}
}

void CheerpWasmRenderInterface::renderLabelForSwitch(int labelId)
{
	writer->encodeInst(WasmU32Opcode::BLOCK, 0x40, code);
	blockTypes.emplace_back(LABEL_FOR_SWITCH, 1, labelId);
}

void CheerpWasmRenderInterface::renderSwitchOnLabel(IdShapeMap& idShapeMap)
{
	int64_t max = std::numeric_limits<int64_t>::min();
	int64_t min = std::numeric_limits<int64_t>::max();
	for (auto iter = idShapeMap.begin(); iter != idShapeMap.end(); iter++)
	{
		int64_t curr = iter->first;
		max = std::max(max, curr);
		min = std::min(min, curr);
	}

	// There should be at least one case.
	uint32_t depth = max - min + 1;
	assert(depth >= 1);

	// Fill the jump table. By default, jump to the first block. This block
	// will do nothing.
	std::vector<uint32_t> table;
	table.assign(depth, 0);
	uint32_t blockIndex = 1;

	for (auto iter = idShapeMap.begin(); iter != idShapeMap.end(); iter++) {
		table.at(iter->first - min) = blockIndex;
		blockIndex++;
	}

	for (uint32_t i = 0; i < idShapeMap.size() + 1; i++)
		writer->encodeInst(WasmU32Opcode::BLOCK, 0x40, code);

	// Wrap the br_table instruction in its own block
	writer->encodeInst(WasmU32Opcode::BLOCK, 0x40, code);
	writer->encodeInst(WasmU32Opcode::GET_LOCAL, labelLocal, code);
	if (min != 0)
	{
		writer->encodeInst(WasmS32Opcode::I32_CONST, min, code);
		writer->encodeInst(WasmOpcode::I32_SUB, code);
	}

	writer->encodeBranchTable(code, table, 0);

	writer->encodeInst(WasmOpcode::END, code);

	// The first block does not do anything, and breaks out of the switch.
	writer->encodeInst(WasmU32Opcode::BR, idShapeMap.size(), code);
	writer->encodeInst(WasmOpcode::END, code);

	blockTypes.emplace_back(SWITCH, 0);
	blockTypes.emplace_back(CASE, idShapeMap.size());
}

void CheerpWasmRenderInterface::renderCaseOnLabel(int)
{
	BlockType prevBlock = blockTypes.back();
	(void)prevBlock;
	assert(prevBlock.type == CASE && prevBlock.depth > 0);
}

uint32_t findBlockInBranchesOutMap(const BasicBlock* dest, BlockBranchMap& branchesOut)
{
	int i = 0;
	for (auto it = branchesOut.begin(); it != branchesOut.end(); ++it) {
		if (it->first->llvmBlock == dest)
			return i;
		// Do not count the default block. The default block will be rendered
		// at the end by relooper.
		if (it->second->branchId == -1)
			continue;
		i++;
	}

	llvm_unreachable("destination not found in branches out");
}

void CheerpWasmRenderInterface::renderSwitchBlockBegin(const SwitchInst* si, BlockBranchMap& branchesOut)
{
	assert(si->getNumCases());

	uint32_t bitWidth = si->getCondition()->getType()->getIntegerBitWidth();

	auto getCaseValue = [](const ConstantInt* c, uint32_t bitWidth) -> int64_t
	{
		return bitWidth == 32 ? c->getSExtValue() : c->getZExtValue();
	};

	llvm::BasicBlock* defaultDest = si->getDefaultDest();
	int64_t max = std::numeric_limits<int64_t>::min();
	int64_t min = std::numeric_limits<int64_t>::max();
	for (auto& c: si->cases())
	{
		if (c.getCaseSuccessor() == defaultDest)
			continue;
		int64_t curr = getCaseValue(c.getCaseValue(), bitWidth);
		max = std::max(max, curr);
		min = std::min(min, curr);
	}

	// There should be at least one default case and zero or more cases.
	uint32_t depth = max - min + 1;
	assert(depth >= 1);

	// Fill the jump table.
	std::vector<uint32_t> table;
	table.assign(depth, numeric_limits<uint32_t>::max());

	std::unordered_map<const llvm::BasicBlock*, uint32_t> blockIndexMap;
	uint32_t caseBlocks = 0;

	auto it = si->case_begin();
	auto itE = si->case_begin();
	for (;it != itE; ++it)
	{
		const BasicBlock* dest = it->getCaseSuccessor();
		if (dest == defaultDest)
			continue;
		const auto& found = blockIndexMap.find(dest);

		if (found == blockIndexMap.end())
		{
			// Use the block index from the Relooper branches list. Otherwise,
			// it is possible that the Relooper branches list does not match
			// with the order of the LLVM Basic Blocks.
			uint32_t blockIndex = findBlockInBranchesOutMap(dest, branchesOut);
			blockIndexMap.emplace(dest, blockIndex);
			table.at(getCaseValue(it->getCaseValue(), bitWidth) - min) = blockIndex;
			assert(blockIndex != numeric_limits<uint32_t>::max());

			// Add cases that have the same destination
			auto it_next = it;
			for (++it_next; it_next != si->case_end(); ++it_next)
			{
				if (it_next->getCaseSuccessor() != dest)
					continue;

				table.at(getCaseValue(it_next->getCaseValue(), bitWidth) - min) = blockIndex;
			}

			caseBlocks++;
		}
	}

	// Elements that are not set, will jump to the default block.
	std::replace(table.begin(), table.end(), numeric_limits<uint32_t>::max(), caseBlocks);

	// Print the case blocks and the default block.
	for (uint32_t i = 0; i < caseBlocks + 1; i++)
		writer->encodeInst(WasmU32Opcode::BLOCK, 0x40, code);

	// Wrap the br_table instruction in its own block.
	writer->encodeInst(WasmU32Opcode::BLOCK, 0x40, code);
	writer->compileOperand(code, si->getCondition());
	if (min != 0)
	{
		writer->encodeInst(WasmS32Opcode::I32_CONST, min, code);
		writer->encodeInst(WasmOpcode::I32_SUB, code);
	}
	if (bitWidth != 32 && CheerpWriter::needsUnsignedTruncation(si->getCondition(), /*asmjs*/true))
	{
		assert(bitWidth < 32);
		writer->encodeInst(WasmS32Opcode::I32_CONST, getMaskForBitWidth(bitWidth), code);
		writer->encodeInst(WasmOpcode::I32_AND, code);
	}

	// Print the case labels and the default label.
	writer->encodeBranchTable(code, table, caseBlocks);

	writer->encodeInst(WasmOpcode::END, code);

	blockTypes.emplace_back(SWITCH, 0);
	blockTypes.emplace_back(CASE, caseBlocks + 1);
}

void CheerpWasmRenderInterface::renderCaseBlockBegin(const BasicBlock*, int branchId)
{
	BlockType prevBlock = blockTypes.back();
	(void)prevBlock;
	assert(prevBlock.type == CASE && prevBlock.depth > 0);
}

void CheerpWasmRenderInterface::renderDefaultBlockBegin(bool)
{
	renderCaseBlockBegin(nullptr, 0);
}

void CheerpWasmRenderInterface::renderIfBlockBegin(const BasicBlock* bb, int branchId, bool first, int labelId)
{
	if(!first)
	{
		writer->encodeInst(WasmOpcode::ELSE, code);
	}
	// The condition goes first
	renderCondition(bb, {branchId}, NormalCondition);
	writer->encodeInst(WasmU32Opcode::IF, 0x40, code);
	if(first)
	{
		blockTypes.emplace_back(IF, 1, labelId);
	}
	else
	{
		assert(blockTypes.back().type == IF);
		blockTypes.back().depth += 1;
	}
}

void CheerpWasmRenderInterface::renderIfBlockBegin(const BasicBlock* bb, const std::vector<int>& skipBranchIds, bool first, int labelId)
{
	if(!first)
	{
		writer->encodeInst(WasmOpcode::ELSE, code);
	}
	// The condition goes first
	renderCondition(bb, skipBranchIds, InvertCondition);
	writer->encodeInst(WasmU32Opcode::IF, 0x40, code);

	if(first)
	{
		blockTypes.emplace_back(IF, 1, labelId);
	}
	else
	{
		assert(blockTypes.back().type == IF);
		blockTypes.back().depth += 1;
	}
}

void CheerpWasmRenderInterface::renderElseBlockBegin()
{
	assert(!blockTypes.empty());
	assert(blockTypes.back().type == IF);

	writer->encodeInst(WasmOpcode::ELSE, code);
}

void CheerpWasmRenderInterface::renderIfBlockEnd(bool)
{
	assert(!blockTypes.empty());
	BlockType block = blockTypes.back();
	blockTypes.pop_back();
	assert(block.type == IF);

	for(uint32_t i = 0; i < block.depth; i++)
	{
		writer->encodeInst(WasmOpcode::END, code);
	}
}

void CheerpWasmRenderInterface::renderBlockEnd(bool)
{
	assert(!blockTypes.empty());
	BlockType block = blockTypes.back();
	blockTypes.pop_back();

	if(block.type == WHILE1)
	{
		writer->encodeInst(WasmU32Opcode::BR, 1, code);
		writer->encodeInst(WasmOpcode::END, code);
		writer->encodeInst(WasmOpcode::END, code);
	}
	else if (block.type == CASE)
	{
		if (--block.depth > 0)
			blockTypes.push_back(block);
		writer->encodeInst(WasmOpcode::END, code);
	}
	else if(block.type == IF || block.type == DO)
	{
		for(uint32_t i = 0; i < block.depth; i++)
		{
			writer->encodeInst(WasmOpcode::END, code);
		}
	}
	else if (block.type == SWITCH)
	{
		assert(block.depth == 0);
		if (!blockTypes.empty() && blockTypes.back().type == LABEL_FOR_SWITCH) {
			blockTypes.pop_back();
			writer->encodeInst(WasmOpcode::END, code);
		}
	}
	else
	{
		assert(false);
	}
}

void CheerpWasmRenderInterface::renderBlockPrologue(const BasicBlock* bbTo, const BasicBlock* bbFrom)
{
	writer->compilePHIOfBlockFromOtherBlock(code, bbTo, bbFrom);
}

void CheerpWasmRenderInterface::renderWhileBlockBegin()
{
	// Wrap a block in a loop so that:
	// br 1 -> break
	// br 2 -> continue
	writer->encodeInst(WasmU32Opcode::LOOP, 0x40, code);
	writer->encodeInst(WasmU32Opcode::BLOCK, 0x40, code);

	blockTypes.emplace_back(WHILE1, 2);
}

void CheerpWasmRenderInterface::renderWhileBlockBegin(int blockLabel)
{
	// Wrap a block in a loop so that:
	// br 1 -> break
	// br 2 -> continue

	writer->encodeInst(WasmU32Opcode::LOOP, 0x40, code);

	writer->encodeInst(WasmU32Opcode::BLOCK, 0x40, code);

	blockTypes.emplace_back(WHILE1, 2, blockLabel);
}

void CheerpWasmRenderInterface::renderDoBlockBegin()
{
	writer->encodeInst(WasmU32Opcode::BLOCK, 0x40, code);
	blockTypes.emplace_back(DO, 1);
}

void CheerpWasmRenderInterface::renderDoBlockBegin(int blockLabel)
{
	writer->encodeInst(WasmU32Opcode::BLOCK, 0x40, code);

	blockTypes.emplace_back(DO, 1, blockLabel);
}

void CheerpWasmRenderInterface::renderDoBlockEnd()
{
	assert(!blockTypes.empty());
	assert(blockTypes.back().type == DO);
	blockTypes.pop_back();

	writer->encodeInst(WasmOpcode::END, code);
}

void CheerpWasmRenderInterface::renderBlockBegin(int labelId)
{
	renderDoBlockBegin(labelId);
}

int CheerpWasmRenderInterface::findClosestBlockIndex()
{
	uint32_t breakIndex = 0;
	for (uint32_t i = 0; i < blockTypes.size(); i++)
	{
		BLOCK_TYPE bt = blockTypes[blockTypes.size() - i - 1].type;
		breakIndex += blockTypes[blockTypes.size() - i - 1].depth;
		if (bt == WHILE1)
			breakIndex -= 1;
		if (bt == DO || bt == WHILE1 || bt == SWITCH || bt == LOOP)
			break;
	}
	assert(breakIndex > 0);
	return breakIndex - 1;
}

int CheerpWasmRenderInterface::findIndexFromLabel(int labelId)
{
	uint32_t breakIndex = 0;
	uint32_t i = 0;
	BLOCK_TYPE bt;
	assert(!blockTypes.empty());
	for (; i < blockTypes.size(); i++)
	{
		BlockType& block = blockTypes[blockTypes.size() - i - 1];
		bt = block.type;

		breakIndex += block.depth;

		if (block.label == labelId)
			break;
	}
	if (bt == WHILE1)
		breakIndex -= 1;
	assert(i < blockTypes.size() && "cannot find labelId in block types");
	return breakIndex - 1;
}

void CheerpWasmRenderInterface::renderBreak()
{
	BlockType block = blockTypes.back();
	if (block.type == CASE)
	{
		assert(block.depth > 0);
		writer->encodeInst(WasmU32Opcode::BR, block.depth - 1, code);
	}
	else
	{
		writer->encodeInst(WasmU32Opcode::BR, findClosestBlockIndex(), code);
	}
}

void CheerpWasmRenderInterface::renderBreak(int labelId)
{
	int breakIndex = findIndexFromLabel(labelId);
	writer->encodeInst(WasmU32Opcode::BR, breakIndex, code);
}

void CheerpWasmRenderInterface::renderContinue()
{
	// Find the last loop's block
	uint32_t breakIndex = 0;
	for (uint32_t i = 0; i < blockTypes.size(); i++)
	{
		BLOCK_TYPE bt = blockTypes[blockTypes.size() - i - 1].type;

		breakIndex += blockTypes[blockTypes.size() - i - 1].depth;

		if (bt == WHILE1 || bt == LOOP)
			break;
	}
	writer->encodeInst(WasmU32Opcode::BR, breakIndex - 1, code);
}

void CheerpWasmRenderInterface::renderContinue(int labelId)
{
	uint32_t breakIndex = 0;
	uint32_t i = 0;
	for (; i < blockTypes.size(); i++)
	{
		BlockType& block = blockTypes[blockTypes.size() - i - 1];

		breakIndex += block.depth;

		if (block.label == labelId)
			break;
	}
	assert(i < blockTypes.size() && "cannot find labelId in block types");
	writer->encodeInst(WasmU32Opcode::BR, breakIndex - 1, code);
}

void CheerpWasmRenderInterface::renderLabel(int labelId)
{
	writer->encodeInst(WasmS32Opcode::I32_CONST, labelId, code);
	writer->encodeInst(WasmU32Opcode::SET_LOCAL, labelLocal, code);
}

void CheerpWasmRenderInterface::renderIfOnLabel(int labelId, bool first)
{
	// TODO: Use first to optimize dispatch
	writer->encodeInst(WasmS32Opcode::I32_CONST, labelId, code);
	writer->encodeInst(WasmU32Opcode::GET_LOCAL, labelLocal, code);
	writer->encodeInst(WasmOpcode::I32_EQ, code);
	writer->encodeInst(WasmU32Opcode::IF, 0x40, code);
	blockTypes.emplace_back(IF, 1);
}

uint32_t CheerpWasmWriter::findDepth(const Value* v) const
{
	// Must be an instruction
	const Instruction* I = dyn_cast<Instruction>(v);
	if(!I)
		return -1;
	if(isInlineable(*I))
	{
		if(I->getNumOperands() < 1)
			return -1;
		uint32_t res = findDepth(I->getOperand(0));
		if(I->isCommutative())
		{
			assert(I->getNumOperands() == 2);
			res = std::min(res, findDepth(I->getOperand(1)));
		}
		return res;
	}
	else
	{
		return teeLocals.findDepth(v);
	}
}

void CheerpWasmWriter::filterNop(SmallVectorImpl<char>& buf) const
{
	assert(buf.back() == 0x0b);
	nopLocations.push_back(buf.size());
	std::sort(nopLocations.begin(), nopLocations.end());
	uint32_t nopIndex = 0;
	uint32_t old = 0;
	uint32_t curr = 0;
	while (old < buf.size())
	{
		if (nopLocations[nopIndex] <= old)
		{
			while (buf[old] == 0x01)
			{
				++old;
			}
			++nopIndex;
			continue;
		}
		buf[curr] = buf[old];
		++curr;
		++old;
	}
	buf.resize(curr);
	assert(buf.back() == 0x0b);
}

void CheerpWasmWriter::encodeBinOp(const llvm::Instruction& I, WasmBuffer& code)
{
	switch (I.getOpcode()) {
		case Instruction::URem:
		case Instruction::UDiv:
			compileUnsignedInteger(code, I.getOperand(0));
			compileUnsignedInteger(code, I.getOperand(1));
			break;
		case Instruction::SRem:
		case Instruction::SDiv:
			compileSignedInteger(code, I.getOperand(0), /*forComparison*/ false);
			compileSignedInteger(code, I.getOperand(1), /*forComparison*/ false);
			break;
		case Instruction::LShr:
			compileUnsignedInteger(code, I.getOperand(0));
			compileOperand(code, I.getOperand(1));
			break;
		case Instruction::AShr:
			compileSignedInteger(code, I.getOperand(0), /*forComparison*/ false);
			compileOperand(code, I.getOperand(1));
			break;
		case Instruction::FSub:
			if (I.getOperand(0) == ConstantFP::getZeroValueForNegation(I.getOperand(0)->getType()))
			{
				//Wasm has an operator negate on floating point
				//(-0.0) - something -> neg(something)
				//Note that this transformation is safe only for negative zero
				compileOperand(code, I.getOperand(1));
				const Type* t = I.getType();
				if (t->isFloatTy())
					encodeInst(WasmOpcode::F32_NEG, code);
				else if (t->isDoubleTy())
					encodeInst(WasmOpcode::F64_NEG, code);
				//We just encoded the operation, so now we can return
				return;
			}
			else
			{
				compileOperand(code, I.getOperand(0));
				compileOperand(code, I.getOperand(1));
				break;
			}
		default:
			if(I.isCommutative())
			{
				// Favor tee_local from the current candidate's stack
				if (findDepth(I.getOperand(0)) > findDepth(I.getOperand(1)))
				{
					compileOperand(code, I.getOperand(1));
					compileOperand(code, I.getOperand(0));
					// Go out of the switch
					break;
				}
				// Fallthrough
			}
			compileOperand(code, I.getOperand(0));
			compileOperand(code, I.getOperand(1));
			break;
	}

	const Type* t = I.getType();
	switch (I.getOpcode())
	{
#define BINOPI(Ty, name) \
		case Instruction::Ty: \
		{ \
			assert(t->isIntegerTy() || t->isPointerTy()); \
			if (t->isIntegerTy(64)) { \
				encodeInst(WasmOpcode::I64_##name, code); \
			} \
			else { \
				encodeInst(WasmOpcode::I32_##name, code); \
			} \
			return; \
		}
		BINOPI( Add,   ADD)
		BINOPI( Sub,   SUB)
		BINOPI( Mul,   MUL)
		BINOPI(SDiv, DIV_S)
		BINOPI(UDiv, DIV_U)
		BINOPI(SRem, REM_S)
		BINOPI(URem, REM_U)
		BINOPI( And,   AND)
		BINOPI(  Or,    OR)
		BINOPI( Xor,   XOR)
		BINOPI( Shl,   SHL)
		BINOPI(AShr, SHR_S)
		BINOPI(LShr, SHR_U)
#undef BINOPI

#define BINOPF(Ty, name) \
		case Instruction::Ty: \
		{ \
			if (t->isFloatTy()) { \
				encodeInst(WasmOpcode::F32_##name, code); \
				return; \
			} \
			else if (t->isDoubleTy()) { \
				encodeInst(WasmOpcode::F64_##name, code); \
				return; \
			} \
			else { \
				llvm_unreachable("unsupported type"); \
			} \
			break; \
		}
		BINOPF(FAdd,   ADD)
		BINOPF(FSub,   SUB)
		BINOPF(FMul,   MUL)
		BINOPF(FDiv,   DIV)
#undef BINOPF
		default:
		{
#ifndef NDEBUG
			I.dump();
#endif
			llvm_unreachable("unknown binop instruction");
		}
	}

#ifndef NDEBUG
	I.dump();
#endif
	llvm_unreachable("unknown type for binop instruction");
}

void CheerpWasmWriter::encodeInst(WasmOpcode opcode, WasmBuffer& code)
{
	code << static_cast<char>(opcode);
}

void CheerpWasmWriter::encodeInst(WasmS32Opcode opcode, int32_t immediate, WasmBuffer& code)
{
	code << static_cast<char>(opcode);
	encodeSLEB128(immediate, code);
}

void CheerpWasmWriter::encodeInst(WasmS64Opcode opcode, int64_t immediate, WasmBuffer& code)
{
	code << static_cast<char>(opcode);
	encodeSLEB128(immediate, code);
}

void CheerpWasmWriter::encodeInst(WasmU32Opcode opcode, uint32_t immediate, WasmBuffer& code)
{
	code << static_cast<char>(opcode);
	encodeULEB128(immediate, code);
}

void CheerpWasmWriter::encodeInst(WasmU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code)
{
	code << static_cast<char>(opcode);
	encodeULEB128(i1, code);
	encodeULEB128(i2, code);
}

void CheerpWasmWriter::encodePredicate(const llvm::Type* ty, const llvm::CmpInst::Predicate predicate, WasmBuffer& code)
{
	assert(ty->isIntegerTy() || ty->isPointerTy());
	switch(predicate)
	{
#define PREDICATE(Ty, name) \
		case CmpInst::ICMP_##Ty: \
			if (ty->isIntegerTy(64)) \
				encodeInst(WasmOpcode::I64_##name, code); \
			else \
				encodeInst(WasmOpcode::I32_##name, code); \
			break;
		PREDICATE( EQ,   EQ);
		PREDICATE( NE,   NE);
		PREDICATE(SLT, LT_S);
		PREDICATE(ULT, LT_U);
		PREDICATE(SGT, GT_S);
		PREDICATE(UGT, GT_U);
		PREDICATE(SLE, LE_S);
		PREDICATE(ULE, LE_U);
		PREDICATE(SGE, GE_S);
		PREDICATE(UGE, GE_U);
#undef PREDICATE
		default:
			llvm::errs() << "Handle predicate " << predicate << "\n";
			llvm_unreachable("unknown predicate");
	}
}

void CheerpWasmWriter::encodeLoad(const llvm::Type* ty, uint32_t offset,
		WasmBuffer& code, bool signExtend)
{
	if(ty->isIntegerTy())
	{
		uint32_t bitWidth = ty->getIntegerBitWidth();
		if(bitWidth == 1)
			bitWidth = 8;

		switch (bitWidth)
		{
			// Currently assume unsigned, like Cheerp. We may optimize
			// this be looking at a following sext or zext instruction.
			case 8:
				encodeInst(signExtend ? WasmU32U32Opcode::I32_LOAD8_S : WasmU32U32Opcode::I32_LOAD8_U, 0x0, offset, code);
				break;
			case 16:
				encodeInst(signExtend ? WasmU32U32Opcode::I32_LOAD16_S : WasmU32U32Opcode::I32_LOAD16_U, 0x1, offset, code);
				break;
			case 32:
				encodeInst(WasmU32U32Opcode::I32_LOAD, 0x2, offset, code);
				break;
			case 64:
				encodeInst(WasmU32U32Opcode::I64_LOAD, 0x2, offset, code);
				break;
			default:
				llvm::errs() << "bit width: " << bitWidth << '\n';
				llvm_unreachable("unknown integer bit width");
		}
	} else {
		if (ty->isFloatTy())
			encodeInst(WasmU32U32Opcode::F32_LOAD, 0x2, offset, code);
		else if (ty->isDoubleTy())
			encodeInst(WasmU32U32Opcode::F64_LOAD, 0x3, offset, code);
		else
			encodeInst(WasmU32U32Opcode::I32_LOAD, 0x2, offset, code);
	}
}

void CheerpWasmWriter::encodeWasmIntrinsic(WasmBuffer& code, const llvm::Function* F)
{
	const auto& builtin = TypedBuiltinInstr::getMathTypedBuiltin(*F);

	assert(TypedBuiltinInstr::isValidWasmMathBuiltin(builtin) && "Only proper Wasm builtin can be emitted");

	encodeInst(TypedBuiltinInstr::opcodeWasmBuiltin(builtin),code);
}

//Return whether explicit assigment to the phi is needed
//Also insert the relevant instruction into getLocalDone when needed
bool CheerpWasmWriter::requiresExplicitAssigment(const Instruction* phi, const Value* incoming)
{
	const Instruction* incomingInst=getUniqueIncomingInst(incoming, PA);
	if(!incomingInst)
		return true;
	assert(!isInlineable(*incomingInst));
	const bool isSameRegister = (registerize.getRegisterId(phi, EdgeContext::emptyContext())==registerize.getRegisterId(incomingInst, edgeContext));
	if (isSameRegister)
		getLocalDone.insert(incomingInst);
	return !isSameRegister;
}

int CheerpWasmWriter::gainOfHandlingPhiOnTheEdge(const PHINode* phi, const Value* incoming) const
{
	const Instruction* incomingInst=getUniqueIncomingInst(incoming, PA);
	if(!incomingInst)
		return 2;
	assert(!isInlineable(*incomingInst));
	const bool isSameRegister = (registerize.getRegisterId(phi, EdgeContext::emptyContext())==registerize.getRegisterId(incomingInst, edgeContext));

	return (isSameRegister ? -2 : +2);
}

int CheerpWasmWriter::gainOfHandlingPhiOnTheEdge(const PHINode* phi) const
{
	int res = -2;
	for (unsigned int i = 0; i<phi->getNumIncomingValues(); i++)
	{
		res += gainOfHandlingPhiOnTheEdge(phi, phi->getIncomingValue(i));
	}
	return res;
}

void CheerpWasmWriter::compilePHIOfBlockFromOtherBlock(WasmBuffer& code, const BasicBlock* to, const BasicBlock* from, const PHINode* phiHandledAsResult)
{
	class WriterPHIHandler: public PHIHandlerUsingStack
	{
	public:
		WriterPHIHandler(CheerpWasmWriter& w, WasmBuffer& c, const BasicBlock* from)
			:PHIHandlerUsingStack(w.PA),writer(w), code(c), fromBB(from)
		{
		}
		~WriterPHIHandler()
		{
		}
	private:
		CheerpWasmWriter& writer;
		WasmBuffer& code;
		const BasicBlock* fromBB;
		void handlePHIStackGroup(const std::vector<const llvm::PHINode*>& phiToHandle) override
		{
			std::vector<std::pair<const Value*, std::vector<const llvm::PHINode*>>> toProcessOrdered;
			std::map<const Value*, std::vector<const llvm::PHINode*>> toProcessMap;
			for (auto& phi : phiToHandle)
			{
				const Value* incoming = phi->getIncomingValueForBlock(fromBB);
				// We can avoid assignment from the same register if no pointer kind conversion is required
				if(!writer.requiresExplicitAssigment(phi, incoming))
					continue;
				// We can leave undefined values undefined
				if (isa<UndefValue>(incoming))
					continue;

				if (toProcessMap.count(incoming) == 0)
					toProcessOrdered.push_back({incoming,{}});

				toProcessMap[incoming].push_back(phi);
			}

			//Note that any process order works, as long as it's deterministic
			//So reordering for leaving on the stack whatever is needed also works
			for (auto& pair : toProcessOrdered)
			{
				// 1) Put the value on the stack
				writer.compileOperand(code, pair.first);
				pair.second = std::move(toProcessMap[pair.first]);
			}

			writer.teeLocals.removeConsumed();

			while (!toProcessOrdered.empty())
			{
				const Value* incoming = toProcessOrdered.back().first;
				const auto& phiVector  = toProcessOrdered.back().second;

				for (const PHINode* phi : phiVector)
				{
					// 2) Save the value in the phi
					uint32_t reg = writer.registerize.getRegisterId(phi, EdgeContext::emptyContext());
					uint32_t local = writer.localMap.at(reg);
					if (phi == phiVector.back())
					{
						if (toProcessOrdered.size() == 1)
							writer.teeLocals.addCandidate(incoming, /*isInstructionAssigment*/false, local, code.tell());
						writer.encodeInst(WasmU32Opcode::SET_LOCAL, local, code);
					}
					else
						writer.encodeInst(WasmU32Opcode::TEE_LOCAL, local, code);
				}
				toProcessOrdered.pop_back();
			}
			if (!hasToSkipPHIs())
				writer.teeLocals.instructionStart(code);
		}
	};

	WriterPHIHandler phiHandler(*this, code, from);
	if (phiHandledAsResult)
	{
		//Put the relevant incoming value on the stack, to be used as result from a Wasm generalized block
		compileOperand(code, phiHandledAsResult->getIncomingValueForBlock(from));
		//Inform phiHanlder that will have to skip that PHINode
		phiHandler.skipPHI(phiHandledAsResult);
	}
	phiHandler.runOnEdge(registerize, from, to);
}

const char* CheerpWasmWriter::getTypeString(const Type* t)
{
	if (t->isIntegerTy() || TypeSupport::isRawPointer(t, true))
		return "i32";
	else if(t->isFloatTy())
		return "f32";
	else if(t->isDoubleTy())
		return "f64";
	else if(t->isPointerTy())
		return "anyref";
	else
	{
#ifndef NDEBUG
		llvm::errs() << "Unsupported type ";
		t->dump();
#endif
		llvm_unreachable("Unsuppored type");
	}
}

void CheerpWasmWriter::compileGEP(WasmBuffer& code, const llvm::User* gep_inst, bool standalone)
{
	const auto I = dyn_cast<Instruction>(gep_inst);
	if (I && !isInlineable(*I)) {
		if (!standalone) {
			compileGetLocal(code, I);
			return;
		}
	}

	WasmGepWriter gepWriter(*this, code);
	const llvm::Value *p = linearHelper.compileGEP(gep_inst, &gepWriter, &PA);
	if(const GlobalVariable* GV = dyn_cast<GlobalVariable>(p))
		gepWriter.addConst(linearHelper.getGlobalVariableAddress(GV));
	else if(!isa<ConstantPointerNull>(p))
		gepWriter.addValue(p, 1);

	//compileValues should encode the pointer completely
	const uint32_t offset = gepWriter.compileValues(/*positiveOffsetAllowed*/false);
	assert(offset == 0);
}

void CheerpWasmWriter::encodeBranchTable(WasmBuffer& code, std::vector<uint32_t> table, int32_t defaultBlock)
{
	encodeInst(WasmOpcode::BR_TABLE, code);
	encodeULEB128(table.size(), code);
	for (auto label : table)
		encodeULEB128(label, code);
	encodeULEB128(defaultBlock, code);
}

void CheerpWasmWriter::compileSignedInteger(WasmBuffer& code, const llvm::Value* v, bool forComparison)
{
	uint32_t shiftAmount = std::max(32-(int)v->getType()->getIntegerBitWidth(), 0);
	if(const ConstantInt* C = dyn_cast<ConstantInt>(v))
	{
		int32_t value = C->getSExtValue();
		if(forComparison)
			value <<= shiftAmount;
		if (v->getType()->isIntegerTy(64))
			encodeInst(WasmS64Opcode::I64_CONST, value, code);
		else
			encodeInst(WasmS32Opcode::I32_CONST, value, code);
		return;
	}

	compileOperand(code, v);

	if (shiftAmount == 0)
		return;

	if (forComparison)
	{
		// When comparing two signed values we can avoid the right shift
		encodeInst(WasmS32Opcode::I32_CONST, shiftAmount, code);
		encodeInst(WasmOpcode::I32_SHL, code);
	}
	else
	{
		encodeInst(WasmS32Opcode::I32_CONST, shiftAmount, code);
		encodeInst(WasmOpcode::I32_SHL, code);
		encodeInst(WasmS32Opcode::I32_CONST, shiftAmount, code);
		encodeInst(WasmOpcode::I32_SHR_S, code);
	}
}

void CheerpWasmWriter::compileUnsignedInteger(WasmBuffer& code, const llvm::Value* v)
{
	if(const ConstantInt* C = dyn_cast<ConstantInt>(v))
	{
		if (v->getType()->isIntegerTy(64))
			encodeInst(WasmS64Opcode::I64_CONST, C->getZExtValue(), code);
		else
			encodeInst(WasmS32Opcode::I32_CONST, C->getZExtValue(), code);
		return;
	}

	compileOperand(code, v);

	uint32_t initialSize = v->getType()->getIntegerBitWidth();
	if(initialSize < 32 && CheerpWriter::needsUnsignedTruncation(v, /*asmjs*/true))
	{
		encodeInst(WasmS32Opcode::I32_CONST, getMaskForBitWidth(initialSize), code);
		encodeInst(WasmOpcode::I32_AND, code);
	}
}

void CheerpWasmWriter::compileTypedZero(WasmBuffer& code, llvm::Type* t)
{
	// Encode a literal f64, f32 or i32 zero as the return value.
	encodeLiteralType(t, code);
	if (t->isDoubleTy()) {
		encodeF64(0., code);
	} else if (t->isFloatTy()) {
		encodeF32(0.f, code);
	} else {
		encodeSLEB128(0, code);
	}
}

void CheerpWasmWriter::compileConstantExpr(WasmBuffer& code, const ConstantExpr* ce)
{
	switch(ce->getOpcode())
	{
		case Instruction::Add:
		{
			compileOperand(code, ce->getOperand(0));
			compileOperand(code, ce->getOperand(1));
			encodeInst(WasmOpcode::I32_ADD, code);
			break;
		}
		case Instruction::And:
		{
			compileOperand(code, ce->getOperand(0));
			compileOperand(code, ce->getOperand(1));
			encodeInst(WasmOpcode::I32_AND, code);
			break;
		}
		case Instruction::Or:
		{
			compileOperand(code, ce->getOperand(0));
			compileOperand(code, ce->getOperand(1));
			encodeInst(WasmOpcode::I32_OR, code);
			break;
		}
		case Instruction::Sub:
		{
			compileOperand(code, ce->getOperand(0));
			compileOperand(code, ce->getOperand(1));
			encodeInst(WasmOpcode::I32_SUB, code);
			break;
		}
		case Instruction::GetElementPtr:
		{
			compileGEP(code, ce);
			break;
		}
		case Instruction::BitCast:
		{
			assert(ce->getOperand(0)->getType()->isPointerTy());
			compileOperand(code, ce->getOperand(0));
			break;
		}
		case Instruction::IntToPtr:
		{
			compileOperand(code, ce->getOperand(0));
			break;
		}
		case Instruction::ICmp:
		{
			CmpInst::Predicate p = (CmpInst::Predicate)ce->getPredicate();
			compileICmp(ce->getOperand(0), ce->getOperand(1), p, code);
			break;
		}
		case Instruction::PtrToInt:
		{
			compileOperand(code, ce->getOperand(0));
			break;
		}
		case Instruction::Select:
		{
			compileOperand(code, ce->getOperand(1));
			compileOperand(code, ce->getOperand(2));
			compileCondition(code, ce->getOperand(0), /*booleanInvert*/false);
			encodeInst(WasmOpcode::SELECT, code);
			break;
		}
		case Instruction::ZExt:
		{
			compileUnsignedInteger(code, ce->getOperand(0));
			if (ce->getType()->isIntegerTy(64))
			{
				assert(!ce->getOperand(0)->getType()->isIntegerTy(64));
				encodeInst(WasmOpcode::I64_EXTEND_I32_U, code);
			}
			break;
		}

		default:
			compileTypedZero(code, ce->getType());
			llvm::errs() << "warning: Unsupported constant expr " << ce->getOpcodeName() << '\n';
	}
}

static uint32_t getSLEBEncodingLength(int64_t val)
{
	uint8_t tmp[100];
	return encodeSLEB128(val, tmp);
}

static uint32_t getULEBEncodingLength(int64_t val)
{
	uint8_t tmp[100];
	return encodeULEB128(val, tmp);
}

void CheerpWasmWriter::compileFloatToText(WasmBuffer& code, const APFloat& f, uint32_t precision)
{
	if(f.isInfinity())
	{
		if(f.isNegative())
			code << '-';
		code << "inf";
	}
	else if(f.isNaN())
	{
		code << "nan";
	}
	else
	{
		char buf[40];
		// TODO: Figure out the right amount of hexdigits
		unsigned charCount = f.convertToHexString(buf, precision, false, APFloat::roundingMode::NearestTiesToEven);
		(void)charCount;
		assert(charCount < 40);
		code << buf;
	}
}

void CheerpWasmWriter::compileConstant(WasmBuffer& code, const Constant* c, bool forGlobalInit)
{
	if (hasPutTeeLocalOnStack(code, c))
		return;
	if(const ConstantExpr* CE = dyn_cast<ConstantExpr>(c))
	{
		compileConstantExpr(code, CE);
	}
	else if(const ConstantInt* i=dyn_cast<ConstantInt>(c))
	{
		assert(i->getType()->isIntegerTy() && i->getBitWidth() <= 64);
		if (i->getBitWidth() == 64) {
			encodeInst(WasmS64Opcode::I64_CONST, i->getSExtValue(), code);
		} else if (i->getBitWidth() == 32)
			encodeInst(WasmS32Opcode::I32_CONST, i->getSExtValue(), code);
		else
			encodeInst(WasmS32Opcode::I32_CONST, i->getZExtValue(), code);
	}
	else if(const ConstantFP* f=dyn_cast<ConstantFP>(c))
	{
		encodeLiteralType(c->getType(), code);
		if (c->getType()->isDoubleTy()) {
			encodeF64(f->getValueAPF().convertToDouble(), code);
		} else {
			assert(c->getType()->isFloatTy());
			encodeF32(f->getValueAPF().convertToFloat(), code);
		}
	}
	else if(const GlobalVariable* GV = dyn_cast<GlobalVariable>(c))
	{
		uint32_t address = linearHelper.getGlobalVariableAddress(GV);
		encodeInst(WasmS32Opcode::I32_CONST, address, code);
	}
	else if(isa<ConstantPointerNull>(c))
	{
		encodeInst(WasmS32Opcode::I32_CONST, 0, code);
	}
	else if(isa<Function>(c))
	{
		const Function* F = cast<Function>(c);
		if (linearHelper.functionHasAddress(F))
		{
			uint32_t addr = linearHelper.getFunctionAddress(F);
			if (!addr)
				llvm::errs() << "function name: " << c->getName() << '\n';
			assert(addr && "function address is zero (aka nullptr conflict)");
			encodeInst(WasmS32Opcode::I32_CONST, addr, code);
		}
		else
		{
			// When dealing with indirectly used undefined functions forward them to the null function
			// TODO: This improve the robustness of the compiler, but it might generate unexpected behavor
			//       if the address is ever explicitly compared to 0
			assert(F->empty());
			encodeInst(WasmS32Opcode::I32_CONST, 0, code);
		}
	}
	else if (isa<UndefValue>(c))
	{
		compileTypedZero(code, c->getType());
	}
	else
	{
#ifndef NDEBUG
		c->dump();
#endif
		llvm::report_fatal_error("Cannot handle this constant");
	}
}

void CheerpWasmWriter::compileGetLocal(WasmBuffer& code, const llvm::Instruction* I)
{
	compileInstructionAndSet(code, *I);
	if (hasPutTeeLocalOnStack(code, I))
	{
		//Successfully find a candidate to transform in tee local
		return;
	}
	uint32_t idx = registerize.getRegisterId(I, edgeContext);
	uint32_t localId = localMap.at(idx);
	getLocalDone.insert(I);
	encodeInst(WasmU32Opcode::GET_LOCAL, localId, code);
}

void CheerpWasmWriter::compileOperand(WasmBuffer& code, const llvm::Value* v)
{
	if(const Constant* c=dyn_cast<Constant>(v))
	{
		auto it = globalizedConstants.find(c);
		if(it != globalizedConstants.end())
			encodeInst(WasmU32Opcode::GET_GLOBAL, it->second.first, code);
		else
			compileConstant(code, c, /*forGlobalInit*/false);
	}
	else if(const Instruction* it=dyn_cast<Instruction>(v))
	{
		if(isInlineable(*it)) {
			compileInlineInstruction(code, *it);
		} else {
			compileGetLocal(code, it);
		}
	}
	else if(const Argument* arg=dyn_cast<Argument>(v))
	{
		if (hasPutTeeLocalOnStack(code, arg))
			return;
		uint32_t local = arg->getArgNo();
		encodeInst(WasmU32Opcode::GET_LOCAL, local, code);
	}
	else
	{
#ifndef NDEBUG
		v->dump();
#endif
		assert(false);
	}
}

const char* CheerpWasmWriter::getIntegerPredicate(llvm::CmpInst::Predicate p)
{
	switch(p)
	{
		case CmpInst::ICMP_EQ:
			return "eq";
		case CmpInst::ICMP_NE:
			return "ne";
		case CmpInst::ICMP_SGE:
			return "ge_s";
		case CmpInst::ICMP_SGT:
			return "gt_s";
		case CmpInst::ICMP_SLE:
			return "le_s";
		case CmpInst::ICMP_SLT:
			return "lt_s";
		case CmpInst::ICMP_UGE:
			return "ge_u";
		case CmpInst::ICMP_UGT:
			return "gt_u";
		case CmpInst::ICMP_ULE:
			return "le_u";
		case CmpInst::ICMP_ULT:
			return "lt_u";
		default:
			llvm::errs() << "Handle predicate " << p << "\n";
			break;
	}
	return "";
}

bool CheerpWasmWriter::isSignedLoad(const Value* V) const
{
	const LoadInst* LI = dyn_cast<LoadInst>(V);
	if(!LI)
		return false;
	if(GlobalVariable* ptrGV = dyn_cast<GlobalVariable>(LI->getOperand(0)))
	{
		auto it = globalizedGlobalsIDs.find(ptrGV);
		if(it != globalizedGlobalsIDs.end())
			return false;
	}
	for(const User* U: LI->users())
	{
		const Instruction* userI = cast<Instruction>(U);
		if(userI->getOpcode() == Instruction::SExt)
			continue;
		else if(userI->getOpcode() == Instruction::ICmp && cast<ICmpInst>(userI)->isSigned())
			continue;
		else
			return false;
	}
	return true;
}

void CheerpWasmWriter::compileICmp(const Value* op0, const Value* op1, const CmpInst::Predicate p,
		WasmBuffer& code)
{
	bool useEqz = false;
	if(p == CmpInst::ICMP_EQ)
	{
		// Move the constant on op1 to simplify the logic below
		if(isa<Constant>(op0))
			std::swap(op0, op1);
		if(isa<Constant>(op1) && cast<Constant>(op1)->isNullValue())
			useEqz = true;
	}
	if(op0->getType()->isPointerTy())
	{
		compileOperand(code, op0);
		if(useEqz)
		{
			encodeInst(WasmOpcode::I32_EQZ, code);
			return;
		}
		compileOperand(code, op1);
	}
	else if(CmpInst::isSigned(p))
	{
		bool isOp0Signed = isSignedLoad(op0);
		bool isOp1Signed = isSignedLoad(op1);
		// Only use the "forComparison" trick if neither operands are signed loads
		bool useForComparison = !isOp0Signed && !isOp1Signed;
		if(isOp0Signed)
			compileOperand(code, op0);
		else
			compileSignedInteger(code, op0, useForComparison);
		if(isOp1Signed)
			compileOperand(code, op1);
		else
			compileSignedInteger(code, op1, useForComparison);
	}
	else if (CmpInst::isUnsigned(p) || !op0->getType()->isIntegerTy(32))
	{
		compileUnsignedInteger(code, op0);
		if(useEqz)
		{
			if (op0->getType()->isIntegerTy(64))
				encodeInst(WasmOpcode::I64_EQZ, code);
			else
				encodeInst(WasmOpcode::I32_EQZ, code);
			return;
		}
		compileUnsignedInteger(code, op1);
	}
	else
	{
		compileSignedInteger(code, op0, true);
		if(useEqz)
		{
			if (op0->getType()->isIntegerTy(64))
				encodeInst(WasmOpcode::I64_EQZ, code);
			else
				encodeInst(WasmOpcode::I32_EQZ, code);
			return;
		}
		compileSignedInteger(code, op1, true);
	}
	encodePredicate(op0->getType(), p, code);
}

void CheerpWasmWriter::compileICmp(const ICmpInst& ci, const CmpInst::Predicate p,
		WasmBuffer& code)
{
	compileICmp(ci.getOperand(0), ci.getOperand(1), p, code);
}

void CheerpWasmWriter::compileFCmp(const Value* lhs, const Value* rhs, CmpInst::Predicate p, WasmBuffer& code)
{
	if (p == CmpInst::FCMP_ORD)
	{
		Type* ty = lhs->getType();
		assert(ty->isDoubleTy() || ty->isFloatTy());
		assert(ty == rhs->getType());

		// Check if both operands are equal to itself. A nan-value is
		// never equal to itself. Use a logical and operator for the
		// resulting comparison.
		compileOperand(code, lhs);
		compileOperand(code, lhs);
		if (ty->isDoubleTy())
			encodeInst(WasmOpcode::F64_EQ, code);
		else
			encodeInst(WasmOpcode::F32_EQ, code);

		compileOperand(code, rhs);
		compileOperand(code, rhs);
		if (ty->isDoubleTy())
			encodeInst(WasmOpcode::F64_EQ, code);
		else
			encodeInst(WasmOpcode::F32_EQ, code);

		encodeInst(WasmOpcode::I32_AND, code);
	} else if (p == CmpInst::FCMP_UNO) {
		Type* ty = lhs->getType();
		assert(ty->isDoubleTy() || ty->isFloatTy());
		assert(ty == rhs->getType());

		// Check if at least one operand is not equal to itself.
		// A nan-value is never equal to itself. Use a logical
		// or operator for the resulting comparison.
		compileOperand(code, lhs);
		compileOperand(code, lhs);
		if (ty->isDoubleTy())
			encodeInst(WasmOpcode::F64_NE, code);
		else
			encodeInst(WasmOpcode::F32_NE, code);

		compileOperand(code, rhs);
		compileOperand(code, rhs);
		if (ty->isDoubleTy())
			encodeInst(WasmOpcode::F64_NE, code);
		else
			encodeInst(WasmOpcode::F32_NE, code);

		encodeInst(WasmOpcode::I32_OR, code);
	} else {
		compileOperand(code, lhs);
		compileOperand(code, rhs);
		Type* ty = lhs->getType();
		assert(ty->isDoubleTy() || ty->isFloatTy());
		// It is much more efficient to invert the predicate if we need to check for unorderedness
		bool invertForUnordered = CmpInst::isUnordered(p);
		if(invertForUnordered)
			p = CmpInst::getInversePredicate(p);
		assert(!CmpInst::isUnordered(p));
		switch(p)
		{
#define PREDICATE(Ty, name) \
			case CmpInst::FCMP_O##Ty: \
				if (ty->isDoubleTy()) \
					encodeInst(WasmOpcode::F64_##name, code); \
				else \
					encodeInst(WasmOpcode::F32_##name, code); \
				break;
			PREDICATE(EQ, EQ)
			PREDICATE(NE, NE)
			PREDICATE(LT, LT)
			PREDICATE(GT, GT)
			PREDICATE(LE, LE)
			PREDICATE(GE, GE)
#undef PREDICATE
			default:
				llvm::errs() << "Handle predicate " << p << "\n";
				break;
		}
		if(invertForUnordered)
		{
			// Invert result
			encodeInst(WasmOpcode::I32_EQZ, code);
		}
	}
}

void CheerpWasmWriter::compileDowncast(WasmBuffer& code, const CallBase* callV)
{
	assert(callV->arg_size() == 2);
	assert(callV->getCalledFunction()->getIntrinsicID() == Intrinsic::cheerp_downcast ||
		callV->getCalledFunction()->getIntrinsicID() == Intrinsic::cheerp_virtualcast);

	const Value* src = callV->getOperand(0);
	const Value* offset = callV->getOperand(1);

	Type* t = src->getType()->getPointerElementType();

	compileOperand(code, src);

	if(!TypeSupport::isClientType(t) &&
			(!isa<ConstantInt>(offset) || !cast<ConstantInt>(offset)->isNullValue()))
	{
		compileOperand(code, offset);
		encodeInst(WasmOpcode::I32_ADD, code);
	}
}

uint32_t CheerpWasmWriter::compileLoadStorePointer(WasmBuffer& code, const Value* ptrOp)
{
	uint32_t offset = 0;
	if(isa<Instruction>(ptrOp) && isInlineable(*cast<Instruction>(ptrOp))) {
		// Calling compileGEP is safe on any instruction
		WasmGepWriter gepWriter(*this, code);
		auto p = linearHelper.compileGEP(ptrOp, &gepWriter, &PA);
		if(const GlobalVariable* GV = dyn_cast<GlobalVariable>(p))
			gepWriter.addConst(linearHelper.getGlobalVariableAddress(GV));
		else
			gepWriter.addValue(p, 1);

		//compileValues returs the offset yet to be handled
		offset = gepWriter.compileValues(/*positiveOffsetAllowed*/true);
	} else {
		const Constant* C = dyn_cast<Constant>(ptrOp);
		if (C && !globalizedConstants.count(C))
		{
			struct AddrListener: public LinearMemoryHelper::ByteListener
			{
				uint32_t addr;
				uint32_t off;
				AddrListener():addr(0),off(0)
				{
				}
				void addByte(uint8_t b) override
				{
					addr |= b << off;
					off += 8;
				}
			};
			AddrListener addrListener;
			linearHelper.compileConstantAsBytes(C, /* asmjs */ true, &addrListener);
			encodeInst(WasmS32Opcode::I32_CONST, 0, code);
			offset = addrListener.addr;
		}
		else
		{
			compileOperand(code, ptrOp);
		}
	}
	return offset;
}

void CheerpWasmWriter::compileLoad(WasmBuffer& code, const LoadInst& li, bool signExtend)
{
	const Value* ptrOp=li.getPointerOperand();
	// 1) The pointer
	uint32_t offset = compileLoadStorePointer(code, ptrOp);
	// 2) Load
	encodeLoad(li.getType(), offset, code, signExtend);
}

bool CheerpWasmWriter::compileInstruction(WasmBuffer& code, const Instruction& I)
{
	switch(I.getOpcode())
	{
		case Instruction::GetElementPtr:
		{
			compileGEP(code, &I, true);
			break;
		}
		default:
			return compileInlineInstruction(code, I);
	}
	return false;
}

bool CheerpWasmWriter::isTailCall(const CallInst& ci) const
{
	if(!WasmReturnCalls || !ci.isTailCall())
		return false;
	const Instruction* nextI = ci.getNextNode();
	// The next inst must be a return
	if(!isa<ReturnInst>(nextI))
		return false;
	// Both call and return are void
	if(currentFun->getReturnType()->isVoidTy())
		return ci.getType()->isVoidTy();
	// The return uses the call
	return nextI->getOperand(0) == &ci;
}

bool CheerpWasmWriter::isReturnPartOfTailCall(const Instruction& ti) const
{
	const BasicBlock* BB = ti.getParent();
	// Make sure this return is not the first instruction of the block
	if(&*BB->begin() == &ti)
		return false;
	const Instruction* TermPrev = ti.getPrevNode();
	// Make sure the previous instruction is a call
	if(!isa<CallInst>(TermPrev))
		return false;
	return isTailCall(*cast<CallInst>(TermPrev));
}

void CheerpWasmWriter::checkAndSanitizeDependencies(InstructionToDependenciesMap& dependencies) const
{
	for (auto& pair : dependencies)
	{
		assert(pair.first->getParent() == currentBB);
		pair.second.erase(pair.first);
		for (const auto& I : pair.second)
		{
			assert(!isInlineable(*I));
			assert(I->getParent() == currentBB);
		}
	}
}

void CheerpWasmWriter::flushGeneric(WasmBuffer& code, const Instruction& I, const InstructionToDependenciesMap& dependencies)
{
	const auto it = dependencies.find(&I);
	if (it == dependencies.end())
		return;

	const bool needsSubStack = teeLocals.needsSubStack(code);
	if (needsSubStack)
		teeLocals.addIndentation(code);
	for (const auto& x : it->second)
		compileInstructionAndSet(code, *x);
	if (needsSubStack)
		teeLocals.decreaseIndentation(code, false);
}

void CheerpWasmWriter::flushMemoryDependencies(WasmBuffer& code, const Instruction& I)
{
	flushGeneric(code, I, memoryDependencies);
}

void CheerpWasmWriter::flushSetLocalDependencies(WasmBuffer& code, const Instruction& I)
{
	flushGeneric(code, I, localsDependencies);
}

bool CheerpWasmWriter::compileInlineInstruction(WasmBuffer& code, const Instruction& I)
{
	switch(I.getOpcode())
	{
		case Instruction::Alloca:
		{
			llvm::report_fatal_error("Allocas in wasm should be removed in the AllocaLowering pass. This is a bug");
		}
		case Instruction::Add:
		case Instruction::And:
		case Instruction::AShr:
		case Instruction::LShr:
		case Instruction::Mul:
		case Instruction::Or:
		case Instruction::Shl:
		case Instruction::Sub:
		case Instruction::SDiv:
		case Instruction::UDiv:
		case Instruction::SRem:
		case Instruction::URem:
		case Instruction::Xor:
		case Instruction::FAdd:
		case Instruction::FDiv:
		case Instruction::FMul:
		case Instruction::FSub:
		{
			encodeBinOp(I, code);
			break;
		}
		case Instruction::FNeg:
		{
			compileOperand(code, I.getOperand(0));
			const Type* t = I.getType();
			if (t->isFloatTy())
				encodeInst(WasmOpcode::F32_NEG, code);
			else if (t->isDoubleTy())
				encodeInst(WasmOpcode::F64_NEG, code);
			break;
		}
		case Instruction::BitCast:
		{
			assert(I.getType()->isPointerTy());
			compileOperand(code, I.getOperand(0));
			break;
		}
		case Instruction::Br:
			break;
		case Instruction::VAArg:
		{
			const VAArgInst& vi=cast<VAArgInst>(I);

			// Load the current argument
			compileOperand(code, vi.getPointerOperand());
			encodeInst(WasmU32U32Opcode::I32_LOAD, 0x2, 0x0, code);
			encodeLoad(vi.getType(), 0, code, /*signExtend*/false);

			// Move varargs pointer to next argument
			compileOperand(code, vi.getPointerOperand());
			compileOperand(code, vi.getPointerOperand());
			encodeInst(WasmU32U32Opcode::I32_LOAD, 0x2, 0x0, code);
			encodeInst(WasmS32Opcode::I32_CONST, 8, code);
			encodeInst(WasmOpcode::I32_ADD, code);
			encodeInst(WasmU32U32Opcode::I32_STORE, 0x2, 0x0, code);
			break;
		}
		case Instruction::Call:
		{
			const CallInst& ci = cast<CallInst>(I);
			const Function * calledFunc = ci.getCalledFunction();
			const Value * calledValue = ci.getCalledValue();
			const PointerType* pTy = cast<PointerType>(calledValue->getType());
			const FunctionType* fTy = cast<FunctionType>(pTy->getElementType());
			assert(!ci.isInlineAsm());
			// NOTE: If 'useTailCall' the code _must_ use return_call or insert a return
			//       Returns are not otherwise added in such cases
			const bool useTailCall = isTailCall(ci);

			if (calledFunc)
			{
				unsigned intrinsicId = calledFunc->getIntrinsicID();
				switch (intrinsicId)
				{
					case Intrinsic::trap:
					{
						encodeInst(WasmOpcode::UNREACHABLE, code);
						// NOTE: No point in adding a return even if 'useTailCall' is true
						return true;
					}
					case Intrinsic::stacksave:
					{
						encodeInst(WasmU32Opcode::GET_GLOBAL, stackTopGlobal, code);
						if(useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::stackrestore:
					{
						compileOperand(code, ci.getOperand(0));
						encodeInst(WasmU32Opcode::SET_GLOBAL, stackTopGlobal, code);
						if(useTailCall)
							encodeInst(WasmOpcode::RETURN, code);
						return true;
					}
					case Intrinsic::vastart:
					{
						llvm::report_fatal_error("Vastart in wasm should be removed in the AllocaLowering pass. This is a bug");
					}
					case Intrinsic::vacopy:
					{
						compileOperand(code, ci.getOperand(0));
						compileOperand(code, ci.getOperand(1));
						encodeInst(WasmU32U32Opcode::I32_LOAD, 0x2, 0x0, code);
						encodeInst(WasmU32U32Opcode::I32_STORE, 0x2, 0x0, code);
						if(useTailCall)
							encodeInst(WasmOpcode::RETURN, code);
						return true;
					}
					case Intrinsic::vaend:
					{
						// Do nothing.
						if(useTailCall)
							encodeInst(WasmOpcode::RETURN, code);
						return true;
					}
					case Intrinsic::cheerp_downcast:
					case Intrinsic::cheerp_virtualcast:
					{
						compileDowncast(code, &ci);
						if(useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::cheerp_downcast_current:
					{
						compileOperand(code, ci.getOperand(0));
						if(useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::cheerp_upcast_collapsed:
					{
						compileOperand(code, ci.getOperand(0));
						if(useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::cheerp_cast_user:
					{
						if(ci.use_empty())
							return true;
						compileOperand(code, ci.getOperand(0));
						// NOTE: If there are no uses this cannot be a tail-call (the user would have been the return)
						if(useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::cheerp_grow_memory:
					{
						compileOperand(code, ci.getOperand(0));
						if(useWasmLoader)
						{
							uint32_t importedId = linearHelper.getBuiltinId(BuiltinInstr::BUILTIN::GROW_MEM);
							if(useTailCall)
							{
								encodeInst(WasmU32Opcode::RETURN_CALL, importedId, code);
								return true;
							}
							else
								encodeInst(WasmU32Opcode::CALL, importedId, code);
						}
						else
						{
							encodeInst(WasmS32Opcode::GROW_MEMORY, 0, code);
							if(useTailCall)
							{
								encodeInst(WasmOpcode::RETURN, code);
								return true;
							}
						}
						return false;
					}
					case Intrinsic::flt_rounds:
					{
						// Rounding mode 1: nearest
						encodeInst(WasmS32Opcode::I32_CONST, 1, code);
						if(useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::invariant_start:
					{
						//TODO: Try to optimize using this, for now just pass the second arg
						if (ci.use_empty())
							return true;

						compileOperand(code, ci.getOperand(1));
						// NOTE: If there are no uses this cannot be a tail-call (the user would have been the return)
						if(useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::invariant_end:
					{
						// Do nothing.
						if(useTailCall)
							encodeInst(WasmOpcode::RETURN, code);
						return true;
					}
					case Intrinsic::memmove:
					{
						compileOperand(code, ci.op_begin()->get());
						compileOperand(code, (ci.op_begin() + 1)->get());
						compileOperand(code, (ci.op_begin() + 2)->get());
						llvm::Function* f = module.getFunction("memmove");
						uint32_t functionId = linearHelper.getFunctionIds().at(f);
						encodeInst(WasmU32Opcode::CALL, functionId, code);
						encodeInst(WasmOpcode::DROP, code);
						// NOTE: Cannot tail call, the return type is different
						if(useTailCall)
							encodeInst(WasmOpcode::RETURN, code);
						return true;
					}
					case Intrinsic::memcpy:
					{
						compileOperand(code, ci.op_begin()->get());
						compileOperand(code, (ci.op_begin() + 1)->get());
						compileOperand(code, (ci.op_begin() + 2)->get());
						llvm::Function* f = module.getFunction("memcpy");
						uint32_t functionId = linearHelper.getFunctionIds().at(f);
						encodeInst(WasmU32Opcode::CALL, functionId, code);
						encodeInst(WasmOpcode::DROP, code);
						// NOTE: Cannot tail call, the return type is different
						if(useTailCall)
							encodeInst(WasmOpcode::RETURN, code);
						return true;
					}
					case Intrinsic::memset:
					{
						compileOperand(code, ci.op_begin()->get());
						compileOperand(code, (ci.op_begin() + 1)->get());
						compileOperand(code, (ci.op_begin() + 2)->get());
						llvm::Function* f = module.getFunction("memset");
						uint32_t functionId = linearHelper.getFunctionIds().at(f);
						encodeInst(WasmU32Opcode::CALL, functionId, code);
						encodeInst(WasmOpcode::DROP, code);
						// NOTE: Cannot tail call, the return type is different
						if(useTailCall)
							encodeInst(WasmOpcode::RETURN, code);
						return true;
					}
					case Intrinsic::cheerp_allocate:
					case Intrinsic::cheerp_allocate_array:
					{
						calledFunc = module.getFunction("malloc");
						if (!calledFunc)
							llvm::report_fatal_error("missing malloc definition");
						break;
					}
					case Intrinsic::cheerp_reallocate:
					{
						calledFunc = module.getFunction("realloc");
						if (!calledFunc)
							llvm::report_fatal_error("missing realloc definition");
						break;
					}
					case Intrinsic::cheerp_deallocate:
					{
						calledFunc = module.getFunction("free");
						if (!calledFunc)
							llvm::report_fatal_error("missing free definition");
						break;
					}
					case Intrinsic::ctlz:
					case Intrinsic::cttz:
					case Intrinsic::fabs:
					case Intrinsic::ceil:
					case Intrinsic::floor:
					case Intrinsic::trunc:
					case Intrinsic::minnum:
					case Intrinsic::maxnum:
					case Intrinsic::copysign:
					case Intrinsic::sqrt:
					{
						//Handled below inside if (isWasmIntrinsic(calledFunction))
						break;
					}
					case Intrinsic::cos:
					case Intrinsic::exp:
					case Intrinsic::log:
					case Intrinsic::pow:
					case Intrinsic::sin:
					{
						if (globalDeps.getMathMode() != GlobalDepsAnalyzer::WASM_BUILTINS)
						{
							// Handled below
							break;
						}
						[[clang::fallthrough]];
					}
					default:
					{
						unsigned intrinsic = calledFunc->getIntrinsicID();
#ifndef NDEBUG
						if (intrinsic != Intrinsic::not_intrinsic)
							ci.dump();
#endif
						assert(intrinsic == Intrinsic::not_intrinsic);
					}
					break;
				}

				if (globalDeps.getMathMode() == GlobalDepsAnalyzer::WASM_BUILTINS)
				{
					StringRef ident = calledFunc->getName();
					BuiltinInstr::BUILTIN b = BuiltinInstr::BUILTIN::NONE;
					if(ident=="acos" || ident=="acosf")
					{
						b = BuiltinInstr::BUILTIN::ACOS_F;
					}
					else if(ident=="asin" || ident=="asinf")
					{
						b = BuiltinInstr::BUILTIN::ASIN_F;
					}
					else if(ident=="atan" || ident=="atanf")
					{
						b = BuiltinInstr::BUILTIN::ATAN_F;
					}
					else if(ident=="atan2" || ident=="atan2f")
					{
						b = BuiltinInstr::BUILTIN::ATAN2_F;
					}
					else if(ident=="cos" || ident=="cosf" || intrinsicId==Intrinsic::cos)
					{
						b = BuiltinInstr::BUILTIN::COS_F;
					}
					else if(ident=="exp" || ident=="expf" || intrinsicId==Intrinsic::exp)
					{
						b = BuiltinInstr::BUILTIN::EXP_F;
					}
					else if(ident=="log" || ident=="logf" || intrinsicId==Intrinsic::log)
					{
						b = BuiltinInstr::BUILTIN::LOG_F;
					}
					else if(ident=="pow" || ident=="powf" || intrinsicId==Intrinsic::pow)
					{
						b = BuiltinInstr::BUILTIN::POW_F;
					}
					else if(ident=="sin" || ident=="sinf" || intrinsicId==Intrinsic::sin)
					{
						b = BuiltinInstr::BUILTIN::SIN_F;
					}
					else if(ident=="tan" || ident=="tanf")
					{
						b = BuiltinInstr::BUILTIN::TAN_F;
					}

					if (b == BuiltinInstr::BUILTIN::SIN_F ||
						b == BuiltinInstr::BUILTIN::COS_F)
						b = BuiltinInstr::BUILTIN::NONE;

					if(b != BuiltinInstr::BUILTIN::NONE)
					{
						// We will use a builtin, do float conversion if needed
						bool floatType = calledFunc->getReturnType()->isFloatTy();
						for (auto op = ci.op_begin(); op != ci.op_begin() + fTy->getNumParams(); ++op)
						{
							compileOperand(code, op->get());
							if(floatType)
								encodeInst(WasmOpcode::F64_PROMOTE_F32, code);
						}
						uint32_t importedId = linearHelper.getBuiltinId(b);
						assert(importedId);
						encodeInst(WasmU32Opcode::CALL, importedId, code);
						if(floatType)
							encodeInst(WasmOpcode::F32_DEMOTE_F64, code);
						// TODO: We could tail call if the type matches
						if(useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
				}
			}

			//This corrections is needed basically for ctlz / cttz since they have an extra parameters to be ignored
			const unsigned int numUsedParameters = fTy->getNumParams() - TypedBuiltinInstr::numExtraParameters(calledFunc);
			for (auto op = ci.op_begin();
					op != ci.op_begin() + numUsedParameters; ++op)
			{
				compileOperand(code, op->get());
			}

			if (calledFunc)
			{
				if (TypedBuiltinInstr::isWasmIntrinsic(calledFunc))
				{
					encodeWasmIntrinsic(code, calledFunc);
					if(useTailCall)
						encodeInst(WasmOpcode::RETURN, code);
					return useTailCall;
				}
				else if (linearHelper.getFunctionIds().count(calledFunc))
				{
					uint32_t functionId = linearHelper.getFunctionIds().at(calledFunc);
					if (functionId < COMPILE_METHOD_LIMIT) {
						if(useTailCall)
							encodeInst(WasmU32Opcode::RETURN_CALL, functionId, code);
						else
							encodeInst(WasmU32Opcode::CALL, functionId, code);
					} else {
						encodeInst(WasmOpcode::UNREACHABLE, code);
						// Make sure that we leave a value on the stack anyway (old Edge's validation get unhappy otherwise)
						if(!fTy->getReturnType()->isVoidTy())
							compileTypedZero(code, fTy->getReturnType());
					}
				}
				else
				{
					llvm::errs() << "warning: Undefined function " << calledFunc->getName() << " called\n";
					encodeInst(WasmOpcode::UNREACHABLE, code);
					return true;
				}
			}
			else
			{
				if (linearHelper.getFunctionTables().count(fTy))
				{
					const auto& table = linearHelper.getFunctionTables().at(fTy);
					compileOperand(code, calledValue);
					if(useTailCall)
						encodeInst(WasmU32U32Opcode::RETURN_CALL_INDIRECT, table.typeIndex, 0, code);
					else
						encodeInst(WasmU32U32Opcode::CALL_INDIRECT, table.typeIndex, 0, code);
				}
				else
				{
					encodeInst(WasmOpcode::UNREACHABLE, code);
					if(!fTy->getReturnType()->isVoidTy())
						compileTypedZero(code, fTy->getReturnType());
					return true;
				}
			}

			if(ci.getType()->isVoidTy())
				return true;
			break;
		}
		case Instruction::FCmp:
		{
			const CmpInst& ci = cast<CmpInst>(I);
			compileFCmp(ci.getOperand(0), ci.getOperand(1), ci.getPredicate(), code);
			break;
		}
		case Instruction::FRem:
		{
			// No FRem in wasm, implement manually
			// frem x, y -> fsub (x, fmul( ftrunc ( fdiv (x, y) ), y ) )
			compileOperand(code, I.getOperand(0));
			compileOperand(code, I.getOperand(0));
			compileOperand(code, I.getOperand(1));
#define BINOPF(name) \
			if (I.getType()->isFloatTy()) { \
				encodeInst(WasmOpcode::F32_##name, code); \
			} \
			else if (I.getType()->isDoubleTy()) { \
				encodeInst(WasmOpcode::F64_##name, code); \
			} else { \
				llvm_unreachable("unsupported type"); \
			}
			BINOPF(  DIV)
			BINOPF(TRUNC)
			compileOperand(code, I.getOperand(1));
			BINOPF(  MUL)
			BINOPF(  SUB)
#undef BINOPF
			break;
		}
		case Instruction::GetElementPtr:
		{
			compileGEP(code, &I);
			break;
		}
		case Instruction::ICmp:
		{
			const ICmpInst& ci = cast<ICmpInst>(I);
			ICmpInst::Predicate p = ci.getPredicate();
			compileICmp(ci, p, code);
			break;
		}
		case Instruction::Load:
		{
			const LoadInst& li = cast<LoadInst>(I);
			if(GlobalVariable* ptrGV = dyn_cast<GlobalVariable>(li.getOperand(0)))
			{
				auto it = globalizedGlobalsIDs.find(ptrGV);
				if(it != globalizedGlobalsIDs.end())
				{
					// We can encode this as a get_global
					encodeInst(WasmU32Opcode::GET_GLOBAL, it->second, code);
					break;
				}
			}
			compileLoad(code, li, /*signExtend*/isSignedLoad(&li));
			break;
		}
		case Instruction::PtrToInt:
		{
			compileOperand(code, I.getOperand(0));
			if (I.getType()->isIntegerTy(64))
				encodeInst(WasmOpcode::I64_EXTEND_I32_U, code);
			break;
		}
		case Instruction::Store:
		{
			const StoreInst& si = cast<StoreInst>(I);
			const Value* ptrOp=si.getPointerOperand();
			const Value* valOp=si.getValueOperand();
			if(const GlobalVariable* ptrGV = dyn_cast<GlobalVariable>(ptrOp))
			{
				auto it = globalizedGlobalsIDs.find(ptrGV);
				if(it != globalizedGlobalsIDs.end())
				{
					// We can encode this as a set_global
					compileOperand(code, valOp);
					encodeInst(WasmU32Opcode::SET_GLOBAL, it->second, code);
					break;
				}
			}
			// 1) The pointer
			uint32_t offset = compileLoadStorePointer(code, ptrOp);
			// Special case writing 0 to floats/double
			if(valOp->getType()->isFloatingPointTy() && isa<Constant>(valOp) && cast<Constant>(valOp)->isNullValue())
			{
				if(valOp->getType()->isFloatTy())
				{
					encodeInst(WasmS32Opcode::I32_CONST, 0, code);
					encodeInst(WasmU32U32Opcode::I32_STORE, 0x2, offset, code);
				}
				else
				{
					assert(valOp->getType()->isDoubleTy());
					encodeInst(WasmS64Opcode::I64_CONST, 0, code);
					encodeInst(WasmU32U32Opcode::I64_STORE, 0x3, offset, code);
				}
				break;
			}
			// 2) The value
			compileOperand(code, valOp);
			// 3) Store
			// When storing values with size less than 32-bit we need to truncate them
			if(valOp->getType()->isIntegerTy())
			{
				uint32_t bitWidth = valOp->getType()->getIntegerBitWidth();
				if(bitWidth == 1)
					bitWidth = 8;

				// TODO add support for i64.
				switch (bitWidth)
				{
					case 8:
						encodeInst(WasmU32U32Opcode::I32_STORE8, 0x0, offset, code);
						break;
					case 16:
						encodeInst(WasmU32U32Opcode::I32_STORE16, 0x1, offset, code);
						break;
					case 32:
						encodeInst(WasmU32U32Opcode::I32_STORE, 0x2, offset, code);
						break;
					case 64:
						encodeInst(WasmU32U32Opcode::I64_STORE, 0x2, offset, code);
						break;
					default:
						llvm::errs() << "bit width: " << bitWidth << '\n';
						llvm_unreachable("unknown integer bit width");
				}
			} else {
				if (valOp->getType()->isFloatTy())
					encodeInst(WasmU32U32Opcode::F32_STORE, 0x2, offset, code);
				else if (valOp->getType()->isDoubleTy())
					encodeInst(WasmU32U32Opcode::F64_STORE, 0x3, offset, code);
				else
					encodeInst(WasmU32U32Opcode::I32_STORE, 0x2, offset, code);
			}
			break;
		}
		case Instruction::Switch:
			break;
		case Instruction::Trunc:
		{
			compileOperand(code, I.getOperand(0));
			if (I.getOperand(0)->getType()->isIntegerTy(64))
				encodeInst(WasmOpcode::I32_WRAP_I64, code);
			break;
		}
		case Instruction::Ret:
		{
			const ReturnInst& ri = cast<ReturnInst>(I);
			Value* retVal = ri.getReturnValue();
			if(retVal)
			{
				// NOTE: If the retValue is inlineable we must render it here
				//       If 'isReturnPartOfTailCall' return true then retVal must be a CallInst
				//       so blindly casting it to Instruction is safe
				if(isReturnPartOfTailCall(ri) && !isInlineable(*cast<Instruction>(retVal)))
					break;
				compileOperand(code, I.getOperand(0));
			}
			break;
		}
		case Instruction::Select:
		{
			const SelectInst& si = cast<SelectInst>(I);
			compileOperand(code, si.getTrueValue());
			compileOperand(code, si.getFalseValue());
			compileCondition(code, si.getCondition(), /*booleanInvert*/false);
			encodeInst(WasmOpcode::SELECT, code);
			break;
		}
		case Instruction::SExt:
		{
			const Value* op = I.getOperand(0);
			compileOperand(code, op);
			if(!isSignedLoad(op))
			{
				uint32_t bitWidth = I.getOperand(0)->getType()->getIntegerBitWidth();
				if (bitWidth < 32)
				{
					encodeInst(WasmS32Opcode::I32_CONST, 32-bitWidth, code);
					encodeInst(WasmOpcode::I32_SHL, code);
					encodeInst(WasmS32Opcode::I32_CONST, 32-bitWidth, code);
					encodeInst(WasmOpcode::I32_SHR_S, code);
				}
			}
			// TODO convert directly to i64 without passing from i32
			if (I.getType()->isIntegerTy(64))
				encodeInst(WasmOpcode::I64_EXTEND_S_I32, code);
			break;
		}
		case Instruction::FPToSI:
		{
			// Wasm opcodes traps on invalid values, we need to do an explicit check if requested
			if(!AvoidWasmTraps)
			{
				compileOperand(code, I.getOperand(0));
				if(I.getOperand(0)->getType()->isFloatTy())
				{
					if (I.getType()->isIntegerTy(64))
						encodeInst(WasmOpcode::I64_TRUNC_S_F32, code);
					else
						encodeInst(WasmOpcode::I32_TRUNC_S_F32, code);
				}
				else
				{
					if (I.getType()->isIntegerTy(64))
						encodeInst(WasmOpcode::I64_TRUNC_S_F64, code);
					else
						encodeInst(WasmOpcode::I32_TRUNC_S_F64, code);
				}
			}
			else if (I.getOperand(0)->getType()->isFloatTy())
			{
				int bits = I.getType()->getIntegerBitWidth();
				float threshold = bits == 64 ? (1ull << 63) : (1ull << 31);
				int typeVal = bits == 64 ? 0x7e : 0x7f;

				compileOperand(code, I.getOperand(0));
				encodeInst(WasmOpcode::F32_ABS, code);
				encodeInst(WasmOpcode::F32_CONST, code);
				encodeF32(threshold, code);
				// Use LT here, we are using the first invalid positive integer as the limit value
				encodeInst(WasmOpcode::F32_LT, code);
				encodeInst(WasmU32Opcode::IF, typeVal, code);
				compileOperand(code, I.getOperand(0));
				if (bits == 64)
					encodeInst(WasmOpcode::I64_TRUNC_S_F32, code);
				else
					encodeInst(WasmOpcode::I32_TRUNC_S_F32, code);
				encodeInst(WasmOpcode::ELSE, code);
				// We excluded the valid INT32_MIN in the range above, but in the undefined case we use it unconditionally
				if (bits == 64)
					encodeInst(WasmS64Opcode::I64_CONST, INT64_MIN, code);
				else
					encodeInst(WasmS32Opcode::I32_CONST, INT32_MIN, code);
				encodeInst(WasmOpcode::END, code);
			}
			else
			{
				int bits = I.getType()->getIntegerBitWidth();
				float threshold = bits == 64 ? (1ull << 63) : (1ull << 31);
				int typeVal = bits == 64 ? 0x7e : 0x7f;

				compileOperand(code, I.getOperand(0));
				encodeInst(WasmOpcode::F64_ABS, code);
				encodeInst(WasmOpcode::F32_CONST, code);
				encodeF32(threshold, code);
				encodeInst(WasmOpcode::F64_PROMOTE_F32, code);
				// Use LT here, we are using the first invalid positive integer as the limit value
				encodeInst(WasmOpcode::F64_LT, code);
				encodeInst(WasmU32Opcode::IF, typeVal, code);
				compileOperand(code, I.getOperand(0));
				if (bits == 64)
					encodeInst(WasmOpcode::I64_TRUNC_S_F64, code);
				else
					encodeInst(WasmOpcode::I32_TRUNC_S_F64, code);
				encodeInst(WasmOpcode::ELSE, code);
				// We excluded the valid INT32_MIN in the range above, but in the undefined case we use it unconditionally
				if (bits == 64)
					encodeInst(WasmS64Opcode::I64_CONST, INT64_MIN, code);
				else
					encodeInst(WasmS32Opcode::I32_CONST, INT32_MIN, code);
				encodeInst(WasmOpcode::END, code);
			}
			break;
		}
		case Instruction::FPToUI:
		{
			// Wasm opcodes traps on invalid values, we need to do an explicit check if requested
			if(!AvoidWasmTraps)
			{
				compileOperand(code, I.getOperand(0));
				if(I.getOperand(0)->getType()->isFloatTy())
				{
					if (I.getType()->isIntegerTy(64))
						encodeInst(WasmOpcode::I64_TRUNC_U_F32, code);
					else
						encodeInst(WasmOpcode::I32_TRUNC_U_F32, code);
				}
				else
				{
					if (I.getType()->isIntegerTy(64))
						encodeInst(WasmOpcode::I64_TRUNC_U_F64, code);
					else
						encodeInst(WasmOpcode::I32_TRUNC_U_F64, code);
				}
			}
			else if (I.getOperand(0)->getType()->isFloatTy())
			{
				int bits = I.getType()->getIntegerBitWidth();
				float threshold = bits == 64 ? 18446744073709551616.0 : 4294967296.0;
				int typeVal = bits == 64 ? 0x7e : 0x7f;

				compileOperand(code, I.getOperand(0));
				encodeInst(WasmOpcode::F32_CONST, code);
				encodeF32(threshold, code);
				// Use LT here, we are using the first invalid positive integer as the limit value
				encodeInst(WasmOpcode::F32_LT, code);
				// Also compare against 0
				compileOperand(code, I.getOperand(0));
				encodeInst(WasmOpcode::F32_CONST, code);
				encodeF32(0, code);
				encodeInst(WasmOpcode::F32_GE, code);
				encodeInst(WasmOpcode::I32_AND, code);
				encodeInst(WasmU32Opcode::IF, typeVal, code);
				compileOperand(code, I.getOperand(0));
				if (bits == 64)
					encodeInst(WasmOpcode::I64_TRUNC_U_F32, code);
				else
					encodeInst(WasmOpcode::I32_TRUNC_U_F32, code);
				encodeInst(WasmOpcode::ELSE, code);
				if (bits == 64)
					encodeInst(WasmS64Opcode::I64_CONST, 0, code);
				else
					encodeInst(WasmS32Opcode::I32_CONST, 0, code);
				encodeInst(WasmOpcode::END, code);
			}
			else
			{
				int bits = I.getType()->getIntegerBitWidth();
				float threshold = bits == 64 ? 18446744073709551616.0 : 4294967296.0;
				int typeVal = bits == 64 ? 0x7e : 0x7f;

				compileOperand(code, I.getOperand(0));
				encodeInst(WasmOpcode::F32_CONST, code);
				encodeF32(threshold, code);
				encodeInst(WasmOpcode::F64_PROMOTE_F32, code);
				// Use LT here, we are using the first invalid positive integer as the limit value
				encodeInst(WasmOpcode::F64_LT, code);
				// Also compare against 0
				compileOperand(code, I.getOperand(0));
				encodeInst(WasmOpcode::F32_CONST, code);
				encodeF32(0, code);
				encodeInst(WasmOpcode::F64_PROMOTE_F32, code);
				encodeInst(WasmOpcode::F64_GE, code);
				encodeInst(WasmOpcode::I32_AND, code);
				encodeInst(WasmU32Opcode::IF, typeVal, code);
				compileOperand(code, I.getOperand(0));
				if (bits == 64)
					encodeInst(WasmOpcode::I64_TRUNC_U_F64, code);
				else
					encodeInst(WasmOpcode::I32_TRUNC_U_F64, code);
				encodeInst(WasmOpcode::ELSE, code);
				if (bits == 64)
					encodeInst(WasmS64Opcode::I64_CONST, 0, code);
				else
					encodeInst(WasmS32Opcode::I32_CONST, 0, code);
				encodeInst(WasmOpcode::END, code);
			}
			break;
		}
		case Instruction::SIToFP:
		{
			assert(I.getOperand(0)->getType()->isIntegerTy());
			compileOperand(code, I.getOperand(0));
			uint32_t bitWidth = I.getOperand(0)->getType()->getIntegerBitWidth();
			if(bitWidth < 32)
			{
				// Sign extend
				encodeInst(WasmS32Opcode::I32_CONST, 32-bitWidth, code);
				encodeInst(WasmOpcode::I32_SHL, code);
				encodeInst(WasmS32Opcode::I32_CONST, 32-bitWidth, code);
				encodeInst(WasmOpcode::I32_SHR_S, code);
			}
			if (I.getType()->isDoubleTy()) {
				if (bitWidth == 64)
					encodeInst(WasmOpcode::F64_CONVERT_S_I64, code);
				else
					encodeInst(WasmOpcode::F64_CONVERT_S_I32, code);
			} else {
				assert(I.getType()->isFloatTy());
				if (bitWidth == 64)
					encodeInst(WasmOpcode::F32_CONVERT_S_I64, code);
				else
					encodeInst(WasmOpcode::F32_CONVERT_S_I32, code);
			}
			break;
		}
		case Instruction::UIToFP:
		{
			assert(I.getOperand(0)->getType()->isIntegerTy());
			compileOperand(code, I.getOperand(0));
			uint32_t bitWidth = I.getOperand(0)->getType()->getIntegerBitWidth();
			if(bitWidth < 32)
			{
				encodeInst(WasmS32Opcode::I32_CONST, getMaskForBitWidth(bitWidth), code);
				encodeInst(WasmOpcode::I32_AND, code);
			}
			if (I.getType()->isDoubleTy()) {
				if (bitWidth == 64)
					encodeInst(WasmOpcode::F64_CONVERT_U_I64, code);
				else
					encodeInst(WasmOpcode::F64_CONVERT_U_I32, code);
			} else {
				assert(I.getType()->isFloatTy());
				if (bitWidth == 64)
					encodeInst(WasmOpcode::F32_CONVERT_U_I64, code);
				else
					encodeInst(WasmOpcode::F32_CONVERT_U_I32, code);
			}
			break;
		}
		case Instruction::FPTrunc:
		{
			assert(I.getType()->isFloatTy());
			assert(I.getOperand(0)->getType()->isDoubleTy());
			compileOperand(code, I.getOperand(0));
			encodeInst(WasmOpcode::F32_DEMOTE_F64, code);
			break;
		}
		case Instruction::FPExt:
		{
			assert(I.getType()->isDoubleTy());
			assert(I.getOperand(0)->getType()->isFloatTy());
			compileOperand(code, I.getOperand(0));
			encodeInst(WasmOpcode::F64_PROMOTE_F32, code);
			break;
		}
		case Instruction::ZExt:
		{
			compileUnsignedInteger(code, I.getOperand(0));
			if (I.getType()->isIntegerTy(64))
				encodeInst(WasmOpcode::I64_EXTEND_I32_U, code);
			break;
		}
		case Instruction::IntToPtr:
		{
			compileOperand(code, I.getOperand(0));
			if (I.getType()->isIntegerTy(64))
				encodeInst(WasmOpcode::I64_EXTEND_I32_U, code);
			break;
		}
		case Instruction::Unreachable:
		{
			encodeInst(WasmOpcode::UNREACHABLE, code);
			break;
		}
		default:
		{
#ifndef NDEBUG
			I.dump();
#endif
			llvm::errs() << "\tImplement inst " << I.getOpcodeName() << '\n';
		}
	}
	return false;
}

void CheerpWasmWriter::compileInstructionAndSet(WasmBuffer& code, const llvm::Instruction& I)
{
	if (compiled.count(&I) || I.getParent() != currentBB)
		return;
	if (isa<PHINode>(&I) || isInlineable(I))
		return;
	if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(&I))
	{
		//Skip some kind of intrinsics
		if(II->getIntrinsicID()==Intrinsic::lifetime_start ||
			II->getIntrinsicID()==Intrinsic::lifetime_end ||
			II->getIntrinsicID()==Intrinsic::dbg_declare ||
			II->getIntrinsicID()==Intrinsic::dbg_value ||
			II->getIntrinsicID()==Intrinsic::dbg_label ||
			II->getIntrinsicID()==Intrinsic::assume)
		{
			return;
		}
	}

	const bool needsSubStack = teeLocals.needsSubStack(code);

	if (needsSubStack)
		teeLocals.addIndentation(code);

	auto lastUsedCandidate = teeLocals.lastUsed();

	flushMemoryDependencies(code, I);

	assert(compiled.count(&I) == 0);
	compiled.insert(&I);
	const bool ret = compileInstruction(code, I);

	flushSetLocalDependencies(code, I);

	teeLocals.removeConsumed(lastUsedCandidate);

	if (needsSubStack)
		teeLocals.decreaseIndentation(code, /*performCheck*/false);

	if(!ret && !I.getType()->isVoidTy())
	{
		if(I.use_empty()) {
			encodeInst(WasmOpcode::DROP, code);
		} else {
			uint32_t reg = registerize.getRegisterId(&I, edgeContext);
			uint32_t local = localMap.at(reg);
			teeLocals.addCandidate(&I, /*isInstructionAssigment*/true, local, code.tell());
			encodeInst(WasmU32Opcode::SET_LOCAL, local, code);
		}
	}
	teeLocals.instructionStart(code);
}

bool CheerpWasmWriter::shouldDefer(const llvm::Instruction* I) const
{
	// Figure out if we can defer this instruction for a gain
	// Must have a user in the same BB (this also automatically deals with instruction without users
	bool hasUserInSameBlock = false;
	for(const User* u: I->users())
	{
		if(cast<Instruction>(u)->getParent() == currentBB)
		{
			hasUserInSameBlock = true;
			break;
		}
	}
	return !hasUserInSameBlock;
}

void CheerpWasmWriter::compileBB(WasmBuffer& code, const BasicBlock& BB, const PHINode* phiHandledAsResult)
{
	assert(localsDependencies.empty());
	assert(memoryDependencies.empty());
	assert(!currentBB);
	currentBB = &BB;
	assert(deferred.empty());
	BasicBlock::const_iterator I=BB.begin();
	BasicBlock::const_iterator IE=BB.end();
	const llvm::Instruction* lastStoreLike = nullptr;
	std::vector<const llvm::Instruction*> instructionsLoadLike;
	llvm::DenseMap<uint32_t, std::vector<const Instruction*>> getLocalFromRegister;
	llvm::DenseMap<uint32_t, const Instruction*> lastAssignedToRegister;

	if (phiHandledAsResult)
	{
		//Here we need to assign the PHI handled via Wasm's generalized block results

		const uint32_t reg = registerize.getRegisterId(phiHandledAsResult, EdgeContext::emptyContext());
		const uint32_t local = localMap.at(reg);
		teeLocals.addCandidate(phiHandledAsResult, /*isInstructionAssigment*/true, local, code.tell());
		encodeInst(WasmU32Opcode::SET_LOCAL, local, code);

		teeLocals.instructionStart(code);
	}

	for(;I!=IE;++I)
	{
		//Calculate dependencies for each get local in a tree of inlineable instructions
		if(I->getOpcode()!=Instruction::PHI)
		{
			std::vector<const llvm::Instruction*> queue;
			queue.push_back(&*I);
			while (!queue.empty())
			{
				const llvm::Instruction* curr = queue.back();
				queue.pop_back();
				for (const auto& op : curr->operands())
				{
					if (!isa<Instruction>(op))
						continue;
					const llvm::Instruction* next = cast<Instruction>(op);
					if (registerize.hasRegister(next))
					{
						const uint32_t ID = registerize.getRegisterId(next, edgeContext);
						if (lastAssignedToRegister.count(ID))
							localsDependencies[&*I].insert(lastAssignedToRegister[ID]);
						getLocalFromRegister[ID].push_back(&*I);
					}
					else
						queue.push_back(next);
				}
			}
		}

		//Calculate dependencies for a setLocal, that is all getLocal done on the previously set setLocal for the same ID
		//Note that this HAS to be performed also for PHI
		if (registerize.hasRegister(&*I))
		{
			assert(!isInlineable(*I));

			const uint32_t ID = registerize.getRegisterId(&*I, edgeContext);

			std::vector<const llvm::Instruction*> queue(getLocalFromRegister[ID].begin(), getLocalFromRegister[ID].end());
			while (!queue.empty())
			{
				const llvm::Instruction* curr = queue.back();
				queue.pop_back();
				if (!isInlineable(*curr))
					localsDependencies[&*I].insert(curr);
				else
				{
					for (const User* User : curr->users())
					{
						const llvm::Instruction* next = cast<Instruction>(User);
						if (!isa<PHINode>(next) && next->getParent() == currentBB)
							queue.push_back(next);
					}
				}
			}
			getLocalFromRegister[ID].clear();

			lastAssignedToRegister[ID] = &*I;
		}

		if(I->getOpcode()==Instruction::PHI)
		{
			//Phis are manually handled
			continue;
		}
		if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(&(*I)))
		{
			//Skip some kind of intrinsics
			if(II->getIntrinsicID()==Intrinsic::lifetime_start ||
				II->getIntrinsicID()==Intrinsic::lifetime_end ||
				II->getIntrinsicID()==Intrinsic::dbg_declare ||
				II->getIntrinsicID()==Intrinsic::dbg_value ||
				II->getIntrinsicID()==Intrinsic::assume)
			{
				continue;
			}
		}

		if (!isInlineable(*I))
		{
			deferred.push_back(&*I);

			bool mayHaveSideEffects = I->mayHaveSideEffects();
			bool mayReadFromMemory = I->mayReadFromMemory();
			std::vector<const Instruction*> queue;
			for (auto & op : I->operands())
				if (isa<Instruction>(op))
					queue.push_back(cast<Instruction>(op));
			while (!queue.empty())
			{
				const Instruction* curr = queue.back();
				queue.pop_back();
				if (!isInlineable(*curr))
					continue;
				if (curr->mayReadFromMemory())
					mayReadFromMemory = true;
				if (curr->mayHaveSideEffects())
					mayHaveSideEffects = true;
				for (auto & op : curr->operands())
					if (isa<Instruction>(op))
						queue.push_back(cast<Instruction>(op));
			}

			if (mayHaveSideEffects)
			{
				assert(isInlineable(*I) == false);
				if (lastStoreLike)
					memoryDependencies[&*I].insert(lastStoreLike);
				lastStoreLike = &*I;
				for (auto& x: instructionsLoadLike)
					memoryDependencies[&*I].insert(x);
				instructionsLoadLike.clear();
			}
			else if (mayReadFromMemory)
			{
				instructionsLoadLike.push_back(&*I);
				if (lastStoreLike)
					memoryDependencies[&*I].insert(lastStoreLike);
			}
		}
	}

	checkAndSanitizeDependencies(memoryDependencies);
	checkAndSanitizeDependencies(localsDependencies);

#ifdef STRESS_DEFERRED
	reverse(deferred.begin(), deferred.end()-1);
#endif
	renderDeferred(code, deferred);

	deferred.clear();
	currentBB = nullptr;
	localsDependencies.clear();
	memoryDependencies.clear();
}

void CheerpWasmWriter::renderDeferred(WasmBuffer& code, const vector<const llvm::Instruction*>& deferred)
{
	for (const llvm::Instruction* I : deferred)
	{
		if (shouldDefer(I))
			compileInstructionAndSet(code, *I);
	}
	for (const llvm::Instruction* I : deferred)
		compileInstructionAndSet(code, *I);
}

void CheerpWasmWriter::compileMethodLocals(WasmBuffer& code, const vector<int>& locals)
{
	uint32_t groups = (uint32_t) locals.at(Registerize::INTEGER) > 0;
	groups += (uint32_t) locals.at(Registerize::INTEGER64) > 0;
	groups += (uint32_t) locals.at(Registerize::DOUBLE) > 0;
	groups += (uint32_t) locals.at(Registerize::FLOAT) > 0;
	groups += (uint32_t) locals.at(Registerize::OBJECT) > 0;

	// Local declarations are compressed into a vector whose entries
	// consist of:
	//
	//   - a u32 `count',
	//   - a `ValType',
	//
	// denoting `count' locals of the same `ValType'.
	encodeULEB128(groups, code);

	if (locals.at(Registerize::INTEGER)) {
		encodeULEB128(locals.at(Registerize::INTEGER), code);
		encodeRegisterKind(Registerize::INTEGER, code);
	}

	if (locals.at(Registerize::INTEGER64)) {
		encodeULEB128(locals.at(Registerize::INTEGER64), code);
		encodeRegisterKind(Registerize::INTEGER64, code);
	}

	if (locals.at(Registerize::DOUBLE)) {
		encodeULEB128(locals.at(Registerize::DOUBLE), code);
		encodeRegisterKind(Registerize::DOUBLE, code);
	}

	if (locals.at(Registerize::FLOAT)) {
		encodeULEB128(locals.at(Registerize::FLOAT), code);
		encodeRegisterKind(Registerize::FLOAT, code);
	}

	if (locals.at(Registerize::OBJECT)) {
		encodeULEB128(locals.at(Registerize::OBJECT), code);
		encodeRegisterKind(Registerize::OBJECT, code);
	}
}

void CheerpWasmWriter::compileMethodParams(WasmBuffer& code, const FunctionType* fTy)
{
	uint32_t numArgs = fTy->getNumParams();
		encodeULEB128(numArgs, code);

	for(uint32_t i = 0; i < numArgs; i++)
		encodeValType(fTy->getParamType(i), code);
}

void CheerpWasmWriter::compileMethodResult(WasmBuffer& code, const Type* ty)
{
	if (ty->isVoidTy())
	{
		encodeULEB128(0, code);
	}
	else
	{
		encodeULEB128(1, code);
		encodeValType(ty, code);
	}
}

void CheerpWasmWriter::compileCondition(WasmBuffer& code, const llvm::Value* cond, bool booleanInvert)
{
	bool canInvertCond = isa<Instruction>(cond) && isInlineable(*cast<Instruction>(cond));

	if(canInvertCond && isa<ICmpInst>(cond))
	{
		const ICmpInst* ci = cast<ICmpInst>(cond);
		CmpInst::Predicate p = ci->getPredicate();
		if(booleanInvert)
			p = CmpInst::getInversePredicate(p);
		Value* op0 = ci->getOperand(0);
		Value* op1 = ci->getOperand(1);
		if(ci->isCommutative() && isa<Constant>(op0))
		{
			// Move the constant on op1 to simplify the logic below
			std::swap(op0, op1);
		}
		// Optimize "if (a != 0)" to "if (a)" and "if (a == 0)" to "if (!a)".
		if ((p == CmpInst::ICMP_NE || p == CmpInst::ICMP_EQ) &&
				isa<Constant>(op1) &&
				cast<Constant>(op1)->isNullValue() &&
				!op0->getType()->isIntegerTy(64))
		{
			if(op0->getType()->isPointerTy())
				compileOperand(code, op0);
			else if(op0->getType()->isIntegerTy(32))
				compileSignedInteger(code, op0, /*forComparison*/true);
			else
				compileUnsignedInteger(code, op0);
			if(p == CmpInst::ICMP_EQ)
				encodeInst(WasmOpcode::I32_EQZ, code);
			teeLocals.removeConsumed();
			return;
		}
		compileICmp(op0, op1, p, code);
	}
	else if(canInvertCond && isa<FCmpInst>(cond))
	{
		const CmpInst* ci = cast<CmpInst>(cond);
		CmpInst::Predicate p = ci->getPredicate();
		if(booleanInvert)
			p = CmpInst::getInversePredicate(p);
		compileFCmp(ci->getOperand(0), ci->getOperand(1), p, code);
	}
	else
	{
		compileOperand(code, cond);
		if (booleanInvert) {
			// Invert result
			encodeInst(WasmOpcode::I32_EQZ, code);
		}
	}
	teeLocals.removeConsumed();
}

void CheerpWasmWriter::compileBranchTable(WasmBuffer& code, const llvm::SwitchInst* si,
	const std::vector<std::pair<int, int>>& cases)
{
	assert(si->getNumCases());

	uint32_t bitWidth = si->getCondition()->getType()->getIntegerBitWidth();

	auto getCaseValue = [](const ConstantInt* c, uint32_t bitWidth) -> int64_t
	{
		return bitWidth == 32 ? c->getSExtValue() : c->getZExtValue();
	};

	llvm::BasicBlock* defaultDest = si->getDefaultDest();
	int64_t max = std::numeric_limits<int64_t>::min();
	int64_t min = std::numeric_limits<int64_t>::max();
	for (auto& c: si->cases())
	{
		if (c.getCaseSuccessor() == defaultDest)
			continue;
		int64_t curr = getCaseValue(c.getCaseValue(), bitWidth);
		max = std::max(max, curr);
		min = std::min(min, curr);
	}

	// There should be at least one default case and zero or more cases.
	uint32_t depth = max - min + 1;
	assert(depth >= 1);

	// Fill the jump table.
	std::vector<uint32_t> table;
	table.assign(depth, numeric_limits<uint32_t>::max());
	uint32_t defaultIdx = numeric_limits<uint32_t>::max();

	for (auto c: cases)
	{
		if (c.first == 0)
			defaultIdx = c.second;
		else
		{
			// The value to match for case `i` has index `2*i`
			auto cv = cast<ConstantInt>(si->getOperand(2*c.first));
			table.at(getCaseValue(cv, bitWidth) - min) = c.second;
		}
	}

	// Elements that are not set, will jump to the default block.
	std::replace(table.begin(), table.end(), numeric_limits<uint32_t>::max(),
		defaultIdx);

	// Print the condition
	compileOperand(code, si->getCondition());
	if (min != 0)
	{
		encodeInst(WasmS32Opcode::I32_CONST, min, code);
		encodeInst(WasmOpcode::I32_SUB, code);
	}
	if (bitWidth != 32 && CheerpWriter::needsUnsignedTruncation(si->getCondition(), /*asmjs*/true))
	{
		assert(bitWidth < 32);
		encodeInst(WasmS32Opcode::I32_CONST, getMaskForBitWidth(bitWidth), code);
		encodeInst(WasmOpcode::I32_AND, code);
	}

	// Print the case labels and the default label.
	encodeBranchTable(code, table, defaultIdx);
}

static int getResultKind(const Token& T)
{
	const int kind = T.getResultType();

	//getResultType will map to the proper kind only for
	//	NONE -> 0x40 and {I32, I64, F32 and F64} -> {0x7F, 0x7E, 0x7D, 0x7C}
	if (kind != 0x40 && (kind > 0x7F || kind < 0x7C))
		llvm_unreachable("Unexpected result type");

	//currently anyref or multi-values are not handled

	return kind;
}

const BasicBlock* CheerpWasmWriter::compileTokens(WasmBuffer& code,
	const TokenList& Tokens)
{
	std::vector<const Token*> ScopeStack;
	const BasicBlock* lastDepth0Block = nullptr;
	auto getDepth = [&](const Token* Scope)
	{
		Scope = Scope->getKind() == Token::TK_Loop ? Scope : Scope->getMatch();
		auto it = std::find(ScopeStack.rbegin(), ScopeStack.rend(), Scope);
		assert(it != ScopeStack.rend());
		return std::distance(ScopeStack.rbegin(), it);
	};

	for (TokenList::const_iterator it = Tokens.begin(), ie = Tokens.end(); it != ie; ++it)
	{
		const Token& T = *it;
		teeLocals.instructionStart(code);
		switch (T.getKind())
		{
			case Token::TK_BasicBlock:
			{
				if (ScopeStack.empty())
					lastDepth0Block = T.getBB();
				else
					lastDepth0Block = nullptr;
				compileBB(code, *T.getBB(), T.getPhiHandledAsResult());
				if(!lastDepth0Block)
				{
					const BasicBlock* BB = T.getBB();
					const Instruction* Term = BB->getTerminator();
					if (isa<ReturnInst>(Term) && !isReturnPartOfTailCall(*Term))
						encodeInst(WasmOpcode::RETURN, code);
				}
				break;
			}
			case Token::TK_Loop:
			{
				teeLocals.addIndentation(code);
				encodeInst(WasmU32Opcode::LOOP, getResultKind(T), code);
				ScopeStack.push_back(&T);
				break;
			}
			case Token::TK_Block:
			{
				teeLocals.addIndentation(code);
				encodeInst(WasmU32Opcode::BLOCK, getResultKind(T), code);
				ScopeStack.emplace_back(&T);
				break;
			}
			case Token::TK_Condition:
			{
				const BranchInst* bi=cast<BranchInst>(T.getBB()->getTerminator());
				assert(bi->isConditional());
				compileCondition(code, bi->getCondition(), /*booleanInvert*/false);
				break;
			}
			case Token::TK_BrIf:
			case Token::TK_BrIfNot:
			{
				bool IfNot = T.getKind() == Token::TK_BrIfNot;
				// The condition goes first
				const BranchInst* bi=cast<BranchInst>(T.getBB()->getTerminator());
				assert(bi->isConditional());
				compileCondition(code, bi->getCondition(), IfNot);
				const int Depth = getDepth(T.getMatch());
				teeLocals.clearTopmostCandidates(code, Depth+1);
				encodeInst(WasmU32Opcode::BR_IF, Depth, code);
				break;
			}
			case Token::TK_If:
			case Token::TK_IfNot:
			{
				bool IfNot = T.getKind() == Token::TK_IfNot;
				// The condition goes first
				const BranchInst* bi=cast<BranchInst>(T.getBB()->getTerminator());
				assert(bi->isConditional());
				compileCondition(code, bi->getCondition(), IfNot);
				teeLocals.addIndentation(code);
				encodeInst(WasmU32Opcode::IF, getResultKind(T), code);
				ScopeStack.push_back(&T);
				break;
			}
			case Token::TK_Else:
			{
				teeLocals.decreaseIndentation(code);
				teeLocals.addIndentation(code);
				encodeInst(WasmOpcode::ELSE, code);
				break;
			}
			case Token::TK_Branch:
			{
				const int Depth = getDepth(T.getMatch());
				teeLocals.clearTopmostCandidates(code, Depth+1);
				encodeInst(WasmU32Opcode::BR, Depth, code);
				break;
			}
			case Token::TK_End:
			{
				teeLocals.decreaseIndentation(code);
				ScopeStack.pop_back();
				encodeInst(WasmOpcode::END, code);
				break;
			}
			case Token::TK_Prologue:
			{
				const BasicBlock* To = T.getBB()->getTerminator()->getSuccessor(T.getId());
				compilePHIOfBlockFromOtherBlock(code, To, T.getBB(), T.getPhiHandledAsResult());
				break;
			}
			case Token::TK_Switch:
			{
				std::vector<std::pair<int, int>> Cases;
				const SwitchInst* si = cast<SwitchInst>(T.getBB()->getTerminator());
				it++;
				while(it->getKind() != Token::TK_End)
				{
					assert(it->getKind()==Token::TK_Case);
					std::vector<int> ids;
					while(it->getKind() == Token::TK_Case)
					{
						ids.push_back(it->getId());
						it++;
					}
					assert(it->getKind() == Token::TK_Branch);
					int Depth = getDepth(it->getMatch());
					for (int id: ids)
						Cases.push_back(std::make_pair(id, Depth));
					it++;
				}
				compileBranchTable(code, si, Cases);
				break;
			}
			case Token::TK_Case:
				report_fatal_error("Case token found outside of switch block");
				break;
			case Token::TK_Invalid:
				report_fatal_error("Invalid token found");
				break;
		}
	}
	return lastDepth0Block;
}

std::map<const llvm::BasicBlock*, const llvm::PHINode*> CheerpWasmWriter::selectPHINodesHandledAsResult(const std::vector<const llvm::BasicBlock*>& possibleBB) const
{
	std::map<const llvm::BasicBlock*, const llvm::PHINode*> phiNodesHandledAsResult;

	for (auto BB : possibleBB)
	{
		std::pair<int, const llvm::PHINode*> best{0, nullptr};
		for (const PHINode& phi : BB->phis())
		{
			std::pair<int, const llvm::PHINode*> curr{gainOfHandlingPhiOnTheEdge(&phi), &phi};
			if (curr > best)
				best = curr;
		}

		//Either best as be assigned something, so it's first member is > 0,
		//or there weren't phi at all (and it remained 0),
		//or there were not phi to be rentered for a gain (and it remained 0)
		if (best.first > 0)
		{
			assert(best.second);
			const PHINode* selectedPHI = best.second;
			phiNodesHandledAsResult.insert({selectedPHI->getParent(), selectedPHI});
		}
	}

	return phiNodesHandledAsResult;
}

void CheerpWasmWriter::compileMethod(WasmBuffer& code, const Function& F)
{
	assert(!F.empty());
	currentFun = &F;

	uint32_t numArgs = F.arg_size();
	const llvm::BasicBlock* lastDepth0Block = nullptr;

	Relooper* rl = nullptr;
	bool needsLabel = false;

	if (F.size() != 1 && useCfgLegacy) {
		rl = CheerpWriter::runRelooperOnFunction(F, PA, registerize);
		needsLabel = rl->needsLabel();
	}

	const std::vector<Registerize::RegisterInfo>& regsInfo = registerize.getRegistersForFunction(&F);
	uint32_t localCount = regsInfo.size() + (int)needsLabel;

	vector<int> locals(5, 0);
	localMap.assign(localCount, 0);
	uint32_t reg = 0;

	// Make lookup table for registers to locals.
	for(const Registerize::RegisterInfo& regInfo: regsInfo)
	{
		assert(!regInfo.needsSecondaryName);

		// Save the current local index
		localMap.at(reg) = numArgs + locals.at((int)regInfo.regKind);
		locals.at((int)regInfo.regKind)++;
		reg++;
	}

	if (needsLabel) {
		localMap.at(reg) = numArgs + locals.at((int)Registerize::INTEGER);
		locals.at((int)Registerize::INTEGER)++;
	}

	// Add offset of other local groups to local lookup table.  Since INTEGER
	// is the first group, the local for label does not require an offset.
	reg = 0;
	for(const Registerize::RegisterInfo& regInfo: regsInfo)
	{
		uint32_t offset = 0;
		switch (regInfo.regKind) {
			case Registerize::INTEGER:
				break;
			case Registerize::INTEGER64:
				offset += locals.at((int)Registerize::INTEGER);
				break;
			case Registerize::DOUBLE:
				offset += locals.at((int)Registerize::INTEGER);
				offset += locals.at((int)Registerize::INTEGER64);
				break;
			case Registerize::FLOAT:
				offset += locals.at((int)Registerize::INTEGER);
				offset += locals.at((int)Registerize::INTEGER64);
				offset += locals.at((int)Registerize::DOUBLE);
				break;
			case Registerize::OBJECT:
				offset += locals.at((int)Registerize::INTEGER);
				offset += locals.at((int)Registerize::INTEGER64);
				offset += locals.at((int)Registerize::DOUBLE);
				offset += locals.at((int)Registerize::FLOAT);
				break;
		}
		localMap[reg++] += offset;
	}

	compileMethodLocals(code, locals);

	teeLocals.performInitialization(code);

	if (F.size() == 1)
	{
		compileBB(code, *F.begin());
		lastDepth0Block = &(*F.begin());
	}
	else
	{
		const std::vector<Registerize::RegisterInfo>& regsInfo = registerize.getRegistersForFunction(&F);
		uint32_t numRegs = regsInfo.size();

		// label is the very last local
		uint32_t labelLocal = needsLabel ? localMap[numRegs] : 0;
		if (useCfgLegacy)
		{
			CheerpWasmRenderInterface ri(this, code, labelLocal);
			rl->Render(&ri);
			lastDepth0Block = ri.lastDepth0Block;
		}
		else
		{
			DominatorTree &DT = pass.getAnalysis<DominatorTreeWrapperPass>(const_cast<Function&>(F)).getDomTree();
			LoopInfo &LI = pass.getAnalysis<LoopInfoWrapperPass>(const_cast<Function&>(F)).getLoopInfo();
			CFGStackifier CN(F, LI, DT, registerize, PA, CFGStackifier::Wasm);

			const auto possibleBBs = CN.selectBasicBlocksWithPossibleIncomingResult();

			//Select a subset of possibleBBs, and for each a PHINode that will be handled as result
			const auto phiHandledAsResult = selectPHINodesHandledAsResult(possibleBBs);

			//Use the selected PHINodes to assign a result type to the relevant tokens (blocks/loops/ifs)
			//And tag the relevat tokens with the PHI to be handled there
			CN.addResultToTokens(phiHandledAsResult, registerize);

			lastDepth0Block = compileTokens(code, CN.Tokens);
		}
	}

	if (!useCfgLegacy)
	{
		checkImplicitedAssignedPhi(F);
		generateNOP(code);
	}

	getLocalDone.clear();
	teeLocals.clear(code);
	compiled.clear();

	// A function has to terminate with a return value when the return type is
	// not void.
	if (!lastDepth0Block || (!isa<ReturnInst>(lastDepth0Block->getTerminator()) && !isa<UnreachableInst>(lastDepth0Block->getTerminator())))
	{
		if(!F.getReturnType()->isVoidTy())
		{
			compileTypedZero(code, F.getReturnType());
		}
	}

	// Encode the end of the method.
	encodeULEB128(0x0b, code);
}

//The call to requiresExplicitAssigment has the side effect of perfomring bookkeeping on the implicited assigned instructions
void CheerpWasmWriter::checkImplicitedAssignedPhi(const llvm::Function& F)
{
	for (const BasicBlock& BB : F)
	{
		for (const Instruction& I : BB)
		{
			if (!isa<PHINode>(I))
				break;
			const PHINode& phi = cast<PHINode>(I);
			for (uint32_t index = 0; index < phi.getNumIncomingValues(); index++)
				requiresExplicitAssigment(&phi, phi.getIncomingValue(index));
		}
	}
}

void CheerpWasmWriter::generateNOP(WasmBuffer& code)
{
	for (const TeeLocals::LocalInserted& localInserted : teeLocals.getLocalInserted())
	{
		const Instruction* I = localInserted.I;
		if (getLocalDone.count(I))
			continue;
		putNOP(code, localInserted.localId, localInserted.bufferOffset, teeLocals.isValueUsed(I));
	}
}

void CheerpWasmWriter::compileTypeSection()
{
	if (linearHelper.getFunctionTypes().empty())
		return;

	Section section(0x01, "Type", this);

	// Encode number of entries in the type section.
	encodeULEB128(linearHelper.getFunctionTypes().size(), section);

	// Define function type variables
	for (const auto& fTy : linearHelper.getFunctionTypes())
	{
		encodeULEB128(0x60, section);
		compileMethodParams(section, fTy);
		compileMethodResult(section, fTy->getReturnType());
	}
}

void CheerpWasmWriter::compileImport(WasmBuffer& code, StringRef funcName, FunctionType* fTy)
{
	assert(useWasmLoader);

	std::string fieldName(funcName);

	// Encode the module name.
	std::string moduleName = "i";
	encodeULEB128(moduleName.size(), code);
	code.write(moduleName.data(), moduleName.size());

	// Encode the field name.
	encodeULEB128(fieldName.size(), code);
	code.write(fieldName.data(), fieldName.size());

	// Encode kind as 'Function' (= 0).
	encodeULEB128(0x00, code);

	// Encode type index of function signature.
	const auto& found = linearHelper.getFunctionTypeIndices().find(fTy);
	assert(found != linearHelper.getFunctionTypeIndices().end());
	encodeULEB128(found->second, code);
}

void CheerpWasmWriter::compileImportSection()
{
	// Count imported builtins
	uint32_t importedBuiltins = 0;
	for(uint32_t i=0;i<BuiltinInstr::numGenericBuiltins();i++)
	{
		if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN(i)))
			importedBuiltins++;
	}

	uint32_t importedTotal = importedBuiltins + globalDeps.asmJSImports().size();

	if (importedTotal == 0 || !useWasmLoader)
		return;

	Section section(0x02, "Import", this);

	// Encode number of entries in the import section.
	encodeULEB128(importedTotal, section);

	for (const Function* F : globalDeps.asmJSImports())
		compileImport(section, namegen.getName(F), F->getFunctionType());

	Type* f64 = Type::getDoubleTy(module.getContext());
	Type* i32 = Type::getInt32Ty(module.getContext());
	Type* f64_1[] = { f64 };
	Type* f64_2[] = { f64, f64 };
	Type* i32_1[] = { i32 };
	FunctionType* f64_f64_1 = FunctionType::get(f64, f64_1, false);
	FunctionType* f64_f64_2 = FunctionType::get(f64, f64_2, false);
	FunctionType* i32_i32_1 = FunctionType::get(i32, i32_1, false);
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ACOS_F))
		compileImport(section, namegen.getBuiltinName(NameGenerator::ACOS), f64_f64_1);
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ASIN_F))
		compileImport(section, namegen.getBuiltinName(NameGenerator::ASIN), f64_f64_1);
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ATAN_F))
		compileImport(section, namegen.getBuiltinName(NameGenerator::ATAN), f64_f64_1);
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ATAN2_F))
		compileImport(section, namegen.getBuiltinName(NameGenerator::ATAN2), f64_f64_2);
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::COS_F))
		compileImport(section, namegen.getBuiltinName(NameGenerator::COS), f64_f64_1);
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::EXP_F))
		compileImport(section, namegen.getBuiltinName(NameGenerator::EXP), f64_f64_1);
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::LOG_F))
		compileImport(section, namegen.getBuiltinName(NameGenerator::LOG), f64_f64_1);
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::POW_F))
		compileImport(section, namegen.getBuiltinName(NameGenerator::POW), f64_f64_2);
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::SIN_F))
		compileImport(section, namegen.getBuiltinName(NameGenerator::SIN), f64_f64_1);
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::TAN_F))
		compileImport(section, namegen.getBuiltinName(NameGenerator::TAN), f64_f64_1);
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::GROW_MEM))
		compileImport(section, namegen.getBuiltinName(NameGenerator::GROW_MEM), i32_i32_1);
}

void CheerpWasmWriter::compileFunctionSection()
{
	if (linearHelper.getFunctionTypes().empty())
		return;

	Section section(0x03, "Function", this);

	uint32_t count = linearHelper.functions().size();
	count = std::min(count, COMPILE_METHOD_LIMIT); // TODO

	// Encode number of entries in the function section.
	encodeULEB128(count, section);

	// Define function type ids
	size_t i = 0;
	for (const Function* F : linearHelper.functions()) {
		const FunctionType* fTy = F->getFunctionType();
		const auto& found = linearHelper.getFunctionTypeIndices().find(fTy);
		assert(found != linearHelper.getFunctionTypeIndices().end());
		assert(found->second < linearHelper.getFunctionTypes().size());
		encodeULEB128(found->second, section);

		if (++i >= COMPILE_METHOD_LIMIT)
			break; // TODO
	}
}


void CheerpWasmWriter::compileTableSection()
{
	if (linearHelper.getFunctionTables().empty())
		return;

	uint32_t count = 0;
	for (const auto& table : linearHelper.getFunctionTables())
		count += table.second.functions.size();
	count = std::min(count, COMPILE_METHOD_LIMIT); // TODO

	Section section(0x04, "Table", this);

	// Encode number of function tables in the table section.
	encodeULEB128(1, section);

	// Encode element type 'anyfunc'.
	encodeULEB128(0x70, section);

	// Encode function tables in the table section.
	// Use a 'limit' (= 0x00) with only a maximum value.
	encodeULEB128(0x00, section);
	encodeULEB128(count, section);
}

CheerpWasmWriter::GLOBAL_CONSTANT_ENCODING CheerpWasmWriter::shouldEncodeConstantAsGlobal(const Constant* C, uint32_t useCount, uint32_t getGlobalCost)
{
	assert(useCount > 1);

	auto computeCostAsLiteral = [](const Constant* C) -> uint32_t {
		const Type* type = C->getType();
		if (type->isDoubleTy())
			return 9;
		if (type->isFloatTy())
			return 5;
		if (type->isIntegerTy(64))
		{
			//The case left out is Undefined (+possibly other corner cases), that it not worth encoding
			if (const ConstantInt* CI = dyn_cast<ConstantInt>(C))
			{
				const uint32_t encodingLength = getSLEBEncodingLength(CI->getSExtValue());
				return 1+encodingLength;
			}
		}
		// We don't try to globalize 32bit integer constants as that has a negative performance impact
		return 0;
	};

	const uint32_t computeCost = computeCostAsLiteral(C);

	if (computeCost)	//0 => globalization do not make sense
	{
		const uint32_t costAsLiteral = computeCost;
		// 1 (type) + costAsLiteral + 1 (end byte)
		const uint32_t globalInitCost = 2 + costAsLiteral;
		const uint32_t globalUsesCost = globalInitCost + getGlobalCost * useCount;

		uint32_t directUsesCost = costAsLiteral * useCount;
		if(globalUsesCost < directUsesCost)
			return FULL;
		else
			return NONE;
	}
	else
	{
		return NONE;
	}
}

void CheerpWasmWriter::compileMemoryAndGlobalSection()
{
	// Define the memory for the module in WasmPage units. The heap size is
	// defined in MiB and the wasm page size is 64 KiB. Thus, the wasm heap
	// max size parameter is defined as: heapSize << 20 >> 16 = heapSize << 4.
	uint32_t maxMemory = heapSize << 4;
	uint32_t minMemory = (linearHelper.getHeapStart() + 65535) >> 16;

	// TODO use WasmPage variable instead of hardcoded '1>>16'.
	assert(WasmPage == 64 * 1024);
	
	if (noGrowMemory)
		minMemory = maxMemory;

	{
		Section section(0x05, "Memory", this);

		encodeULEB128(1, section);
		// from the spec:
		//limits ::= 0x00 n:u32          => {min n, max e, unshared}
		//           0x01 n:u32 m:u32    => {min n, max m, unshared}
		//           0x03 n:u32 m:u32    => {min n, max m, shared}
		// We use 0x01 and 0x03 only for now
		int memType = sharedMemory ? 0x03 : 0x01;
		encodeULEB128(memType, section);
		// Encode minimum and maximum memory parameters.
		encodeULEB128(minMemory, section);
		encodeULEB128(maxMemory, section);
	}

	// Temporary map for the globalized constants. We update the global one at the end, to avoid
	// global constants referencing each other
	std::unordered_map<const llvm::Constant*, std::pair<uint32_t, GLOBAL_CONSTANT_ENCODING>> globalizedConstantsTmp;
	std::unordered_map<const llvm::Constant*, uint32_t> orderOfInsertion;
	const LinearMemoryHelper::GlobalUsageMap& globalizedGlobalsUsage(linearHelper.getGlobalizedGlobalUsage());

	for (auto G : linearHelper.globals())
	{
		if (globalizedGlobalsUsage.count(G))
			orderOfInsertion[G] = orderOfInsertion.size();
	}
	// Gather all constants used multiple times, we want to encode those in the global section
	for (const Function* F: linearHelper.functions())
	{
		for(const BasicBlock& BB: *F)
		{
			for(const Instruction& I: BB)
			{
				// Heuristic: Avoid globalizing values which will be anyway encoded as a load/store offset
				// Fully constant GEPs will be a ConstantExpr, so when we see a GEP there are 2 possibilities
				// 1) A constant base pointer and at least a variable index: On a load/store the indexes will
				//    be computed on the stack, while the base pointer will be the offset
				// 2) A variable base pointer: Indexes will be accumulated in a total value, no reason for globalization
				// NOTE: We skip all GEP instructionsa here, assuming a load/store follows. This may not be necessarily the case.
				if(I.getOpcode() == Instruction::GetElementPtr)
					continue;
				for(Value* V: I.operands())
				{
					Constant* C = dyn_cast<Constant>(V);
					if(!C)
						continue;
					if(isa<Function>(C) || isa<ConstantPointerNull>(C))
						continue;
					if (orderOfInsertion.count(C) == 0)
						orderOfInsertion[C] = orderOfInsertion.size();
					if(isa<GlobalVariable>(C) && globalizedGlobalsUsage.count(cast<GlobalVariable>(C)))
					{
						// The whole global is globalized, there is no point in globalizing the address
						continue;
					}
					globalizedConstantsTmp[C].first++;
				}
			}
		}
	}
	// We need to order the constants by use count
	struct GlobalConstant
	{
		const Constant* C;
		uint32_t useCount;
		GLOBAL_CONSTANT_ENCODING encoding;
		uint32_t insertionIndex;
		// NOTE: We want to have the high use counts first
		bool operator<(const GlobalConstant& rhs) const
		{
			// We need to fully order these to keep the output consistent
			// So we use insertionIndex as tie-breaker
			if (useCount != rhs.useCount)
				return useCount > rhs.useCount;
			return insertionIndex < rhs.insertionIndex;
		}
	};
	std::vector<GlobalConstant> orderedConstants;
	// Remove single use constants right away
	auto it = globalizedConstantsTmp.begin();
	auto itEnd = globalizedConstantsTmp.end();
	while (it != itEnd)
	{
		if(it->second.first == 1)
			it = globalizedConstantsTmp.erase(it);
		else
		{
			orderedConstants.push_back(GlobalConstant{it->first, it->second.first, it->second.second, orderOfInsertion.at(it->first)});
			++it;
		}
	}
	for(auto& it: globalizedGlobalsUsage)
	{
		orderedConstants.push_back(GlobalConstant{it.first, it.second, GLOBAL, orderOfInsertion.at(it.first)});
	}

	std::sort(orderedConstants.begin(), orderedConstants.end());

	// Assign global ids
	uint32_t globalId = 1;
	for(uint32_t i=0;i<orderedConstants.size();i++)
	{
		GlobalConstant& GC = orderedConstants[i];
		if(GC.encoding == GLOBAL)
		{
			auto it = globalizedGlobalsUsage.find(cast<GlobalVariable>(GC.C));
			assert(it != globalizedGlobalsUsage.end());
			globalizedGlobalsIDs[it->first] = globalId++;
			continue;
		}

		// NOTE: It is not the same as getSLEBEcodingLength since the global id is unsigned
		const uint32_t getGlobalCost = getULEBEncodingLength(globalId) + 1;

		GLOBAL_CONSTANT_ENCODING encoding = shouldEncodeConstantAsGlobal(GC.C, GC.useCount, getGlobalCost);
		GC.encoding = encoding;
		auto it = globalizedConstantsTmp.find(GC.C);
		if(encoding == NONE)
		{
			// Remove this constant from the map
			// Leave it in the vector, but skip it later
			globalizedConstantsTmp.erase(it);
		}
		else
		{
			it->second.first = globalId++;
			it->second.second = encoding;
		}
	}

	{
		Section section(0x06, "Global", this);

		// Start the stack from the end of default memory
		stackTopGlobal = usedGlobals++;
		uint32_t stackTop = linearHelper.getStackStart();

		// There is the stack and the globalized constants
		encodeULEB128(1 + globalizedConstantsTmp.size() + globalizedGlobalsIDs.size(), section);
		// The global has type i32 (0x7f) and is mutable (0x01).
		encodeULEB128(0x7f, section);
		encodeULEB128(0x01, section);
		// The global value is a 'i32.const' literal.
		encodeLiteralType(Type::getInt32Ty(Ctx), section);
		encodeSLEB128(stackTop, section);
		// Encode the end of the instruction sequence.
		encodeULEB128(0x0b, section);
		// Render globals in reverse order
		for(auto it = orderedConstants.begin(); it != orderedConstants.end(); ++it)
		{
			const Constant* C = it->C;
			if(it->encoding == NONE)
				continue;
			else if(it->encoding == GLOBAL)
			{
				const GlobalVariable* GV = cast<GlobalVariable>(C);
				encodeValType(GV->getValueType(), section);
				// Mutable -> 1
				encodeULEB128(0x01, section);
				assert(GV->hasInitializer());
				compileConstant(section, GV->getInitializer(), /*forGlobalInit*/true);
				encodeULEB128(0x0b, section);
				continue;
			}
			// Constant type
			assert(it->encoding == FULL);
			encodeValType(C->getType(), section);
			// Immutable -> 0
			encodeULEB128(0x00, section);
			compileConstant(section, C, /*forGlobalInit*/true);
			encodeULEB128(0x0b, section);
		}
	}
	globalizedConstants = std::move(globalizedConstantsTmp);
}

void CheerpWasmWriter::compileExportSection()
{
	Section section(0x07, "Export", this);
	std::vector<const llvm::Function*> exports;

	// Export the webMain symbol, if defined.
	const llvm::Function* entry = globalDeps.getEntryPoint();
	if(entry && entry->getSection() == StringRef("asmjs")) {
		assert(globalDeps.asmJSExports().find(entry) == globalDeps.asmJSExports().end());
		exports.push_back(entry);
	}

	// Add the static constructors
	for (const Function * F : globalDeps.constructors() )
	{
		if (F->getSection() == StringRef("asmjs"))
			exports.push_back(F);
	}
	// Add the list of asmjs-exported functions.
	exports.insert(exports.end(), globalDeps.asmJSExports().begin(),
			globalDeps.asmJSExports().end());

	// We export the memory unconditionally, but may also need to export the table
	uint32_t extraExports = 1;
	if(exportedTable)
		extraExports = 2;
	encodeULEB128(exports.size() + extraExports, section);

	// Encode the memory.
	StringRef name = namegen.getBuiltinName(NameGenerator::MEMORY);
	encodeULEB128(name.size(), section);
	section.write(name.data(), name.size());
	encodeULEB128(0x02, section);
	encodeULEB128(0, section);

	if(exportedTable)
	{
		// Encode the table
		StringRef name = "tbl";
		encodeULEB128(name.size(), section);
		section.write(name.data(), name.size());
		encodeULEB128(0x01, section);
		encodeULEB128(0, section);
	}

	for (const llvm::Function* F : exports) {
		// Encode the method name.
		name = namegen.getName(F);

		encodeULEB128(name.size(), section);
		section.write(name.data(), name.size());

		// Encode the function index (where '0x00' means that this export is a
		// function).
		encodeULEB128(0x00, section);
		encodeULEB128(linearHelper.getFunctionIds().find(F)->second, section);
	}
}

void CheerpWasmWriter::compileStartSection()
{
	// It's not possible to run constructors that depend on asmjs / generic js
	// code, since the heap can only be accessed after the module has been
	// initialised. Therefore, we disable the start section when a wasm loader
	// is generated.
	if (useWasmLoader)
		return;

	// Experimental entry point for wasm code
	llvm::Function* entry = module.getFunction("_start");
	if(!entry)
		return;

	uint32_t functionId = linearHelper.getFunctionIds().at(entry);
	if (functionId >= COMPILE_METHOD_LIMIT)
		return;

	Section section(0x08, "Start", this);

	encodeULEB128(functionId, section);
}

void CheerpWasmWriter::compileElementSection()
{
	if (linearHelper.getFunctionTables().empty())
		return;

	Section section(0x09, "Element", this);

	// There is only one element segment.
	encodeULEB128(1, section);

	// The table index is 0 in the MVP.
	encodeULEB128(0, section);

	// The offset into memory, which is the address.
	int32_t offset = 0;
	encodeLiteralType(Type::getInt32Ty(Ctx), section);
	encodeSLEB128(offset, section);
	// Encode the end of the instruction sequence.
	encodeULEB128(0x0b, section);

	// Encode the sequence of function indices.
	Chunk<128> elem;
	size_t count = 0;
	for (const FunctionType* fTy: linearHelper.getFunctionTableOrder()) {
		const auto table = linearHelper.getFunctionTables().find(fTy);
		for (const auto& F : table->second.functions) {
			uint32_t idx = linearHelper.getFunctionIds().at(F);
			encodeULEB128(idx, elem);
			count++;
		}
	}
	encodeULEB128(count, section);
	section << elem.str();
}

void CheerpWasmWriter::compileCodeSection()
{
	Section section(0x0a, "Code", this);

	// Encode the number of methods in the code section.
	uint32_t count = linearHelper.functions().size();
	count = std::min(count, COMPILE_METHOD_LIMIT);
	encodeULEB128(count, section);
#if WASM_DUMP_METHODS
	llvm::errs() << "method count: " << count << '\n';
#endif

	size_t i = 0;

	for (const Function* F: linearHelper.functions())
	{
		Chunk<128> method;
#if WASM_DUMP_METHODS
		llvm::errs() << i << " method name: " << F->getName() << '\n';
#endif
		compileMethod(method, *F);

		filterNop(method.buf());
		nopLocations.clear();

#if WASM_DUMP_METHOD_DATA
		llvm::errs() << "method length: " << method.tell() << '\n';
		llvm::errs() << "method: " << string_to_hex(method.str()) << '\n';
#endif
		encodeULEB128(method.tell(), section);
		section << method.str();

		if (++i == COMPILE_METHOD_LIMIT)
			break; // TODO
	}
}

void CheerpWasmWriter::encodeDataSectionChunk(WasmBuffer& data, uint32_t address, StringRef buf)
{
	// In the current version of WebAssembly, at most one memory is
	// allowed in a module. Consequently, the only valid memidx is 0.
	encodeULEB128(0, data);
	// The offset into memory, which is the address
	encodeLiteralType(Type::getInt32Ty(Ctx), data);
	encodeSLEB128(address, data);
	// Encode the end of the instruction sequence.
	encodeULEB128(0x0b, data);
	// Prefix the number of bytes to the bytes vector.
	encodeULEB128(buf.size(), data);
	data.write(buf.data(), buf.size());
}

uint32_t CheerpWasmWriter::encodeDataSectionChunks(WasmBuffer& data, uint32_t address, StringRef buf)
{
	// Split data section buffer into chunks based on 6 (or more) zero bytes.
	uint32_t chunks = 0;
	size_t cur = 0, last = 0, end = 0;
	std::string delimiter("\0\0\0\0\0\0\0", 6);
	while ((cur = buf.find(delimiter, last)) != std::string::npos) {
		std::string chunk(buf.substr(last, cur - last));
		assert(chunk.size() == cur - last);
		assert(address + last > end);
		encodeDataSectionChunk(data, address + last, chunk);
		chunks++;

		end = address + last + chunk.size();

		// Skip the delimiter and all consecutive zero bytes.
		last = cur + delimiter.length();
		for (; last < buf.size() && buf[last] == 0; last++);
	}

	// If the buffer ends with zero bytes (last == buf.size()), an empty chunk
	// will be encoded. This should not happen, and is prevented by stripping
	// leading and trailing zeros from the buffer when this function is called.
	assert(last < buf.size());
	encodeDataSectionChunk(data, address + last, buf.substr(last));

	return chunks + 1;
}

void CheerpWasmWriter::compileDataSection()
{
	Section section(0x0b, "Data", this);

	Chunk<128> data;
	uint32_t count = 0;

	auto globals = linearHelper.addressableGlobals();
	for (auto g = globals.begin(), e = globals.end(); g != e; ++g)
	{
		const GlobalVariable* GV = *g;

		// Skip global variables that are zero-initialised.
		if (!linearHelper.hasNonZeroInitialiser(GV))
			continue;
		const Constant* init = GV->getInitializer();

		uint32_t address = linearHelper.getGlobalVariableAddress(GV);

		// Concatenate global variables into one big binary blob. This
		// optimization omits the data section item header, and that will save
		// a minimum of 5 bytes per global variable.
		Chunk<128> bytes;
		WasmBytesWriter bytesWriter(bytes, *this);

		for (; g != e; ++g) {
			GV = *g;

			// Do not concatenate global variables that have no initialiser or
			// are zero-initialised.
			if (!linearHelper.hasNonZeroInitialiser(GV))
				break;
			init = GV->getInitializer();

			// Determine amount of padding bytes necessary for the alignment.
			long written = bytes.tell();
			uint32_t nextAddress = linearHelper.getGlobalVariableAddress(GV);
			uint32_t padding = nextAddress - (address + written);
			for (uint32_t i = 0; i < padding; i++)
				bytes << (char)0;

			linearHelper.compileConstantAsBytes(init,/* asmjs */ true, &bytesWriter);
		}

		StringRef buf = bytes.str();

		// Strip leading and trailing zeros.
		size_t pos = 0, len = buf.size();
		for (unsigned i = 0; i < buf.size() && !buf[i]; i++) {
			pos++;
			len--;
		}
		for (unsigned i = buf.size(); i > 0 && !buf[--i];)
			len--;
		buf = buf.substr(pos, len);
		assert(len > 0 && "found a zero-initialised variable");

		address += pos;

		count += encodeDataSectionChunks(data, address, buf);

		// Break the outer loop when the last global variable is concatenated.
		// Without this check, the outer loop will increment `g` as well, which
		// will cause the condition `g != e` to pass, resulting in an
		// out-of-bounds access on the iterator.
		if (g == e)
			break;
	}

	encodeULEB128(count, section);
	section << data.str();
}

void CheerpWasmWriter::compileNameSection()
{
	assert(prettyCode);
	Section section(0x00, "name", this);

	// Assign names to functions
	{
		Chunk<128> data;
		uint32_t count = linearHelper.functions().size();
		encodeULEB128(count, data);

		for (const Function* F : linearHelper.functions())
		{
			uint32_t functionId = linearHelper.getFunctionIds().at(F);
			encodeULEB128(functionId, data);
			encodeULEB128(F->getName().size(), data);
			data << F->getName().str();
		}

		encodeULEB128(0x01, section);
		encodeULEB128(data.tell(), section);
		section << data.str();
	}
}

void CheerpWasmWriter::compileModule()
{
	// Magic number for wasm.
	encodeULEB128(0x00, stream);
	encodeULEB128(0x61, stream);
	encodeULEB128(0x73, stream);
	encodeULEB128(0x6D, stream);
	// Version number.
	encodeULEB128(0x01, stream);
	encodeULEB128(0x00, stream);
	encodeULEB128(0x00, stream);
	encodeULEB128(0x00, stream);

	compileTypeSection();

	compileImportSection();

	compileFunctionSection();

	compileTableSection();

	compileMemoryAndGlobalSection();

	compileExportSection();

	compileStartSection();

	compileElementSection();

	compileCodeSection();

	compileDataSection();

	if (prettyCode) {
		compileNameSection();
	}
}

void CheerpWasmWriter::makeWasm()
{
	compileModule();
}

void CheerpWasmWriter::WasmBytesWriter::addByte(uint8_t byte)
{
	code.write(reinterpret_cast<char*>(&byte), 1);
}

void CheerpWasmWriter::WasmGepWriter::addValue(const llvm::Value* v, uint32_t size)
{
	addedValues.emplace_back(v, size);
}

void CheerpWasmWriter::WasmGepWriter::subValue(const llvm::Value* v, uint32_t size)
{
	subbedValues.emplace_back(v, size);
}

void CheerpWasmWriter::WasmGepWriter::compileValue(const llvm::Value* v, uint32_t size) const
{
	writer.compileOperand(code, v);
	if (size > 1)
	{
		if (isPowerOf2_32(size))
		{
			writer.encodeInst(WasmS32Opcode::I32_CONST, Log2_32(size), code);
			writer.encodeInst(WasmOpcode::I32_SHL, code);
		}
		else
		{
			writer.encodeInst(WasmS32Opcode::I32_CONST, size, code);
			writer.encodeInst(WasmOpcode::I32_MUL, code);
		}
	}
}

uint32_t CheerpWasmWriter::WasmGepWriter::compileValues(bool positiveOffsetAllowed) const
{
	struct ValuesToAdd
	{
		ValuesToAdd(const llvm::Value* v, bool toInvert, uint32_t multiplier)
			: v(v), toInvert(toInvert), multiplier(multiplier)
		{
		}
		const llvm::Value* v;
		bool toInvert;
		uint32_t multiplier;
	};

	std::vector<ValuesToAdd> V;

	for (auto& it: addedValues)
	{
		V.push_back(ValuesToAdd(it.first, /*toInvert*/false, it.second));
	}

	for (auto& it: subbedValues)
	{
		V.push_back(ValuesToAdd(it.first, /*toInvert*/true, it.second));
	}
	//Since we first insert addedValues and only then subbedValues
	//And the sorting method is stable, for a given multiplier class,
	//IFF there are values with toInvert == false they have to be clustered at the beginning

	std::sort(V.begin(), V.end(), [](const ValuesToAdd& A, const ValuesToAdd& B) -> bool {
				if (A.multiplier != B.multiplier)
					return (A.multiplier > B.multiplier);
				return false;
			}
	    );

	struct GroupedValuesToAdd
	{
		GroupedValuesToAdd(uint32_t multiplier)
			: constantPart(0), multiplier(multiplier)
		{
		}
		void add(const ValuesToAdd& v)
		{
			assert(v.multiplier == multiplier);
			if (v.toInvert)
				valuesToInvert.push_back(v.v);
			else
				valuesToAdd.push_back(v.v);
		}
		void addConstant(uint32_t c)
		{
			assert(c % multiplier == 0);
			assert(c > 0);
			constantPart = c/multiplier;
		}
		bool hasPositive() const
		{
			return constantPart != 0 || !valuesToAdd.empty();
		}
		void sort(const CheerpWasmWriter& writer)
		{
			sortImpl(writer, valuesToAdd);
			sortImpl(writer, valuesToInvert);
		}
	private:
		void sortImpl(const CheerpWasmWriter& writer, std::vector<const llvm::Value*>& toSort)
		{
			std::vector<std::pair<uint32_t, const llvm::Value*>> V;
			for (const llvm::Value* v : toSort)
			{
				V.push_back({writer.findDepth(v), v});
			}
			std::sort(V.begin(), V.end(), [](const std::pair<uint32_t, const llvm::Value*>& A, const std::pair<uint32_t, const llvm::Value*>&B) -> bool
					{
						if (A.first != B.first)
							return A.first < B.first;
						return false;
					});
			for (uint32_t i=0; i<V.size(); i++)
			{
				toSort[i] = V[i].second;
			}

		}
	public:
		uint32_t constantPart;
		uint32_t multiplier;
		std::vector<const llvm::Value*> valuesToAdd;
		std::vector<const llvm::Value*> valuesToInvert;
	};

	std::vector<GroupedValuesToAdd> V2;

	for (auto& v: V)
	{
		if (V2.empty() || V2.back().multiplier != v.multiplier)
		{
			V2.push_back(GroupedValuesToAdd(v.multiplier));
		}
		V2.back().add(v);
	}


	std::sort(V2.begin(), V2.end(), [](const GroupedValuesToAdd& A, const GroupedValuesToAdd& B) -> bool {
				if (A.hasPositive() != B.hasPositive())
					return (!A.hasPositive());
				return false;
			}
	    );

	auto initializeYetToBeEncodedOffset = [this, &positiveOffsetAllowed](const std::vector<GroupedValuesToAdd>& V2, bool& first) -> uint32_t
	{
		if(constPart != 0 && (!positiveOffsetAllowed || constPart < 0))
		{
			writer.encodeInst(WasmS32Opcode::I32_CONST, constPart, code);
			first = false;
			return 0;
		}
		else if (V2.empty() || V2.front().hasPositive() == false)
		{
			//If we have to put a 0, it's always beneficial to put the maximum value that comes in the single byte encoding
			int smallOffset;
			if (constPart > 0)
				smallOffset = std::min(constPart, (int32_t)63);
			else
				smallOffset = std::max(constPart, (int32_t)-64);
			writer.encodeInst(WasmS32Opcode::I32_CONST, smallOffset, code);
			first = false;
			return constPart - smallOffset;
		}
		else
		{
			return constPart;
		}
	};

	bool first = true;
	//The call to the lambda will properly initialize yetToBeEncodedOffset AND set first to true if something has been written in the stack
	uint32_t yetToBeEncodedOffset = initializeYetToBeEncodedOffset(V2, first);


	for (GroupedValuesToAdd& p : V2)
	{
		p.sort(writer);
	}

	for (const GroupedValuesToAdd& p : V2)
	{
		const bool hasPositive = p.hasPositive();
		bool is_first = true;

		if (p.constantPart)
		{
			writer.encodeInst(WasmS32Opcode::I32_CONST, p.constantPart, code);
			is_first = false;
		}

		for (const llvm::Value* v : p.valuesToAdd)
		{
			writer.compileOperand(code, v);
			if (!is_first)
				writer.encodeInst(WasmOpcode::I32_ADD, code);
			is_first = false;
		}

		for (const llvm::Value* v : p.valuesToInvert)
		{
			writer.compileOperand(code, v);
			if (!is_first)
				writer.encodeInst(WasmOpcode::I32_SUB, code);
			is_first = false;
		}

		const uint32_t sizeCurr = p.multiplier;
		if (sizeCurr > 1)
		{
			if (isPowerOf2_32(sizeCurr))
			{
				writer.encodeInst(WasmS32Opcode::I32_CONST, Log2_32(sizeCurr), code);
				writer.encodeInst(WasmOpcode::I32_SHL, code);
			}
			else
			{
				writer.encodeInst(WasmS32Opcode::I32_CONST, sizeCurr, code);
				writer.encodeInst(WasmOpcode::I32_MUL, code);
			}
		}

		if(!first)
		{
			if (hasPositive)
				writer.encodeInst(WasmOpcode::I32_ADD, code);
			else
				writer.encodeInst(WasmOpcode::I32_SUB, code);
		}
		first = false;
	}

	//In any case we have put something on the stack
	assert(!first);

	return yetToBeEncodedOffset;
}

void CheerpWasmWriter::WasmGepWriter::addConst(int64_t v)
{
	assert(v);
	// Just make sure that the constant part of the offset is not too big
	// TODO: maybe use i64.const here instead of crashing
	assert(v>=std::numeric_limits<int32_t>::min());
	assert(v<=std::numeric_limits<int32_t>::max());

	constPart += v;
}

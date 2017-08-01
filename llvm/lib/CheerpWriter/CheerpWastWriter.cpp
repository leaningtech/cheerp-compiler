//===-- CheerpWastWriter.cpp - The Cheerp JavaScript generator ------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <algorithm>

#include "Relooper.h"
#include "llvm/Cheerp/NameGenerator.h"
#include "llvm/Cheerp/WastWriter.h"
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

static uint32_t COMPILE_METHOD_LIMIT = 100000;

enum BLOCK_TYPE { WHILE1 = 0, DO, SWITCH, CASE, LABEL_FOR_SWITCH, IF };

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

static inline void encodeSLEB128(int64_t Value, std::ostream& OS) {
	bool More;
	do {
		uint8_t Byte = Value & 0x7f;
		// NOTE: this assumes that this signed shift is an arithmetic right shift.
		Value >>= 7;
		More = !((((Value == 0) && ((Byte & 0x40) == 0)) ||
					((Value == -1) && ((Byte & 0x40) != 0))));
		if (More)
			Byte |= 0x80; // Mark this byte to show that more bytes will follow.
		OS << char(Byte);
	} while (More);
}

static inline void encodeULEB128(uint64_t Value, std::ostream& OS,
		unsigned Padding = 0) {
	do {
		uint8_t Byte = Value & 0x7f;
		Value >>= 7;
		if (Value != 0 || Padding != 0)
			Byte |= 0x80; // Mark this byte to show that more bytes will follow.
		OS << char(Byte);
	} while (Value != 0);

	// Pad with 0x80 and emit a null byte at the end.
	if (Padding != 0) {
		for (; Padding != 1; --Padding)
			OS << '\x80';
		OS << '\x00';
	}
}

static inline void encodeF32(float f, std::ostream& stream)
{
	stream.write(reinterpret_cast<const char*>(&f), sizeof(float));
}

static inline void encodeF64(double f, std::ostream& stream)
{
	stream.write(reinterpret_cast<const char*>(&f), sizeof(double));
}

static inline void encodeRegisterKind(Registerize::REGISTER_KIND regKind, std::ostream& stream)
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
		default:
			assert(false);
	}
}

static void encodeValType(const Type* t, std::ostream& stream)
{
	if (t->isIntegerTy() || t->isPointerTy())
		encodeULEB128(0x7f, stream);
	else if (t->isFloatTy())
		encodeULEB128(0x7d, stream);
	else if (t->isDoubleTy())
		encodeULEB128(0x7c, stream);
	else
	{
		llvm::errs() << "Unsupported type ";
		t->dump();
		llvm_unreachable("Unsuppored type");
	}
}

static void encodeLiteralType(Type* t, std::ostream& stream)
{
	if (t->isIntegerTy() || t->isPointerTy())
		encodeULEB128(0x41, stream);
	else if(t->isFloatTy())
		encodeULEB128(0x43, stream);
	else if(t->isDoubleTy())
		encodeULEB128(0x44, stream);
	else
	{
		llvm::errs() << "Unsupported type " << *t << "\n";
		llvm_unreachable("Unsuppored type");
	}
}

static void encodeOpcode(uint32_t opcode, const char* name,
		CheerpWastWriter& writer, std::ostream& code)
{
	if (writer.cheerpMode == CHEERP_MODE_WASM) {
		encodeULEB128(opcode, code);
	} else {
		assert(writer.cheerpMode == CHEERP_MODE_WAST);
		code << name << '\n';
	}
}

static void encodeS32Opcode(uint32_t opcode, const char* name,
		int32_t immediate, CheerpWastWriter& writer, std::ostream& code)
{
	if (writer.cheerpMode == CHEERP_MODE_WASM) {
		encodeULEB128(opcode, code);
		encodeSLEB128(immediate, code);
	} else {
		assert(writer.cheerpMode == CHEERP_MODE_WAST);
		code << name << ' ' << immediate << '\n';
	}
}

static void encodeU32Opcode(uint32_t opcode, const char* name,
		uint32_t immediate, CheerpWastWriter& writer, std::ostream& code)
{
	if (writer.cheerpMode == CHEERP_MODE_WASM) {
		encodeULEB128(opcode, code);
		encodeULEB128(immediate, code);
	} else {
		assert(writer.cheerpMode == CHEERP_MODE_WAST);
		code << name << ' ' << immediate << '\n';
	}
}

static void encodeU32U32Opcode(uint32_t opcode, const char* name,
		uint32_t i1, uint32_t i2, CheerpWastWriter& writer, std::ostream& code)
{
	if (writer.cheerpMode == CHEERP_MODE_WASM) {
		encodeULEB128(opcode, code);
		encodeULEB128(i1, code);
		encodeULEB128(i2, code);
	} else {
		assert(writer.cheerpMode == CHEERP_MODE_WAST);
		code << name << ' ' << i1 << ' ' << i2 << '\n';
	}
}

std::string string_to_hex(const std::string& input)
{
	static const char* const lut = "0123456789abcdef";
	size_t len = input.length();

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

Section::Section(uint32_t sectionId, const char* sectionName, CheerpWastWriter* writer)
	: sectionId(sectionId), sectionName(sectionName), writer(writer)
{
	if (writer->cheerpMode == CHEERP_MODE_WASM)
		encodeULEB128(sectionId, writer->stream);
}
Section::~Section()
{
	std::string buf = str();
	if (writer->cheerpMode == CHEERP_MODE_WASM) {
#if WASM_DUMP_SECTIONS
		uint64_t start = writer->stream.tell();
		fprintf(stderr, "%10s id=0x%x start=0x%08lx end=0x%08lx size=0x%08lx\n",
				sectionName, sectionId, start, start + buf.size(), buf.size());
#if WASM_DUMP_SECTION_DATA
		llvm::errs() << "section: " << string_to_hex(buf) << '\n';
#endif
#endif
		encodeULEB128(buf.size(), writer->stream);
	}
	writer->stream << buf;
}

class CheerpWastRenderInterface: public RenderInterface
{
private:
	CheerpWastWriter* writer;
	std::ostream& code;
	std::vector<BlockType> blockTypes;
	uint32_t labelLocal;
	void renderCondition(const BasicBlock* B, int branchId);
	void indent();
public:
	const BasicBlock* lastDepth0Block;
	CheerpWastRenderInterface(CheerpWastWriter* w, std::ostream& code, uint32_t labelLocal)
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
	void renderDefaultBlockBegin();
	void renderIfBlockBegin(const BasicBlock* condBlock, int branchId, bool first);
	void renderIfBlockBegin(const BasicBlock* condBlock, const vector<int>& branchId, bool first);
	void renderElseBlockBegin();
	void renderBlockEnd();
	void renderBlockPrologue(const BasicBlock* blockTo, const BasicBlock* blockFrom);
	void renderWhileBlockBegin();
	void renderWhileBlockBegin(int labelId);
	void renderDoBlockBegin();
	void renderDoBlockBegin(int labelId);
	void renderDoBlockEnd();
	void renderBreak();
	void renderBreak(int labelId);
	void renderContinue();
	void renderContinue(int labelId);
	void renderLabel(int labelId);
	void renderIfOnLabel(int labelId, bool first);
};

void CheerpWastRenderInterface::renderBlock(const BasicBlock* bb)
{
	if (blockTypes.empty())
		lastDepth0Block = bb;
	else
		lastDepth0Block = nullptr;
	writer->compileBB(code, *bb);
}

void CheerpWastRenderInterface::indent()
{
	if (writer->cheerpMode == CHEERP_MODE_WASM)
		return;

	for(uint32_t i=0;i<blockTypes.size();i++)
		code << "  ";
}

void CheerpWastRenderInterface::renderCondition(const BasicBlock* bb, int branchId)
{
	const TerminatorInst* term=bb->getTerminator();

	if(isa<BranchInst>(term))
	{
		const BranchInst* bi=cast<BranchInst>(term);
		assert(bi->isConditional());
		//The second branch is the default
		assert(branchId==0);
		writer->compileOperand(code, bi->getCondition());
	}
	else if(isa<SwitchInst>(term))
	{
		const SwitchInst* si=cast<SwitchInst>(term);
		assert(branchId > 0);
		SwitchInst::ConstCaseIt it=si->case_begin();
		for(int i=1;i<branchId;i++)
			++it;
		const BasicBlock* dest=it.getCaseSuccessor();
		writer->compileOperand(code, si->getCondition());
		writer->compileOperand(code, it.getCaseValue());
		writer->encodeInst(0x46, "i32.eq", code);
		//We found the destination, there may be more cases for the same
		//destination though
		for(++it;it!=si->case_end();++it)
		{
			if(it.getCaseSuccessor()==dest)
			{
				//Also add this condition
				writer->compileOperand(code, si->getCondition());
				writer->compileOperand(code, it.getCaseValue());
				writer->encodeInst(0x46, "i32.eq", code);
				writer->encodeInst(0x72, "i32.or", code);
			}
		}
	}
	else
	{
		term->dump();
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}
}

void CheerpWastRenderInterface::renderLabelForSwitch(int labelId)
{
	if (writer->cheerpMode == CHEERP_MODE_WASM)
		writer->encodeU32Inst(0x02, "block", 0x40, code);
	else
		code << "block $" << labelId << '\n';
	blockTypes.emplace_back(LABEL_FOR_SWITCH, 1, labelId);
}

void CheerpWastRenderInterface::renderSwitchOnLabel(IdShapeMap& idShapeMap)
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
		writer->encodeU32Inst(0x02, "block", 0x40, code);

	// Wrap the br_table instruction in its own block
	writer->encodeU32Inst(0x02, "block", 0x40, code);
	writer->encodeU32Inst(0x20, "get_local", labelLocal, code);
	if (min != 0)
	{
		writer->encodeS32Inst(0x41, "i32.const", min, code);
		writer->encodeInst(0x6b, "i32.sub", code);
	}

	if (writer->cheerpMode == CHEERP_MODE_WASM) {
		writer->encodeInst(0x0e, "br_table", code);
		encodeULEB128(table.size(), code);
		for (auto label : table)
			encodeULEB128(label, code);
		encodeULEB128(0, code);
	} else {
		code << "br_table";
		for (auto label : table)
			code << " " << label;
		code << " 0\n";
	}

	writer->encodeInst(0x0b, "end", code);

	// The first block does not do anything, and breaks out of the switch.
	writer->encodeU32Inst(0x0c, "br", idShapeMap.size(), code);
	writer->encodeInst(0x0b, "end", code);

	blockTypes.emplace_back(SWITCH, idShapeMap.size());
}

void CheerpWastRenderInterface::renderCaseOnLabel(int labelId)
{
	BlockType prevBlock = blockTypes.back();
	assert(prevBlock.type == SWITCH || prevBlock.type == CASE);
	assert(findSwitchBlockType(blockTypes)->depth > 0);

	blockTypes.emplace_back(CASE);
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

void CheerpWastRenderInterface::renderSwitchBlockBegin(const SwitchInst* si, BlockBranchMap& branchesOut)
{
	assert(si->getNumCases());

	int64_t max = std::numeric_limits<int64_t>::min();
	int64_t min = std::numeric_limits<int64_t>::max();
	for (auto& c: si->cases())
	{
		int64_t curr = c.getCaseValue()->getSExtValue();
		max = std::max(max, curr);
		min = std::min(min, curr);
	}

	// There should be at least one default case and zero or more cases.
	uint32_t depth = (max - min + 1) + 1;
	assert(depth >= 1);

	// Fill the jump table.
	std::vector<int32_t> table;
	table.assign(depth, -1);

	std::unordered_map<const llvm::BasicBlock*, uint32_t> blockIndexMap;
	uint32_t caseBlocks = 0;

	for (auto it : si->cases())
	{
		const BasicBlock* dest = it.getCaseSuccessor();
		const auto& found = blockIndexMap.find(dest);

		if (found == blockIndexMap.end())
		{
			// Use the block index from the Relooper branches list. Otherwise,
			// it is possible that the Relooper branches list does not match
			// with the order of the LLVM Basic Blocks.
			uint32_t blockIndex = findBlockInBranchesOutMap(dest, branchesOut);
			blockIndexMap.emplace(dest, blockIndex);
			table.at(it.getCaseValue()->getSExtValue() - min) = blockIndex;

			// Add cases that have the same destination
			auto it_next = it;
			for (++it_next; it_next != si->case_end(); ++it_next)
			{
				if (it_next.getCaseSuccessor() != dest)
					continue;

				table.at(it_next.getCaseValue()->getSExtValue() - min) = blockIndex;
			}

			caseBlocks++;
		}
	}

	// Elements that are not set, will jump to the default block.
	std::replace(table.begin(), table.end(), -1, (int32_t)caseBlocks);

	// Print the case blocks and the default block.
	for (uint32_t i = 0; i < caseBlocks + 1; i++)
		writer->encodeU32Inst(0x02, "block", 0x40, code);

	// Wrap the br_table instruction in its own block.
	writer->encodeU32Inst(0x02, "block", 0x40, code);
	writer->compileOperand(code, si->getCondition());
	if (min != 0)
	{
		writer->encodeS32Inst(0x41, "i32.const", min, code);
		writer->encodeInst(0x6b, "i32.sub", code);
	}

	// Print the case labels and the default label.
	if (writer->cheerpMode == CHEERP_MODE_WASM) {
		writer->encodeInst(0x0e, "br_table", code);
		encodeULEB128(table.size(), code);
		for (auto label : table)
			encodeULEB128(label, code);
		encodeULEB128(caseBlocks, code);
	} else {
		code << "br_table";
		for (auto label : table)
			code << " " << label;
		code << " " << caseBlocks << "\n";
	}

	writer->encodeInst(0x0b, "end", code);

	blockTypes.emplace_back(SWITCH, caseBlocks + 1);
}

void CheerpWastRenderInterface::renderCaseBlockBegin(const BasicBlock*, int branchId)
{
	BlockType prevBlock = blockTypes.back();
	assert(prevBlock.type == SWITCH || prevBlock.type == CASE);
	assert(findSwitchBlockType(blockTypes)->depth > 0);

	blockTypes.emplace_back(CASE);
}

void CheerpWastRenderInterface::renderDefaultBlockBegin()
{
	renderCaseBlockBegin(nullptr, 0);
}

void CheerpWastRenderInterface::renderIfBlockBegin(const BasicBlock* bb, int branchId, bool first)
{
	if(!first)
	{
		indent();
		writer->encodeInst(0x05, "else", code);
	}
	// The condition goes first
	renderCondition(bb, branchId);
	indent();
	writer->encodeU32Inst(0x04, "if", 0x40, code);
	if(first)
	{
		blockTypes.emplace_back(IF, 1);
	}
	else
	{
		assert(blockTypes.back().type == IF);
		blockTypes.back().depth += 1;
	}
}

void CheerpWastRenderInterface::renderIfBlockBegin(const BasicBlock* bb, const std::vector<int>& skipBranchIds, bool first)
{
	if(!first)
	{
		indent();
		writer->encodeInst(0x05, "else", code);
	}
	// The condition goes first
	for(uint32_t i=0;i<skipBranchIds.size();i++)
	{
		if(i!=0)
		{
			assert(false);
		}
		renderCondition(bb, skipBranchIds[i]);
	}
	// Invert result
	writer->encodeS32Inst(0x41, "i32.const", 1, code);
	writer->encodeInst(0x73, "i32.xor", code);
	indent();
	writer->encodeU32Inst(0x04, "if", 0x40, code);

	if(first)
	{
		blockTypes.emplace_back(IF, 1);
	}
	else
	{
		assert(blockTypes.back().type == IF);
		blockTypes.back().depth += 1;
	}
}

void CheerpWastRenderInterface::renderElseBlockBegin()
{
	assert(!blockTypes.empty());
	assert(blockTypes.back().type == IF);

	indent();
	writer->encodeInst(0x05, "else", code);
}

void CheerpWastRenderInterface::renderBlockEnd()
{
	assert(!blockTypes.empty());
	BlockType block = blockTypes.back();
	blockTypes.pop_back();

	if(block.type == WHILE1)
	{
		// TODO: Why do we even need to fake value
		writer->encodeS32Inst(0x41, "i32.const", 0, code);
		writer->encodeU32Inst(0x0c, "br", 1, code);
		writer->encodeInst(0x0b, "end", code);
		writer->encodeInst(0x0b, "end", code);
	}
	else if (block.type == CASE)
	{
		writer->encodeInst(0x0b, "end", code);
		BlockType* switchBlock = findSwitchBlockType(blockTypes);
		assert(switchBlock->depth > 0);
		switchBlock->depth--;
	}
	else if(block.type == IF)
	{
		for(uint32_t i = 0; i < block.depth; i++)
		{
			indent();
			writer->encodeInst(0x0b, "end", code);
		}
	}
	else if (block.type == SWITCH)
	{
		assert(block.depth == 0);
		if (!blockTypes.empty() && blockTypes.back().type == LABEL_FOR_SWITCH) {
			blockTypes.pop_back();
			writer->encodeInst(0x0b, "end", code);
		}
	}
	else
	{
		assert(false);
	}
}

void CheerpWastRenderInterface::renderBlockPrologue(const BasicBlock* bbTo, const BasicBlock* bbFrom)
{
	writer->compilePHIOfBlockFromOtherBlock(code, bbTo, bbFrom);
}

void CheerpWastRenderInterface::renderWhileBlockBegin()
{
	// Wrap a block in a loop so that:
	// br 1 -> break
	// br 2 -> continue
	indent();
	writer->encodeU32Inst(0x03, "loop", 0x40, code);
	indent();
	writer->encodeU32Inst(0x02, "block", 0x40, code);

	blockTypes.emplace_back(WHILE1, 2);
}

void CheerpWastRenderInterface::renderWhileBlockBegin(int blockLabel)
{
	// Wrap a block in a loop so that:
	// br 1 -> break
	// br 2 -> continue
	indent();

	if (writer->cheerpMode == CHEERP_MODE_WASM)
		writer->encodeU32Inst(0x03, "loop", 0x40, code);
	else
		code << "loop $c" << blockLabel << "\n";

	indent();

	if (writer->cheerpMode == CHEERP_MODE_WASM)
		writer->encodeU32Inst(0x02, "block", 0x40, code);
	else
		code << "block $" << blockLabel << "\n";

	blockTypes.emplace_back(WHILE1, 2, blockLabel);
}

void CheerpWastRenderInterface::renderDoBlockBegin()
{
	indent();
	writer->encodeU32Inst(0x02, "block", 0x40, code);
	blockTypes.emplace_back(DO, 1);
}

void CheerpWastRenderInterface::renderDoBlockBegin(int blockLabel)
{
	indent();

	if (writer->cheerpMode == CHEERP_MODE_WASM)
		writer->encodeU32Inst(0x02, "block", 0x40, code);
	else
		code << "block $" << blockLabel << "\n";

	blockTypes.emplace_back(DO, 1, blockLabel);
}

void CheerpWastRenderInterface::renderDoBlockEnd()
{
	assert(!blockTypes.empty());
	assert(blockTypes.back().type == DO);
	blockTypes.pop_back();

	indent();
	writer->encodeInst(0x0b, "end", code);
}

void CheerpWastRenderInterface::renderBreak()
{
	BlockType block = blockTypes.back();
	if (block.type == CASE)
	{
		BlockType* switchBlock = findSwitchBlockType(blockTypes);
		assert(switchBlock->depth > 0);
		writer->encodeU32Inst(0x0c, "br", switchBlock->depth - 1, code);
	}
	else
	{
		// Find the last loop's block
		uint32_t breakIndex = 0;
		for (uint32_t i = 0; i < blockTypes.size(); i++)
		{
			BLOCK_TYPE bt = blockTypes[blockTypes.size() - i - 1].type;
			breakIndex += blockTypes[blockTypes.size() - i - 1].depth;
			if (bt == WHILE1)
				breakIndex -= 1;
			if (bt == DO || bt == WHILE1 || bt == SWITCH)
				break;
		}
		assert(breakIndex > 1);
		writer->encodeU32Inst(0x0c, "br", breakIndex - 1, code);
	}
}

void CheerpWastRenderInterface::renderBreak(int labelId)
{
	uint32_t breakIndex = 0;
	uint32_t i = 0;
	for (; i < blockTypes.size(); i++)
	{
		BlockType& block = blockTypes[blockTypes.size() - i - 1];

		if (block.label == labelId)
			break;

		breakIndex += block.depth;
	}
	assert(i < blockTypes.size() && "cannot find labelId in block types");
	writer->encodeU32Inst(0x0c, "br", breakIndex, code);
}

void CheerpWastRenderInterface::renderContinue()
{
	// Find the last loop's block
	uint32_t breakIndex = 0;
	for (uint32_t i = 0; i < blockTypes.size(); i++)
	{
		BLOCK_TYPE bt = blockTypes[blockTypes.size() - i - 1].type;
		if (bt == DO || bt == WHILE1)
			break;

		breakIndex += blockTypes[blockTypes.size() - i - 1].depth;
	}
	breakIndex += 1;
	writer->encodeU32Inst(0x0c, "br", breakIndex, code);
}

void CheerpWastRenderInterface::renderContinue(int labelId)
{
	uint32_t breakIndex = 0;
	uint32_t i = 0;
	for (; i < blockTypes.size(); i++)
	{
		BlockType& block = blockTypes[blockTypes.size() - i - 1];

		if (block.label == labelId)
			break;

		breakIndex += block.depth;
	}
	assert(i < blockTypes.size() && "cannot find labelId in block types");
	breakIndex += 1;
	writer->encodeU32Inst(0x0c, "br", breakIndex, code);
}

void CheerpWastRenderInterface::renderLabel(int labelId)
{
	writer->encodeS32Inst(0x41, "i32.const", labelId, code);
	writer->encodeU32Inst(0x21, "set_local", labelLocal, code);
}

void CheerpWastRenderInterface::renderIfOnLabel(int labelId, bool first)
{
	// TODO: Use first to optimize dispatch
	writer->encodeS32Inst(0x41, "i32.const", labelId, code);
	writer->encodeU32Inst(0x20, "get_local", labelLocal, code);
	writer->encodeInst(0x46, "i32.eq", code);
	indent();
	writer->encodeU32Inst(0x04, "if", 0x40, code);
	blockTypes.emplace_back(IF, 1);
}

void CheerpWastWriter::encodeInst(uint32_t opcode, const char* name, std::ostream& code)
{
	encodeOpcode(opcode, name, *this, code);
}

void CheerpWastWriter::encodeBinOp(const llvm::Instruction& I, std::ostream& code)
{
	compileOperand(code, I.getOperand(0));
	compileOperand(code, I.getOperand(1));

	const Type* t = I.getType();
	switch (I.getOpcode())
	{
#define BINOPI(Ty, name, i32, i64) \
		case Instruction::Ty: \
		{ \
			assert(t->isIntegerTy() || t->isPointerTy()); \
			encodeInst(i32, "i32."#name, code); \
			return; \
		}
		BINOPI( Add,   add, 0x6a, 0x7c)
		BINOPI( Sub,   sub, 0x6b, 0x7d)
		BINOPI( Mul,   mul, 0x6c, 0x7e)
		BINOPI(SDiv, div_s, 0x6d, 0x7f)
		BINOPI(UDiv, div_u, 0x6e, 0x80)
		BINOPI(SRem, rem_s, 0x6f, 0x81)
		BINOPI(URem, rem_u, 0x70, 0x82)
		BINOPI( And,   and, 0x71, 0x83)
		BINOPI(  Or,    or, 0x72, 0x84)
		BINOPI( Xor,   xor, 0x73, 0x85)
		BINOPI( Shl,   shl, 0x74, 0x86)
		BINOPI(AShr, shr_s, 0x75, 0x87)
		BINOPI(LShr, shr_u, 0x76, 0x88)
#undef BINOPI

#define BINOPF(Ty, name, f32, f64) \
		case Instruction::Ty: \
		{ \
			if (t->isFloatTy()) { \
				encodeInst(f32, "f32."#name, code); \
				return; \
			} \
			if (t->isDoubleTy()) { \
				encodeInst(f64, "f64."#name, code); \
				return; \
			} \
			break; \
		}
		BINOPF(FAdd,   add, 0x92, 0xa0)
		BINOPF(FSub,   sub, 0x93, 0xa1)
		BINOPF(FMul,   mul, 0x94, 0xa2)
		BINOPF(FDiv,   div, 0x95, 0xa3)
#undef BINOPF
		default:
		{
			I.dump();
			llvm_unreachable("unknown binop instruction");
		}
	}

	I.dump();
	llvm_unreachable("unknown type for binop instruction");
}

void CheerpWastWriter::encodeS32Inst(uint32_t opcode, const char* name, int32_t immediate, std::ostream& code)
{
	encodeS32Opcode(opcode, name, immediate, *this, code);
}

void CheerpWastWriter::encodeU32Inst(uint32_t opcode, const char* name, uint32_t immediate, std::ostream& code)
{
	if (cheerpMode == CHEERP_MODE_WAST) {
		// Do not print the immediate for some opcodes when mode is set to
		// wast. Wast doesn't need the immediate, while wasm does.
		switch(opcode) {
			case 0x02: // "block"
			case 0x03: // "loop"
			case 0x04: // "if"
				encodeOpcode(opcode, name, *this, code);
				return;
			default:
				break;
		}
	}
	encodeU32Opcode(opcode, name, immediate, *this, code);
}

void CheerpWastWriter::encodeU32U32Inst(uint32_t opcode, const char* name, uint32_t i1, uint32_t i2, std::ostream& code)
{
	if (cheerpMode == CHEERP_MODE_WAST) {
		// Do not print the immediates for some opcodes when mode is set to
		// wast. Wast doesn't need the immediate, while wasm does.
		switch(opcode) {
			case 0x28: // "i32.load"
			case 0x2a: // "f32.load"
			case 0x2b: // "f64.load"
			case 0x2c: // "i32.load8_s"
			case 0x2d: // "i32.load8_u"
			case 0x2e: // "i32.load16_s"
			case 0x2f: // "i32.load16_u"
			case 0x36: // "i32.store"
			case 0x38: // "f32.store"
			case 0x39: // "f64.store"
			case 0x3a: // "i32.store8"
			case 0x3b: // "i32.store16"
				encodeOpcode(opcode, name, *this, code);
				return;
			default:
				break;
		}
	}
	encodeU32U32Opcode(opcode, name, i1, i2, *this, code);
}

void CheerpWastWriter::encodePredicate(const llvm::Type* ty, const llvm::CmpInst::Predicate predicate, std::ostream& code)
{
	// TODO add i64 support.
	assert(ty->isIntegerTy() || ty->isPointerTy());
	switch(predicate)
	{
#define PREDICATE(Ty, name, opcode) \
		case CmpInst::ICMP_##Ty: \
			encodeInst(opcode, "i32."#name, code); \
			break;
		PREDICATE( EQ,   eq, 0x46);
		PREDICATE( NE,   ne, 0x47);
		PREDICATE(SLT, lt_s, 0x48);
		PREDICATE(ULT, lt_u, 0x49);
		PREDICATE(SGT, gt_s, 0x4a);
		PREDICATE(UGT, gt_u, 0x4b);
		PREDICATE(SLE, le_s, 0x4c);
		PREDICATE(ULE, le_u, 0x4d);
		PREDICATE(SGE, ge_s, 0x4e);
		PREDICATE(UGE, ge_u, 0x4f);
#undef PREDICATE
		default:
			llvm::errs() << "Handle predicate " << predicate << "\n";
			llvm_unreachable("unknown predicate");
	}
}

void CheerpWastWriter::encodeLoad(const llvm::Type* ty, std::ostream& code)
{
	if(ty->isIntegerTy())
	{
		uint32_t bitWidth = ty->getIntegerBitWidth();
		if(bitWidth == 1)
			bitWidth = 8;

		// TODO add support for i64.
		switch (bitWidth)
		{
			// Currently assume unsigned, like Cheerp. We may optimize
			// this be looking at a following sext or zext instruction.
			case 8:
				encodeU32U32Inst(0x2d, "i32.load8_u", 0x0, 0x0, code);
				break;
			case 16:
				encodeU32U32Inst(0x2f, "i32.load16_u", 0x1, 0x0, code);
				break;
			case 32:
				encodeU32U32Inst(0x28, "i32.load", 0x2, 0x0, code);
				break;
			default:
				llvm::errs() << "bit width: " << bitWidth << '\n';
				llvm_unreachable("unknown integer bit width");
		}
	} else {
		if (ty->isFloatTy())
			encodeU32U32Inst(0x2a, "f32.load", 0x2, 0x0, code);
		else if (ty->isDoubleTy())
			encodeU32U32Inst(0x2b, "f64.load", 0x3, 0x0, code);
		else
			encodeU32U32Inst(0x28, "i32.load", 0x2, 0x0, code);
	}
}

bool CheerpWastWriter::needsPointerKindConversion(const Instruction* phi, const Value* incoming)
{
	const Instruction* incomingInst=dyn_cast<Instruction>(incoming);
	if(!incomingInst)
		return true;
	return isInlineable(*incomingInst, PA) ||
		registerize.getRegisterId(phi)!=registerize.getRegisterId(incomingInst);
}

void CheerpWastWriter::compilePHIOfBlockFromOtherBlock(std::ostream& code, const BasicBlock* to, const BasicBlock* from)
{
	class WriterPHIHandler: public EndOfBlockPHIHandler
	{
	public:
		WriterPHIHandler(CheerpWastWriter& w, std::ostream& c, const BasicBlock* f, const BasicBlock* t)
			:EndOfBlockPHIHandler(w.PA),writer(w), code(c),fromBB(f),toBB(t)
		{
		}
		~WriterPHIHandler()
		{
		}
	private:
		CheerpWastWriter& writer;
		std::ostream& code;
		const BasicBlock* fromBB;
		const BasicBlock* toBB;
		void handleRecursivePHIDependency(const Instruction* incoming) override
		{
			assert(incoming);
			writer.encodeU32Inst(0x20, "get_local", 1 + writer.currentFun->arg_size() + writer.registerize.getRegisterId(incoming), code);
			writer.encodeU32Inst(0x21, "set_local", 1 + writer.currentFun->arg_size() + writer.registerize.getRegisterIdForEdge(incoming, fromBB, toBB), code);
		}
		void handlePHI(const Instruction* phi, const Value* incoming, bool selfReferencing) override
		{
			// We can avoid assignment from the same register if no pointer kind conversion is required
			if(!writer.needsPointerKindConversion(phi, incoming))
				return;
			// 1) Put the value on the stack
			writer.registerize.setEdgeContext(fromBB, toBB);
			writer.compileOperand(code, incoming);
			writer.registerize.clearEdgeContext();
			// 2) Save the value in the phi
			uint32_t local = 1 + writer.currentFun->arg_size() + writer.registerize.getRegisterId(phi);
			writer.encodeU32Inst(0x21, "set_local", local, code);
		}
	};
	WriterPHIHandler(*this, code, from, to).runOnEdge(registerize, from, to);
}

const char* CheerpWastWriter::getTypeString(const Type* t)
{
	if(t->isIntegerTy() || t->isPointerTy())
		return "i32";
	else if(t->isFloatTy())
		return "f32";
	else if(t->isDoubleTy())
		return "f64";
	else
	{
		llvm::errs() << "Unsupported type ";
		t->dump();
		llvm_unreachable("Unsuppored type");
	}
}

void CheerpWastWriter::compileGEP(std::ostream& code, const llvm::User* gep_inst)
{
	WastGepWriter gepWriter(*this, code);
	const llvm::Value *p = linearHelper.compileGEP(gep_inst, &gepWriter);
	compileOperand(code, p);
	if(!gepWriter.first)
		code << "\ni32.add";
}

void CheerpWastWriter::compileSignedInteger(std::ostream& code, const llvm::Value* v, bool forComparison)
{
	uint32_t shiftAmount = 32-v->getType()->getIntegerBitWidth();
	if(const ConstantInt* C = dyn_cast<ConstantInt>(v))
	{
		if(forComparison)
			code << "i32.const " << (C->getSExtValue() << shiftAmount) << '\n';
		else
			code << "i32.const " << C->getSExtValue() << '\n';
		return;
	}

	compileOperand(code, v);
	code << '\n';

	if (shiftAmount == 0)
		return;

	if (forComparison)
	{
		// When comparing two signed values we can avoid the right shift
		code << "i32.const " << shiftAmount << '\n';
		code << "i32.shl\n";
	}
	else
	{
		code << "i32.const " << shiftAmount << '\n';
		code << "i32.shl\n";
		code << "i32.const " << shiftAmount << '\n';
		code << "i32.shr_s\n";
	}
}

void CheerpWastWriter::compileUnsignedInteger(std::ostream& code, const llvm::Value* v)
{
	if(const ConstantInt* C = dyn_cast<ConstantInt>(v))
	{
		code << "i32.const " << C->getZExtValue() << '\n';
		return;
	}

	compileOperand(code, v);
	code << '\n';

	uint32_t initialSize = v->getType()->getIntegerBitWidth();
	if(initialSize != 32)
	{
		code << "i32.const " << getMaskForBitWidth(initialSize) << '\n';
		code << "i32.and\n";
	}
}

void CheerpWastWriter::compileConstantExpr(std::ostream& code, const ConstantExpr* ce)
{
	switch(ce->getOpcode())
	{
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
			compileOperand(code, ce->getOperand(0));
			code << '\n';
			compileOperand(code, ce->getOperand(1));
			code << '\n';
			code << getTypeString(ce->getOperand(0)->getType())
				<< '.' << getIntegerPredicate(p);
			break;
		}
		case Instruction::PtrToInt:
		{
			compileOperand(code, ce->getOperand(0));
			break;
		}
#if 0
		case Instruction::Select:
		{
			compileSelect(ce, ce->getOperand(0), ce->getOperand(1), ce->getOperand(2), HIGHEST);
			break;
		}
		case Instruction::Sub:
		{
			compileSubtraction(ce->getOperand(0), ce->getOperand(1), HIGHEST);
			break;
		}
#endif
		default:
			code << "undefined";
			llvm::errs() << "warning: Unsupported constant expr " << ce->getOpcodeName() << '\n';
	}
}

void CheerpWastWriter::compileConstant(std::ostream& code, const Constant* c)
{
	if(const ConstantExpr* CE = dyn_cast<ConstantExpr>(c))
	{
		compileConstantExpr(code, CE);
	}
	else if(const ConstantInt* i=dyn_cast<ConstantInt>(c))
	{
		code << getTypeString(i->getType()) << ".const ";
		if(i->getBitWidth()==32)
			code << i->getSExtValue();
		else
			code << i->getZExtValue();
	}
	else if(const ConstantFP* f=dyn_cast<ConstantFP>(c))
	{
		code << getTypeString(f->getType()) << ".const ";
		if(f->getValueAPF().isInfinity())
		{
			if(f->getValueAPF().isNegative())
				code << '-';
			code << "inf";
		}
		else if(f->getValueAPF().isNaN())
		{
			code << "nan";
		}
		else
		{
			APFloat apf = f->getValueAPF();
			char buf[40];
			// TODO: Figure out the right amount of hexdigits
			unsigned charCount = apf.convertToHexString(buf, f->getType()->isFloatTy() ? 8 : 16, false, APFloat::roundingMode::rmNearestTiesToEven);
			assert(charCount < 40);
			code << buf;
		}
	}
	else if(const GlobalVariable* GV = dyn_cast<GlobalVariable>(c))
	{
		code << "i32.const " << linearHelper.getGlobalVariableAddress(GV);
	}
	else if(isa<ConstantPointerNull>(c))
	{
		code << "i32.const 0";
	}
	else if(isa<Function>(c))
	{
		const Function* F = cast<Function>(c);
		if (linearHelper.functionHasAddress(F))
		{
			uint32_t addr = linearHelper.getFunctionAddress(F);
			code << "i32.const " << addr;
		}
		else
		{
			c->dump();
			assert(false);
		}
	}
	else if (isa<UndefValue>(c))
	{
		code << getTypeString(c->getType()) << ".const 0";
	}
	else
	{
		c->dump();
		assert(false);
	}
}

void CheerpWastWriter::compileOperand(std::ostream& code, const llvm::Value* v)
{
	if(const Constant* c=dyn_cast<Constant>(v))
		compileConstant(code, c);
	else if(const Instruction* it=dyn_cast<Instruction>(v))
	{
		if(isInlineable(*it, PA))
			compileInstruction(code, *it);
		else
			code << "get_local " << (1 + currentFun->arg_size() + registerize.getRegisterId(it));
	}
	else if(const Argument* arg=dyn_cast<Argument>(v))
	{
		code << "get_local " << arg->getArgNo();
	}
	else
	{
		v->dump();
		assert(false);
	}
}

const char* CheerpWastWriter::getIntegerPredicate(llvm::CmpInst::Predicate p)
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

void CheerpWastWriter::compileDowncast(std::ostream& code, ImmutableCallSite callV)
{
	assert(callV.arg_size() == 2);
	assert(callV.getCalledFunction()->getIntrinsicID() == Intrinsic::cheerp_downcast);

	const Value* src = callV.getArgument(0);
	const Value* offset = callV.getArgument(1);

	Type* t = src->getType()->getPointerElementType();

	compileOperand(code, src);
	code << '\n';

	if(!TypeSupport::isClientType(t) &&
			(!isa<ConstantInt>(offset) || !cast<ConstantInt>(offset)->isNullValue()))
	{
		compileOperand(code, offset);
		code << '\n';
		code << "i32.add\n";
	}
}

bool CheerpWastWriter::compileInstruction(std::ostream& code, const Instruction& I)
{
	switch(I.getOpcode())
	{
		case Instruction::Alloca:
		{
			const AllocaInst* ai = cast<AllocaInst>(&I);
			Type* allocTy = ai->getAllocatedType();
			// TODO: There is another method that includes the alignment
			uint32_t size = targetData.getTypeAllocSize(allocTy);
			uint32_t alignment = TypeSupport::getAlignmentAsmJS(targetData, allocTy);
			assert((alignment & (alignment-1)) == 0 && "alignment must be power of 2");

			// We grow the stack down for now
			// 1) Push the current stack pointer
			code << "get_global " << stackTopGlobal << '\n';
			// 2) Push the allocation size
			if (ai->isArrayAllocation()) {
				const Value* n = ai->getArraySize();
				if(const ConstantInt* C = dyn_cast<ConstantInt>(n))
				{
					code << "i32.const " << (size * C->getSExtValue()) << '\n';
				}
				else
				{
					code << "i32.const " << size << '\n';
					compileOperand(code, n);
					code << "\n";
					code << "i32.mul\n";
				}
			} else {
				code << "i32.const " << size << '\n';
			}
			// 3) Substract the size
			code << "i32.sub\n";
			// 3.1) Optionally align the stack down
			if(size % alignment)
			{
				code << "i32.const " << uint32_t(0-alignment) << '\n';
				code << "i32.and\n";
			}
			// 4) Write the location to the local, but preserve the value
			code << "tee_local " << (1 + currentFun->arg_size() + registerize.getRegisterId(&I)) << '\n';
			// 5) Save the new stack position
			code << "set_global " << stackTopGlobal << '\n';
			return true;
		}
		case Instruction::Add:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".add";
			break;
		}
		case Instruction::And:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".and";
			break;
		}
		case Instruction::AShr:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".shr_s";
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
			code << "\n";
			code << "i32.load\n";
			code << getTypeString(vi.getType()) << ".load\n";

			// Move varargs pointer to next argument
			compileOperand(code, vi.getPointerOperand());
			code << "\n";
			compileOperand(code, vi.getPointerOperand());
			code << "\n";
			code << "i32.load\n";
			code << "i32.const 8\n";
			code << "i32.add\n";
			code << "i32.store\n";
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

			if (calledFunc)
			{
				switch (calledFunc->getIntrinsicID())
				{
					case Intrinsic::trap:
					{
						code << "unreachable ;; trap\n";
						return true;
					}
					case Intrinsic::vastart:
					{
						compileOperand(code, ci.getOperand(0));
						code << '\n';
						uint32_t numArgs = I.getParent()->getParent()->arg_size();
						code << "get_local " << numArgs << "\n";
						code << "i32.store\n";
						return true;
					}
					case Intrinsic::vaend:
					{
						// Do nothing.
						return true;
					}
					case Intrinsic::cheerp_downcast:
					{
						compileDowncast(code, &ci);
						return false;
					}
					case Intrinsic::cheerp_downcast_current:
					{
						compileOperand(code, ci.getOperand(0));
						return false;
					}
					case Intrinsic::cheerp_cast_user:
					{
						if(ci.use_empty())
							return true;

						compileOperand(code, ci.getOperand(0));
						return false;
					}
					case Intrinsic::flt_rounds:
					{
						// Rounding mode 1: nearest
						code << "i32.const 1\n";
						return false;
					}
					case Intrinsic::ctlz:
					{
						compileOperand(code, ci.getOperand(0));
						code << '\n';
						code << "i32.clz\n";
						return false;
					}
					case Intrinsic::invariant_start:
					{
						//TODO: Try to optimize using this, for now just pass the second arg
						if (ci.use_empty())
							return true;

						compileOperand(code, ci.getOperand(1));
						return false;
					}
					case Intrinsic::invariant_end:
					{
						// Do nothing.
						return true;
					}
					case Intrinsic::cheerp_allocate:
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
					default:
					{
						unsigned intrinsic = calledFunc->getIntrinsicID();
						if (intrinsic != Intrinsic::not_intrinsic)
							ci.dump();
						assert(intrinsic == Intrinsic::not_intrinsic);
					}
					break;
				}
			}

			// Calling convention for variadic arguments in Wast mode:
			// arguments are pushed into the stack in the reverse order
			// in which they appear.
			if (fTy->isVarArg())
			{
				size_t n = ci.getNumArgOperands();
				size_t arg_size = fTy->getNumParams();
				size_t i = 0;
				for (auto op = ci.op_begin() + n - 1;
						op != ci.op_begin() + arg_size - 1; op--)
				{
					i++;
					code << "get_global " << stackTopGlobal << '\n';
					code << "i32.const 8\n";
					code << "i32.sub\n";
					// TODO: use 'tee_global' when it's available?
					code << "set_global " << stackTopGlobal << '\n';
					code << "get_global " << stackTopGlobal << '\n';
					compileOperand(code, op->get());
					code << "\n";
					code << getTypeString(op->get()->getType()) << ".store\n";
				}
			}

			for (auto op = ci.op_begin();
					op != ci.op_begin() + fTy->getNumParams(); ++op)
			{
				compileOperand(code, op->get());
				code << '\n';
			}

			if (calledFunc)
			{
				if (linearHelper.getFunctionIds().count(calledFunc))
				{
					code << "call " << linearHelper.getFunctionIds().at(calledFunc);
				}
				else
				{
					// TODO implement ffi calls to the browser side.
					code << "unreachable ;; unknown call \""
						<< calledFunc->getName().str() << "\"\n";
					return true;
				}
			}
			else
			{
				if (linearHelper.getFunctionTables().count(fTy))
				{
					const auto& table = linearHelper.getFunctionTables().at(fTy);
					compileOperand(code, calledValue);
					code << '\n';
					code << "call_indirect $vt_" << table.name;
				}
				else
				{
					// TODO implement ffi calls to the browser side.
					code << "unreachable ;; unknown indirect call\n";
					return true;
				}
			}

			if(ci.getType()->isVoidTy())
			{
				code << '\n';
				return true;
			}
			break;
		}
		case Instruction::FAdd:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".add\n";
			break;
		}
		case Instruction::FCmp:
		{
			const CmpInst& ci = cast<CmpInst>(I);
			if (ci.getPredicate() == CmpInst::FCMP_ORD)
			{
				assert(ci.getOperand(0)->getType() == ci.getOperand(1)->getType());

				// Check if both operands are equal to itself. A nan-value is
				// never equal to itself. Use a logical and operator for the
				// resulting comparison.
				compileOperand(code, ci.getOperand(0));
				code << '\n';
				compileOperand(code, ci.getOperand(0));
				code << '\n';
				code << getTypeString(ci.getOperand(0)->getType()) << ".eq\n";

				compileOperand(code, ci.getOperand(1));
				code << '\n';
				compileOperand(code, ci.getOperand(1));
				code << '\n';
				code << getTypeString(ci.getOperand(1)->getType()) << ".eq\n";

				code << "i32.and\n";
			} else {
				compileOperand(code, ci.getOperand(0));
				code << '\n';
				compileOperand(code, ci.getOperand(1));
				code << '\n';
				code << getTypeString(ci.getOperand(0)->getType()) << '.';
				switch(ci.getPredicate())
				{
					// TODO: Handle ordered vs unordered
					case CmpInst::FCMP_UEQ:
					case CmpInst::FCMP_OEQ:
						code << "eq\n";
						break;
					case CmpInst::FCMP_UNE:
					case CmpInst::FCMP_ONE:
						code << "ne\n";
						break;
					case CmpInst::FCMP_ULT:
					case CmpInst::FCMP_OLT:
						code << "lt\n";
						break;
					case CmpInst::FCMP_OGT:
					case CmpInst::FCMP_UGT:
						code << "gt\n";
						break;
					case CmpInst::FCMP_ULE:
					case CmpInst::FCMP_OLE:
						code << "le\n";
						break;
					case CmpInst::FCMP_UGE:
					case CmpInst::FCMP_OGE:
						code << "ge\n";
						break;
					case CmpInst::FCMP_ORD:
						llvm_unreachable("This case is handled above");
						break;
					default:
						llvm::errs() << "Handle predicate for " << ci << "\n";
						break;
				}
			}
			break;
		}
		case Instruction::FDiv:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".div";
			break;
		}
		case Instruction::FRem:
		{
			// No FRem in wasm, implement manually
			// frem x, y -> fsub (x, fmul( ftrunc ( fdiv (x, y) ), y ) )
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".div\n";
			code << getTypeString(I.getType()) << ".trunc\n";
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".mul\n";
			code << getTypeString(I.getType()) << ".sub";
			break;
		}
		case Instruction::FMul:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".mul";
			break;
		}
		case Instruction::FSub:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".sub";
			break;
		}
		case Instruction::GetElementPtr:
		{
			compileGEP(code, &I);
			break;
		}
		case Instruction::ICmp:
		{
			const CmpInst& ci = cast<CmpInst>(I);
			CmpInst::Predicate p = (CmpInst::Predicate)ci.getPredicate();
			if(ci.getOperand(0)->getType()->isPointerTy())
			{
				compileOperand(code, ci.getOperand(0));
				code << '\n';
				compileOperand(code, ci.getOperand(1));
				code << '\n';
			}
			else if(CmpInst::isSigned(p))
			{
				compileSignedInteger(code, ci.getOperand(0), true);
				code << '\n';
				compileSignedInteger(code, ci.getOperand(1), true);
				code << '\n';
			}
			else if (CmpInst::isUnsigned(p) || !I.getOperand(0)->getType()->isIntegerTy(32))
			{
				compileUnsignedInteger(code, ci.getOperand(0));
				code << '\n';
				compileUnsignedInteger(code, ci.getOperand(1));
				code << '\n';
			}
			else
			{
				compileSignedInteger(code, ci.getOperand(0), true);
				code << '\n';
				compileSignedInteger(code, ci.getOperand(1), true);
				code << '\n';
			}
			code << getTypeString(ci.getOperand(0)->getType()) << '.' << getIntegerPredicate(ci.getPredicate());
			break;
		}
		case Instruction::Load:
		{
			const LoadInst& li = cast<LoadInst>(I);
			const Value* ptrOp=li.getPointerOperand();
			// 1) The pointer
			compileOperand(code, ptrOp);
			code << '\n';
			// 2) Load
			code << getTypeString(li.getType()) << ".load";
			if(li.getType()->isIntegerTy())
			{
				uint32_t bitWidth = li.getType()->getIntegerBitWidth();
				if(bitWidth == 1)
					bitWidth = 8;
				if(bitWidth < 32)
				{
					assert(bitWidth == 8 || bitWidth == 16);
					// Currently assume unsigned, like Cheerp. We may optimize this be looking at a following sext or zext instruction.
					code << bitWidth << "_u";
				}
			}
			break;
		}
		case Instruction::LShr:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".shr_u";
			break;
		}
		case Instruction::Mul:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".mul";
			break;
		}
		case Instruction::Or:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".or";
			break;
		}
		case Instruction::PtrToInt:
		{
			compileOperand(code, I.getOperand(0));
			break;
		}
		case Instruction::Shl:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".shl";
			break;
		}
		case Instruction::Store:
		{
			const StoreInst& si = cast<StoreInst>(I);
			const Value* ptrOp=si.getPointerOperand();
			const Value* valOp=si.getValueOperand();
			// 1) The pointer
			compileOperand(code, ptrOp);
			code << '\n';
			// 2) The value
			compileOperand(code, valOp);
			code << '\n';
			// 3) Store
			code << getTypeString(valOp->getType()) << ".store";
			// When storing values with size less than 32-bit we need to truncate them
			if(valOp->getType()->isIntegerTy())
			{
				uint32_t bitWidth = valOp->getType()->getIntegerBitWidth();
				if(bitWidth == 1)
					bitWidth = 8;
				if(bitWidth < 32)
				{
					assert(bitWidth == 8 || bitWidth == 16);
					code << bitWidth;
				}
			}
			code << '\n';
			break;
		}
		case Instruction::Sub:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".sub";
			break;
		}
		case Instruction::Switch:
			break;
		case Instruction::Trunc:
		{
			// TODO: We need to mask the value
			compileOperand(code, I.getOperand(0));
			break;
		}
		case Instruction::Ret:
		{
			const ReturnInst& ri = cast<ReturnInst>(I);
			Value* retVal = ri.getReturnValue();
			if(retVal)
			{
				compileOperand(code, I.getOperand(0));
				code << '\n';
			}
			// Restore old stack
			code << "get_local " << currentFun->arg_size() << "\n";
			code << "set_global " << stackTopGlobal << '\n';
			code << "return\n";
			break;
		}
		case Instruction::SDiv:
		case Instruction::UDiv:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".div_"
				<< (I.getOpcode() == Instruction::SDiv ? 's' : 'u');
			break;
		}
		case Instruction::SRem:
		case Instruction::URem:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".rem_"
				<< (I.getOpcode() == Instruction::SRem ? 's' : 'u');
			break;
		}
		case Instruction::Select:
		{
			const SelectInst& si = cast<SelectInst>(I);
			compileOperand(code, si.getTrueValue());
			code << '\n';
			compileOperand(code, si.getFalseValue());
			code << '\n';
			compileOperand(code, si.getCondition());
			code << '\n';
			code << "select";
			break;
		}
		case Instruction::SExt:
		{
			uint32_t bitWidth = I.getOperand(0)->getType()->getIntegerBitWidth();
			compileOperand(code, I.getOperand(0));
			code << "\ni32.const " << (32-bitWidth) << '\n';
			code << "i32.shl\n";
			code << "i32.const " << (32-bitWidth) << '\n';
			code << "i32.shr_s";
			break;
		}
		case Instruction::FPToSI:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n' << getTypeString(I.getType()) << ".trunc_s/" << getTypeString(I.getOperand(0)->getType());
			break;
		}
		case Instruction::FPToUI:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n' << getTypeString(I.getType()) << ".trunc_u/" << getTypeString(I.getOperand(0)->getType());
			break;
		}
		case Instruction::SIToFP:
		{
			assert(I.getOperand(0)->getType()->isIntegerTy());
			compileOperand(code, I.getOperand(0));
			uint32_t bitWidth = I.getOperand(0)->getType()->getIntegerBitWidth();
			if(bitWidth != 32)
			{
				// Sign extend
				code << "\ni32.const " << (32-bitWidth) << '\n';
				code << "i32.shl\n";
				code << "i32.const " << (32-bitWidth) << '\n';
				code << "i32.shr_s";
			}
			code << '\n' << getTypeString(I.getType()) << ".convert_s/" << getTypeString(I.getOperand(0)->getType());
			break;
		}
		case Instruction::UIToFP:
		{
			assert(I.getOperand(0)->getType()->isIntegerTy());
			compileOperand(code, I.getOperand(0));
			uint32_t bitWidth = I.getOperand(0)->getType()->getIntegerBitWidth();
			if(bitWidth != 32)
			{
				code << "\ni32.const " << getMaskForBitWidth(bitWidth);
				code << "\ni32.and";
			}
			code << '\n' << getTypeString(I.getType()) << ".convert_u/" << getTypeString(I.getOperand(0)->getType());
			break;
		}
		case Instruction::FPTrunc:
		{
			assert(I.getType()->isFloatTy());
			assert(I.getOperand(0)->getType()->isDoubleTy());
			compileOperand(code, I.getOperand(0));
			code << '\n' << getTypeString(I.getType()) << ".demote/" << getTypeString(I.getOperand(0)->getType());
			break;
		}
		case Instruction::FPExt:
		{
			assert(I.getType()->isDoubleTy());
			assert(I.getOperand(0)->getType()->isFloatTy());
			compileOperand(code, I.getOperand(0));
			code << '\n' << getTypeString(I.getType()) << ".promote/" << getTypeString(I.getOperand(0)->getType());
			break;
		}
		case Instruction::Xor:
		{
			compileOperand(code, I.getOperand(0));
			code << '\n';
			compileOperand(code, I.getOperand(1));
			code << '\n';
			code << getTypeString(I.getType()) << ".xor";
			break;
		}
		case Instruction::ZExt:
		{
			uint32_t bitWidth = I.getOperand(0)->getType()->getIntegerBitWidth();
			compileOperand(code, I.getOperand(0));
			code << "\ni32.const " << getMaskForBitWidth(bitWidth) << '\n';
			code << "i32.and";
			break;
		}
		case Instruction::Unreachable:
		{
			code << "unreachable\n";
			break;
		}
		default:
		{
			I.dump();
			llvm::errs() << "\tImplement inst " << I.getOpcodeName() << '\n';
		}
	}
	return false;
}

void CheerpWastWriter::compileBB(std::ostream& code, const BasicBlock& BB)
{
	BasicBlock::const_iterator I=BB.begin();
	BasicBlock::const_iterator IE=BB.end();
	for(;I!=IE;++I)
	{
		if(isInlineable(*I, PA))
			continue;
		if(I->getOpcode()==Instruction::PHI) //Phys are manually handled
			continue;
		if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(&(*I)))
		{
			//Skip some kind of intrinsics
			if(II->getIntrinsicID()==Intrinsic::lifetime_start ||
				II->getIntrinsicID()==Intrinsic::lifetime_end ||
				II->getIntrinsicID()==Intrinsic::dbg_declare ||
				II->getIntrinsicID()==Intrinsic::dbg_value)
			{
				continue;
			}
		}

		// Display file and line markers in WAST for debugging purposes
		const llvm::DebugLoc& debugLoc = I->getDebugLoc();
		if (!debugLoc.isUnknown() && cheerpMode == CHEERP_MODE_WAST) {
			MDNode* file = debugLoc.getScope(Ctx);
			assert(file);
			assert(file->getNumOperands()>=2);
			MDNode* fileNamePath = cast<MDNode>(file->getOperand(1));
			assert(fileNamePath->getNumOperands()==2);
			StringRef fileName = cast<MDString>(fileNamePath->getOperand(0))->getString();
			uint32_t currentLine = debugLoc.getLine();
			code << ";; " << fileName.str() << ":" << currentLine << "\n";
		}

		if(I->isTerminator() || !I->use_empty() || I->mayHaveSideEffects())
		{
			if(!compileInstruction(code, *I) && !I->getType()->isVoidTy())
			{
				if(I->use_empty())
					code << "\ndrop\n";
				else
					code << "\nset_local " << (1 + currentFun->arg_size() + registerize.getRegisterId(I)) << '\n';
			}
		}
	}
}

void CheerpWastWriter::compileMethodLocals(std::ostream& code, const Function& F, bool needsLabel)
{
	const std::vector<Registerize::RegisterInfo>& regsInfo = registerize.getRegistersForFunction(&F);
	if (cheerpMode == CHEERP_MODE_WASM) {
		// Store the local declaration in a buffer since we need to write the
		// vector length before the vector data.
		std::stringstream locals;

		// Local declarations are compressed into a vector whose entries
		// consist of:
		//
		//   - a u32 `count',
		//   - a `ValType',
		//
		// denoting `count' locals of the same `ValType'.
		//
		// The first local after the params stores the previous stack address
		Registerize::REGISTER_KIND lastKind = Registerize::INTEGER;
		uint32_t count = 1;
		uint32_t pairs = 0;

		// Emit the compressed vector of local registers.
		for(const Registerize::RegisterInfo& regInfo: regsInfo)
		{
			assert(regInfo.regKind != Registerize::OBJECT);
			assert(!regInfo.needsSecondaryName);

			if (regInfo.regKind != lastKind) {
				encodeULEB128(count, locals);
				encodeRegisterKind(regInfo.regKind, locals);
				pairs++;

				lastKind = regInfo.regKind;
				count = 1;
			} else {
				count++;
			}
		}

		// If needed, label is the very last local
		if (needsLabel && lastKind == Registerize::INTEGER)
			count++;

		encodeULEB128(count, locals);
		encodeRegisterKind(lastKind, locals);
		pairs++;

		if (needsLabel && lastKind != Registerize::INTEGER) {
			encodeULEB128(1, locals);
			encodeRegisterKind(Registerize::INTEGER, locals);
			pairs++;
		}

		encodeULEB128(pairs, code);
		code << locals.str();
#if WASM_DUMP_METHODS
		fprintf(stderr, "method locals (%u): ", pairs);
		llvm::errs() << string_to_hex(locals.str()) << '\n';
#endif
	} else {
		// The first local after the params stores the previous stack address
		code << "(local i32";
		// Emit the registers, careful as the registerize id is offset by the number of args
		for(const Registerize::RegisterInfo& regInfo: regsInfo)
		{
			code << ' ';
			assert(regInfo.regKind != Registerize::OBJECT);
			assert(!regInfo.needsSecondaryName);
			switch(regInfo.regKind)
			{
				case Registerize::DOUBLE:
					code << "f64";
					break;
				case Registerize::FLOAT:
					code << "f32";
					break;
				case Registerize::INTEGER:
					code << "i32";
					break;
				default:
					assert(false);
			}
		}
		// If needed, label is the very last local
		if(needsLabel)
			code << " i32";
		code << ")\n";
	}
}

void CheerpWastWriter::compileMethodParams(std::ostream& code, const FunctionType* fTy)
{
	uint32_t numArgs = fTy->getNumParams();
	if (cheerpMode == CHEERP_MODE_WASM)
	{
		encodeULEB128(numArgs, code);

		for(uint32_t i = 0; i < numArgs; i++)
			encodeValType(fTy->getParamType(i), code);
	}
	else if(numArgs)
	{
		assert(cheerpMode == CHEERP_MODE_WAST);
		code << "(param";
		for(uint32_t i = 0; i < numArgs; i++)
			code << ' ' << getTypeString(fTy->getParamType(i));
		code << ')';
	}
}

void CheerpWastWriter::compileMethodResult(std::ostream& code, const Type* ty)
{
	if (cheerpMode == CHEERP_MODE_WASM)
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
	else if(!ty->isVoidTy())
	{
		assert(cheerpMode == CHEERP_MODE_WAST);
		code << "(result " << getTypeString(ty) << ')';
	}
}

void CheerpWastWriter::compileMethod(std::ostream& code, const Function& F)
{
	currentFun = &F;

	if (cheerpMode == CHEERP_MODE_WAST)
	{
		code << "(func $" << F.getName().str();

		// TODO: We should not export them all
		code << " (export \"" << NameGenerator::filterLLVMName(F.getName(),
					NameGenerator::NAME_FILTER_MODE::GLOBAL).str().str() << "\")";

		compileMethodParams(code, F.getFunctionType());
		compileMethodResult(code, F.getReturnType());

		code << '\n';
	}

	uint32_t numArgs = F.arg_size();
	const llvm::BasicBlock* lastDepth0Block = nullptr;

	if(F.size() == 1)
	{
		compileMethodLocals(code, F, false);

		if (cheerpMode == CHEERP_MODE_WAST)
		{
			// TODO: Only save the stack address if required
			code << "get_global " << stackTopGlobal << '\n';
			code << "set_local " << numArgs << "\n";

			compileBB(code, *F.begin());
			lastDepth0Block = &(*F.begin());
		}
	}
	else
	{
		Relooper* rl = CheerpWriter::runRelooperOnFunction(F, PA, registerize);
		compileMethodLocals(code, F, rl->needsLabel());

		if (cheerpMode == CHEERP_MODE_WAST)
		{
			// TODO: Only save the stack address if required
			code << "get_global " << stackTopGlobal << '\n';
			code << "set_local " << numArgs << "\n";
		}

		const std::vector<Registerize::RegisterInfo>& regsInfo = registerize.getRegistersForFunction(&F);
		uint32_t numRegs = regsInfo.size();

		// label is the very last local
		CheerpWastRenderInterface ri(this, code, 1 + numArgs + numRegs);

		if (cheerpMode == CHEERP_MODE_WAST)
		{
			rl->Render(&ri);
			lastDepth0Block = ri.lastDepth0Block;
		}
	}

	// A function has to terminate with a return instruction
	if (!lastDepth0Block || !isa<ReturnInst>(lastDepth0Block->getTerminator()))
	{
		// Add a fake return
		if(!F.getReturnType()->isVoidTy())
		{
#if WASM_DUMP_METHODS
			llvm::errs() << "return type: "; F.getReturnType()->dump();
#endif

			if (cheerpMode == CHEERP_MODE_WASM) {
				// Encode a literal f64, f32 or i32 zero as the return value.
				encodeLiteralType(F.getReturnType(), code);
				if (F.getReturnType()->isDoubleTy()) {
					encodeF64(0., code);
				} else if (F.getReturnType()->isFloatTy()) {
					encodeF32(0.f, code);
				} else {
					encodeSLEB128(0, code);
				}
			} else {
				code << getTypeString(F.getReturnType()) << ".const 0\n";
			}
		}


		if (cheerpMode == CHEERP_MODE_WAST)
			code << "return\n";
	}

	if (cheerpMode == CHEERP_MODE_WASM) {
		// Encode the end of the method.
		encodeULEB128(0x0b, code);
	} else {
		assert(cheerpMode == CHEERP_MODE_WAST);
		code << ")\n";
	}
}

void CheerpWastWriter::compileImport(std::ostream& code, const Function& F)
{
	assert(useWastLoader);
	code << "(func (import \"imports\" \"";
	code << NameGenerator::filterLLVMName(F.getName(),NameGenerator::NAME_FILTER_MODE::GLOBAL).str().str();
	code << "\")";
	uint32_t numArgs = F.arg_size();
	if(numArgs)
	{
		code << "(param";
		llvm::FunctionType* FTy = F.getFunctionType();
		for(uint32_t i = 0; i < numArgs; i++)
			code << ' ' << getTypeString(FTy->getParamType(i));
		code << ')';
	}
	if(!F.getReturnType()->isVoidTy())
		code << "(result " << getTypeString(F.getReturnType()) << ')';
	code << ")\n";
}

void CheerpWastWriter::compileTypeSection()
{
	if (linearHelper.getFunctionTypes().empty())
		return;

	Section section(0x01, "Type", this);

	if (cheerpMode == CHEERP_MODE_WASM)
	{
		// Encode number of entries in the type section.
		encodeULEB128(linearHelper.getFunctionTypes().size(), section);

		// Define function type variables
		for (const auto& fTy : linearHelper.getFunctionTypes())
		{
			encodeULEB128(0x60, section);
			compileMethodParams(section, fTy);
			compileMethodResult(section, fTy->getReturnType());
		}
	} else {
		// Define function type variables
		for (const auto& fTy : linearHelper.getFunctionTypes())
		{
			section << "(type " << "$vt_" << linearHelper.getFunctionTableName(fTy) << " (func ";

			compileMethodParams(section, fTy);
			compileMethodResult(section, fTy->getReturnType());
			section << "))\n";
		}
	}
}

void CheerpWastWriter::compileImportSection()
{
	if (globalDeps.asmJSImports().empty() || !useWastLoader)
		return;

	assert(false);
	Section section(0x02, "Import", this);

	if (cheerpMode == CHEERP_MODE_WASM) {
		// Encode number of entries in the import section.
		encodeULEB128(globalDeps.asmJSImports().size(), section);
	}

	for (const Function* F : globalDeps.asmJSImports())
		compileImport(section, *F);
}

void CheerpWastWriter::compileFunctionSection()
{
	if (linearHelper.getFunctionTypes().empty() || cheerpMode != CHEERP_MODE_WASM)
		return;

	Section section(0x03, "Function", this);

	uint32_t count = 0;
	for (const Function& F : module.functions())
	{
		if (!F.empty() && F.getSection() == StringRef("asmjs"))
			count++;
	}
	count = std::min(count, COMPILE_METHOD_LIMIT); // TODO

	// Encode number of entries in the function section.
	encodeULEB128(count, section);

	// Define function type ids
	size_t i = 0;
	for (const Function& F : module.getFunctionList()) {
		if (F.getSection() != StringRef("asmjs"))
			continue;

		const FunctionType* fTy = F.getFunctionType();
		const auto& found = linearHelper.getFunctionTypeIndices().find(fTy);
		assert(found != linearHelper.getFunctionTypeIndices().end());
		assert(found->second < linearHelper.getFunctionTypes().size());
		encodeULEB128(found->second, section);

		if (++i >= COMPILE_METHOD_LIMIT)
			break; // TODO
	}
}


void CheerpWastWriter::compileTableSection()
{
	if (linearHelper.getFunctionTables().empty())
		return;

	uint32_t count = 0;
	for (const auto& table : linearHelper.getFunctionTables())
		count += table.second.functions.size();

	Section section(0x04, "Table", this);

	if (cheerpMode == CHEERP_MODE_WASM) {
		// Encode number of function tables in the table section.
		encodeULEB128(1, section);

		// Encode element type 'anyfunc'.
		encodeULEB128(0x70, section);

		// Encode function tables in the table section.
		// Use a 'limit' (= 0x00) with only a maximum value.
		encodeULEB128(0x00, section);
		encodeULEB128(count, section);
	} else {
		assert(cheerpMode == CHEERP_MODE_WAST);
		section << "(table anyfunc (elem";
		for (const auto& table : linearHelper.getFunctionTables()) {
			for (const auto& F : table.second.functions) {
				section << " $" << F->getName().str();
			}
		}
		section << "))\n";

	}
}

void CheerpWastWriter::compileMemoryAndGlobalSection()
{
	// Define the memory for the module in WasmPage units. The heap size is
	// defined in MiB and the wasm page size is 64 KiB. Thus, the wasm heap
	// size parameter is defined as: heapSize << 20 >> 16 = heapSize << 4.
	uint32_t minMemory = heapSize << 4;
	uint32_t maxMemory = heapSize << 4;

	// TODO use WasmPage variable instead of hardcoded '1>>16'.
	assert(WasmPage == 64 * 1024);

	{
		Section section(0x05, "Memory", this);

		if (cheerpMode == CHEERP_MODE_WASM) {
			// There is 1 memtype, and the memtype is encoded as {min,max} (= 0x01).
			encodeULEB128(1, section);
			encodeULEB128(0x01, section);
			// Encode minimum and maximum memory parameters.
			encodeULEB128(minMemory, section);
			encodeULEB128(maxMemory, section);
		} else {
			section << "(memory (export \"memory\") " << minMemory << ' ' << maxMemory << ")\n";
		}
	}

	{
		Section section(0x06, "Global", this);

		// Start the stack from the end of default memory
		stackTopGlobal = usedGlobals++;
		uint32_t stackTop = (minMemory * WasmPage);

		if (cheerpMode == CHEERP_MODE_WASM) {
			// There is 1 global.
			encodeULEB128(1, section);
			// The global has type i32 (0x7f) and is mutable (0x01).
			encodeULEB128(0x7f, section);
			encodeULEB128(0x01, section);
			// The global value is a 'i32.const' literal.
			encodeLiteralType(Type::getInt32Ty(Ctx), section);
			encodeSLEB128(stackTop, section);
			// Encode the end of the instruction sequence.
			encodeULEB128(0x0b, section);
		} else {
			section << "(global (mut i32) (i32.const " << stackTop << "))\n";
		}
	}
}

void CheerpWastWriter::compileStartSection()
{
	// Experimental entry point for wasm code
	llvm::Function* entry = module.getFunction("_start");
	if(!entry)
		return;

	Section section(0x08, "Start", this);

	uint32_t functionId = linearHelper.getFunctionIds().at(entry);

	if (cheerpMode == CHEERP_MODE_WASM) {
		encodeULEB128(functionId, section);
	} else {
		section << "(start " << functionId << ")\n";
	}
}

void CheerpWastWriter::compileCodeSection()
{
	Section section(0x0a, "Code", this);

	if (cheerpMode == CHEERP_MODE_WASM) {
		// Encode the number of methods in the code section.
		uint32_t count = 0;
		for (const auto& F: module.functions())
		{
			if (F.getSection() != StringRef("asmjs"))
				continue;
			count++;
		}
		count = std::min(count, COMPILE_METHOD_LIMIT);
		encodeULEB128(count, section);
#if WASM_DUMP_METHODS
		llvm::errs() << "method count: " << count << '\n';
#endif
	}

	size_t i = 0;

	for (const auto& F: module.functions())
	{
		if (F.getSection() != StringRef("asmjs"))
			continue;
		if (cheerpMode == CHEERP_MODE_WASM) {
			std::stringstream method;
#if WASM_DUMP_METHODS
			llvm::errs() << i << " method name: " << F.getName() << '\n';
#endif
			compileMethod(method, F);
			std::string buf = method.str();
#if WASM_DUMP_METHODS
			llvm::errs() << "method length: " << buf.size() << '\n';
			llvm::errs() << "method: " << string_to_hex(buf) << '\n';
#endif
			encodeULEB128(buf.size(), section);
			section << buf;
		} else {
			compileMethod(section, F);
		}
		if (++i == COMPILE_METHOD_LIMIT)
			break; // TODO
	}
}

void CheerpWastWriter::compileDataSection()
{
	Section section(0x0b, "Data", this);

	std::stringstream data;
	uint32_t count = 0;

	for (const GlobalVariable& GV : module.getGlobalList())
	{
		if (GV.getSection() != StringRef("asmjs") || !GV.hasInitializer())
			continue;

		const Constant* init = GV.getInitializer();
		Type* ty = init->getType();
		// If the initializer is a function, skip it
		if (ty->isPointerTy() && ty->getPointerElementType()->isFunctionTy())
			continue;

		count++;

		uint32_t address = linearHelper.getGlobalVariableAddress(&GV);
		if (cheerpMode == CHEERP_MODE_WASM) {
			// In the current version of WebAssembly, at most one memory is
			// allowed in a module. Consequently, the only valid memidx is 0.
			encodeULEB128(0, data);
			// The offset into memory, which is the address
			encodeLiteralType(Type::getInt32Ty(Ctx), data);
			encodeSLEB128(address, data);
			// Encode the end of the instruction sequence.
			encodeULEB128(0x0b, data);
		} else {
			data << "(data (i32.const " << address << ") \"";
		}

		std::stringstream bytes;
		WastBytesWriter bytesWriter(bytes, *this);
		linearHelper.compileConstantAsBytes(init,/* asmjs */ true, &bytesWriter);

		if (cheerpMode == CHEERP_MODE_WASM) {
			// Prefix the number of bytes to the bytes vector.
			std::string buf = bytes.str();
			encodeULEB128(buf.size(), data);
			data.write(buf.data(), buf.size());
		} else {
			data << bytes.str() << "\")\n";
		}
	}

	if (cheerpMode == CHEERP_MODE_WASM)
		encodeULEB128(count, section);

	std::string buf = data.str();
	section.write(buf.data(), buf.size());
}

void CheerpWastWriter::compileModule()
{
	if (cheerpMode == CHEERP_MODE_WAST) {
		stream << "(module\n";
	} else {
		assert(cheerpMode == CHEERP_MODE_WASM);
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
	}

	compileTypeSection();

	compileImportSection();

	compileFunctionSection();

	compileTableSection();

	compileMemoryAndGlobalSection();

	// TODO compileExportSection();

	compileStartSection();

	// TODO compileElementSection();

	compileCodeSection();

	compileDataSection();
	
	if (cheerpMode == CHEERP_MODE_WAST) {
		stream << ')';
	}
}

void CheerpWastWriter::makeWast()
{
	if (getenv("USE_WASM"))
		cheerpMode = CHEERP_MODE_WASM;
	else
		cheerpMode = CHEERP_MODE_WAST;

	compileModule();
}

void CheerpWastWriter::WastBytesWriter::addByte(uint8_t byte)
{
	if (writer.cheerpMode == CHEERP_MODE_WASM) {
		code.write(reinterpret_cast<char*>(&byte), 1);
	} else {
		char buf[4];
		snprintf(buf, 4, "\\%02x", byte);
		code << buf;
	}
}

void CheerpWastWriter::WastGepWriter::addValue(const llvm::Value* v, uint32_t size)
{
	writer.compileOperand(code, v);
	code << '\n';
	if(size != 1)
	{
		code << "i32.const " << size << '\n';
		code << "i32.mul\n";
	}
	if(!first)
		code << "i32.add\n";
	first = false;
}

void CheerpWastWriter::WastGepWriter::addConst(uint32_t v)
{
	assert(v);
	code << "i32.const " << v << '\n';
	if(!first)
		code << "i32.add\n";
	first = false;
}

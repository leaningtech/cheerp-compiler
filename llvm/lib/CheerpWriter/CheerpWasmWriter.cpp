//===-- CheerpWasmWriter.cpp - The Cheerp JavaScript generator ------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2018 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <algorithm>
#include <limits>

#include "Relooper.h"
#include "CFGStackifier.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/NameGenerator.h"
#include "llvm/Cheerp/WasmWriter.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/IR/Type.h"

using namespace cheerp;
using namespace llvm;
using namespace std;

//#define WASM_DUMP_SECTIONS 1
//#define WASM_DUMP_SECTION_DATA 1
//#define WASM_DUMP_METHODS 1
//#define WASM_DUMP_METHOD_DATA 1

// Calling math builtins is very expensive on SpiderMonkey at this time
//#define USE_BUILTINS 1

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

// Methods in this namespace require that |encodeBufferedSetLocal| is called
// before calling the internal method. Otherwise, a buffered set_local will be
// encoded after the to be encoded value, causes subtle bugs.
namespace internal {

// The methods encodeSLEB128 and encodeULEB128 are identical to the ones in
// llvm/Support/LEB128.h, but require a parameter of type `WasmBuffer&`
// instead of the llvm output stream.
static inline void encodeSLEB128(int64_t Value, WasmBuffer& OS) {
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

static inline void encodeULEB128(uint64_t Value, WasmBuffer& OS,
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
		default:
			assert(false);
	}
}

static uint32_t getValType(const Type* t)
{
	if (t->isIntegerTy() || t->isPointerTy())
		return 0x7f;
	else if (t->isFloatTy())
		return 0x7d;
	else if (t->isDoubleTy())
		return 0x7c;
	else
	{
		llvm::errs() << "Unsupported type ";
		t->dump();
		llvm_unreachable("Unsuppored type");
	}
}

static void encodeValType(const Type* t, WasmBuffer& stream)
{
	encodeULEB128(getValType(t), stream);
}

static void encodeLiteralType(const Type* t, WasmBuffer& stream)
{
	if (t->isIntegerTy() || t->isPointerTy())
		encodeULEB128(0x41, stream);
	else if(t->isFloatTy())
		encodeULEB128(0x43, stream);
	else if(t->isDoubleTy())
		encodeULEB128(0x44, stream);
	else
	{
		llvm::errs() << "Unsupported type: ";
		t->dump();
		llvm_unreachable("Unsuppored type");
	}
}

static void encodeOpcode(uint32_t opcode, const char* name,
		CheerpWasmWriter& writer, WasmBuffer& code)
{
	if (writer.cheerpMode == CHEERP_MODE_WASM) {
		assert(opcode <= 255);
		code << char(opcode);
	} else {
		assert(writer.cheerpMode == CHEERP_MODE_WAST);
		code << name << '\n';
	}
}

static void encodeS32Opcode(uint32_t opcode, const char* name,
		int32_t immediate, CheerpWasmWriter& writer, WasmBuffer& code)
{
	if (writer.cheerpMode == CHEERP_MODE_WASM) {
		assert(opcode <= 255);
		code << char(opcode);
		encodeSLEB128(immediate, code);
	} else {
		assert(writer.cheerpMode == CHEERP_MODE_WAST);
		code << name << ' ' << immediate << '\n';
	}
}

static void encodeU32Opcode(uint32_t opcode, const char* name,
		uint32_t immediate, CheerpWasmWriter& writer, WasmBuffer& code)
{
	if (writer.cheerpMode == CHEERP_MODE_WASM) {
		assert(opcode <= 255);
		code << char(opcode);
		encodeULEB128(immediate, code);
	} else {
		assert(writer.cheerpMode == CHEERP_MODE_WAST);
		code << name << ' ' << immediate << '\n';
	}
}

static void encodeU32U32Opcode(uint32_t opcode, const char* name,
		uint32_t i1, uint32_t i2, CheerpWasmWriter& writer, WasmBuffer& code)
{
	if (writer.cheerpMode == CHEERP_MODE_WASM) {
		assert(opcode <= 255);
		code << char(opcode);
		encodeULEB128(i1, code);
		encodeULEB128(i2, code);
	} else {
		assert(writer.cheerpMode == CHEERP_MODE_WAST);
		code << name << ' ' << i1 << ' ' << i2 << '\n';
	}
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

Section::Section(uint32_t sectionId, const char* sectionName, CheerpWasmWriter* writer)
	: sectionId(sectionId), sectionName(sectionName), writer(writer)
{
	if (writer->cheerpMode == CHEERP_MODE_WASM) {
		std::stringstream header;
		internal::encodeULEB128(sectionId, header);
		writer->stream << header.str();

		// Custom sections have a section name.
		if (!sectionId) {
			internal::encodeULEB128(strlen(sectionName), *this);
			(*this) << sectionName;
		}
	}
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

		std::stringstream prefix;
		internal::encodeULEB128(buf.size(), prefix);
		writer->stream << prefix.str();
	}
	writer->stream << buf;
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
	void indent();
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
	void renderSwitchBlockBegin(const llvm::SwitchInst* switchInst, const std::vector<int>& cases);
	void renderCaseBlockBegin(const BasicBlock* caseBlock, int branchId);
	void renderDefaultBlockBegin(bool empty = false);
	void renderIfBlockBegin(const BasicBlock* condBlock, int branchId, bool first);
	void renderIfBlockBegin(const BasicBlock* condBlock, const vector<int>& branchId, bool first);
	void renderElseBlockBegin();
	void renderBlockEnd(bool empty = false);
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

void CheerpWasmRenderInterface::renderSwitchBlockBegin(const llvm::SwitchInst* si,
	const std::vector<int>& cases)
{
	const Value* cond = si->getCondition();

	assert(si->getNumCases());

	llvm::BasicBlock* defaultDest = si->getDefaultDest();
	int64_t max = std::numeric_limits<int64_t>::min();
	int64_t min = std::numeric_limits<int64_t>::max();
	for (auto& c: si->cases())
	{
		if (c.getCaseSuccessor() == defaultDest)
			continue;
		int64_t curr = c.getCaseValue()->getSExtValue();
		max = std::max(max, curr);
		min = std::min(min, curr);
	}

	// There should be at least one default case and zero or more cases.
	uint32_t depth = max - min + 1;
	assert(depth >= 1);

	// Fill the jump table.
	std::vector<uint32_t> table;
	table.assign(depth, numeric_limits<uint32_t>::max());
	uint32_t defaultLabel = numeric_limits<uint32_t>::max();

	uint32_t caseBlocks = 0;
	std::unordered_map<BasicBlock*, uint32_t> blockLabelMap;
	for (int i: cases)
	{
		BasicBlock* dest = i == -1 ? si->getSuccessor(0) : si->getSuccessor(i);
		auto it = blockLabelMap.find(dest);
		if (it == blockLabelMap.end())
		{
			it = blockLabelMap.emplace(dest, caseBlocks++).first;
		}
		if (i == -1)
			defaultLabel = it->second;
		else
		{
			// The value to match for case `i` has index `2*i`
			auto cv = cast<ConstantInt>(si->getOperand(2*i));
			table.at(cv->getSExtValue() - min) = it->second;
		}
	}

	// Elements that are not set, will jump to the default block.
	std::replace(table.begin(), table.end(), numeric_limits<uint32_t>::max(), defaultLabel);

	// Print the case blocks.
	for (uint32_t i = 0; i < caseBlocks; i++)
		writer->encodeU32Inst(0x02, "block", 0x40, code);

	// Wrap the br_table in its own block
	writer->encodeU32Inst(0x02, "block", 0x40, code);

	// Print the condition
	writer->compileOperand(code, si->getCondition());
	uint32_t bitWidth = si->getCondition()->getType()->getIntegerBitWidth();
	if (bitWidth != 32 && CheerpWriter::needsUnsignedTruncation(si->getCondition(), /*asmjs*/true))
	{
		assert(bitWidth < 32);
		writer->encodeS32Inst(0x41, "i32.const", getMaskForBitWidth(bitWidth), code);
		writer->encodeInst(0x71, "i32.and", code);
	}
	if (min != 0)
	{
		writer->encodeS32Inst(0x41, "i32.const", min, code);
		writer->encodeInst(0x6b, "i32.sub", code);
		if (bitWidth != 32)
		{
			writer->encodeS32Inst(0x41, "i32.const", getMaskForBitWidth(bitWidth), code);
			writer->encodeInst(0x71, "i32.and", code);
		}
	}

	// Print the case labels and the default label.
	writer->encodeBranchTable(code, table, defaultLabel);

	writer->encodeInst(0x0b, "end", code);

	blockTypes.emplace_back(SWITCH, caseBlocks);
}

void CheerpWasmRenderInterface::renderBlock(const BasicBlock* bb)
{
	if (blockTypes.empty())
		lastDepth0Block = bb;
	else
		lastDepth0Block = nullptr;
	writer->compileBB(code, *bb);

	if (!lastDepth0Block && isa<ReturnInst>(bb->getTerminator()))
	{
		writer->encodeInst(0x0f, "return", code);
	}
}

void CheerpWasmRenderInterface::indent()
{
	if (writer->cheerpMode == CHEERP_MODE_WASM)
		return;

	for(uint32_t i=0;i<blockTypes.size();i++)
		code << "  ";
}

void CheerpWasmRenderInterface::renderCondition(const BasicBlock* bb,
		const std::vector<int>& branchIds,
		ConditionRenderMode mode)
{
	assert(!branchIds.empty());
	const TerminatorInst* term=bb->getTerminator();

	if(isa<BranchInst>(term))
	{
		assert(branchIds.size() == 1);
		int branchId = branchIds[0];
		const BranchInst* bi=cast<BranchInst>(term);
		assert(bi->isConditional());
		//The second branch is the default
		assert(branchId==0);

		const Value* cond = bi->getCondition();
		bool canInvertCond = isa<Instruction>(cond) && isInlineable(*cast<Instruction>(cond), writer->PA);

		if(canInvertCond && isa<ICmpInst>(cond))
		{
			const ICmpInst* ci = cast<ICmpInst>(cond);
			CmpInst::Predicate p = ci->getPredicate();
			if(mode == InvertCondition)
				p = CmpInst::getInversePredicate(p);
			const ConstantInt* C;
			// Optimize "if (a != 0)" to "if (a)".
			if (p == CmpInst::ICMP_NE &&
					(C = dyn_cast<ConstantInt>(ci->getOperand(1))) &&
					C->getSExtValue() == 0) {
				if(ci->getOperand(0)->getType()->isIntegerTy(32))
					writer->compileSignedInteger(code, ci->getOperand(0), /*forComparison*/true);
				else
					writer->compileUnsignedInteger(code, ci->getOperand(0));
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
				writer->encodeInst(0x45, "i32.eqz", code);
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
			if (!first)
			{
				writer->encodeInst(0x72, "i32.or", code);
			}
			first = false;
		}

		// TODO optimize this by inverting the boolean logic above.
		if (mode == InvertCondition) {
			// Invert result
			writer->encodeS32Inst(0x41, "i32.const", 1, code);
			writer->encodeInst(0x73, "i32.xor", code);
		}
	}
	else
	{
		term->dump();
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}
}

void CheerpWasmRenderInterface::renderLabelForSwitch(int labelId)
{
	if (writer->cheerpMode == CHEERP_MODE_WASM)
		writer->encodeU32Inst(0x02, "block", 0x40, code);
	else
		code << "block $" << labelId << '\n';
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
		writer->encodeU32Inst(0x02, "block", 0x40, code);

	// Wrap the br_table instruction in its own block
	writer->encodeU32Inst(0x02, "block", 0x40, code);
	writer->encodeU32Inst(0x20, "get_local", labelLocal, code);
	if (min != 0)
	{
		writer->encodeS32Inst(0x41, "i32.const", min, code);
		writer->encodeInst(0x6b, "i32.sub", code);
	}

	writer->encodeBranchTable(code, table, 0);

	writer->encodeInst(0x0b, "end", code);

	// The first block does not do anything, and breaks out of the switch.
	writer->encodeU32Inst(0x0c, "br", idShapeMap.size(), code);
	writer->encodeInst(0x0b, "end", code);

	blockTypes.emplace_back(SWITCH, idShapeMap.size());
}

void CheerpWasmRenderInterface::renderCaseOnLabel(int labelId)
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

void CheerpWasmRenderInterface::renderSwitchBlockBegin(const SwitchInst* si, BlockBranchMap& branchesOut)
{
	assert(si->getNumCases());

	llvm::BasicBlock* defaultDest = si->getDefaultDest();
	int64_t max = std::numeric_limits<int64_t>::min();
	int64_t min = std::numeric_limits<int64_t>::max();
	for (auto& c: si->cases())
	{
		if (c.getCaseSuccessor() == defaultDest)
			continue;
		int64_t curr = c.getCaseValue()->getSExtValue();
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

	for (auto it : si->cases())
	{
		const BasicBlock* dest = it.getCaseSuccessor();
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
			table.at(it.getCaseValue()->getSExtValue() - min) = blockIndex;
			assert(blockIndex != numeric_limits<uint32_t>::max());

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
	std::replace(table.begin(), table.end(), numeric_limits<uint32_t>::max(), caseBlocks);

	// Print the case blocks and the default block.
	for (uint32_t i = 0; i < caseBlocks + 1; i++)
		writer->encodeU32Inst(0x02, "block", 0x40, code);

	// Wrap the br_table instruction in its own block.
	writer->encodeU32Inst(0x02, "block", 0x40, code);
	writer->compileOperand(code, si->getCondition());
	uint32_t bitWidth = si->getCondition()->getType()->getIntegerBitWidth();
	if (bitWidth != 32 && CheerpWriter::needsUnsignedTruncation(si->getCondition(), /*asmjs*/true))
	{
		assert(bitWidth < 32);
		writer->encodeS32Inst(0x41, "i32.const", getMaskForBitWidth(bitWidth), code);
		writer->encodeInst(0x71, "i32.and", code);
	}
	if (min != 0)
	{
		writer->encodeS32Inst(0x41, "i32.const", min, code);
		writer->encodeInst(0x6b, "i32.sub", code);
		if (bitWidth != 32)
		{
			writer->encodeS32Inst(0x41, "i32.const", getMaskForBitWidth(bitWidth), code);
			writer->encodeInst(0x71, "i32.and", code);
		}
	}

	// Print the case labels and the default label.
	writer->encodeBranchTable(code, table, caseBlocks);

	writer->encodeInst(0x0b, "end", code);

	blockTypes.emplace_back(SWITCH, caseBlocks + 1);
}

void CheerpWasmRenderInterface::renderCaseBlockBegin(const BasicBlock*, int branchId)
{
	BlockType prevBlock = blockTypes.back();
	assert(prevBlock.type == SWITCH || prevBlock.type == CASE);
	assert(findSwitchBlockType(blockTypes)->depth > 0);

	blockTypes.emplace_back(CASE);
}

void CheerpWasmRenderInterface::renderDefaultBlockBegin(bool)
{
	renderCaseBlockBegin(nullptr, 0);
}

void CheerpWasmRenderInterface::renderIfBlockBegin(const BasicBlock* bb, int branchId, bool first)
{
	if(!first)
	{
		indent();
		writer->encodeInst(0x05, "else", code);
	}
	// The condition goes first
	renderCondition(bb, {branchId}, NormalCondition);
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

void CheerpWasmRenderInterface::renderIfBlockBegin(const BasicBlock* bb, const std::vector<int>& skipBranchIds, bool first)
{
	if(!first)
	{
		indent();
		writer->encodeInst(0x05, "else", code);
	}
	// The condition goes first
	renderCondition(bb, skipBranchIds, InvertCondition);
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

void CheerpWasmRenderInterface::renderElseBlockBegin()
{
	assert(!blockTypes.empty());
	assert(blockTypes.back().type == IF);

	indent();
	writer->encodeInst(0x05, "else", code);
}

void CheerpWasmRenderInterface::renderBlockEnd(bool)
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

void CheerpWasmRenderInterface::renderBlockPrologue(const BasicBlock* bbTo, const BasicBlock* bbFrom)
{
	writer->compilePHIOfBlockFromOtherBlock(code, bbTo, bbFrom);
}

void CheerpWasmRenderInterface::renderWhileBlockBegin()
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

void CheerpWasmRenderInterface::renderWhileBlockBegin(int blockLabel)
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

void CheerpWasmRenderInterface::renderDoBlockBegin()
{
	indent();
	writer->encodeU32Inst(0x02, "block", 0x40, code);
	blockTypes.emplace_back(DO, 1);
}

void CheerpWasmRenderInterface::renderDoBlockBegin(int blockLabel)
{
	indent();

	if (writer->cheerpMode == CHEERP_MODE_WASM)
		writer->encodeU32Inst(0x02, "block", 0x40, code);
	else
		code << "block $" << blockLabel << "\n";

	blockTypes.emplace_back(DO, 1, blockLabel);
}

void CheerpWasmRenderInterface::renderDoBlockEnd()
{
	assert(!blockTypes.empty());
	assert(blockTypes.back().type == DO);
	blockTypes.pop_back();

	indent();
	writer->encodeInst(0x0b, "end", code);
}

void CheerpWasmRenderInterface::renderBreak()
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
		assert(breakIndex > 0);
		writer->encodeU32Inst(0x0c, "br", breakIndex - 1, code);
	}
}

void CheerpWasmRenderInterface::renderBreak(int labelId)
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

void CheerpWasmRenderInterface::renderContinue()
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

void CheerpWasmRenderInterface::renderContinue(int labelId)
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

void CheerpWasmRenderInterface::renderLabel(int labelId)
{
	writer->encodeS32Inst(0x41, "i32.const", labelId, code);
	writer->encodeU32Inst(0x21, "set_local", labelLocal, code);
}

void CheerpWasmRenderInterface::renderIfOnLabel(int labelId, bool first)
{
	// TODO: Use first to optimize dispatch
	writer->encodeS32Inst(0x41, "i32.const", labelId, code);
	writer->encodeU32Inst(0x20, "get_local", labelLocal, code);
	writer->encodeInst(0x46, "i32.eq", code);
	indent();
	writer->encodeU32Inst(0x04, "if", 0x40, code);
	blockTypes.emplace_back(IF, 1);
}

void CheerpWasmWriter::encodeInst(uint32_t opcode, const char* name, WasmBuffer& code)
{
	encodeBufferedSetLocal(code);
	internal::encodeOpcode(opcode, name, *this, code);
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
		default:
			compileOperand(code, I.getOperand(0));
			compileOperand(code, I.getOperand(1));
			break;
	}

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

void CheerpWasmWriter::encodeS32Inst(uint32_t opcode, const char* name, int32_t immediate, WasmBuffer& code)
{
	encodeBufferedSetLocal(code);
	internal::encodeS32Opcode(opcode, name, immediate, *this, code);
}

void CheerpWasmWriter::encodeU32Inst(uint32_t opcode, const char* name, uint32_t immediate, WasmBuffer& code)
{
	// It should not be possible to have two consecutive set_local's with
	// the same local ID.
	assert(opcode != 0x21 || !hasSetLocal || immediate != setLocalId);

	// If this is a get_local instruction and the immediate matches with the
	// buffered set_local instruction, clear the buffered set_local and emit a
	// tee_local.
	if (opcode == 0x20 && hasSetLocal && setLocalId == immediate) {
		internal::encodeU32Opcode(0x22, "tee_local", immediate, *this, code);
		hasSetLocal = false;
		setLocalId = (uint32_t) -1;
		return;
	}

	encodeBufferedSetLocal(code);

	// If this is a set_local instruction, buffer the instruction.
	if (opcode == 0x21) {
		hasSetLocal = true;
		setLocalId = immediate;
		return;
	}

	if (cheerpMode == CHEERP_MODE_WAST) {
		// Do not print the immediate for some opcodes when mode is set to
		// wast. Wast doesn't need the immediate, while wasm does.
		switch(opcode) {
			case 0x02: // "block"
			case 0x03: // "loop"
			case 0x04: // "if"
				internal::encodeOpcode(opcode, name, *this, code);
				return;
			default:
				break;
		}
	}
	internal::encodeU32Opcode(opcode, name, immediate, *this, code);
}

void CheerpWasmWriter::encodeBufferedSetLocal(WasmBuffer& code)
{
	if (hasSetLocal) {
		assert(setLocalId != (uint32_t) -1);
		internal::encodeU32Opcode(0x21, "set_local", setLocalId, *this, code);
		setLocalId = (uint32_t) -1;
		hasSetLocal = false;
	} else {
		assert(setLocalId == (uint32_t) -1);
	}
}

void CheerpWasmWriter::encodeU32U32Inst(uint32_t opcode, const char* name, uint32_t i1, uint32_t i2, WasmBuffer& code)
{
	encodeBufferedSetLocal(code);
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
				code << name;
				if (i2)
					code << " offset=" << i2;
				if (i1)
					code << " align=" << (1 << i1);
				code << '\n';
				return;
			default:
				break;
		}
	}
	internal::encodeU32U32Opcode(opcode, name, i1, i2, *this, code);
}

void CheerpWasmWriter::encodePredicate(const llvm::Type* ty, const llvm::CmpInst::Predicate predicate, WasmBuffer& code)
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

void CheerpWasmWriter::encodeLoad(const llvm::Type* ty, uint32_t offset,
		WasmBuffer& code)
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
				encodeU32U32Inst(0x2d, "i32.load8_u", 0x0, offset, code);
				break;
			case 16:
				encodeU32U32Inst(0x2f, "i32.load16_u", 0x1, offset, code);
				break;
			case 32:
				encodeU32U32Inst(0x28, "i32.load", 0x2, offset, code);
				break;
			default:
				llvm::errs() << "bit width: " << bitWidth << '\n';
				llvm_unreachable("unknown integer bit width");
		}
	} else {
		if (ty->isFloatTy())
			encodeU32U32Inst(0x2a, "f32.load", 0x2, offset, code);
		else if (ty->isDoubleTy())
			encodeU32U32Inst(0x2b, "f64.load", 0x3, offset, code);
		else
			encodeU32U32Inst(0x28, "i32.load", 0x2, offset, code);
	}
}

void CheerpWasmWriter::encodeWasmIntrinsic(WasmBuffer& code, const llvm::Function* F)
{
	if (false) {}
#define WASM_INTRINSIC(name, opcode, symbol) \
	else if (F->getName() == symbol) \
		encodeInst(opcode, name, code);
WASM_INTRINSIC_LIST(WASM_INTRINSIC)
#undef WASM_INTRINSIC
}

bool CheerpWasmWriter::needsPointerKindConversion(const Instruction* phi, const Value* incoming)
{
	const Instruction* incomingInst=getUniqueIncomingInst(incoming, PA);
	if(!incomingInst)
		return true;
	assert(!isInlineable(*incomingInst, PA));
	return registerize.getRegisterId(phi)!=registerize.getRegisterId(incomingInst);
}

void CheerpWasmWriter::compilePHIOfBlockFromOtherBlock(WasmBuffer& code, const BasicBlock* to, const BasicBlock* from)
{
	class WriterPHIHandler: public EndOfBlockPHIHandler
	{
	public:
		WriterPHIHandler(CheerpWasmWriter& w, WasmBuffer& c, const BasicBlock* f, const BasicBlock* t)
			:EndOfBlockPHIHandler(w.PA),writer(w), code(c),fromBB(f),toBB(t)
		{
		}
		~WriterPHIHandler()
		{
		}
	private:
		CheerpWasmWriter& writer;
		WasmBuffer& code;
		const BasicBlock* fromBB;
		const BasicBlock* toBB;
		void handleRecursivePHIDependency(const Instruction* incoming) override
		{
			assert(incoming);
			uint32_t reg = writer.registerize.getRegisterId(incoming);
			uint32_t local = writer.localMap.at(reg);
			writer.encodeU32Inst(0x20, "get_local", local, code);

			reg = writer.registerize.getRegisterIdForEdge(incoming, fromBB, toBB);
			local = writer.localMap.at(reg);
			writer.encodeU32Inst(0x21, "set_local", local, code);

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
			uint32_t reg = writer.registerize.getRegisterId(phi);
			uint32_t local = writer.localMap.at(reg);
			writer.encodeU32Inst(0x21, "set_local", local, code);
		}
	};
	WriterPHIHandler(*this, code, from, to).runOnEdge(registerize, from, to);
}

const char* CheerpWasmWriter::getTypeString(const Type* t)
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

void CheerpWasmWriter::compileGEP(WasmBuffer& code, const llvm::User* gep_inst, bool standalone)
{
	const auto I = dyn_cast<Instruction>(gep_inst);
	if (I && !isInlineable(*I, PA)) {
		if (!standalone) {
			uint32_t reg = registerize.getRegisterId(I);
			uint32_t local = localMap.at(reg);
			encodeU32Inst(0x20, "get_local", local, code);
			return;
		}
	}

	WasmGepWriter gepWriter(*this, code);
	const llvm::Value *p = linearHelper.compileGEP(gep_inst, &gepWriter);
	compileOperand(code, p);
	if(!gepWriter.first)
		encodeInst(0x6a, "i32.add", code);
	if (gepWriter.constPart) {
		encodeS32Inst(0x41, "i32.const", gepWriter.constPart, code);
		encodeInst(0x6a, "i32.add", code);
	}
}

void CheerpWasmWriter::encodeBranchTable(WasmBuffer& code, std::vector<uint32_t> table, int32_t defaultBlock)
{
	encodeBufferedSetLocal(code);

	if (cheerpMode == CHEERP_MODE_WASM) {
		encodeInst(0x0e, "br_table", code);
		internal::encodeULEB128(table.size(), code);
		for (auto label : table)
			internal::encodeULEB128(label, code);
		internal::encodeULEB128(defaultBlock, code);
	} else {
		code << "br_table";
		for (auto label : table)
			code << " " << label;
		code << " " << defaultBlock << "\n";
	}
}

void CheerpWasmWriter::compileSignedInteger(WasmBuffer& code, const llvm::Value* v, bool forComparison)
{
	uint32_t shiftAmount = 32-v->getType()->getIntegerBitWidth();
	if(const ConstantInt* C = dyn_cast<ConstantInt>(v))
	{
		int32_t value = C->getSExtValue();
		if(forComparison)
			value <<= shiftAmount;
		encodeS32Inst(0x41, "i32.const", value, code);
		return;
	}

	compileOperand(code, v);

	if (shiftAmount == 0)
		return;

	if (forComparison)
	{
		// When comparing two signed values we can avoid the right shift
		encodeS32Inst(0x41, "i32.const", shiftAmount, code);
		encodeInst(0x74, "i32.shl", code);
	}
	else
	{
		encodeS32Inst(0x41, "i32.const", shiftAmount, code);
		encodeInst(0x74, "i32.shl", code);
		encodeS32Inst(0x41, "i32.const", shiftAmount, code);
		encodeInst(0x75, "i32.shr_s", code);
	}
}

void CheerpWasmWriter::compileUnsignedInteger(WasmBuffer& code, const llvm::Value* v)
{
	if(const ConstantInt* C = dyn_cast<ConstantInt>(v))
	{
		encodeS32Inst(0x41, "i32.const", C->getZExtValue(), code);
		return;
	}

	compileOperand(code, v);

	uint32_t initialSize = v->getType()->getIntegerBitWidth();
	if(initialSize != 32 && CheerpWriter::needsUnsignedTruncation(v, /*asmjs*/true))
	{
		encodeS32Inst(0x41, "i32.const", getMaskForBitWidth(initialSize), code);
		encodeInst(0x71, "i32.and", code);
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
			encodeInst(0x6a, "i32.add", code);
			break;
		}
		case Instruction::Sub:
		{
			compileOperand(code, ce->getOperand(0));
			compileOperand(code, ce->getOperand(1));
			encodeInst(0x6b, "i32.sub", code);
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
			compileOperand(code, ce->getOperand(0));
			compileOperand(code, ce->getOperand(1));
			encodePredicate(ce->getOperand(0)->getType(), p, code);
			break;
		}
		case Instruction::PtrToInt:
		{
			compileOperand(code, ce->getOperand(0));
			break;
		}
		default:
			encodeInst(0x00, "unreachable", code);
			llvm::errs() << "warning: Unsupported constant expr " << ce->getOpcodeName() << '\n';
	}
}

bool CheerpWasmWriter::tryEncodeFloatAsInt(WasmBuffer& code, const ConstantFP* f)
{
	APFloat apf = f->getValueAPF();
	if (!apf.isInteger())
		return false;

	APFloat truncated = apf;
	truncated.roundToIntegral(APFloat::rmTowardZero);
	int64_t value;
	if (f->getType()->isDoubleTy())
		value = static_cast<int64_t>(truncated.convertToDouble());
	else
		value = static_cast<int64_t>(truncated.convertToFloat());

	// Check that the integer bytes plus conversion byte is smaller than the
	// original float/double in bytes.
	std::stringstream tmp;
	internal::encodeSLEB128(value, tmp);
	if (f->getType()->isDoubleTy() && tmp.str().size() + 1 >= 8)
		return false;

	if (f->getType()->isFloatTy() && tmp.str().size() + 1 >= 4)
		return false;

	// Verify that the value fits in the i32 range.
	if (value < INT32_MIN || value > INT32_MAX)
		return false;

	// Encode the float/double using an integer representation.
	encodeS32Inst(0x41, "i32.const", value, code);

	if (f->getType()->isDoubleTy())
		encodeInst(0xb7, "f64.convert_s/i32", code);
	else
		encodeInst(0xb2, "f32.convert_s/i32", code);

	return true;
}

bool CheerpWasmWriter::tryEncodeFloat64AsFloat32(WasmBuffer& code, const ConstantFP* f)
{
	APFloat apf = f->getValueAPF();
	if (f->getType()->isFloatTy())
		return false;

	double d = apf.convertToDouble();
	float value = d;
	if ((double)value != d)
		return false;

	encodeBufferedSetLocal(code);
	encodeInst(0x43, "f32.const", code);
	internal::encodeF32(value, code);
	encodeInst(0xbb, "f64.promote/f32", code);

	return true;
}

void CheerpWasmWriter::compileConstant(WasmBuffer& code, const Constant* c)
{
	if(const ConstantExpr* CE = dyn_cast<ConstantExpr>(c))
	{
		compileConstantExpr(code, CE);
	}
	else if(const ConstantInt* i=dyn_cast<ConstantInt>(c))
	{
		assert(i->getType()->isIntegerTy() && i->getBitWidth() <= 64);
		if (i->getBitWidth() == 64) {
			assert(i->getSExtValue() <= INT32_MAX);
			assert(i->getSExtValue() >= INT32_MIN);
			encodeS32Inst(0x41, "i32.const", i->getSExtValue(), code);
		} else if (i->getBitWidth() == 32)
			encodeS32Inst(0x41, "i32.const", i->getSExtValue(), code);
		else
			encodeS32Inst(0x41, "i32.const", i->getZExtValue(), code);
	}
	else if(const ConstantFP* f=dyn_cast<ConstantFP>(c))
	{
		// Flush the set local buffer since the next methods use the code
		// stream directly.
		encodeBufferedSetLocal(code);

		if (cheerpMode == CHEERP_MODE_WASM) {
			// Try to encode the float/double using a more compact representation.
			if (tryEncodeFloatAsInt(code, f))
				return;
			if (tryEncodeFloat64AsFloat32(code, f))
				return;

			internal::encodeLiteralType(c->getType(), code);
			if (c->getType()->isDoubleTy()) {
				internal::encodeF64(f->getValueAPF().convertToDouble(), code);
			} else {
				assert(c->getType()->isFloatTy());
				internal::encodeF32(f->getValueAPF().convertToFloat(), code);
			}
		} else {
			// TODO: use encodeInst() and friends.
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
			code << '\n';
		}
	}
	else if(const GlobalVariable* GV = dyn_cast<GlobalVariable>(c))
	{
		uint32_t address = linearHelper.getGlobalVariableAddress(GV);
		encodeS32Inst(0x41, "i32.const", address, code);
	}
	else if(isa<ConstantPointerNull>(c))
	{
		encodeS32Inst(0x41, "i32.const", 0, code);
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
			encodeS32Inst(0x41, "i32.const", addr, code);
		}
		else
		{
			c->dump();
			assert(false);
		}
	}
	else if (isa<UndefValue>(c))
	{
		// Flush the set local buffer since the next methods use the code
		// stream directly.
		encodeBufferedSetLocal(code);

		if (cheerpMode == CHEERP_MODE_WASM) {
			// Encode a literal f64, f32 or i32 zero as the return value.
			internal::encodeLiteralType(c->getType(), code);
			if (c->getType()->isDoubleTy()) {
				internal::encodeF64(0., code);
			} else if (c->getType()->isFloatTy()) {
				internal::encodeF32(0.f, code);
			} else {
				internal::encodeSLEB128(0, code);
			}
		} else {
			code << getTypeString(c->getType()) << ".const 0\n";
		}
	}
	else
	{
		c->dump();
		assert(false);
	}
}

void CheerpWasmWriter::compileOperand(WasmBuffer& code, const llvm::Value* v)
{
	if(const Constant* c=dyn_cast<Constant>(v))
		compileConstant(code, c);
	else if(const Instruction* it=dyn_cast<Instruction>(v))
	{
		if(isInlineable(*it, PA)) {
			compileInlineInstruction(code, *it);
		} else {
			uint32_t idx = registerize.getRegisterId(it);
			uint32_t local = localMap.at(idx);
			encodeU32Inst(0x20, "get_local", local, code);
		}
	}
	else if(const Argument* arg=dyn_cast<Argument>(v))
	{
		uint32_t local = arg->getArgNo();
		encodeU32Inst(0x20, "get_local", local, code);
	}
	else
	{
		v->dump();
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

void CheerpWasmWriter::compileICmp(const ICmpInst& ci, const CmpInst::Predicate p,
		WasmBuffer& code)
{
	if(ci.getOperand(0)->getType()->isPointerTy())
	{
		compileOperand(code, ci.getOperand(0));
		compileOperand(code, ci.getOperand(1));
	}
	else if(CmpInst::isSigned(p))
	{
		compileSignedInteger(code, ci.getOperand(0), true);
		compileSignedInteger(code, ci.getOperand(1), true);
	}
	else if (CmpInst::isUnsigned(p) || !ci.getOperand(0)->getType()->isIntegerTy(32))
	{
		compileUnsignedInteger(code, ci.getOperand(0));
		compileUnsignedInteger(code, ci.getOperand(1));
	}
	else
	{
		compileSignedInteger(code, ci.getOperand(0), true);
		compileSignedInteger(code, ci.getOperand(1), true);
	}
	encodePredicate(ci.getOperand(0)->getType(), p, code);
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
			encodeInst(0x61, "f64.eq", code);
		else
			encodeInst(0x5b, "f32.eq", code);

		compileOperand(code, rhs);
		compileOperand(code, rhs);
		if (ty->isDoubleTy())
			encodeInst(0x61, "f64.eq", code);
		else
			encodeInst(0x5b, "f32.eq", code);

		encodeInst(0x71, "i32.and", code);
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
			encodeInst(0x62, "f64.ne", code);
		else
			encodeInst(0x5c, "f32.ne", code);

		compileOperand(code, rhs);
		compileOperand(code, rhs);
		if (ty->isDoubleTy())
			encodeInst(0x62, "f64.ne", code);
		else
			encodeInst(0x5c, "f32.ne", code);

		encodeInst(0x73, "i32.or", code);
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
#define PREDICATE(Ty, name, f32, f64) \
			case CmpInst::FCMP_O##Ty: \
				if (ty->isDoubleTy()) \
					encodeInst(f64, "f64."#name, code); \
				else \
					encodeInst(f32, "f32."#name, code); \
				break;
			PREDICATE(EQ, eq, 0x5b, 0x61)
			PREDICATE(NE, ne, 0x5c, 0x62)
			PREDICATE(LT, lt, 0x5d, 0x63)
			PREDICATE(GT, gt, 0x5e, 0x64)
			PREDICATE(LE, le, 0x5f, 0x65)
			PREDICATE(GE, ge, 0x60, 0x66)
#undef PREDICATE
			default:
				llvm::errs() << "Handle predicate " << p << "\n";
				break;
		}
		if(invertForUnordered)
		{
			// Invert result
			encodeInst(0x45, "i32.eqz", code);
		}
	}
}

void CheerpWasmWriter::compileDowncast(WasmBuffer& code, ImmutableCallSite callV)
{
	assert(callV.arg_size() == 2);
	assert(callV.getCalledFunction()->getIntrinsicID() == Intrinsic::cheerp_downcast ||
		callV.getCalledFunction()->getIntrinsicID() == Intrinsic::cheerp_virtualcast);

	const Value* src = callV.getArgument(0);
	const Value* offset = callV.getArgument(1);

	Type* t = src->getType()->getPointerElementType();

	compileOperand(code, src);

	if(!TypeSupport::isClientType(t) &&
			(!isa<ConstantInt>(offset) || !cast<ConstantInt>(offset)->isNullValue()))
	{
		compileOperand(code, offset);
		encodeInst(0x6a, "i32.add", code);
	}
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
			encodeU32U32Inst(0x28, "i32.load", 0x2, 0x0, code);
			encodeLoad(vi.getType(), 0, code);

			// Move varargs pointer to next argument
			compileOperand(code, vi.getPointerOperand());
			compileOperand(code, vi.getPointerOperand());
			encodeU32U32Inst(0x28, "i32.load", 0x2, 0x0, code);
			encodeS32Inst(0x41, "i32.const", 8, code);
			encodeInst(0x6a, "i32.add", code);
			encodeU32U32Inst(0x36, "i32.store", 0x2, 0x0, code);
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
				unsigned intrinsicId = calledFunc->getIntrinsicID();
				switch (intrinsicId)
				{
					case Intrinsic::trap:
					{
						encodeInst(0x00, "unreachable", code);
						return true;
					}
					case Intrinsic::stacksave:
					{
						encodeU32Inst(0x23, "get_global", stackTopGlobal, code);
						return false;
					}
					case Intrinsic::stackrestore:
					{
						compileOperand(code, ci.getOperand(0));
						encodeU32Inst(0x24, "set_global", stackTopGlobal, code);
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
						encodeU32U32Inst(0x28, "i32.load", 0x2, 0x0, code);
						encodeU32U32Inst(0x36, "i32.store", 0x2, 0x0, code);
						return true;
					}
					case Intrinsic::vaend:
					{
						// Do nothing.
						return true;
					}
					case Intrinsic::cheerp_downcast:
					case Intrinsic::cheerp_virtualcast:
					{
						compileDowncast(code, &ci);
						return false;
					}
					case Intrinsic::cheerp_downcast_current:
					{
						compileOperand(code, ci.getOperand(0));
						return false;
					}
					case Intrinsic::cheerp_upcast_collapsed:
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
					case Intrinsic::cheerp_grow_memory:
					{
						uint32_t importedId = linearHelper.getBuiltinId(GlobalDepsAnalyzer::GROW_MEM);
						assert(importedId);
						compileOperand(code, ci.getOperand(0));
						encodeU32Inst(0x10, "call", importedId, code);
						return false;
					}
					case Intrinsic::flt_rounds:
					{
						// Rounding mode 1: nearest
						encodeS32Inst(0x41, "i32.const", 1, code);
						return false;
					}
					case Intrinsic::ctlz:
					{
						compileOperand(code, ci.getOperand(0));
						encodeInst(0x67, "i32.clz", code);
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
					case Intrinsic::memmove:
					{
						compileOperand(code, ci.op_begin()->get());
						compileOperand(code, (ci.op_begin() + 1)->get());
						compileOperand(code, (ci.op_begin() + 2)->get());
						llvm::Function* f = module.getFunction("memmove");
						uint32_t functionId = linearHelper.getFunctionIds().at(f);
						encodeU32Inst(0x10, "call", functionId, code);
						encodeInst(0x1a, "drop", code);
						return true;
					}
					case Intrinsic::memcpy:
					{
						compileOperand(code, ci.op_begin()->get());
						compileOperand(code, (ci.op_begin() + 1)->get());
						compileOperand(code, (ci.op_begin() + 2)->get());
						llvm::Function* f = module.getFunction("memcpy");
						uint32_t functionId = linearHelper.getFunctionIds().at(f);
						encodeU32Inst(0x10, "call", functionId, code);
						encodeInst(0x1a, "drop", code);
						return true;
					}
					case Intrinsic::memset:
					{
						compileOperand(code, ci.op_begin()->get());
						compileOperand(code, (ci.op_begin() + 1)->get());
						compileOperand(code, (ci.op_begin() + 2)->get());
						llvm::Function* f = module.getFunction("memset");
						uint32_t functionId = linearHelper.getFunctionIds().at(f);
						encodeU32Inst(0x10, "call", functionId, code);
						encodeInst(0x1a, "drop", code);
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
#ifdef USE_BUILTINS
					case Intrinsic::cos:
					case Intrinsic::exp:
					case Intrinsic::log:
					case Intrinsic::pow:
					case Intrinsic::sin:
					{
						// Handled below
						break;
					}
#endif
					default:
					{
						unsigned intrinsic = calledFunc->getIntrinsicID();
						if (intrinsic != Intrinsic::not_intrinsic)
							ci.dump();
						assert(intrinsic == Intrinsic::not_intrinsic);
					}
					break;
				}
#ifdef USE_BUILTINS
				if(!NoNativeJavaScriptMath || intrinsicId)
				{
					StringRef ident = calledFunc->getName();
					bool builtinFound = false;
					GlobalDepsAnalyzer::BUILTIN b;
					if(ident=="acos" || ident=="acosf")
					{
						builtinFound = true;
						b = GlobalDepsAnalyzer::ACOS_F64;
					}
					else if(ident=="asin" || ident=="asinf")
					{
						builtinFound = true;
						b = GlobalDepsAnalyzer::ASIN_F64;
					}
					else if(ident=="atan" || ident=="atanf")
					{
						builtinFound = true;
						b = GlobalDepsAnalyzer::ATAN_F64;
					}
					else if(ident=="atan2" || ident=="atan2f")
					{
						builtinFound = true;
						b = GlobalDepsAnalyzer::ATAN2_F64;
					}
					else if(ident=="cos" || ident=="cosf" || intrinsicId==Intrinsic::cos)
					{
						builtinFound = true;
						b = GlobalDepsAnalyzer::COS_F64;
					}
					else if(ident=="exp" || ident=="expf" || intrinsicId==Intrinsic::exp)
					{
						builtinFound = true;
						b = GlobalDepsAnalyzer::EXP_F64;
					}
					else if(ident=="log" || ident=="logf" || intrinsicId==Intrinsic::log)
					{
						builtinFound = true;
						b = GlobalDepsAnalyzer::LOG_F64;
					}
					else if(ident=="pow" || ident=="powf" || intrinsicId==Intrinsic::pow)
					{
						builtinFound = true;
						b = GlobalDepsAnalyzer::POW_F64;
					}
					else if(ident=="sin" || ident=="sinf" || intrinsicId==Intrinsic::sin)
					{
						builtinFound = true;
						b = GlobalDepsAnalyzer::SIN_F64;
					}
					else if(ident=="tan" || ident=="tanf")
					{
						builtinFound = true;
						b = GlobalDepsAnalyzer::TAN_F64;
					}

					if(builtinFound)
					{
						// We will use a builtin, do float conversion if needed
						bool floatType = calledFunc->getReturnType()->isFloatTy();
						for (auto op = ci.op_begin(); op != ci.op_begin() + fTy->getNumParams(); ++op)
						{
							compileOperand(code, op->get());
							if(floatType)
								encodeInst(0xbb, "f64.promote/f32", code);
						}
						uint32_t importedId = linearHelper.getBuiltinId(b);
						assert(importedId);
						encodeU32Inst(0x10, "call", importedId, code);
						if(floatType)
							encodeInst(0xb6, "f32.demote/f64", code);
						return false;
					}
				}
#endif
			}

			for (auto op = ci.op_begin();
					op != ci.op_begin() + fTy->getNumParams(); ++op)
			{
				compileOperand(code, op->get());
			}

			if (calledFunc)
			{
				if (isWasmIntrinsic(calledFunc))
				{
					encodeWasmIntrinsic(code, calledFunc);
				}
				else if (linearHelper.getFunctionIds().count(calledFunc))
				{
					uint32_t functionId = linearHelper.getFunctionIds().at(calledFunc);
					if (functionId < COMPILE_METHOD_LIMIT) {
						encodeU32Inst(0x10, "call", functionId, code);
					} else {
						encodeInst(0x00, "unreachable", code);
					}
				}
				else
				{
					llvm::errs() << "warning: Undefined function " << calledFunc->getName() << " called\n";
					encodeInst(0x00, "unreachable", code);
					return true;
				}
			}
			else
			{
				if (linearHelper.getFunctionTables().count(fTy))
				{
					const auto& table = linearHelper.getFunctionTables().at(fTy);
					compileOperand(code, calledValue);
					if (cheerpMode == CHEERP_MODE_WASM) {
						encodeU32U32Inst(0x11, "call_indirect", table.typeIndex, 0, code);
					} else {
						//code << "call_indirect $vt_" << table.name << '\n';
						code << "call_indirect " << table.typeIndex << '\n';
					}
				}
				else
				{
					encodeInst(0x00, "unreachable", code);
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
#define BINOPF(name, f32, f64) \
			if (I.getType()->isFloatTy()) { \
				encodeInst(f32, "f32."#name, code); \
			} \
			else if (I.getType()->isDoubleTy()) { \
				encodeInst(f64, "f64."#name, code); \
			} else { \
				assert(false); \
			}
			BINOPF(  div, 0x95, 0xa3)
			BINOPF(trunc, 0x8f, 0x9d)
			compileOperand(code, I.getOperand(1));
			BINOPF(  mul, 0x94, 0xa2)
			BINOPF(  sub, 0x93, 0xa1)
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
			const Value* ptrOp=li.getPointerOperand();
			uint32_t offset = 0;
			// 1) The pointer
			if (isGEP(ptrOp)) {
				const auto O = dyn_cast<Instruction>(ptrOp);
				if (O && !isInlineable(*O, PA)) {
					uint32_t reg = registerize.getRegisterId(O);
					uint32_t local = localMap.at(reg);
					encodeU32Inst(0x20, "get_local", local, code);
				} else {
					WasmGepWriter gepWriter(*this, code);
					auto p = linearHelper.compileGEP(ptrOp, &gepWriter);
					compileOperand(code, p);
					if(!gepWriter.first)
						encodeInst(0x6a, "i32.add", code);
					// The immediate offset of a load instruction is an unsigned
					// 32-bit integer. Negative immediate offsets are not supported.
					if (gepWriter.constPart < 0) {
						encodeS32Inst(0x41, "i32.const", gepWriter.constPart, code);
						encodeInst(0x6a, "i32.add", code);
					} else {
						offset = gepWriter.constPart;
					}
				}
			} else {
				compileOperand(code, ptrOp);
			}
			// 2) Load
			encodeLoad(li.getType(), offset, code);
			break;
		}
		case Instruction::PtrToInt:
		{
			compileOperand(code, I.getOperand(0));
			break;
		}
		case Instruction::Store:
		{
			const StoreInst& si = cast<StoreInst>(I);
			const Value* ptrOp=si.getPointerOperand();
			const Value* valOp=si.getValueOperand();
			uint32_t offset = 0;
			// 1) The pointer
			if (isGEP(ptrOp)) {
				const auto O = dyn_cast<Instruction>(ptrOp);
				if (O && !isInlineable(*O, PA)) {
					uint32_t reg = registerize.getRegisterId(O);
					uint32_t local = localMap.at(reg);
					encodeU32Inst(0x20, "get_local", local, code);
				} else {
					WasmGepWriter gepWriter(*this, code);
					auto p = linearHelper.compileGEP(ptrOp, &gepWriter);
					compileOperand(code, p);
					if(!gepWriter.first)
						encodeInst(0x6a, "i32.add", code);
					// The immediate offset of a store instruction is an unsigned
					// 32-bit integer. Negative immediate offsets are not supported.
					if (gepWriter.constPart < 0) {
						encodeS32Inst(0x41, "i32.const", gepWriter.constPart, code);
						encodeInst(0x6a, "i32.add", code);
					} else {
						offset = gepWriter.constPart;
					}
				}
			} else {
				compileOperand(code, ptrOp);
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
						encodeU32U32Inst(0x3a, "i32.store8", 0x0, offset, code);
						break;
					case 16:
						encodeU32U32Inst(0x3b, "i32.store16", 0x1, offset, code);
						break;
					case 32:
						encodeU32U32Inst(0x36, "i32.store", 0x2, offset, code);
						break;
					default:
						llvm::errs() << "bit width: " << bitWidth << '\n';
						llvm_unreachable("unknown integer bit width");
				}
			} else {
				if (valOp->getType()->isFloatTy())
					encodeU32U32Inst(0x38, "f32.store", 0x2, offset, code);
				else if (valOp->getType()->isDoubleTy())
					encodeU32U32Inst(0x39, "f64.store", 0x3, offset, code);
				else
					encodeU32U32Inst(0x36, "i32.store", 0x2, offset, code);
			}
			break;
		}
		case Instruction::Switch:
			break;
		case Instruction::Trunc:
		{
			compileOperand(code, I.getOperand(0));
			break;
		}
		case Instruction::Ret:
		{
			const ReturnInst& ri = cast<ReturnInst>(I);
			Value* retVal = ri.getReturnValue();
			if(retVal)
				compileOperand(code, I.getOperand(0));

			break;
		}
		case Instruction::Select:
		{
			const SelectInst& si = cast<SelectInst>(I);
			if(si.getType()->isIntegerTy() || si.getType()->isPointerTy())
			{
				compileOperand(code, si.getTrueValue());
				compileOperand(code, si.getFalseValue());
				compileOperand(code, si.getCondition());
				encodeInst(0x1b, "select", code);
			}
			else
			{
				compileOperand(code, si.getCondition());
				encodeU32Inst(0x04, "if", internal::getValType(si.getType()), code);
				compileOperand(code, si.getTrueValue());
				encodeInst(0x05, "else", code);
				compileOperand(code, si.getFalseValue());
				encodeInst(0x0b, "end", code);
			}
			break;
		}
		case Instruction::SExt:
		{
			uint32_t bitWidth = I.getOperand(0)->getType()->getIntegerBitWidth();
			compileOperand(code, I.getOperand(0));
			encodeS32Inst(0x41, "i32.const", 32-bitWidth, code);
			encodeInst(0x74, "i32.shl", code);
			encodeS32Inst(0x41, "i32.const", 32-bitWidth, code);
			encodeInst(0x75, "i32.shr_s", code);
			break;
		}
		case Instruction::FPToSI:
		{
			// TODO: add support for i64.
			// Wasm opcodes traps on invalid values, we need to do an explicit check if requested
			if(!AvoidWasmTraps)
			{
				compileOperand(code, I.getOperand(0));
				if(I.getOperand(0)->getType()->isFloatTy())
					encodeInst(0xa8, "i32.trunc_s/f32", code);
				else
					encodeInst(0xaa, "i32.trunc_s/f64", code);
			}
			else if (I.getOperand(0)->getType()->isFloatTy())
			{
				compileOperand(code, I.getOperand(0));
				encodeInst(0x8b, "f32.abs", code);
				encodeInst(0x43, "f32.const", code);
				internal::encodeF32(0x80000000, code);
				// Use LT here, we are using the first invalid positive integer as the limit value
				encodeInst(0x5d, "f32.lt", code);
				encodeU32Inst(0x04, "if", 0x7f, code);
				compileOperand(code, I.getOperand(0));
				encodeInst(0xa8, "i32.trunc_s/f32", code);
				encodeInst(0x05, "else", code);
				// We excluded the valid INT32_MIN in the range above, but in the undefined case we use it unconditionally
				encodeS32Inst(0x41, "i32.const", INT32_MIN, code);
				encodeInst(0x0b, "end", code);
			}
			else
			{
				compileOperand(code, I.getOperand(0));
				encodeInst(0x99, "f64.abs", code);
				encodeInst(0x43, "f32.const", code);
				internal::encodeF32(0x80000000, code);
				encodeInst(0xbb, "f64.promote/f32", code);
				// Use LT here, we are using the first invalid positive integer as the limit value
				encodeInst(0x63, "f64.lt", code);
				encodeU32Inst(0x04, "if", 0x7f, code);
				compileOperand(code, I.getOperand(0));
				encodeInst(0xaa, "i32.trunc_s/f64", code);
				encodeInst(0x05, "else", code);
				// We excluded the valid INT32_MIN in the range above, but in the undefined case we use it unconditionally
				encodeS32Inst(0x41, "i32.const", INT32_MIN, code);
				encodeInst(0x0b, "end", code);
			}
			break;
		}
		case Instruction::FPToUI:
		{
			// TODO: add support for i64.
			// Wasm opcodes traps on invalid values, we need to do an explicit check if requested
			if(!AvoidWasmTraps)
			{
				compileOperand(code, I.getOperand(0));
				if(I.getOperand(0)->getType()->isFloatTy())
					encodeInst(0xa9, "i32.trunc_u/f32", code);
				else
					encodeInst(0xab, "i32.trunc_u/f64", code);
			}
			else if (I.getOperand(0)->getType()->isFloatTy())
			{
				compileOperand(code, I.getOperand(0));
				encodeInst(0x43, "f32.const", code);
				internal::encodeF32(0x100000000LL, code);
				// Use LT here, we are using the first invalid positive integer as the limit value
				encodeInst(0x5d, "f32.lt", code);
				// Also compare against 0
				compileOperand(code, I.getOperand(0));
				encodeInst(0x43, "f32.const", code);
				internal::encodeF32(0, code);
				encodeInst(0x60, "f32.ge", code);
				encodeInst(0x71, "i32.and", code);
				encodeU32Inst(0x04, "if", 0x7f, code);
				compileOperand(code, I.getOperand(0));
				encodeInst(0xa9, "i32.trunc_u/f32", code);
				encodeInst(0x05, "else", code);
				encodeS32Inst(0x41, "i32.const", 0, code);
				encodeInst(0x0b, "end", code);
			}
			else
			{
				compileOperand(code, I.getOperand(0));
				encodeInst(0x43, "f32.const", code);
				internal::encodeF32(0x100000000LL, code);
				encodeInst(0xbb, "f64.promote/f32", code);
				// Use LT here, we are using the first invalid positive integer as the limit value
				encodeInst(0x63, "f64.lt", code);
				// Also compare against 0
				compileOperand(code, I.getOperand(0));
				encodeInst(0x43, "f32.const", code);
				internal::encodeF32(0, code);
				encodeInst(0xbb, "f64.promote/f32", code);
				encodeInst(0x66, "f64.ge", code);
				encodeInst(0x71, "i32.and", code);
				encodeU32Inst(0x04, "if", 0x7f, code);
				compileOperand(code, I.getOperand(0));
				encodeInst(0xab, "i32.trunc_u/f64", code);
				encodeInst(0x05, "else", code);
				encodeS32Inst(0x41, "i32.const", 0, code);
				encodeInst(0x0b, "end", code);
			}
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
				encodeS32Inst(0x41, "i32.const", 32-bitWidth, code);
				encodeInst(0x74, "i32.shl", code);
				encodeS32Inst(0x41, "i32.const", 32-bitWidth, code);
				encodeInst(0x75, "i32.shr_s", code);
			}
			// TODO: add support for i64.
			if (I.getType()->isDoubleTy()) {
				encodeInst(0xb7, "f64.convert_s/i32", code);
			} else {
				assert(I.getType()->isFloatTy());
				encodeInst(0xb2, "f32.convert_s/i32", code);
			}
			break;
		}
		case Instruction::UIToFP:
		{
			assert(I.getOperand(0)->getType()->isIntegerTy());
			compileOperand(code, I.getOperand(0));
			uint32_t bitWidth = I.getOperand(0)->getType()->getIntegerBitWidth();
			if(bitWidth != 32)
			{
				encodeS32Inst(0x41, "i32.const", getMaskForBitWidth(bitWidth), code);
				encodeInst(0x71, "i32.and", code);
			}
			// TODO: add support for i64.
			if (I.getType()->isDoubleTy()) {
				encodeInst(0xb8, "f64.convert_u/i32", code);
			} else {
				assert(I.getType()->isFloatTy());
				encodeInst(0xb3, "f32.convert_u/i32", code);
			}
			break;
		}
		case Instruction::FPTrunc:
		{
			assert(I.getType()->isFloatTy());
			assert(I.getOperand(0)->getType()->isDoubleTy());
			compileOperand(code, I.getOperand(0));
			encodeInst(0xb6, "f32.demote/f64", code);
			break;
		}
		case Instruction::FPExt:
		{
			assert(I.getType()->isDoubleTy());
			assert(I.getOperand(0)->getType()->isFloatTy());
			compileOperand(code, I.getOperand(0));
			encodeInst(0xbb, "f64.promote/f32", code);
			break;
		}
		case Instruction::ZExt:
		{
			compileUnsignedInteger(code, I.getOperand(0));
			break;
		}
		case Instruction::IntToPtr:
		{
			compileOperand(code, I.getOperand(0));
			break;
		}
		case Instruction::Unreachable:
		{
			encodeInst(0x00, "unreachable", code);
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

void CheerpWasmWriter::compileBB(WasmBuffer& code, const BasicBlock& BB)
{
	BasicBlock::const_iterator I=BB.begin();
	BasicBlock::const_iterator IE=BB.end();
	for(;I!=IE;++I)
	{
		if(isInlineable(*I, PA))
			continue;
		if(I->getOpcode()==Instruction::PHI) //Phis are manually handled
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
		if (debugLoc && cheerpMode == CHEERP_MODE_WAST) {
			MDNode* file = debugLoc.getScope();
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
				if(I->use_empty()) {
					encodeInst(0x1a, "drop", code);
				} else {
					uint32_t reg = registerize.getRegisterId(I);
					uint32_t local = localMap.at(reg);
					encodeU32Inst(0x21, "set_local", local, code);
				}
			}
		}
	}
}

void CheerpWasmWriter::compileMethodLocals(WasmBuffer& code, const vector<int>& locals)
{
	if (cheerpMode == CHEERP_MODE_WASM) {
		uint32_t groups = (uint32_t) locals.at(Registerize::INTEGER) > 0;
		groups += (uint32_t) locals.at(Registerize::DOUBLE) > 0;
		groups += (uint32_t) locals.at(Registerize::FLOAT) > 0;

		// Local declarations are compressed into a vector whose entries
		// consist of:
		//
		//   - a u32 `count',
		//   - a `ValType',
		//
		// denoting `count' locals of the same `ValType'.
		internal::encodeULEB128(groups, code);

		if (locals.at(Registerize::INTEGER)) {
			internal::encodeULEB128(locals.at(Registerize::INTEGER), code);
			internal::encodeRegisterKind(Registerize::INTEGER, code);
		}

		if (locals.at(Registerize::DOUBLE)) {
			internal::encodeULEB128(locals.at(Registerize::DOUBLE), code);
			internal::encodeRegisterKind(Registerize::DOUBLE, code);
		}

		if (locals.at(Registerize::FLOAT)) {
			internal::encodeULEB128(locals.at(Registerize::FLOAT), code);
			internal::encodeRegisterKind(Registerize::FLOAT, code);
		}
	} else {
		code << "(local";

		if (locals.at(Registerize::INTEGER)) {
			for (int i = 0; i < locals.at(Registerize::INTEGER); i++)
				code << " i32";
		}

		if (locals.at(Registerize::DOUBLE)) {
			for (int i = 0; i < locals.at(Registerize::DOUBLE); i++)
				code << " f64";
		}

		if (locals.at(Registerize::FLOAT)) {
			for (int i = 0; i < locals.at(Registerize::FLOAT); i++)
				code << " f32";
		}

		code << ")\n";
	}
}

void CheerpWasmWriter::compileMethodParams(WasmBuffer& code, const FunctionType* fTy)
{
	uint32_t numArgs = fTy->getNumParams();
	if (cheerpMode == CHEERP_MODE_WASM)
	{
		internal::encodeULEB128(numArgs, code);

		for(uint32_t i = 0; i < numArgs; i++)
			internal::encodeValType(fTy->getParamType(i), code);
	}
	else if(fTy->getNumParams())
	{
		assert(cheerpMode == CHEERP_MODE_WAST);
		code << "(param";
		for(uint32_t i = 0; i < numArgs; i++)
			code << ' ' << getTypeString(fTy->getParamType(i));
		code << ')';
	}
}

void CheerpWasmWriter::compileMethodResult(WasmBuffer& code, const Type* ty)
{
	if (cheerpMode == CHEERP_MODE_WASM)
	{
		if (ty->isVoidTy())
		{
			internal::encodeULEB128(0, code);
		}
		else
		{
			internal::encodeULEB128(1, code);
			internal::encodeValType(ty, code);
		}
	}
	else if(!ty->isVoidTy())
	{
		assert(cheerpMode == CHEERP_MODE_WAST);
		code << "(result " << getTypeString(ty) << ')';
	}
}

void CheerpWasmWriter::compileMethod(WasmBuffer& code, Function& F)
{
	assert(!F.empty());
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

	Relooper* rl = nullptr;
	bool needsLabel = false;

	if (F.size() != 1 && !useCfgStackifier) {
		rl = CheerpWriter::runRelooperOnFunction(F, PA, registerize);
		needsLabel = rl->needsLabel();
	}

	const std::vector<Registerize::RegisterInfo>& regsInfo = registerize.getRegistersForFunction(&F);
	uint32_t localCount = regsInfo.size() + (int)needsLabel;

	vector<int> locals(4, 0);
	localMap.assign(localCount, 0);
	uint32_t reg = 0;

	// Make lookup table for registers to locals.
	for(const Registerize::RegisterInfo& regInfo: regsInfo)
	{
		assert(regInfo.regKind != Registerize::OBJECT);
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
			case Registerize::DOUBLE:
				offset += locals.at((int)Registerize::INTEGER);
				break;
			case Registerize::FLOAT:
				offset += locals.at((int)Registerize::INTEGER);
				offset += locals.at((int)Registerize::DOUBLE);
				break;
			case Registerize::OBJECT:
				assert(false);
				break;
		}
		localMap[reg++] += offset;
	}

	compileMethodLocals(code, locals);

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
		CheerpWasmRenderInterface ri(this, code, labelLocal);
		if (useCfgStackifier)
		{
			DominatorTree &DT = pass.getAnalysis<DominatorTreeWrapperPass>(F).getDomTree();
			LoopInfo &LI = pass.getAnalysis<LoopInfoWrapperPass>(F).getLoopInfo();
			CFGStackifier C(F, LI, DT);
			C.render(ri, registerize, PA, true);
		}
		else
		{
			rl->Render(&ri);
		}
		lastDepth0Block = ri.lastDepth0Block;
	}

	// A function has to terminate with a return value when the return type is
	// not void.
	if (!lastDepth0Block || !isa<ReturnInst>(lastDepth0Block->getTerminator()))
	{
		if(!F.getReturnType()->isVoidTy())
		{
			if (cheerpMode == CHEERP_MODE_WASM) {
				// Encode a literal f64, f32 or i32 zero as the return value.
				internal::encodeLiteralType(F.getReturnType(), code);
				if (F.getReturnType()->isDoubleTy()) {
					internal::encodeF64(0., code);
				} else if (F.getReturnType()->isFloatTy()) {
					internal::encodeF32(0.f, code);
				} else {
					internal::encodeSLEB128(0, code);
				}
			} else {
				code << getTypeString(F.getReturnType()) << ".const 0\n";
			}
		}
	}

	assert(!hasSetLocal && setLocalId == (uint32_t) -1);

	if (cheerpMode == CHEERP_MODE_WASM) {
		// Encode the end of the method.
		internal::encodeULEB128(0x0b, code);
	} else {
		assert(cheerpMode == CHEERP_MODE_WAST);
		code << ")\n";
	}
}

void CheerpWasmWriter::compileTypeSection()
{
	if (linearHelper.getFunctionTypes().empty())
		return;

	Section section(0x01, "Type", this);

	if (cheerpMode == CHEERP_MODE_WASM)
	{
		// Encode number of entries in the type section.
		internal::encodeULEB128(linearHelper.getFunctionTypes().size(), section);

		// Define function type variables
		for (const auto& fTy : linearHelper.getFunctionTypes())
		{
			internal::encodeULEB128(0x60, section);
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

void CheerpWasmWriter::compileImport(WasmBuffer& code, StringRef funcName, FunctionType* fTy)
{
	assert(useWasmLoader);

	std::string fieldName = funcName;

	if (cheerpMode == CHEERP_MODE_WASM) {
		// Encode the module name.
		std::string moduleName = "imports";
		internal::encodeULEB128(moduleName.size(), code);
		code.write(moduleName.data(), moduleName.size());

		// Encode the field name.
		internal::encodeULEB128(fieldName.size(), code);
		code.write(fieldName.data(), fieldName.size());

		// Encode kind as 'Function' (= 0).
		internal::encodeULEB128(0x00, code);

		// Encode type index of function signature.
		const auto& found = linearHelper.getFunctionTypeIndices().find(fTy);
		assert(found != linearHelper.getFunctionTypeIndices().end());
		internal::encodeULEB128(found->second, code);
	} else {
		code << "(func (import \"imports\" \"";
		code.write(fieldName.data(), fieldName.size());
		code << "\")";
		uint32_t numArgs = fTy->getNumParams();
		if(numArgs)
		{
			code << "(param";
			for(uint32_t i = 0; i < numArgs; i++)
				code << ' ' << getTypeString(fTy->getParamType(i));
			code << ')';
		}
		if(!fTy->getReturnType()->isVoidTy())
			code << "(result " << getTypeString(fTy->getReturnType()) << ')';
		code << ")\n";
	}
}

void CheerpWasmWriter::compileImportSection()
{
	// Count imported builtins
	uint32_t importedBuiltins = 0;
	for(uint32_t i=0;i<GlobalDepsAnalyzer::MAX_BUILTIN;i++)
	{
		if(linearHelper.getBuiltinId(GlobalDepsAnalyzer::BUILTIN(i)))
			importedBuiltins++;
	}

	uint32_t importedTotal = importedBuiltins + globalDeps.asmJSImports().size();

	if (importedTotal == 0 || !useWasmLoader)
		return;

	Section section(0x02, "Import", this);

	if (cheerpMode == CHEERP_MODE_WASM) {
		// Encode number of entries in the import section.
		internal::encodeULEB128(importedTotal, section);
	}

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
	if(linearHelper.getBuiltinId(GlobalDepsAnalyzer::ACOS_F64))
		compileImport(section, namegen.getBuiltinName(NameGenerator::ACOS), f64_f64_1);
	if(linearHelper.getBuiltinId(GlobalDepsAnalyzer::ASIN_F64))
		compileImport(section, namegen.getBuiltinName(NameGenerator::ASIN), f64_f64_1);
	if(linearHelper.getBuiltinId(GlobalDepsAnalyzer::ATAN_F64))
		compileImport(section, namegen.getBuiltinName(NameGenerator::ATAN), f64_f64_1);
	if(linearHelper.getBuiltinId(GlobalDepsAnalyzer::ATAN2_F64))
		compileImport(section, namegen.getBuiltinName(NameGenerator::ATAN2), f64_f64_2);
	if(linearHelper.getBuiltinId(GlobalDepsAnalyzer::COS_F64))
		compileImport(section, namegen.getBuiltinName(NameGenerator::COS), f64_f64_1);
	if(linearHelper.getBuiltinId(GlobalDepsAnalyzer::EXP_F64))
		compileImport(section, namegen.getBuiltinName(NameGenerator::EXP), f64_f64_1);
	if(linearHelper.getBuiltinId(GlobalDepsAnalyzer::LOG_F64))
		compileImport(section, namegen.getBuiltinName(NameGenerator::LOG), f64_f64_1);
	if(linearHelper.getBuiltinId(GlobalDepsAnalyzer::POW_F64))
		compileImport(section, namegen.getBuiltinName(NameGenerator::POW), f64_f64_2);
	if(linearHelper.getBuiltinId(GlobalDepsAnalyzer::SIN_F64))
		compileImport(section, namegen.getBuiltinName(NameGenerator::SIN), f64_f64_1);
	if(linearHelper.getBuiltinId(GlobalDepsAnalyzer::TAN_F64))
		compileImport(section, namegen.getBuiltinName(NameGenerator::TAN), f64_f64_1);
	if(linearHelper.getBuiltinId(GlobalDepsAnalyzer::GROW_MEM))
		compileImport(section, namegen.getBuiltinName(NameGenerator::GROW_MEM), i32_i32_1);
}

void CheerpWasmWriter::compileFunctionSection()
{
	if (linearHelper.getFunctionTypes().empty() || cheerpMode != CHEERP_MODE_WASM)
		return;

	Section section(0x03, "Function", this);

	uint32_t count = linearHelper.functions().size();
	count = std::min(count, COMPILE_METHOD_LIMIT); // TODO

	// Encode number of entries in the function section.
	internal::encodeULEB128(count, section);

	// Define function type ids
	size_t i = 0;
	for (const Function* F : linearHelper.functions()) {
		const FunctionType* fTy = F->getFunctionType();
		const auto& found = linearHelper.getFunctionTypeIndices().find(fTy);
		assert(found != linearHelper.getFunctionTypeIndices().end());
		assert(found->second < linearHelper.getFunctionTypes().size());
		internal::encodeULEB128(found->second, section);

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

	if (cheerpMode == CHEERP_MODE_WASM) {
		// Encode number of function tables in the table section.
		internal::encodeULEB128(1, section);

		// Encode element type 'anyfunc'.
		internal::encodeULEB128(0x70, section);

		// Encode function tables in the table section.
		// Use a 'limit' (= 0x00) with only a maximum value.
		internal::encodeULEB128(0x00, section);
		internal::encodeULEB128(count, section);
	} else {
		assert(cheerpMode == CHEERP_MODE_WAST);
		section << "(table anyfunc (elem";
		size_t j = 0;
		for (const FunctionType* fTy: linearHelper.getFunctionTableOrder()) {
			const auto table = linearHelper.getFunctionTables().find(fTy);
			for (const auto& F : table->second.functions) {
				section << " $" << F->getName().str();
				if (++j == COMPILE_METHOD_LIMIT)
					break; // TODO
			}
			if (j == COMPILE_METHOD_LIMIT)
				break; // TODO
		}
		section << "))\n";

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

	{
		Section section(0x05, "Memory", this);

		if (cheerpMode == CHEERP_MODE_WASM) {
			// There is 1 memtype, and the memtype is encoded as {min,max} (= 0x01).
			internal::encodeULEB128(1, section);
			internal::encodeULEB128(0x01, section);
			// Encode minimum and maximum memory parameters.
			internal::encodeULEB128(minMemory, section);
			internal::encodeULEB128(maxMemory, section);
		} else {
			section << "(memory (export \"memory\") " << minMemory << ' ' << maxMemory << ")\n";
		}
	}

	{
		Section section(0x06, "Global", this);

		// Start the stack from the end of default memory
		stackTopGlobal = usedGlobals++;
		uint32_t stackTop = linearHelper.getStackStart();

		if (cheerpMode == CHEERP_MODE_WASM) {
			// There is 1 global.
			internal::encodeULEB128(1, section);
			// The global has type i32 (0x7f) and is mutable (0x01).
			internal::encodeULEB128(0x7f, section);
			internal::encodeULEB128(0x01, section);
			// The global value is a 'i32.const' literal.
			internal::encodeLiteralType(Type::getInt32Ty(Ctx), section);
			internal::encodeSLEB128(stackTop, section);
			// Encode the end of the instruction sequence.
			internal::encodeULEB128(0x0b, section);
		} else {
			section << "(global (mut i32) (i32.const " << stackTop << "))\n";
		}
	}
}

void CheerpWasmWriter::compileExportSection()
{
	if (cheerpMode == CHEERP_MODE_WAST)
		return;

	Section section(0x07, "Export", this);
	std::vector<const llvm::Function*> exports;

	// Export the webMain symbol, if defined.
	const llvm::Function* entry = globalDeps.getEntryPoint();
	if(entry && entry->getSection() == StringRef("asmjs")) {
		assert(globalDeps.asmJSExports().find(entry) == globalDeps.asmJSExports().end());
		exports.push_back(entry);
	}

	if (llvm::Function* _start = module.getFunction("_start"))
		exports.push_back(_start);

	// Add the list of asmjs-exported functions.
	exports.insert(exports.end(), globalDeps.asmJSExports().begin(),
			globalDeps.asmJSExports().end());

	// Add 1 to the count, since we always want to export the memory.
	internal::encodeULEB128(exports.size() + 1, section);

	// Encode the memory.
	std::string name = "memory";
	internal::encodeULEB128(name.size(), section);
	section.write(name.data(), name.size());
	internal::encodeULEB128(0x02, section);
	internal::encodeULEB128(0, section);

	for (const llvm::Function* F : exports) {
		// Encode the method name.
		name = namegen.getName(F);

		internal::encodeULEB128(name.size(), section);
		section.write(name.data(), name.size());

		// Encode the function index (where '0x00' means that this export is a
		// function).
		internal::encodeULEB128(0x00, section);
		internal::encodeULEB128(linearHelper.getFunctionIds().find(F)->second, section);
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

	if (cheerpMode == CHEERP_MODE_WASM) {
		internal::encodeULEB128(functionId, section);
	} else {
		section << "(start " << functionId << ")\n";
	}
}

void CheerpWasmWriter::compileElementSection()
{
	if (cheerpMode == CHEERP_MODE_WAST)
		return;
	if (linearHelper.getFunctionTables().empty())
		return;

	Section section(0x09, "Element", this);

	// There is only one element segment.
	internal::encodeULEB128(1, section);

	// The table index is 0 in the MVP.
	internal::encodeULEB128(0, section);

	// The offset into memory, which is the address.
	int32_t offset = 0;
	internal::encodeLiteralType(Type::getInt32Ty(Ctx), section);
	internal::encodeSLEB128(offset, section);
	// Encode the end of the instruction sequence.
	internal::encodeULEB128(0x0b, section);

	// Encode the sequence of function indices.
	std::stringstream elem;
	size_t count = 0;
	for (const FunctionType* fTy: linearHelper.getFunctionTableOrder()) {
		const auto table = linearHelper.getFunctionTables().find(fTy);
		for (const auto& F : table->second.functions) {
			uint32_t idx = linearHelper.getFunctionIds().at(F);
			internal::encodeULEB128(idx, elem);
			count++;
		}
	}
	std::string buf = elem.str();
	internal::encodeULEB128(count, section);
	section << buf;
}

void CheerpWasmWriter::compileCodeSection()
{
	Section section(0x0a, "Code", this);

	if (cheerpMode == CHEERP_MODE_WASM) {
		// Encode the number of methods in the code section.
		uint32_t count = linearHelper.functions().size();
		count = std::min(count, COMPILE_METHOD_LIMIT);
		internal::encodeULEB128(count, section);
#if WASM_DUMP_METHODS
		llvm::errs() << "method count: " << count << '\n';
#endif
	}

	size_t i = 0;

	for (Function* F: linearHelper.functions())
	{
		if (cheerpMode == CHEERP_MODE_WASM) {
			std::stringstream method;
#if WASM_DUMP_METHODS
			llvm::errs() << i << " method name: " << F->getName() << '\n';
#endif
			compileMethod(method, *F);
			std::string buf = method.str();
#if WASM_DUMP_METHOD_DATA
			llvm::errs() << "method length: " << buf.size() << '\n';
			llvm::errs() << "method: " << string_to_hex(buf) << '\n';
#endif
			internal::encodeULEB128(buf.size(), section);
			section << buf;
		} else {
			compileMethod(section, *F);
		}
		if (++i == COMPILE_METHOD_LIMIT)
			break; // TODO
	}
}

void CheerpWasmWriter::encodeDataSectionChunk(WasmBuffer& data, uint32_t address, const std::string& buf)
{
	if (cheerpMode == CHEERP_MODE_WASM) {
		// In the current version of WebAssembly, at most one memory is
		// allowed in a module. Consequently, the only valid memidx is 0.
		internal::encodeULEB128(0, data);
		// The offset into memory, which is the address
		internal::encodeLiteralType(Type::getInt32Ty(Ctx), data);
		internal::encodeSLEB128(address, data);
		// Encode the end of the instruction sequence.
		internal::encodeULEB128(0x0b, data);
		// Prefix the number of bytes to the bytes vector.
		internal::encodeULEB128(buf.size(), data);
		data.write(buf.data(), buf.size());
	} else {
		data << "(data (i32.const " << address << ") \"" << buf << "\")\n";
	}
}

uint32_t CheerpWasmWriter::encodeDataSectionChunks(WasmBuffer& data, uint32_t address, const std::string& buf)
{
	// Split data section buffer into chunks based on 6 (or more) zero bytes.
	uint32_t chunks = 0;
	size_t cur = 0, last = 0, end = 0;
	std::string delimiter("\0\0\0\0\0\0\0", 6);
	while ((cur = buf.find(delimiter, last)) != std::string::npos) {
		std::string chunk = buf.substr(last, cur - last);
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

	std::stringstream data;
	uint32_t count = 0;

	auto globals = linearHelper.globals();
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
		std::stringstream bytes;
		WasmBytesWriter bytesWriter(bytes, *this);

		for (; g != e; ++g) {
			GV = *g;

			// Do not concatenate global variables that have no initialiser or
			// are zero-initialised.
			if (!linearHelper.hasNonZeroInitialiser(GV))
				break;
			init = GV->getInitializer();

			// Determine amount of padding bytes necessary for the alignment.
			long written = bytes.tellp();
			uint32_t nextAddress = linearHelper.getGlobalVariableAddress(GV);
			uint32_t padding = nextAddress - (address + written);
			for (uint32_t i = 0; i < padding; i++)
				bytes << (char)0;

			linearHelper.compileConstantAsBytes(init,/* asmjs */ true, &bytesWriter);
		}

		std::string buf = bytes.str();

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

	if (cheerpMode == CHEERP_MODE_WASM)
		internal::encodeULEB128(count, section);

	std::string buf = data.str();
	section.write(buf.data(), buf.size());
}

void CheerpWasmWriter::compileNameSection()
{
	if (cheerpMode != CHEERP_MODE_WASM)
		return;

	assert(prettyCode);
	Section section(0x00, "name", this);

	// Assign names to functions
	{
		std::stringstream data;
		uint32_t count = linearHelper.functions().size();
		internal::encodeULEB128(count, data);

		for (const Function* F : linearHelper.functions())
		{
			uint32_t functionId = linearHelper.getFunctionIds().at(F);
			internal::encodeULEB128(functionId, data);
			internal::encodeULEB128(F->getName().size(), data);
			data << F->getName().str();
		}

		std::string buf = data.str();

		internal::encodeULEB128(0x01, section);
		internal::encodeULEB128(buf.size(), section);
		section.write(buf.data(), buf.size());
	}
}

void CheerpWasmWriter::compileModule()
{
	if (cheerpMode == CHEERP_MODE_WAST) {
		stream << "(module\n";
	} else {
		assert(cheerpMode == CHEERP_MODE_WASM);
		std::stringstream code;

		// Magic number for wasm.
		internal::encodeULEB128(0x00, code);
		internal::encodeULEB128(0x61, code);
		internal::encodeULEB128(0x73, code);
		internal::encodeULEB128(0x6D, code);
		// Version number.
		internal::encodeULEB128(0x01, code);
		internal::encodeULEB128(0x00, code);
		internal::encodeULEB128(0x00, code);
		internal::encodeULEB128(0x00, code);

		stream << code.str();
	}

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
	
	if (cheerpMode == CHEERP_MODE_WAST) {
		stream << ')';
	}
}

void CheerpWasmWriter::makeWasm()
{
	compileModule();
}

void CheerpWasmWriter::WasmBytesWriter::addByte(uint8_t byte)
{
	if (writer.cheerpMode == CHEERP_MODE_WASM) {
		code.write(reinterpret_cast<char*>(&byte), 1);
	} else {
		char buf[4];
		snprintf(buf, 4, "\\%02x", byte);
		code << buf;
	}
}

void CheerpWasmWriter::WasmGepWriter::addValue(const llvm::Value* v, uint32_t size)
{
	writer.compileOperand(code, v);
	if (size > 1)
	{
		if (isPowerOf2_32(size))
		{
			writer.encodeS32Inst(0x41, "i32.const", Log2_32(size), code);
			writer.encodeInst(0x74, "i32.shl", code);
		}
		else
		{
			writer.encodeS32Inst(0x41, "i32.const", size, code);
			writer.encodeInst(0x6c, "i32.mul", code);
		}
	}
	if(!first)
		writer.encodeInst(0x6a, "i32.add", code);
	first = false;
}

void CheerpWasmWriter::WasmGepWriter::addConst(int64_t v)
{
	assert(v);
	// Just make sure that the constant part of the offset is not too big
	// TODO: maybe use i64.const here instead of crashing
	assert(v>=std::numeric_limits<int32_t>::min());
	assert(v<=std::numeric_limits<int32_t>::max());

	constPart = v;
}

bool CheerpWasmWriter::WasmGepWriter::isInlineable(const llvm::Value* p)
{
	if (const auto I = dyn_cast<BitCastInst>(p))
		return ::isInlineable(*I, writer.PA);

	if (const auto I = dyn_cast<GetElementPtrInst>(p))
		return ::isInlineable(*I, writer.PA);

	return true;
}

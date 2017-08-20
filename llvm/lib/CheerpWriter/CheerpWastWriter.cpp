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
#include "llvm/Cheerp/Writer.h"
#include "llvm/Cheerp/WastWriter.h"
#include "llvm/Support/LEB128.h"

using namespace cheerp;
using namespace llvm;
using namespace std;

enum BLOCK_TYPE { WHILE1 = 0, DO, SWITCH, CASE, LABEL_FOR_SWITCH, IF };

class BlockType
{
public:
	BLOCK_TYPE type;
	uint32_t depth;

	BlockType(BLOCK_TYPE bt, uint32_t depth = 0)
	 :
		type(bt),
		depth(depth)
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

inline void encodeULEB128(uint64_t Value, std::ostream& OS,
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

Section::Section(uint32_t sectionId, CheerpWastWriter* writer)
	: std::stringstream(), writer(writer)
{
	if (writer->cheerpMode == CHEERP_MODE_WASM) {
		llvm::errs() << "section id: " << sectionId << '\n';
		encodeULEB128(sectionId, writer->stream);
	}
}
Section::~Section()
{
	std::string buf = str();
	if (writer->cheerpMode == CHEERP_MODE_WASM) {
		llvm::errs() << "section length: " << buf.size() << '\n';
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
		code << '\n';
		writer->compileOperand(code, it.getCaseValue());
		code << "\ni32.eq";
		//We found the destination, there may be more cases for the same
		//destination though
		for(++it;it!=si->case_end();++it)
		{
			if(it.getCaseSuccessor()==dest)
			{
				//Also add this condition
				code << '\n';
				writer->compileOperand(code, si->getCondition());
				code << '\n';
				writer->compileOperand(code, it.getCaseValue());
				code << "\ni32.eq\ni32.or";
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
	code << "block $" << labelId << '\n';
	blockTypes.emplace_back(LABEL_FOR_SWITCH, 1);
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
		code << "block\n";

	// Wrap the br_table instruction in its own block
	code << "block\n";
	code << "get_local " << labelLocal;
	if (min != 0)
	{
		code << "\ni32.const " << min;
		code << "\ni32.sub";
	}
	code << "\nbr_table";

	for (auto label : table)
		code << " " << label;
	code << " 0\n";

	code << "\nend\n";

	// The first block does not do anything, and breaks out of the switch.
	code << "br " << idShapeMap.size() << "\n";
	code << "\nend\n";

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
		code << "block\n";

	// Wrap the br_table instruction in its own block.
	code << "block\n";
	writer->compileOperand(code, si->getCondition());
	if (min != 0)
	{
		code << "\ni32.const " << min;
		code << "\ni32.sub";
	}
	code << "\nbr_table";

	// Print the case labels and the default label.
	for (auto label : table)
		code << " " << label;
	code << " " << caseBlocks << "\n";

	code << "end\n";

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
		code << "else\n";
	}
	// The condition goes first
	renderCondition(bb, branchId);
	code << '\n';
	indent();
	code << "if\n";
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
		code << "else\n";
	}
	// The condition goes first
	for(uint32_t i=0;i<skipBranchIds.size();i++)
	{
		if(i!=0)
		{
			assert(false);
		}
		renderCondition(bb, skipBranchIds[i]);
		code << '\n';
	}
	// Invert result
	code << "i32.const 1\n";
	code << "i32.xor\n";
	indent();
	code << "if\n";

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
	code << "else\n";
}

void CheerpWastRenderInterface::renderBlockEnd()
{
	assert(!blockTypes.empty());
	BlockType block = blockTypes.back();
	blockTypes.pop_back();

	if(block.type == WHILE1)
	{
		// TODO: Why do we even need to fake value
		code << "i32.const 0\n";
		code << "br 1\n";
		code << "end\n";
		code << "end\n";
	}
	else if (block.type == CASE)
	{
		code << "end\n";
		BlockType* switchBlock = findSwitchBlockType(blockTypes);
		assert(switchBlock->depth > 0);
		switchBlock->depth--;
	}
	else if(block.type == IF)
	{
		for(uint32_t i = 0; i < block.depth; i++)
		{
			indent();
			code << "end\n";
		}
	}
	else if (block.type == SWITCH)
	{
		assert(block.depth == 0);
		if (!blockTypes.empty() && blockTypes.back().type == LABEL_FOR_SWITCH) {
			blockTypes.pop_back();
			code << "end\n";
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
	code << "loop\n";
	indent();
	code << "block\n";
	blockTypes.emplace_back(WHILE1, 1);
}

void CheerpWastRenderInterface::renderWhileBlockBegin(int blockLabel)
{
	// Wrap a block in a loop so that:
	// br 1 -> break
	// br 2 -> continue
	indent();
	code << "loop $c" << blockLabel << "\n";
	indent();
	code << "block $" << blockLabel << "\n";
	blockTypes.emplace_back(WHILE1, 1);
}

void CheerpWastRenderInterface::renderDoBlockBegin()
{
	indent();
	code << "block\n";
	blockTypes.emplace_back(DO, 1);
}

void CheerpWastRenderInterface::renderDoBlockBegin(int blockLabel)
{
	indent();
	code << "block $" << blockLabel << "\n";
	blockTypes.emplace_back(DO, 1);
}

void CheerpWastRenderInterface::renderDoBlockEnd()
{
	assert(!blockTypes.empty());
	assert(blockTypes.back().type == DO);
	blockTypes.pop_back();

	indent();
	code << "end\n";
}

void CheerpWastRenderInterface::renderBreak()
{
	BlockType block = blockTypes.back();
	if (block.type == CASE)
	{
		BlockType* switchBlock = findSwitchBlockType(blockTypes);
		assert(switchBlock->depth > 0);
		code << "br " << (switchBlock->depth - 1) << "\n";
	}
	else
	{
		// Find the last loop's block
		uint32_t breakIndex = 0;
		for (uint32_t i = 0; i < blockTypes.size(); i++)
		{
			BLOCK_TYPE bt = blockTypes[blockTypes.size() - i - 1].type;
			breakIndex += blockTypes[blockTypes.size() - i - 1].depth;
			if (bt == DO || bt == WHILE1 || bt == SWITCH)
				break;
		}
		assert(breakIndex > 1);
		code << "br " << (breakIndex - 1) << "\n";
	}
}

void CheerpWastRenderInterface::renderBreak(int labelId)
{
	// DO blocks only have one label
	// WHILE1 blocks have the "block" without a prefix
	code << "br $" << labelId << '\n';
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
	code << "br " << breakIndex << "\n";
}

void CheerpWastRenderInterface::renderContinue(int labelId)
{
	code << "br $c" << labelId << '\n';
}

void CheerpWastRenderInterface::renderLabel(int labelId)
{
	code << "i32.const " << labelId << '\n';
	code << "set_local " << labelLocal << '\n';
}

void CheerpWastRenderInterface::renderIfOnLabel(int labelId, bool first)
{
	// TODO: Use first to optimize dispatch
	code << "i32.const " << labelId << '\n';
	code << "get_local " << labelLocal << '\n';
	code << "i32.eq\n";
	indent();
	code << "if\n";
	blockTypes.emplace_back(IF, 1);
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
			code << "get_local " << (1 + writer.currentFun->arg_size() + writer.registerize.getRegisterId(incoming)) << '\n';
			code << "set_local " << (1 + writer.currentFun->arg_size() + writer.registerize.getRegisterIdForEdge(incoming, fromBB, toBB)) << '\n';
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
			code << "\nset_local " << (1 + writer.currentFun->arg_size() + writer.registerize.getRegisterId(phi)) << '\n';
		}
	};
	WriterPHIHandler(*this, code, from, to).runOnEdge(registerize, from, to);
}

const char* CheerpWastWriter::getTypeString(Type* t)
{
	if(t->isIntegerTy() || t->isPointerTy())
		return "i32";
	else if(t->isFloatTy())
		return "f32";
	else if(t->isDoubleTy())
		return "f64";
	else
	{
		llvm::errs() << "Unsupported type " << *t << "\n";
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
				if (functionIds.count(calledFunc))
				{
					code << "call " << functionIds[calledFunc];
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
		if (!debugLoc.isUnknown()) {
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
	// The first local after ther params stores the previous stack address
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

void CheerpWastWriter::compileMethodParams(std::ostream& code, const Function& F)
{
	uint32_t numArgs = F.arg_size();
	if(numArgs)
	{
		code << "(param";
		llvm::FunctionType* FTy = F.getFunctionType();
		for(uint32_t i = 0; i < numArgs; i++)
			code << ' ' << getTypeString(FTy->getParamType(i));
		code << ')';
	}
}

void CheerpWastWriter::compileMethodResult(std::ostream& code, const Function& F)
{
	if(!F.getReturnType()->isVoidTy())
		code << "(result " << getTypeString(F.getReturnType()) << ')';
}

void CheerpWastWriter::compileMethod(std::ostream& code, const Function& F)
{
	currentFun = &F;
	code << "(func $" << F.getName().str();
	// TODO: We should not export them all
	code << " (export \"" << NameGenerator::filterLLVMName(F.getName(),NameGenerator::NAME_FILTER_MODE::GLOBAL).str().str() << "\")";
	uint32_t numArgs = F.arg_size();
	compileMethodParams(code, F);
	compileMethodResult(code, F);
	code << '\n';
	const llvm::BasicBlock* lastDepth0Block = nullptr;
	if(F.size() == 1)
	{
		compileMethodLocals(code, F, false);
		// TODO: Only save the stack address if required
		code << "get_global " << stackTopGlobal << '\n';
		code << "set_local " << numArgs << "\n";
		compileBB(code, *F.begin());
		lastDepth0Block = &(*F.begin());
	}
	else
	{
		Relooper* rl = CheerpWriter::runRelooperOnFunction(F, PA, registerize);
		compileMethodLocals(code, F, rl->needsLabel());
		// TODO: Only save the stack address if required
		code << "get_global " << stackTopGlobal << '\n';
		code << "set_local " << numArgs << "\n";
		uint32_t numArgs = F.arg_size();
		const std::vector<Registerize::RegisterInfo>& regsInfo = registerize.getRegistersForFunction(&F);
		uint32_t numRegs = regsInfo.size();
		// label is the very last local
		CheerpWastRenderInterface ri(this, code, 1+numArgs+numRegs);
		rl->Render(&ri);
		lastDepth0Block = ri.lastDepth0Block;
	}
	// A function has to terminate with a return instruction
	if(!lastDepth0Block || !isa<ReturnInst>(lastDepth0Block->getTerminator()))
	{
		// Add a fake return
		if(!F.getReturnType()->isVoidTy())
			code << getTypeString(F.getReturnType()) << ".const 0\n";
		code << "return\n";
	}
	code << ")\n";
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

void CheerpWastWriter::compileDataSection()
{
	Section section(0x0b, this);

	for ( const GlobalVariable & GV : module.getGlobalList() )
	{
		if (GV.getSection() != StringRef("asmjs"))
			continue;
		if (GV.hasInitializer())
		{
			const Constant* init = GV.getInitializer();
			Type* ty = init->getType();
			// If the initializer is a function, skip it
			if (ty->isPointerTy() && ty->getPointerElementType()->isFunctionTy())
				continue;
			// The offset into memory, which is the address
			section << "(data (i32.const " << linearHelper.getGlobalVariableAddress(&GV) << ") \"";
			WastBytesWriter bytesWriter(section);
			linearHelper.compileConstantAsBytes(init,/* asmjs */ true, &bytesWriter);
			section << "\")\n";
		}
	}
}

void CheerpWastWriter::makeWast()
{
	cheerpMode = CHEERP_MODE_WAST;

	// First run, assign required Ids to functions and globals
	if (useWastLoader) {
		for ( const Function * F : globalDeps.asmJSImports() )
		{
			functionIds.insert(std::make_pair(F, functionIds.size()));
		}
	}
	for ( const Function & F : module.getFunctionList() )
	{
		if (!F.empty() && F.getSection() == StringRef("asmjs"))
		{
			functionIds.insert(std::make_pair(&F, functionIds.size()));
		}
	}

	// Emit S-expressions for the module
	std::stringstream code;
	code << "(module\n";

	// Second run, actually compile the code (imports needs to be before everything)
	if (useWastLoader) {
		for ( const Function * F : globalDeps.asmJSImports() )
		{
			compileImport(code, *F);
		}
	}

	// Define function type variables
	for (const auto& table : linearHelper.getFunctionTables())
	{
		code << "(type " << "$vt_" << table.second.name << " (func ";
		const llvm::Function& F = *table.second.functions[0];
		compileMethodParams(code, F);
		compileMethodResult(code, F);
		code << "))\n";
	}

	// Define 'table' with functions
	if (!linearHelper.getFunctionTables().empty())
		code << "(table anyfunc (elem";

	for (const auto& table : linearHelper.getFunctionTables()) {
		for (const auto& F : table.second.functions) {
			code << " $" << F->getName().str();
		}
	}

	if (!linearHelper.getFunctionTables().empty())
		code << "))\n";

	// Define the memory for the module in WasmPage units. The heap size is
	// defined in MiB and the wasm page size is 64 KiB. Thus, the wasm heap
	// size parameter is defined as: heapSize << 20 >> 16 = heapSize << 4.
	uint32_t minMemory = heapSize << 4;
	uint32_t maxMemory = heapSize << 4;
	code << "(memory (export \"memory\") " << minMemory << ' ' << maxMemory << ")\n";

	// Assign globals in the module, these are used for codegen they are not part of the user program
	stackTopGlobal = usedGlobals++;
	// Start the stack from the end of default memory
	code << "(global (mut i32) (i32.const " << (minMemory*WasmPage) << "))\n";

	// Experimental entry point for wast code
	llvm::Function* wastStart = module.getFunction("_Z9wastStartv");
	if(wastStart && globalDeps.constructors().empty())
	{
		assert(functionIds.count(wastStart));
		code << "(start " << functionIds[wastStart] << ")\n";
	}
	else if (!globalDeps.constructors().empty() && !useWastLoader)
	{
		code << "(start " << functionIds.size() << ")\n";
	}

	// Second run, actually compile the code
	for ( const Function & F : module.getFunctionList() )
	{
		if (!F.empty() && F.getSection() == StringRef("asmjs"))
		{
			compileMethod(code, F);
		}
	}

	// Construct an anonymous function that calls the global constructors.
	if (!globalDeps.constructors().empty() && !useWastLoader)
	{
		code << "(func\n";
		for (const Function* F : globalDeps.constructors())
		{
			if (F->getSection() == StringRef("asmjs"))
				code << "call " << functionIds.at(F) << "\n";
		}

		if (wastStart)
			code << "call " << functionIds.at(wastStart) << "\n";

		code << ")\n";
	}

	stream << code.str();

	compileDataSection();
	
	stream << ')';
}

void CheerpWastWriter::WastBytesWriter::addByte(uint8_t byte)
{
	char buf[4];
	snprintf(buf, 4, "\\%02x", byte);
	code << buf;
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

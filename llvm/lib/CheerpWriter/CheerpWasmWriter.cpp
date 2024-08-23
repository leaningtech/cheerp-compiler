//===-- CheerpWasmWriter.cpp - The Cheerp JavaScript generator ------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2017-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include <algorithm>
#include <limits>

#include "CFGStackifier.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Cheerp/BuiltinInstructions.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/PHIHandler.h"
#include "llvm/Cheerp/NameGenerator.h"
#include "llvm/Cheerp/WasmWriter.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/IR/IntrinsicsWebAssembly.h"
#include "llvm/IR/ProfDataUtils.h"
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

static void printPtrKind(POINTER_KIND k)
{
	switch (k)
	{
		case COMPLETE_OBJECT: errs() << "COMPLETE_OBJECT\n"; break;
		case REGULAR: errs() << "REGULAR\n"; break;
		case SPLIT_REGULAR: errs() << "SPLIT_REGULAR\n"; break;
		case BYTE_LAYOUT: errs() << "BYTE_LAYOUT\n"; break;
		case RAW: errs() << "RAW\n"; break;
		case CONSTANT: errs() << "CONSTANT\n"; break;
		default:
			assert(false && "Unexpected pointer kind\n");
	}
}


static uint32_t COMPILE_METHOD_LIMIT = 100000;

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
			encodeULEB128(0x6e, stream);
			// encodeULEB128(0x6f, stream);  tmp for GC testing
			break;
		case Registerize::VECTOR:
			encodeULEB128(0x7b, stream);
			break;
	}
}

static uint32_t getValType(const Type* t)
{
	errs() << "[getValType] trying to find: " << *t << "\n";
	if (TypeSupport::isTypeGC_arraysTrue(t))
		return 0x6e;
	else if (t->isIntegerTy(64))
		return 0x7e;
	else if (t->isIntegerTy() || TypeSupport::isRawPointer(t, true))
		return 0x7f;
	else if (t->isFloatTy())
		return 0x7d;
	else if (t->isDoubleTy())
		return 0x7c;
	else if (t->isPointerTy())
		return 0x6f;
	else if (t->isVectorTy())
		return 0x7b;
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
	errs() << "encoding: " << getValType(t) << "\n";
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

enum class BranchHint
{
	Likely,
	Unlikely,
	Neutral,
};

static BranchHint shouldBranchBeHinted(const llvm::BranchInst* bi, const bool IfNot)
{
	uint64_t weight_false = 0;
	uint64_t weight_true = 0;
	if (extractBranchWeights(*bi, weight_true, weight_false))
	{
		if (IfNot)
			std::swap(weight_false, weight_true);

		const uint64_t total = weight_true + weight_false;
		if (total == 0)
			return BranchHint::Neutral;

		auto isConvenientToHint = [](uint64_t part, uint64_t total) -> bool {
			return (part*1.0 / total) > 0.8;
		};

		if (isConvenientToHint(weight_true, total))
			return BranchHint::Likely;

		if (isConvenientToHint(weight_false, total))
			return BranchHint::Unlikely;
	}
	return BranchHint::Neutral;
}

Section::Section(uint32_t sectionId, const char* sectionName, CheerpWasmWriter* writer)
	: hasName(sectionName), name(hasName ? (sectionName) : ""), sectionId(sectionId), writer(writer)
{
	// Custom sections have a section name.
	if (!sectionId) {
		encodeULEB128(strlen(sectionName), *this);
		(*this) << sectionName;
	}
}
void Section::encode()
{
	assert(state == State::GENERATING);

#if WASM_DUMP_SECTIONS
	uint64_t start = writer->stream.tell();
	if (hasName)
		llvm::errs() << name << " ";
	else
		llvm::errs() << "unnamed section ";
	llvm::errs() << "id=" << sectionId << " ";
	llvm::errs() << "start=" << start << " ";
	llvm::errs() << "end=" << start + tell() << " ";
	llvm::errs() << "size=" << tell() << "\n";
#if WASM_DUMP_SECTION_DATA
	llvm::errs() << "section: " << string_to_hex(str()) << '\n';
#endif
#endif
	encodeULEB128(sectionId, writer->stream);

	encodeULEB128(tell(), writer->stream);
	writer->stream << str();

	state = State::ENCODED;
}

void Section::discard()
{
	assert(state == State::GENERATING);
	state = State::DISCARDED;
}

Section::~Section()
{
	assert(state == State::ENCODED || state == State::DISCARDED);
}

enum ConditionRenderMode {
	NormalCondition = 0,
	InvertCondition
};

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

void CheerpWasmWriter::filterNop(SmallVectorImpl<char>& buf, std::function<void(uint32_t, char)> filterCallback) const
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
			//TODO: improve/justify the logic here
			if (buf[old] == 0x01)
			{
				while (buf[old] == 0x01)
				{
					++old;
				}
			}
			else if (buf[old] == (char)WasmInvalidOpcode::BRANCH_LIKELY || buf[old] == (char)WasmInvalidOpcode::BRANCH_UNLIKELY)
			{
				filterCallback(curr, buf[old]);
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

	if (t->isVectorTy())
	{
		const Type *et = dyn_cast<VectorType>(t)->getElementType();
		if (I.getOpcode() == Instruction::Mul)
		{
			if (et->isIntegerTy(32))
				encodeInst(WasmSIMDOpcode::I32x4_MUL, code);
			else if (et->isIntegerTy(64))
				encodeInst(WasmSIMDOpcode::I64x2_MUL, code);
			else if (et->isIntegerTy(16))
				encodeInst(WasmSIMDOpcode::I16x8_MUL, code);
			else
				llvm::report_fatal_error("unsupported bit width for vector integer mul");
			return ;
		}
		else if (I.getOpcode() == Instruction::Add)
		{
			if (et->isIntegerTy(32))
				encodeInst(WasmSIMDOpcode::I32x4_ADD, code);
			else if (et->isIntegerTy(64))
				encodeInst(WasmSIMDOpcode::I64x2_ADD, code);
			else if (et->isIntegerTy(16))
				encodeInst(WasmSIMDOpcode::I16x8_ADD, code);
			else if (et->isIntegerTy(8))
				encodeInst(WasmSIMDOpcode::I8x16_ADD, code);
			else
				llvm::report_fatal_error("unsupported bit width for vector integer add");
			return ;
		}
		else if (I.getOpcode() == Instruction::Sub)
		{
			if (et->isIntegerTy(32))
				encodeInst(WasmSIMDOpcode::I32x4_SUB, code);
			else if (et->isIntegerTy(64))
				encodeInst(WasmSIMDOpcode::I64x2_SUB, code);
			else if (et->isIntegerTy(16))
				encodeInst(WasmSIMDOpcode::I16x8_SUB, code);
			else if (et->isIntegerTy(8))
				encodeInst(WasmSIMDOpcode::I8x16_SUB, code);
			else
				llvm::report_fatal_error("unsupported bit width for vector integer sub");
			return ;
		}
		else if (I.getOpcode() == Instruction::FMul)
		{
			if (et->isFloatTy())
				encodeInst(WasmSIMDOpcode::F32x4_MUL, code);
			else if (et->isDoubleTy())
				encodeInst(WasmSIMDOpcode::F64x2_MUL, code);
			else
				llvm::report_fatal_error("unsupported format for vector float mul");
			return ;
		}
		else if (I.getOpcode() == Instruction::FAdd)
		{
			if (et->isFloatTy())
				encodeInst(WasmSIMDOpcode::F32x4_ADD, code);
			else if (et->isDoubleTy())
				encodeInst(WasmSIMDOpcode::F64x2_ADD, code);
			else
				llvm::report_fatal_error("unsupported format for vector float add");
			return ;
		}
		else if (I.getOpcode() == Instruction::FSub)
		{
			if (et->isFloatTy())
				encodeInst(WasmSIMDOpcode::F32x4_SUB, code);
			else if (et->isDoubleTy())
				encodeInst(WasmSIMDOpcode::F64x2_SUB, code);
			else
				llvm::report_fatal_error("unsupported format for vector float sub");
			return ;
		}
		else if (I.getOpcode() == Instruction::FDiv)
		{
			if (et->isFloatTy())
				encodeInst(WasmSIMDOpcode::F32x4_DIV, code);
			else if (et->isDoubleTy())
				encodeInst(WasmSIMDOpcode::F64x2_DIV, code);
			else
				llvm::report_fatal_error("unsupported format for vector float div");
			return ;
		}
		else if (I.getOpcode() == Instruction::Xor)
		{
			encodeInst(WasmSIMDOpcode::V128_XOR, code);
			return ;
		}
		else if (I.getOpcode() == Instruction::Or)
		{
			encodeInst(WasmSIMDOpcode::V128_OR, code);
			return ;
		}
		else if (I.getOpcode() == Instruction::And)
		{
			encodeInst(WasmSIMDOpcode::V128_AND, code);
			return ;
		}
		llvm::errs() << "Opcode is: " << I.getOpcodeName() << "\n";
		llvm::report_fatal_error("unhandled vector op");
		return ;
	}

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

void CheerpWasmWriter::encodeInst(WasmGCOpcode opcode, WasmBuffer& code)
{
	code << static_cast<char>(WasmOpcode::GC);
	code << static_cast<char>(opcode);
}

void CheerpWasmWriter::encodeInst(WasmGCOpcode opcode, int32_t immediate, WasmBuffer& code)
{
	code << static_cast<char>(WasmOpcode::GC);
	code << static_cast<char>(opcode);
	encodeSLEB128(immediate, code);
}

void CheerpWasmWriter::encodeInst(WasmGCOpcode opcode, int32_t i1, int32_t i2, WasmBuffer& code)
{
	code << static_cast<char>(WasmOpcode::GC);
	code << static_cast<char>(opcode);
	encodeSLEB128(i1, code);
	encodeSLEB128(i2, code);
}

void CheerpWasmWriter::encodeInst(WasmFCU32Opcode opcode, uint32_t immediate, WasmBuffer& code)
{
	code << static_cast<char>(WasmOpcode::FC);
	encodeULEB128(static_cast<uint64_t>(opcode), code);
	encodeULEB128(immediate, code);
}

void CheerpWasmWriter::encodeInst(WasmFCU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code)
{
	code << static_cast<char>(WasmOpcode::FC);
	encodeULEB128(static_cast<uint64_t>(opcode), code);
	encodeULEB128(i1, code);
	encodeULEB128(i2, code);
}

void CheerpWasmWriter::encodeInst(WasmSIMDOpcode opcode, WasmBuffer& code)
{
	code << static_cast<char>(WasmOpcode::SIMD);
	encodeULEB128(static_cast<uint64_t>(opcode), code);
}

void CheerpWasmWriter::encodeInst(WasmSIMDU32Opcode opcode, uint32_t immediate, WasmBuffer& code)
{
	code << static_cast<char>(WasmOpcode::SIMD);
	encodeULEB128(static_cast<uint64_t>(opcode), code);
	encodeULEB128(immediate, code);
}

void CheerpWasmWriter::encodeInst(WasmSIMDU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code)
{
	code << static_cast<char>(WasmOpcode::SIMD);
	encodeULEB128(static_cast<uint64_t>(opcode), code);
	encodeULEB128(i1, code);
	encodeULEB128(i2, code);
}

void CheerpWasmWriter::encodeInst(WasmSIMDU32U32U32Opcode opcode, uint32_t i1, uint32_t i2, uint32_t i3, WasmBuffer& code)
{
	code << static_cast<char>(WasmOpcode::SIMD);
	encodeULEB128(static_cast<uint64_t>(opcode), code);
	encodeULEB128(i1, code);
	encodeULEB128(i2, code);
	encodeULEB128(i3, code);
}

void CheerpWasmWriter::encodeInst(WasmThreadsU32Opcode opcode, uint32_t immediate, WasmBuffer& code)
{
	code << static_cast<char>(WasmOpcode::Threads);
	encodeULEB128(static_cast<uint64_t>(opcode), code);
	encodeULEB128(immediate, code);
}

void CheerpWasmWriter::encodeInst(WasmThreadsU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code)
{
	code << static_cast<char>(WasmOpcode::Threads);
	encodeULEB128(static_cast<uint64_t>(opcode), code);
	encodeULEB128(i1, code);
	encodeULEB128(i2, code);
}

void CheerpWasmWriter::encodeInst(WasmInvalidOpcode opcode, WasmBuffer& code)
{
	nopLocations.push_back(code.tell());
	code << static_cast<char>(opcode);
}

void CheerpWasmWriter::encodeVectorConstantZero(WasmBuffer& code)
{
	code << static_cast<char>(WasmOpcode::SIMD);
	encodeULEB128(static_cast<uint64_t>(WasmSIMDOpcode::V128_CONST), code);
	for (int i = 0; i < 16; i++)
		code << static_cast<char>(0);
}

void CheerpWasmWriter::encodeConstantDataVector(WasmBuffer& code, const llvm::ConstantDataVector* cdv)
{
	char bytes[16];
	memset(bytes, 0, 16);
	const unsigned amount = cdv->getNumElements();
	const unsigned fakeWidth = 128 / amount;
	unsigned offset = 0;
	if (cdv->getElementType()->isIntegerTy(32))
	{
		for (unsigned i = 0; i < amount; i++)
		{
			support::endian::write32le(bytes + offset, cdv->getElementAsInteger(i));
			offset += fakeWidth / 8;
		}
	}
	else if (cdv->getElementType()->isIntegerTy(64))
	{
		support::endian::write64le(bytes, cdv->getElementAsInteger(0));
		support::endian::write64le(bytes + 8, cdv->getElementAsInteger(1));
	}
	else if (cdv->getElementType()->isIntegerTy(8))
	{
		for (unsigned i = 0; i < amount; i++)
		{
			bytes[offset] = cdv->getElementAsInteger(i);
			offset += fakeWidth / 8;
		}
	}
	else if (cdv->getElementType()->isIntegerTy(16))
	{
		for (unsigned i = 0; i < amount; i++)
		{
			support::endian::write16le(bytes + offset, cdv->getElementAsInteger(i));
			offset += fakeWidth / 8;
		}
	}
	else if (cdv->getElementType()->isFloatTy())
	{
		for (unsigned i = 0; i < amount; i++)
		{
			float floatElement = cdv->getElementAsFloat(i);
			uint32_t intReinterpret = *reinterpret_cast<uint32_t*>(&floatElement);
			support::endian::write32le(bytes + offset, intReinterpret);
			offset += fakeWidth / 8;
		}
	}
	else if (cdv->getElementType()->isDoubleTy())
	{
		for (unsigned i = 0; i < 2; i++)
		{
			double doubleElement = cdv->getElementAsDouble(i);
			uint64_t intReinterpret = *reinterpret_cast<uint64_t*>(&doubleElement);
			support::endian::write64le(bytes + i * 8, intReinterpret);
		}
	}
	else
		llvm::report_fatal_error("unhandled type for encode vector constant");

	code << static_cast<char>(WasmOpcode::SIMD);
	encodeULEB128(static_cast<uint64_t>(WasmSIMDOpcode::V128_CONST), code);
	for (int i = 0; i < 16; i++)
		code << bytes[i];
}

void CheerpWasmWriter::encodeConstantVector(WasmBuffer& code, const llvm::ConstantVector* cv)
{
	// Loop over each element, encode it into the bytes array, then write the bytes array
	const FixedVectorType* vecType = cv->getType();
	const unsigned num = vecType->getNumElements();
	char bytes[16];
	memset(bytes, 0, 16);
	int offset = 0;
	Constant* element;
	Type* type;
	for (unsigned i = 0; i < num; i++)
	{
		element = cv->getAggregateElement(i);
		if (ConstantInt* ci = dyn_cast<ConstantInt>(element))
		{
			int width = ci->getBitWidth();
			// If it's a boolean, calculate the correct size by the number of elements.
			if (width == 1)
				width = 128 / vecType->getNumElements();
			if (width == 8)
				bytes[offset] = ci->getZExtValue();
			else if (width == 16)
				support::endian::write16le(bytes + offset, ci->getZExtValue());
			else if (width == 32)
				support::endian::write32le(bytes + offset, ci->getZExtValue());
			else if (width == 64)
				support::endian::write64le(bytes + offset, ci->getZExtValue());
			offset += width / 8;
		}
		else if (ConstantFP* cf = dyn_cast<ConstantFP>(element))
		{
			if (cf->getType()->isDoubleTy())
			{
				support::endian::write64le(bytes + offset, cf->getValueAPF().convertToDouble());
				offset += 8;
			}
			else
			{
				assert(cf->getType()->isFloatTy());
				support::endian::write32le(bytes + offset, cf->getValueAPF().convertToFloat());
				offset += 4;
			}
		}
		else if (isa<ConstantPointerNull>(element))
		{
			support::endian::write32le(bytes + offset, 0);
			offset += 4;
		}
		else if (UndefValue* uv = dyn_cast<UndefValue>(element))
		{
			type = uv->getType();
			if (type->isIntegerTy(32) || type->isFloatTy() || type->isPointerTy())
			{
				support::endian::write32le(bytes + offset, 0);
				offset += 4;
			}
			else if (type->isIntegerTy(64) || type->isDoubleTy())
			{
				support::endian::write64le(bytes + offset, 0);
				offset += 8;
			}
			else if (type->isIntegerTy(16))
			{
				support::endian::write16le(bytes + offset, 0);
				offset += 2;
			}
			else if (type->isIntegerTy(8))
			{
				bytes[offset] = 0;
				offset++;
			}
			else
				llvm::report_fatal_error("Unimplemented type for UndefValue");
		}
		else if (GlobalVariable* gv = dyn_cast<GlobalVariable>(element))
		{
			uint32_t address = linearHelper.getGlobalVariableAddress(gv);
			support::endian::write32le(bytes + offset, address);
			offset += 4;
		}
		else if (Function* f = dyn_cast<Function>(element))
		{
			if (linearHelper.functionHasAddress(f))
			{
				uint32_t addr = linearHelper.getFunctionAddress(f);
				assert(addr && "function address is zero (aka nullptr conflict)");
				encodeInst(WasmS32Opcode::I32_CONST, addr, code);
			}
			else
			{
				// When dealing with indirectly used undefined functions forward them to the null function
				// TODO: This improve the robustness of the compiler, but it might generate unexpected behavor
				//       if the address is ever explicitly compared to 0
				assert(f->empty());
				encodeInst(WasmS32Opcode::I32_CONST, 0, code);
			}
			offset += 4;
		}
		else
			llvm::report_fatal_error("Unimplemented type for encodeConstantVector");
	}
	code << static_cast<char>(WasmOpcode::SIMD);
	encodeULEB128(static_cast<uint64_t>(WasmSIMDOpcode::V128_CONST), code);
	for (int i = 0; i < 16; i++)
		code << bytes[i];
}

void CheerpWasmWriter::encodeExtractLane(WasmBuffer& code, const llvm::ExtractElementInst& eei)
{
	const FixedVectorType* vecType = cast<FixedVectorType>(eei.getVectorOperandType());
	const unsigned num = vecType->getNumElements();
	const Type* elementType = vecType->getElementType();
	assert(isa<ConstantInt>(eei.getIndexOperand()));
	uint64_t index = (dyn_cast<ConstantInt>(eei.getIndexOperand()))->getZExtValue();
	if (getVectorBitwidth(vecType) != 128 && !elementType->isIntegerTy(1))
	{
		// We need to recalculate the index if the vector is not 128 bits in size.
		const unsigned scaleFactor = 128 / getVectorBitwidth(vecType);
		index *= scaleFactor;
	}
	compileOperand(code, eei.getVectorOperand());
	if (elementType->isIntegerTy(32) || elementType->isPointerTy() || (elementType->isIntegerTy(1) && num == 4))
		encodeInst(WasmSIMDU32Opcode::I32x4_EXTRACT_LANE, index, code);
	else if (elementType->isIntegerTy(64) || (elementType->isIntegerTy(1) && num == 2))
	{
		encodeInst(WasmSIMDU32Opcode::I64x2_EXTRACT_LANE, index, code);
		// If the vector holds booleans, resize to i32.
		if (elementType->isIntegerTy(1))
			encodeInst(WasmOpcode::I32_WRAP_I64, code);
	}
	else if (elementType->isIntegerTy(16) || (elementType->isIntegerTy(1) && num == 8))
		encodeInst(WasmSIMDU32Opcode::I16x8_EXTRACT_LANE_U, index, code);
	else if (elementType->isIntegerTy(8) || (elementType->isIntegerTy(1) && num == 16))
		encodeInst(WasmSIMDU32Opcode::I8x16_EXTRACT_LANE_U, index, code);
	else if (elementType->isFloatTy())
		encodeInst(WasmSIMDU32Opcode::F32x4_EXTRACT_LANE, index, code);
	else if (elementType->isDoubleTy())
		encodeInst(WasmSIMDU32Opcode::F64x2_EXTRACT_LANE, index, code);
	else
		llvm::report_fatal_error("unhandled type for extract element");
}

void CheerpWasmWriter::encodeReplaceLane(WasmBuffer& code, const llvm::InsertElementInst& iei)
{
	const FixedVectorType* vecType = cast<FixedVectorType>(iei.getType());
	const unsigned num = vecType->getNumElements();
	const Type* elementType = vecType->getElementType();
	assert(isa<ConstantInt>(iei.getOperand(2)));
	uint64_t index = (cast<ConstantInt>(iei.getOperand(2)))->getZExtValue();
	if (getVectorBitwidth(vecType) != 128 && !elementType->isIntegerTy(1))
	{
		// We need to recalculate the index if the vector is not 128 bits in size.
		const unsigned scaleFactor = 128 / getVectorBitwidth(vecType);
		index *= scaleFactor;
	}
	compileOperand(code, iei.getOperand(0));
	compileOperand(code, iei.getOperand(1));
	if (elementType->isIntegerTy(32) || elementType->isPointerTy() || (elementType->isIntegerTy(1) && num == 4))
		encodeInst(WasmSIMDU32Opcode::I32x4_REPLACE_LANE, index, code);
	else if (elementType->isIntegerTy(64) || (elementType->isIntegerTy(1) && num == 2))
	{
		// If the insert is from a boolean value, extend to i64 first.
		if (elementType->isIntegerTy(1))
			encodeInst(WasmOpcode::I64_EXTEND_S_I32, code);
		encodeInst(WasmSIMDU32Opcode::I64x2_REPLACE_LANE, index, code);
	}
	else if (elementType->isIntegerTy(16) || (elementType->isIntegerTy(1) && num == 8))
		encodeInst(WasmSIMDU32Opcode::I16x8_REPLACE_LANE, index, code);
	else if (elementType->isIntegerTy(8) || (elementType->isIntegerTy(1) && num == 16))
		encodeInst(WasmSIMDU32Opcode::I8x16_REPLACE_LANE, index, code);
	else if (elementType->isFloatTy())
		encodeInst(WasmSIMDU32Opcode::F32x4_REPLACE_LANE, index, code);
	else if (elementType->isDoubleTy())
		encodeInst(WasmSIMDU32Opcode::F64x2_REPLACE_LANE, index, code);
	else
		llvm::report_fatal_error("unhandled type for insert element");
}

void CheerpWasmWriter::encodeVectorTruncation(WasmBuffer& code, const llvm::Instruction& I)
{
	// We zero out the now unused lanes.
	const CastInst& ci = cast<CastInst>(I);
	const FixedVectorType* destVecType = cast<FixedVectorType>(ci.getDestTy());
	const unsigned amount = destVecType->getNumElements();
	const unsigned elementWidth = destVecType->getScalarSizeInBits();
	const unsigned fakeWidth = 128 / amount;
	// We need to create a vector to function as a mask. This will be a vector with 16 bytes.
	unsigned currentWidth = 0;
	SmallVector<uint8_t, 16> mask;
	for (unsigned i = 0; i < 16; i++)
	{
		if (currentWidth < elementWidth)
			mask.push_back(-1);
		else
			mask.push_back(0);
		currentWidth = (currentWidth + 8) % fakeWidth;
	}
	Value* maskVector = ConstantDataVector::get(module.getContext(), mask);
	const ConstantDataVector* cdv = cast<ConstantDataVector>(maskVector);
	encodeConstantDataVector(code, cdv);
	encodeInst(WasmSIMDOpcode::V128_AND, code);
}

void CheerpWasmWriter::encodeLoadingShuffle(WasmBuffer& code, const llvm::FixedVectorType* vecType)
{
	// This function will encode a shuffle instruction that is used to support vectors
	// smaller than 128 bits. Only the first bits are loaded, and need to be shuffled
	// to the correct spots in the vector.
	encodeVectorConstantZero(code);
	encodeInst(WasmSIMDOpcode::I8x16_SHUFFLE, code);

	const unsigned num = vecType->getNumElements();
	const Type* elementType = vecType->getElementType();
	const unsigned elementWidth = elementType->isPointerTy() ? 32 : vecType->getScalarSizeInBits();
	const unsigned fakeWidth = 128 / num;
	unsigned currentWidth = 0;
	unsigned index = 0;
	for (unsigned i = 0; i < 16; i++)
	{
		if (currentWidth < elementWidth)
			code << static_cast<char>(index++);
		else
			code << static_cast<char>(8);
		currentWidth = (currentWidth + 8) % fakeWidth;
	}
}

void CheerpWasmWriter::encodeStoringShuffle(WasmBuffer& code, const llvm::FixedVectorType* vecType)
{
	// This function will encode a shuffle instruction that is used to support vector
	// smaller than 128 bits. It will shuffle the elements from the various lanes into
	// a single bigger lane, so they can be stored all at once.
	encodeVectorConstantZero(code);
	encodeInst(WasmSIMDOpcode::I8x16_SHUFFLE, code);

	const unsigned num = vecType->getNumElements();
	const Type* elementType = vecType->getElementType();
	const unsigned elementWidth = elementType->isPointerTy() ? 32 : vecType->getScalarSizeInBits();
	const unsigned fakeLaneWidth = 16 / num;
	unsigned elementsHandled = 0;
	unsigned currentWidth = 0;
	for (unsigned i = 0; i < 16; i++)
	{
		if (elementsHandled < num)
		{
			unsigned laneNum = elementsHandled * fakeLaneWidth + (currentWidth / 8);
			code << static_cast<char>(laneNum);
			currentWidth += 8;
			if (currentWidth == elementWidth)
			{
				currentWidth = 0;
				elementsHandled++;
			}
		}
		else
			code << static_cast<char>(15);
	}
}

void CheerpWasmWriter::encodeBranchHint(const llvm::BranchInst* BI, const bool IfNot, WasmBuffer& code)
{
	auto branchHint = shouldBranchBeHinted(BI, IfNot);

	if (branchHint == BranchHint::Likely)
		encodeInst(WasmInvalidOpcode::BRANCH_LIKELY, code);
	else if (branchHint == BranchHint::Unlikely)
		encodeInst(WasmInvalidOpcode::BRANCH_UNLIKELY, code);
}

void CheerpWasmWriter::encodePredicate(const llvm::Type* ty, const llvm::CmpInst::Predicate predicate, WasmBuffer& code)
{
	assert(ty->isIntegerTy() || ty->isPointerTy() || ty->isVectorTy());
	const Type* elementType = nullptr;
	unsigned num = 0;
	bool booleanCompare = false;
	if (ty->isVectorTy())
	{
		const FixedVectorType* vecType = cast<FixedVectorType>(ty);
		elementType = vecType->getElementType();
		num = vecType->getNumElements();
		booleanCompare = elementType->isIntegerTy(1);
		if (elementType->isIntegerTy(64) || (booleanCompare && num == 2))
		{
			if (predicate == CmpInst::ICMP_EQ)
				encodeInst(WasmSIMDOpcode::I64x2_EQ, code);
			else if (predicate == CmpInst::ICMP_NE)
				encodeInst(WasmSIMDOpcode::I64x2_NE, code);
			else if (predicate == CmpInst::ICMP_SLT)
				encodeInst(WasmSIMDOpcode::I64x2_LT_S, code);
			else if (predicate == CmpInst::ICMP_SGT)
				encodeInst(WasmSIMDOpcode::I64x2_GT_S, code);
			else if (predicate == CmpInst::ICMP_SLE)
				encodeInst(WasmSIMDOpcode::I64x2_LE_S, code);
			else if (predicate == CmpInst::ICMP_SGE)
				encodeInst(WasmSIMDOpcode::I64x2_GE_S, code);
			else
				llvm::report_fatal_error("Unsupported predicate for 64-bit icmp");
			return ;
		}
	}
	switch(predicate)
	{
#define PREDICATE(Ty, name) \
		case CmpInst::ICMP_##Ty: \
			if (ty->isIntegerTy(64)) \
				encodeInst(WasmOpcode::I64_##name, code); \
			else if (ty->isVectorTy()) \
			{ \
				if (elementType->isIntegerTy(32) || elementType->isPointerTy() || (booleanCompare && num == 4)) \
					encodeInst(WasmSIMDOpcode::I32x4_##name, code); \
				else if (elementType->isIntegerTy(8) || (booleanCompare && num == 16))\
					encodeInst(WasmSIMDOpcode::I8x16_##name, code); \
				else if (elementType->isIntegerTy(16) || (booleanCompare && num == 8))\
					encodeInst(WasmSIMDOpcode::I16x8_##name, code); \
				else \
					assert(false); \
			} \
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

void CheerpWasmWriter::encodeLoad(llvm::Type* ty, uint32_t offset,
		WasmBuffer& code, bool signExtend, bool atomic)
{
	assert(!(atomic && signExtend));
	assert(!TypeSupport::isTypeGC(ty)); // TODO: check for GC loads for VA list
	if (ty->isIntegerTy())
	{
		uint32_t bitWidth = targetData.getTypeStoreSizeInBits(ty);

		switch (bitWidth)
		{
			// Currently assume unsigned, like Cheerp. We may optimize
			// this be looking at a following sext or zext instruction.
			case 8:
				if (atomic)
					encodeInst(WasmThreadsU32U32Opcode::I32_ATOMIC_LOAD8_U, 0x0, offset, code);
				else if (signExtend)
					encodeInst(WasmU32U32Opcode::I32_LOAD8_S, 0x0, offset, code);
				else
					encodeInst(WasmU32U32Opcode::I32_LOAD8_U, 0x0, offset, code);
				break;
			case 16:
				if (atomic)
					encodeInst(WasmThreadsU32U32Opcode::I32_ATOMIC_LOAD16_U, 0x1, offset, code);
				else if (signExtend)
					encodeInst(WasmU32U32Opcode::I32_LOAD16_S, 0x1, offset, code);
				else
					encodeInst(WasmU32U32Opcode::I32_LOAD16_U, 0x1, offset, code);
				break;
			case 32:
				if (atomic)
					encodeInst(WasmThreadsU32U32Opcode::I32_ATOMIC_LOAD, 0x2, offset, code);
				else
					encodeInst(WasmU32U32Opcode::I32_LOAD, 0x2, offset, code);
				break;
			case 64:
				if (atomic)
					encodeInst(WasmThreadsU32U32Opcode::I64_ATOMIC_LOAD, 0x2, offset, code);
				else
					encodeInst(WasmU32U32Opcode::I64_LOAD, 0x2, offset, code);
				break;
			default:
				llvm::errs() << "bit width: " << bitWidth << '\n';
				llvm_unreachable("unknown integer bit width");
		}
	} else if (ty->isVectorTy()) {
		const FixedVectorType* vecTy = cast<FixedVectorType>(ty);
		const unsigned vectorBitwidth = getVectorBitwidth(vecTy);
		if (vectorBitwidth == 128)
			encodeInst(WasmSIMDU32U32Opcode::V128_LOAD, 0x2, offset, code);
		else
		{
			if (vectorBitwidth == 64)
			{
				encodeInst(WasmSIMDU32U32Opcode::V128_LOAD64_ZERO, 0x2, offset, code);
				encodeLoadingShuffle(code, vecTy);
			}
			else if (vectorBitwidth == 32)
			{
				encodeInst(WasmSIMDU32U32Opcode::V128_LOAD32_ZERO, 0x2, offset, code);
				encodeLoadingShuffle(code, vecTy);
			}
			else if (vectorBitwidth == 16)
			{
				// There is no opcode equivalent for LOAD16_ZERO, so we have to use LOAD16_LANE
				// on a zeroed out vector.
				encodeVectorConstantZero(code);
				encodeInst(WasmSIMDU32U32U32Opcode::V128_LOAD16_LANE, 0x0, offset, 0, code);
				encodeLoadingShuffle(code, vecTy);
			}
			else
				llvm::report_fatal_error("vector bitwidth not supported");
		}
	} else {
		assert(!atomic && "atomic loads only supported on integers");
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
	const bool isSameRegister = (registerize.getRegisterId(phi, 0, EdgeContext::emptyContext())
			== registerize.getRegisterId(incomingInst, 0, edgeContext));
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
	const bool isSameRegister = (registerize.getRegisterId(phi, 0, EdgeContext::emptyContext())
			== registerize.getRegisterId(incomingInst, 0, edgeContext));

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
		void handlePHIStackGroup(const std::vector<InstElem>& phiToHandle) override
		{
			using ValueElem = std::pair<const Value*, uint32_t>;
			using PHIElem = std::pair<const PHINode*, uint32_t>;
			std::vector<std::pair<ValueElem, std::vector<PHIElem>>> toProcessOrdered;
			std::map<ValueElem, std::vector<PHIElem>> toProcessMap;
			for (const auto& phiEl : phiToHandle)
			{
				const auto* phi = cast<PHINode>(phiEl.instruction);
				assert(phiEl.ptrIdx == 0);
				uint32_t elemIdx = phiEl.structIdx;
				const Value* incoming = phi->getIncomingValueForBlock(fromBB);
				// We can avoid assignment from the same register if no pointer kind conversion is required
				if(!writer.requiresExplicitAssigment(phi, incoming))
					continue;
				// We can leave undefined values undefined
				if (isa<UndefValue>(incoming))
					continue;

				auto it = toProcessMap.find({incoming, elemIdx});
				if(it == toProcessMap.end())
				{
					it = toProcessMap.insert(it, {{incoming, elemIdx}, {}});
					toProcessOrdered.push_back({{incoming,elemIdx}, {}});
				}
				it->second.emplace_back(phi, elemIdx);
			}

			//Note that any process order works, as long as it's deterministic
			//So reordering for leaving on the stack whatever is needed also works
			for (auto& [incomingElem, phis] : toProcessOrdered)
			{
				// 1) Put the value on the stack
				if(incomingElem.first->getType()->isStructTy())
				{
					writer.compileAggregateElem(code, incomingElem.first, incomingElem.second);
				}
				else
				{
					writer.compileOperand(code, incomingElem.first);
				}
				phis = std::move(toProcessMap[incomingElem]);
			}

			writer.teeLocals.removeConsumed();

			while (!toProcessOrdered.empty())
			{
				auto incomingElem = toProcessOrdered.back().first;
				const auto& phiVector  = toProcessOrdered.back().second;

				for (const auto& [phi, elemIdx] : phiVector)
				{
					// 2) Save the value in the phi
					uint32_t reg = writer.registerize.getRegisterId(phi, elemIdx, EdgeContext::emptyContext());
					uint32_t local = writer.localMap.at(reg);
					if (phiVector.back() == PHIElem{phi, elemIdx})
					{
						if (toProcessOrdered.size() == 1)
							writer.teeLocals.addCandidate(incomingElem.first, /*isInstructionAssigment*/false, local, code.tell());
						errs() << "[compilePHIOfBlockFromOtherBlock] Adding local: " << local << "\n";
						writer.encodeInst(WasmU32Opcode::SET_LOCAL, local, code);
					}
					else
					{
						errs() << "Setting TeeLocal: " << local << "\n" ;
						writer.encodeInst(WasmU32Opcode::TEE_LOCAL, local, code);
						// TODO: add compileRefCast, how do we get the right type?
						// assert(false);
					}
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
	if (TypeSupport::isTypeGC(cast<GetElementPtrInst>(gep_inst)->getPointerOperandType()))
	{
		errs() << "[compileGEP] Calling compileGEPGC\n";
		compileGEPGC(code, gep_inst, PA.getPointerKind(gep_inst), true);
		return ;
	}

	const auto I = dyn_cast<Instruction>(gep_inst);
	if (I && !isInlineable(*I)) {
		if (!standalone) {
			compileGetLocal(code, I, 0);
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
		int64_t value = C->getSExtValue();
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
	if(initialSize < 32 && needsUnsignedTruncation(v, /*asmjs*/true))
	{
		encodeInst(WasmS32Opcode::I32_CONST, getMaskForBitWidth(initialSize), code);
		encodeInst(WasmOpcode::I32_AND, code);
	}
}

void CheerpWasmWriter::compileTypedZero(WasmBuffer& code, const llvm::Type* t)
{
	if (t->isVectorTy())
	{
		encodeVectorConstantZero(code);
		return;
	}

	// Encode a literal f64, f32, i64 or i32 zero as the return value.
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
			errs() << "Calling compileCondition: Select false\n";
			compileCondition(code, ce->getOperand(0), /*booleanInvert*/false);
			if (ce->getOperand(0)->getType()->isVectorTy())
				encodeInst(WasmSIMDOpcode::V128_BITSELECT, code);
			else
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

bool CheerpWasmWriter::doesConstantDependOnUndefined(const Constant* C) const
{
	if(isa<ConstantExpr>(C) && C->getOperand(0)->getType()->isPointerTy())
		return doesConstantDependOnUndefined(cast<Constant>(C->getOperand(0)));
	else if(isa<GlobalVariable>(C) && cast<GlobalVariable>(C)->getSection() == "asmjs" && !globalizedConstants.count(C))
		return true;
	return false;
}

void CheerpWasmWriter::compileConstantAggregate(WasmBuffer& code, const ConstantAggregate* ca)
{
	errs() << "[compileConstantAggregate] compiling aggregate: " << *ca << "\n";
	const Type* Ty = ca->getType();
	assert(TypeSupport::isTypeGC(Ty));
	if (auto aTy = dyn_cast<ConstantArray>(ca))
	{
		Type* elemType = aTy->getType()->getElementType();
		const int32_t typeIdx = linearHelper.getGCTypeIndex(Ty, COMPLETE_OBJECT);
		for(uint32_t i = 0; i < aTy->getNumOperands(); i++)
		{
			if(elemType->isPointerTy())
				compilePointerAs(code, aTy->getOperand(i), PA.getPointerKindForStoredType(elemType));
			else
				compileOperand(code, aTy->getOperand(i));
		}
		encodeInst(WasmGCOpcode::ARRAY_NEW_FIXED, typeIdx, aTy->getNumOperands(), code);
	}
	else if (auto cs = dyn_cast<ConstantStruct>(ca))
	{
		StructType* sTy = cs->getType();
		assert(sTy->getNumElements() == cs->getNumOperands());
		const int32_t typeIdx = linearHelper.getGCTypeIndex(Ty, COMPLETE_OBJECT);
		for (uint32_t i = 0; i < cs->getNumOperands(); i++)
		{
			const Constant* currOp = cs->getOperand(i);
			bool dependOnUndefined = !currentFun && doesConstantDependOnUndefined(currOp);
			bool useWrapperArray = types.useWrapperArrayForMember(PA, sTy, i);
			Type* elemTy = sTy->getStructElementType(i);

			if (elemTy->isPointerTy())
			{
				TypeAndIndex baseAndIndex(sTy, i, TypeAndIndex::STRUCT_MEMBER);
				POINTER_KIND kind = PA.getPointerKindForMemberPointer(baseAndIndex);

				if((kind == REGULAR || kind == SPLIT_REGULAR) && PA.getConstantOffsetForMember(baseAndIndex))
				{
					if(dependOnUndefined)
						encodeInst(WasmU32Opcode::REF_NULL, 0x6E, code);
					else
						compilePointerBaseTyped(code, currOp, elemTy);
				}
				else if(kind == SPLIT_REGULAR)
				{
					if(dependOnUndefined)
						encodeInst(WasmU32Opcode::REF_NULL, 0x6E, code);
					else
						compilePointerBaseTyped(code, currOp, elemTy);
	
					if(dependOnUndefined)
						encodeInst(WasmS32Opcode::I32_CONST, 0, code);
					else
						compilePointerOffset(code, currOp);
				}
				else
				{
					if(dependOnUndefined)
						encodeInst(WasmU32Opcode::REF_NULL, 0x6E, code);
					else
						compilePointerAs(code, currOp, kind);
				}
			}
			else if (dependOnUndefined)
				encodeInst(WasmU32Opcode::REF_NULL, 0x6E, code);
			else
				compileOperand(code, currOp);

			if (useWrapperArray)
			{
				int32_t wrapperTypeIdx = linearHelper.getGCTypeIndex(elemTy, SPLIT_REGULAR);
				encodeInst(WasmGCOpcode::ARRAY_NEW_FIXED, wrapperTypeIdx, 1, code);
			}
		}
		encodeInst(WasmGCOpcode::STRUCT_NEW, typeIdx, code);
	}
	else
	{
#ifndef NDEBUG
		ca->dump();
#endif
		llvm::report_fatal_error("Cannot handle this constant aggregate type");
	}
}

void CheerpWasmWriter::compileConstant(WasmBuffer& code, const Constant* c, bool forGlobalInit)
{
	if (c->hasName())
		errs() << "[compileConstant] compiling constant: " << *c << "\n";
	else
		errs() << "[compileConstant] compiling a literal constant\n";

	if (hasPutTeeLocalOnStack(code, c))
	{
		errs() << "[compileConstant] hasPutTeeLocal on stack\n";
		return;
	}
	if(const ConstantExpr* CE = dyn_cast<ConstantExpr>(c))
	{
		errs() << "[compileConstant] compileConstantExpr\n";
		compileConstantExpr(code, CE);
	}
	else if(const ConstantInt* i=dyn_cast<ConstantInt>(c))
	{
		errs() << "[compileConstant] compileConstantInt: " << *i << "\n";
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
		errs() << "[compileConstant] compileConstantFP\n";
		encodeLiteralType(c->getType(), code);
		if (c->getType()->isDoubleTy()) {
			encodeF64(f->getValueAPF().convertToDouble(), code);
		} else {
			assert(c->getType()->isFloatTy());
			encodeF32(f->getValueAPF().convertToFloat(), code);
		}
	}
	else if(const ConstantAggregateZero* caz = dyn_cast<ConstantAggregateZero>(c))
	{
		errs() << "[compileConstant] compileConstantAggregateZero\n";
		assert(caz->getType()->isVectorTy());
		encodeVectorConstantZero(code);
	}
	else if (const ConstantDataVector* cdv = dyn_cast<ConstantDataVector>(c))
	{
		errs() << "[compileConstant] compileConstantGlobalDataVector\n";
		encodeConstantDataVector(code, cdv);
	}
	else if (const ConstantVector* cv = dyn_cast<ConstantVector>(c))
	{
		errs() << "[compileConstant] compileConstantGlobalVector\n";
		encodeConstantVector(code, cv);
	}
	else if(const GlobalVariable* GV = dyn_cast<GlobalVariable>(c))
	{
		errs() << "[compileConstant] compileConstantGlobalVariable\n";
		if (TypeSupport::isTypeGC(GV->getType()))
		{
			auto it = globalizedGlobalsIDs.find(GV);
			assert(it != globalizedGlobalsIDs.end());
			errs() << "[compileOperand] GET GLOBAL\n";
			encodeInst(WasmU32Opcode::GET_GLOBAL, it->second, code);
			const Type* Ty = GV->getType();
			POINTER_KIND kind = COMPLETE_OBJECT;
			if (Ty->isPointerTy())
				kind = PA.getPointerKind(GV);
			errs() << "[compileInlineOperand] Casting global " << *Ty << " to ptr kind: "; printPtrKind(kind);
			compileRefCast(code, Ty, kind);
		}
		else
		{
			uint32_t address = linearHelper.getGlobalVariableAddress(GV);
			encodeInst(WasmS32Opcode::I32_CONST, address, code);
		}
	}
	else if(isa<ConstantPointerNull>(c))
	{
		errs() << "[compileConstant] compileConstantPointerNull\n";
		if (TypeSupport::isTypeGC(c->getType()))
		{
			POINTER_KIND kind = PA.getPointerKind(c);
			if (kind == COMPLETE_OBJECT || kind == CONSTANT)
			{
				int32_t typeIdx = linearHelper.getGCTypeIndex(c->getType(), COMPLETE_OBJECT);
				encodeInst(WasmS32Opcode::REF_NULL, typeIdx, code);
			}
			else
				encodeInst(WasmU32Opcode::GET_GLOBAL, nullObjGlobal, code);
		}
		else
			encodeInst(WasmS32Opcode::I32_CONST, 0, code);
	}
	else if(isa<Function>(c))
	{
		errs() << "[compileConstant] compileConstantFunction\n";
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
		errs() << "[compileConstant] compileConstantUndefValue\n";
		compileTypedZero(code, c->getType());
	} 
	else if (const ConstantAggregate* ca = dyn_cast<ConstantAggregate>(c))
	{
		errs() << "[compileConstant] Found a ConstantAggregate: " << *ca << "\n";
		compileConstantAggregate(code, ca);
		errs() << "[compileConstant] Cone compiling ConstantAggregate: " << *ca << "\n";
	}
	else
	{
#ifndef NDEBUG
		c->dump();
#endif
		llvm::report_fatal_error("Cannot handle this constant");
	}
}

void CheerpWasmWriter::compileGetLocal(WasmBuffer& code, const llvm::Instruction* I, uint32_t elemIdx)
{
	errs() << "[compileGetLocal] compiling get local for: ", I->dump(); 
	compileInstructionAndSet(code, *I);
	if (hasPutTeeLocalOnStack(code, I))
	{
		errs() << "[compileGetLocal] did a TEE_LOCAL\n";
		//Successfully find a candidate to transform in tee local
		return;
	}
	const Type* localType = I->getType();
	uint32_t idx = registerize.getRegisterId(I, elemIdx, edgeContext);
	uint32_t localId = localMap.at(idx);
	getLocalDone.insert(I);
	encodeInst(WasmU32Opcode::GET_LOCAL, localId, code);
	errs() << "[compileGetLocal] localId: " << localId << " elemIdx was: " << elemIdx << "\n";

	// Cast from anyref to a GC reference type
	// If the elemIdx is not 0 we are accessing the offset int
	// used for a SPLIT_REGULAR and we do not need to perform a cast
	if (elemIdx == 0)
	{
		POINTER_KIND kind = COMPLETE_OBJECT;
		if (localType->isPointerTy())
			kind = PA.getPointerKind(I);
		compileRefCast(code, localType, kind);
	}
	errs() << "[compileGetLocal] DONE\n";
}

void CheerpWasmWriter::compileAggregateElem(WasmBuffer& code, const llvm::Value* v, uint32_t elemIdx)
{
	assert(v->getType()->isAggregateType());
	if(auto* I = dyn_cast<Instruction>(v))
	{
		compileGetLocal(code, I, elemIdx);
	}
	else if(auto* U = dyn_cast<UndefValue>(v))
	{
		compileOperand(code, U->getAggregateElement(elemIdx));
	}
	else if(auto* C = dyn_cast<ConstantStruct>(v))
	{
		compileConstant(code, C->getAggregateElement(elemIdx), false);
	}
	else
	{
#ifndef NDEBUG
		v->dump();
#endif
		report_fatal_error("unsupported aggregate");
	}
}

void CheerpWasmWriter::compileOperand(WasmBuffer& code, const llvm::Value* v)
{
	errs() << "[compileOperand] value: ", v->dump();
	if(const Constant* c=dyn_cast<Constant>(v))
	{
		errs() << "[compileOperand] found the constant: " << *c << "\n";
		auto it = globalizedConstants.find(c);
		if(it != globalizedConstants.end())
		{
			encodeInst(WasmU32Opcode::GET_GLOBAL, it->second.first, code);
			errs() << "[compileOperand] GET GLOBAL\n";
			const Type* Ty = v->getType();
			// TODO: Asserting, not sure if GC globals can be constants and if we even need a cast here
			// we most likely do so for now i'm leaving this here
			assert(!TypeSupport::isTypeGC(Ty));
			if (TypeSupport::isTypeGC(Ty))
			{
				POINTER_KIND kind = COMPLETE_OBJECT;
				if (Ty->isPointerTy())
					kind = PA.getPointerKind(v);
				compileRefCast(code, Ty, kind);
				errs() << "[compileInlineOperand] Casting global " << *Ty << " to ptr kind: "; printPtrKind(kind);
			}
		}
		else
		{
			errs() << "[compileOperand] Constant is not found inside globalizedConstants\n";
			compileConstant(code, c, /*forGlobalInit*/false);
		}
	}
	else if(const Instruction* it=dyn_cast<Instruction>(v))
	{
		errs() << "[compileOperand] operand is an instruction\n";
		if(isInlineable(*it)) {
			errs() << "[compileOperand] operand is an inlineInstruction\n";
			compileInlineInstruction(code, *it);
		} else {
			errs() << "[compileOperand] operand is a getLocal\n";
			compileGetLocal(code, it, 0);
		}
	}
	else if(const Argument* arg=dyn_cast<Argument>(v))
	{
		errs() << "[compileOperand] compiling argument: "; arg->dump();
		if (hasPutTeeLocalOnStack(code, arg)) {
			errs() << "[compileOperand] has put a teeLocal on stack\n";
			return;
		}
		
		uint32_t local = arg->getArgNo();
		uint32_t splitRegsInArgs = 0;
		// Add an offset for the amount of split regular indices that came before this argument
		// TODO: make a helper function for this that caches offsets
		for (size_t i = 0; i < local; i++)
		{
			Argument* prevArg = arg->getParent()->getArg(i);
			if (prevArg->getType()->isPointerTy() && PA.getPointerKindForArgument(prevArg) == SPLIT_REGULAR)
				splitRegsInArgs++;
		}
		errs() << "[compileOperand] getLocal: " << local + splitRegsInArgs << "\n";
		encodeInst(WasmU32Opcode::GET_LOCAL, local + splitRegsInArgs, code);

		// If the get_local is called on a WasmGC type we need to cast it from anyref to the right reftype
		POINTER_KIND kind = COMPLETE_OBJECT;
		if (arg->getType()->isPointerTy())
			kind = PA.getPointerKindForArgument(arg);
		compileRefCast(code, arg->getType(), kind);

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
	else if (op0->getType()->isVectorTy())
	{
		compileOperand(code, op0);
		compileOperand(code, op1);
		encodePredicate(op0->getType(), p, code);
		return ;
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
	Type* ty = lhs->getType();
	assert(ty->isDoubleTy() || ty->isFloatTy() || ty->isVectorTy());
	assert(ty == rhs->getType());

	if (p == CmpInst::FCMP_ORD)
	{
		// Check if both operands are equal to itself. A nan-value is
		// never equal to itself. Use a logical and operator for the
		// resulting comparison.
		compileOperand(code, lhs);
		compileOperand(code, lhs);
		if (ty->isVectorTy())
		{
			if (ty->isDoubleTy())
				encodeInst(WasmSIMDOpcode::F64x2_EQ, code);
			else
				encodeInst(WasmSIMDOpcode::F32x4_EQ, code);
		}
		else if (ty->isDoubleTy())
			encodeInst(WasmOpcode::F64_EQ, code);
		else
			encodeInst(WasmOpcode::F32_EQ, code);

		compileOperand(code, rhs);
		compileOperand(code, rhs);
		if (ty->isVectorTy())
		{
			if (ty->isDoubleTy())
				encodeInst(WasmSIMDOpcode::F64x2_EQ, code);
			else
				encodeInst(WasmSIMDOpcode::F32x4_EQ, code);
		}
		else if (ty->isDoubleTy())
			encodeInst(WasmOpcode::F64_EQ, code);
		else
			encodeInst(WasmOpcode::F32_EQ, code);

		if (ty->isVectorTy())
			encodeInst(WasmSIMDOpcode::V128_AND, code);
		else
			encodeInst(WasmOpcode::I32_AND, code);
	} else if (p == CmpInst::FCMP_UNO) {
		// Check if at least one operand is not equal to itself.
		// A nan-value is never equal to itself. Use a logical
		// or operator for the resulting comparison.
		compileOperand(code, lhs);
		compileOperand(code, lhs);
		if (ty->isVectorTy())
		{
			if (ty->isDoubleTy())
				encodeInst(WasmSIMDOpcode::F64x2_NE, code);
			else
				encodeInst(WasmSIMDOpcode::F32x4_NE, code);
		}
		else if (ty->isDoubleTy())
			encodeInst(WasmOpcode::F64_NE, code);
		else
			encodeInst(WasmOpcode::F32_NE, code);

		compileOperand(code, rhs);
		compileOperand(code, rhs);
		if (ty->isVectorTy())
		{
			if (ty->isDoubleTy())
				encodeInst(WasmSIMDOpcode::F64x2_NE, code);
			else
				encodeInst(WasmSIMDOpcode::F32x4_NE, code);
		}
		else if (ty->isDoubleTy())
			encodeInst(WasmOpcode::F64_NE, code);
		else
			encodeInst(WasmOpcode::F32_NE, code);

		if (ty->isVectorTy())
			encodeInst(WasmSIMDOpcode::V128_OR, code);
		else
			encodeInst(WasmOpcode::I32_OR, code);
	} else if (p == CmpInst::FCMP_ONE || p == CmpInst::FCMP_UEQ) {
		// Check whether either is bigger than the other
		// If there is a NaN operad, both will fail
		// If there are different, both will fail
		// So the answer will be true only for FCMP_ONE
		// (to be flipped in the case of FCMP_UEQ)
		compileOperand(code, lhs);
		compileOperand(code, rhs);
		if (ty->isDoubleTy())
			encodeInst(WasmOpcode::F64_LT, code);
		else
			encodeInst(WasmOpcode::F32_LT, code);

		compileOperand(code, lhs);
		compileOperand(code, rhs);
		if (ty->isDoubleTy())
			encodeInst(WasmOpcode::F64_GT, code);
		else
			encodeInst(WasmOpcode::F32_GT, code);

		encodeInst(WasmOpcode::I32_OR, code);

		//Invert
		if (p == CmpInst::FCMP_UEQ)
			encodeInst(WasmOpcode::I32_EQZ, code);
	} else {
		compileOperand(code, lhs);
		compileOperand(code, rhs);
		// It is much more efficient to invert the predicate if we need to check for unorderedness
		bool invertForUnordered = CmpInst::isUnordered(p);
		if(invertForUnordered)
			p = CmpInst::getInversePredicate(p);
		assert(!CmpInst::isUnordered(p));
		const Type* elementType = nullptr;
		if (ty->isVectorTy())
			elementType = cast<VectorType>(ty)->getElementType();
		switch(p)
		{
#define PREDICATE(Ty, name) \
			case CmpInst::FCMP_O##Ty: \
				if (ty->isDoubleTy()) \
					encodeInst(WasmOpcode::F64_##name, code); \
				else if (ty->isVectorTy()) \
					if (elementType->isFloatTy()) \
						encodeInst(WasmSIMDOpcode::F32x4_##name, code); \
					else \
						encodeInst(WasmSIMDOpcode::F64x2_##name, code); \
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
			if (ty->isVectorTy())
				encodeInst(WasmSIMDOpcode::V128_NOT, code);
			else
				encodeInst(WasmOpcode::I32_EQZ, code);
		}
	}
}

void CheerpWasmWriter::compileDowncastGC(WasmBuffer& code, const CallBase* callV)
{
	errs() << "[compileDowncastGC] Start: " << *callV << "\n";
	assert( callV->arg_size() == 2 );
	assert( callV->getCalledFunction() && callV->getCalledFunction()->getIntrinsicID() == Intrinsic::cheerp_downcast);

	POINTER_KIND result_kind = PA.getPointerKindAssert(callV);
	const Value * src = callV->getOperand(0);
	const Value * offset = callV->getOperand(1);


	Type* t = callV->getParamElementType(0);
	// TODO: what is a client type and do we need to support it from the wasm side?
	if(TypeSupport::isClientType(t) || (isa<ConstantInt>(offset) && cast<ConstantInt>(offset)->isNullValue()))
	{
		// TODO: test if it works
		assert(false);
		if (result_kind == SPLIT_REGULAR)
		{
			uint32_t reg = registerize.getRegisterId(callV, 1, edgeContext);
			uint32_t local = localMap.at(reg);
			compilePointerOffset(code, src);
			encodeInst(WasmU32Opcode::SET_LOCAL, local, code);
			compilePointerBaseTyped(code, src, t);
		}
		else
			compilePointerAs(code, src, result_kind);
	}
	else
	{
		Type* returnTy = callV->getType();
		int32_t returnTypeIdx = linearHelper.getGCTypeIndex(returnTy, COMPLETE_OBJECT, true);
		StructType* sTy = cast<StructType>(returnTy->getPointerElementType());
		//Do a runtime downcast
		if(result_kind == SPLIT_REGULAR)
		{
			// TODO: test if it works
			assert(false);
			uint32_t reg = registerize.getRegisterId(callV, 1, edgeContext);
			uint32_t local = localMap.at(reg);
			assert(downcastArrayIndices.find(sTy) != downcastArrayIndices.end());
			assert(linearHelper.hasDowncastArray(sTy));

			// push the .a onto the stack so it can get stored
			compileCompleteObject(code, src);
			// TODO: find a way to have the correct cast on the get local that happens above ^
			// Root structs have to be cast from the root to the type that includes the
			// downcast array and offset
			if (!sTy->hasDirectBase())
				encodeInst(WasmGCOpcode::REF_CAST, returnTypeIdx, code);
			encodeInst(WasmGCOpcode::STRUCT_GET, returnTypeIdx, downcastArrayIndices[sTy], code);
			// store the .o-num
			compileCompleteObject(code, src);
			// TODO: find a way to have the correct cast on the get local that happens above ^
			// Root structs have to be cast from the root to the type that includes the
			// downcast array and offset
			if (!sTy->hasDirectBase())
				encodeInst(WasmGCOpcode::REF_CAST, returnTypeIdx, code);
			encodeInst(WasmGCOpcode::STRUCT_GET, returnTypeIdx, downcastArrayIndices[sTy] + 1, code);
			compileOperand(code, offset);
			encodeInst(WasmOpcode::I32_SUB, code);
			encodeInst(WasmU32Opcode::SET_LOCAL, local, code);
		}
		else if(result_kind == REGULAR)
		{
			// create the .d using the .a
			compileCompleteObject(code, src);
			encodeInst(WasmGCOpcode::STRUCT_GET, returnTypeIdx, downcastArrayIndices[sTy], code);
			// create the .o using the .o-num
			compileCompleteObject(code, src);
			encodeInst(WasmGCOpcode::STRUCT_GET, returnTypeIdx, downcastArrayIndices[sTy] + 1, code);
			compileOperand(code, offset);
			encodeInst(WasmOpcode::I32_SUB, code);

			// create the new regular object
			encodeInst(WasmGCOpcode::STRUCT_NEW, linearHelper.getRegularObjectIdx(), code);
			// // TODO:
			assert(false);
		}
		else if(result_kind == RAW)
		{
			// TODO:
			assert(false);
		}
		else
		{
			const int32_t typeIdx = linearHelper.getGCTypeIndex(src->getType(), COMPLETE_OBJECT, true);
			const StructType* sTy = cast<StructType>(src->getType()->getPointerElementType());
			assert(downcastArrayIndices.find(sTy) != downcastArrayIndices.end());
			assert(linearHelper.hasDowncastArray(sTy));

			// push the downcast array on the stack
			compileCompleteObject(code, src);
			// TODO: find a way to have the correct cast on the get local that happens above ^
			// Root structs have to be cast from the root to the type that includes the
			// downcast array and offset
			if (!sTy->hasDirectBase())
				encodeInst(WasmGCOpcode::REF_CAST, typeIdx, code);
			// errs() << "[compileDowncastGC] getting the downcast array from typeidx: " << typeIdx << " at offset: " << (downcastArrayIndices[sTy] + 1) << "\n";  
			encodeInst(WasmGCOpcode::STRUCT_GET, typeIdx, downcastArrayIndices[sTy], code);

			// encode the .o-num
			compileCompleteObject(code, src);
			// TODO: find a way to have the correct cast on the get local that happens above ^
			// Root structs have to be cast from the root to the type that includes the
			// downcast array and offset
			if (!sTy->hasDirectBase())
				encodeInst(WasmGCOpcode::REF_CAST, typeIdx, code);
			// errs() << "[compileDowncastGC] getting the downcast offset from typeidx: " << typeIdx << " at offset: " << (downcastArrayIndices[sTy]) << "\n";
			encodeInst(WasmGCOpcode::STRUCT_GET, typeIdx, downcastArrayIndices[sTy] + 1, code);
			compileOperand(code, offset);
			encodeInst(WasmOpcode::I32_SUB, code);

			// access the downcast array
			encodeInst(WasmGCOpcode::ARRAY_GET, linearHelper.getSplitRegularObjectIdx(), code);
			// TODO: this cast is not always needed (if we store it in a local right after)
			compileRefCast(code, callV->getType(), COMPLETE_OBJECT);
		}
	}
	errs() << "[compileDowncastGC] Done\n";
}

void CheerpWasmWriter::compileVirtualcastGC(WasmBuffer& code, const CallBase* callV)
{
	// TODO: find a test case
	assert(false);

	assert( callV->arg_size() == 2 );
	assert( callV->getCalledFunction() && callV->getCalledFunction()->getIntrinsicID() == Intrinsic::cheerp_virtualcast);

	POINTER_KIND kind = PA.getPointerKindAssert(callV);
	const Value * src = callV->getOperand(0);
	const Value * offset = callV->getOperand(1);
	const StructType* sTy = cast<StructType>(src->getType()->getNonOpaquePointerElementType());
	const int32_t typeIdx = linearHelper.getGCTypeIndex(sTy, COMPLETE_OBJECT, true);

	assert(kind != RAW);
	if (kind == SPLIT_REGULAR)
	{
		// push the .a on the stack
		compileCompleteObject(code, src);
		encodeInst(WasmGCOpcode::STRUCT_GET, typeIdx, downcastArrayIndices[sTy], code);
		// store the offset
		uint32_t reg = registerize.getRegisterId(callV, 1, edgeContext);
		uint32_t local = localMap.at(reg);

		compileOperand(code, offset);
		encodeInst(WasmU32Opcode::SET_LOCAL, local, code);
	}
	else if (kind == REGULAR)
	{
		// encode the .d
		compileCompleteObject(code, src);
		encodeInst(WasmGCOpcode::STRUCT_GET, typeIdx, downcastArrayIndices[sTy], code);
		// encode the .o
		compileOperand(code, offset);
		// create a new REGULAR object
		encodeInst(WasmGCOpcode::STRUCT_NEW, linearHelper.getRegularObjectIdx(), code);
	}
	else
	{
		// Access the downcast array using the offset
		compileCompleteObject(code, src);
		encodeInst(WasmGCOpcode::STRUCT_GET, typeIdx, downcastArrayIndices[sTy], code);
		compileOperand(code, offset);
		encodeInst(WasmGCOpcode::ARRAY_GET, linearHelper.getSplitRegularObjectIdx(), code);
		// TODO: cast to the result type
		assert(false);
	}
}

void CheerpWasmWriter::compileDowncast(WasmBuffer& code, const CallBase* callV)
{
	errs() << "[compileDowncast]\n";
	assert(callV->arg_size() == 2);
	assert(callV->getCalledFunction()->getIntrinsicID() == Intrinsic::cheerp_downcast ||
		callV->getCalledFunction()->getIntrinsicID() == Intrinsic::cheerp_virtualcast);

	const Value* src = callV->getOperand(0);
	const Value* offset = callV->getOperand(1);

	Type* t = callV->getParamElementType(0);

	compileOperand(code, src);

	if(!TypeSupport::isClientType(t) &&
			(!isa<ConstantInt>(offset) || !cast<ConstantInt>(offset)->isNullValue()))
	{
		compileOperand(code, offset);
		encodeInst(WasmOpcode::I32_ADD, code);
	}
}

void CheerpWasmWriter::compileRefCast(WasmBuffer& code, const Type* Ty, POINTER_KIND kind)
{
	if (TypeSupport::isTypeGC(Ty))
	{
		int32_t idx = linearHelper.getGCTypeIndex(Ty, kind);
		errs() << "[compileRefCast] casting to index: " << idx << "\n";
		errs() << "[compileRefCast] casting " << *Ty << " to a "; printPtrKind(kind);
		encodeInst(WasmGCOpcode::REF_CAST_NULL, code);
		encodeSLEB128(linearHelper.getGCTypeIndex(Ty, kind), code);
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
		else if(isa<IntToPtrInst>(p) && isa<ConstantInt>(cast<IntToPtrInst>(p)->getOperand(0)) && isInlineable(*cast<Instruction>(p)))
			gepWriter.addConst(cast<ConstantInt>(cast<IntToPtrInst>(p)->getOperand(0))->getSExtValue());
		else
			gepWriter.addValue(p, 1);

		//compileValues returs the offset yet to be handled
		offset = gepWriter.compileValues(/*positiveOffsetAllowed*/true);
	} else {
		errs() << "[Load Store Pointer] Non-inlineable compileLoadStorePointer\n";
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
			errs() << "[Load Store Pointer] Encoding const 0\n";
			offset = addrListener.addr;
		}
		else
		{
			errs() << "[Load Store Pointer] Compiling operand: "; ptrOp->dump();
			compileOperand(code, ptrOp);
		}
	}
	return offset;
}

void CheerpWasmWriter::compileLoadGC(WasmBuffer& code, const LoadInst& li, Type* loadedType, const Value* ptrOp, StructType* sTy, uint32_t structElemIdx, bool isOffset)
{
	// For aggregate type handling see the implementation inside CheerpWriter
	assert(sTy == NULL && "Loads of aggregates are not supported");
	POINTER_KIND kind = PA.getPointerKind(ptrOp);
	POINTER_KIND loadedPtrKind = COMPLETE_OBJECT;
	assert(kind != BYTE_LAYOUT);
	assert(kind != RAW);

	errs() << "[compileLoadGC] compiling load: " << *ptrOp << "\n";
	errs() << "[compileLoadGC] ptr kind: "; printPtrKind(kind);
	if (kind == CONSTANT)
	{
		// An invalid access to null/undefined which has not been removed by optimizations.
		// Generate code that will trap at runtime.
		assert(false); // TODO: check if this passes validation
		encodeInst(WasmOpcode::UNREACHABLE, code);
		return ;
	}

	// Compiles the reference and offset the load is performed on
	compileCompleteObject(code, ptrOp, NULL, false);

	// If the loaded type is a pointer get its type and its kind so
	// we can get the correct type index and reference cast
	if (loadedType->isPointerTy())
	{
		loadedType = loadedType->getNonOpaquePointerElementType();
		loadedPtrKind = PA.getPointerKind(&li);
	}

	// The load
	if (kind == SPLIT_REGULAR || kind == POINTER_KIND::REGULAR)
	{
		assert(!isOffset);
		const size_t typeIdx = linearHelper.getGCTypeIndex(loadedType, SPLIT_REGULAR);
		errs() << "[compileLoadGC] ARRAY_GET on SPLIT_REGULAR\n";
		errs() << "[compileLoadGC] ARRAY_GET on: " << *loadedType << "\n";
		encodeInst(WasmGCOpcode::ARRAY_GET, typeIdx, code);
	}
	else if (kind == COMPLETE_OBJECT)
	{
		const GetElementPtrInst* gep_inst = cast<GetElementPtrInst>(ptrOp);
		Type* targetType = getGEPContainerType(gep_inst);
		int32_t elemIdx = cast<ConstantInt>(std::prev(gep_inst->op_end())->get())->getLimitedValue();

		errs() << "[compileLoadGC] Trying to store into targetType: " << *targetType << "\n";
		errs() << "[compileLoadGC] GEP: " << *gep_inst << "\n";

		// If a wrapper array was used we load from the array instead
		StructType* sTy = cast<StructType>(targetType);
		if (types.useWrapperArrayForMember(PA, sTy, elemIdx))
		{
			assert(!isOffset);
			const int32_t arrayTypeIdx = linearHelper.getGCTypeIndex(loadedType, SPLIT_REGULAR);
			encodeInst(WasmGCOpcode::ARRAY_GET, arrayTypeIdx, code);
		}
		else
		{
			const int32_t typeIdx = linearHelper.getGCTypeIndex(targetType, kind);
			elemIdx = getExpandedStructElemIdx(sTy, elemIdx);

			// Load the offset instead
			if (isOffset)
				elemIdx++;
			errs() << "[compileLoadGC] STRUCT_SET of typeIdx: " << typeIdx << " at field: " << elemIdx << "\n";
			encodeInst(WasmGCOpcode::STRUCT_GET, typeIdx, elemIdx, code);
		}
	}
	else
	{
		llvm::report_fatal_error("unsupported GC load");
	}

	// If the we did not load an offset try to compile a reference cast
	if (!isOffset)
		compileRefCast(code, loadedType, loadedPtrKind);
}

void CheerpWasmWriter::compileLoad(WasmBuffer& code, const LoadInst& li, bool signExtend)
{
	const Value* ptrOp=li.getPointerOperand();
	auto* Ty = li.getType();
	auto* STy = dyn_cast<StructType>(Ty);

	bool LoadIsGC = PA.getPointerKind(ptrOp) != RAW || TypeSupport::isTypeGC(ptrOp->getType()); // TODO: use address space
	errs() << "[compileLoad] compiling load: " << li << "\n";
	errs() << "[compileLoad] ptrOp: " << *ptrOp << "\n";
	errs() << "[compileLoad] ptrOp type: " << *ptrOp->getType() << "\n"; 
	for(const auto& ie: getInstElems(&li, PA))
	{
		if (LoadIsGC)
		{
			if (STy)
			{
				// Aggregate loads are currently asserted inside compileLoadGC
			}
			errs() << "[compileLoad] compilingLoadGC\n";
			bool isOffset = ie.ptrIdx == 1;
			compileLoadGC(code, li, Ty, ptrOp, STy, ie.structIdx, isOffset);
		}
		else
		{
			errs() << "[compileLoad] Compiling regular load: " << li << "\n";
			// 1) The pointer
			uint32_t offset = compileLoadStorePointer(code, ptrOp);
			if(STy)
			{
				Ty = STy->getElementType(ie.structIdx);
				const StructLayout* SL = targetData.getStructLayout(STy);
				int64_t elementOffset =  SL->getElementOffset(ie.structIdx);
				offset += elementOffset;
			}
			// 2) Load
			encodeLoad(Ty, offset, code, signExtend, li.isAtomic());
		}
	}
}

void CheerpWasmWriter::compileStoreGC(WasmBuffer& code, const StoreInst& si, Type* storedType, StructType* sTy, uint32_t structElemIdx, bool isOffset, POINTER_KIND ptrKind, POINTER_KIND storeKind)
{
	errs() << "[compileStoreGC] START\n";
	// For aggregate type handling see the implementation inside CheerpWriter
	assert(sTy == NULL && "Stores of aggregates are not supported");
	assert(ptrKind != CONSTANT);
	assert(ptrKind != BYTE_LAYOUT);
	assert(storeKind != BYTE_LAYOUT);
	auto* ptrOp = si.getPointerOperand();
	auto* valOp = si.getValueOperand(); 

	errs() << "[compileStoreGC] compiling pointer operand\n";
	errs() << "[compileStoreGC] storeKind: "; printPtrKind(storeKind);
	errs() << "[compileStoreGC] ptrKind: "; printPtrKind(ptrKind);
	errs() << "[compileStoreGC] storedType: " << *storedType << "\n";
	errs() << "[compileStoreGC] struct elemIdx: " << structElemIdx << "\n";
	errs() << "[compileStoreGC] is offset: " << (isOffset ? "true" : "false") << "\n";
	errs() << "[compileStoreGC] storeInst: " << si << "\n";


	// Compiles the reference and offset where the store is performed on
	compileCompleteObject(code, ptrOp, nullptr, false);

	errs() << "\n\n[compileStoreGC] compiling value to be stored\n";
	// The value
	if(storedType->isPointerTy())
	{
		errs() << "[compileStoreGC] compiling pointer type\n";
		assert(storeKind != CONSTANT);
		bool hasConstantOffset = PA.getConstantOffsetForPointer(ptrOp);
		if(storeKind==SPLIT_REGULAR || (storeKind == REGULAR && hasConstantOffset))
		{
			if (isOffset)
			{
				assert(storeKind == SPLIT_REGULAR);
				compilePointerOffset(code, valOp);
			}
			else
				compilePointerBase(code, valOp, structElemIdx);
		}
		else
			compilePointerAs(code, valOp, storeKind);
	}
	else
	{
		errs() << "[compileStoreGC] compiling value operand\n";
		compileOperand(code, valOp);
	}


	// The store
	errs() << "\n\n[compileStoreGC] compiling the store: " << si << "\n";
	if (ptrKind == POINTER_KIND::SPLIT_REGULAR || ptrKind == POINTER_KIND::REGULAR)
	{
		assert(!isOffset);
		errs() << "[compileStoreGC] ARRAY_SET for a SPLIT_REGULAR\n";
		const uint32_t typeIdx = linearHelper.getGCTypeIndex(storedType, SPLIT_REGULAR);
		encodeInst(WasmGCOpcode::ARRAY_SET, typeIdx, code);
	}
	else if (ptrKind == POINTER_KIND::COMPLETE_OBJECT)
	{
		errs() << "[compileStoreGC] Storing into complete object\n";
		const GetElementPtrInst* gep_inst = cast<GetElementPtrInst>(ptrOp);
		Type* targetType = getGEPContainerType(gep_inst);
		int32_t elemIdx = cast<ConstantInt>(std::prev(gep_inst->op_end())->get())->getLimitedValue();

		errs() << "[compileStoreGC] Trying to store into targetType: " << *targetType << "\n";
		errs() << "[compileStoreGC] GEP: " << *gep_inst << "\n";

		// If a wrapper array was used we store into the array instead
		StructType* sTy = cast<StructType>(targetType);
		if (types.useWrapperArrayForMember(PA, sTy, elemIdx))
		{
			const int32_t arrayTypeIdx = linearHelper.getGCTypeIndex(storedType, SPLIT_REGULAR);
			encodeInst(WasmGCOpcode::ARRAY_SET, arrayTypeIdx, code);
		}
		else
		{
			const int32_t typeIdx = linearHelper.getGCTypeIndex(targetType, ptrKind);
			elemIdx = getExpandedStructElemIdx(sTy, elemIdx);

			// Store into the offset instead
			if (isOffset)
				elemIdx++;
			errs() << "[compileStoreGC] STRUCT_SET of typeIdx: " << typeIdx << " at field: " << elemIdx << "\n";
			encodeInst(WasmGCOpcode::STRUCT_SET, typeIdx, elemIdx, code);
		}
	}
	else
	{
		llvm::report_fatal_error("unsupported GC store");
	}
	errs() << "[compileStoreGC] END\n\n\n";
}

void CheerpWasmWriter::compileStore(WasmBuffer& code, const StoreInst& si)
{
	const Value* ptrOp=si.getPointerOperand();
	const Value* valOp=si.getValueOperand();
	POINTER_KIND ptrKind = PA.getPointerKind(ptrOp);
	auto* Ty = valOp->getType();
	auto* STy = dyn_cast<StructType>(Ty);
	bool StoreIsGC = ptrKind != RAW; // TODO: check address space
	for(const auto& ie: getInstElems(&si, PA))
	{
		if (StoreIsGC)
		{
			errs() << "[compileStore] GC store found\n";
			POINTER_KIND storePtrKind = COMPLETE_OBJECT;
			if(STy)
			{
				// Aggregate stores are currently asserted inside compileStoreGC
			}
			else if(Ty->isPointerTy())
			{
				storePtrKind = PA.getPointerKind(&si);
			}
			bool isOffset = ie.ptrIdx == 1;
			compileStoreGC(code, si, Ty, STy, ie.structIdx, isOffset, ptrKind, storePtrKind);
		}
		else
		{
			errs() << "[compileStore] Compiling loadStorePointer for ptrOp: ", ptrOp->dump(), errs() << "[compileStore] Instruction: ", si.dump();
			// 1) The pointer
			uint32_t offset = compileLoadStorePointer(code, ptrOp);
			if(STy)
			{
				Ty = STy->getElementType(ie.structIdx);
				const StructLayout* SL = targetData.getStructLayout(STy);
				int64_t elementOffset =  SL->getElementOffset(ie.structIdx);
				offset += elementOffset;
			}
			// Special case writing 0 to floats/double
			if(Ty->isFloatingPointTy() && isa<Constant>(valOp) && cast<Constant>(valOp)->isNullValue())
			{
				if(Ty->isFloatTy())
				{
					encodeInst(WasmS32Opcode::I32_CONST, 0, code);
					encodeInst(WasmU32U32Opcode::I32_STORE, 0x2, offset, code);
				}
				else
				{
					assert(Ty->isDoubleTy());
					encodeInst(WasmS64Opcode::I64_CONST, 0, code);
					encodeInst(WasmU32U32Opcode::I64_STORE, 0x3, offset, code);
				}
				return;
			}
			// 2) The value
			if(STy)
			{
				compileAggregateElem(code, valOp, ie.structIdx);
			}
			else
			{
				compileOperand(code, valOp);
			}
			// 3) Store
			// When storing values with size less than 32-bit we need to truncate them
			bool atomic = si.isAtomic();
			if(Ty->isIntegerTy())
			{
				uint32_t bitWidth = targetData.getTypeStoreSizeInBits(Ty);

				switch (bitWidth)
				{
					case 8:
						if (atomic)
							encodeInst(WasmThreadsU32U32Opcode::I32_ATOMIC_STORE8, 0x0, offset, code);
						else
							encodeInst(WasmU32U32Opcode::I32_STORE8, 0x0, offset, code);
						break;
					case 16:
						if (atomic)
							encodeInst(WasmThreadsU32U32Opcode::I32_ATOMIC_STORE16, 0x1, offset, code);
						else
							encodeInst(WasmU32U32Opcode::I32_STORE16, 0x1, offset, code);
						break;
					case 32:
						if (atomic)
							encodeInst(WasmThreadsU32U32Opcode::I32_ATOMIC_STORE, 0x2, offset, code);
						else
							encodeInst(WasmU32U32Opcode::I32_STORE, 0x2, offset, code);
						break;
					case 64:
						if (atomic)
							encodeInst(WasmThreadsU32U32Opcode::I64_ATOMIC_STORE, 0x3, offset, code);
						else
							encodeInst(WasmU32U32Opcode::I64_STORE, 0x2, offset, code);
						break;
					default:
						llvm::errs() << "bit width: " << bitWidth << '\n';
						llvm_unreachable("unknown integer bit width");
				}
			}
			else if (Ty->isVectorTy())
			{
				assert(!atomic && "atomic stores only supported on integers");
				const FixedVectorType* vecType = cast<FixedVectorType>(Ty);
				const unsigned vecWidth = getVectorBitwidth(vecType);
				if (vecWidth == 128)
					encodeInst(WasmSIMDU32U32Opcode::V128_STORE, 0x2, offset, code);
				else if (vecWidth == 64)
				{
					encodeStoringShuffle(code, vecType);
					encodeInst(WasmSIMDU32U32U32Opcode::V128_STORE64_LANE, 0x3, offset, 0, code);
				}
				else if (vecWidth == 32)
				{
					encodeStoringShuffle(code, vecType);
					encodeInst(WasmSIMDU32U32U32Opcode::V128_STORE32_LANE, 0x2, offset, 0, code);
				}
				else if (vecWidth == 16)
				{
					encodeStoringShuffle(code, vecType);
					encodeInst(WasmSIMDU32U32U32Opcode::V128_STORE16_LANE, 0x1, offset, 0, code);
				}
				else
				{
					llvm::errs() << "bit width: " << vecWidth << "\n";
					llvm_unreachable("unknown vector bit width");
				}
			}
			else
			{
				assert(!atomic && "atomic stores only supported on integers");
				if (Ty->isFloatTy())
					encodeInst(WasmU32U32Opcode::F32_STORE, 0x2, offset, code);
				else if (Ty->isDoubleTy())
					encodeInst(WasmU32U32Opcode::F64_STORE, 0x3, offset, code);
				else
					encodeInst(WasmU32U32Opcode::I32_STORE, 0x2, offset, code);
			}
		}
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

void CheerpWasmWriter::callDowncastArrayInit(WasmBuffer&code, const Type* Ty)
{
	assert(Ty->isStructTy());
	assert(linearHelper.getDowncastFuncIds().find(Ty) != linearHelper.getDowncastFuncIds().end());
	uint32_t initId = linearHelper.getDowncastFuncIds().at(Ty);
	encodeInst(WasmU32Opcode::CALL, initId, code);
}

void CheerpWasmWriter::allocateSimpleType(WasmBuffer& code, Type* Ty, const Value* init)
{
	assert(TypeSupport::isSimpleType(Ty, false));
	assert(!TypeSupport::hasByteLayout(Ty));

	errs() << "[allocateSimpleType] allocating type: " << *Ty << "\n";
	if (init)
		errs() << "[allocateSimpleType] allocating with init value: " << *init << "\n";  

	switch(Ty->getTypeID())
	{
		case Type::IntegerTyID:
		{
			if(init)
				compileOperand(code, init);
			else if (Ty->isIntegerTy(64))
				encodeInst(WasmS64Opcode::I64_CONST, 0, code);
			else
				encodeInst(WasmS32Opcode::I32_CONST, 0, code);
			break;
		}
		case Type::FloatTyID:
		{
			if(init)
				compileOperand(code, init);
			else
				encodeInst(WasmOpcode::F32_CONST, code);
				encodeF32(0, code);
			break;
		}
		case Type::DoubleTyID:
		{
			if(init)
				compileOperand(code, init);
			else
				encodeInst(WasmOpcode::F64_CONST, code);
				encodeF64(0, code);
			break;
		}
		case Type::PointerTyID:
		{
			if(init)
				compilePointerAs(code, init, PA.getPointerKindForStoredType(Ty));
			else
			{
				POINTER_KIND kind = PA.getPointerKindForStoredType(Ty);
				assert(kind != RAW);
				if (kind == COMPLETE_OBJECT)
				{
					int32_t typeIdx = linearHelper.getGCTypeIndex(Ty, kind);
					encodeInst(WasmS32Opcode::REF_NULL, typeIdx, code);
				}
				else if (kind == RAW)
					encodeInst(WasmS32Opcode::I32_CONST, 0, code);
				else
					encodeInst(WasmU32Opcode::GET_GLOBAL, nullObjGlobal, code);
				errs() << "[allocateSimpleType] allocating null\n";
			}
			break;
		}
		default:
			assert(false && "unexpected type");
	}
}

void CheerpWasmWriter::allocateComplexType(WasmBuffer& code, Type* Ty, bool hasDowncastArray, uint32_t& usedValuesFromMap, const AllocaStoresExtractor::OffsetToValueMap *offsetToValueMap, uint32_t offset)
{
	assert(!TypeSupport::isSimpleType(Ty, false));

	errs() << "[allocateComplexType] Allocating for type: " << *Ty << "\n";

	if (StructType* sTy = dyn_cast<StructType>(Ty))
	{
		assert(!TypeSupport::hasByteLayout(sTy));
		uint32_t downcastIdx = 0;
		if (hasDowncastArray)
		{
			assert(downcastArrayIndices.find(sTy) != downcastArrayIndices.end());
			assert(linearHelper.hasDowncastArray(sTy));
			downcastIdx = downcastArrayIndices[sTy];
		}

		const StructLayout* SL = targetData.getStructLayout(sTy);
		for (uint32_t i = 0; i < sTy->getNumElements(); i++)
		{
			Type* elemTy = sTy->getStructElementType(i);
			errs() << "[allocateComplexType] allocating elem index: " << i << " type: " << *elemTy << "\n";


			llvm::Value* init = nullptr;
			uint32_t totalOffset = offset + SL->getElementOffset(i);
			if(offsetToValueMap)
			{
				auto it = offsetToValueMap->find(totalOffset);
				if(it != offsetToValueMap->end())
					init = it->second;
			}
			if (init)
				errs() << "[allocateComplexType] index: " << i << " has init: " << *init << "\n";

			if (elemTy->isPointerTy())
			{
				TypeAndIndex baseAndIndex(sTy, i, TypeAndIndex::STRUCT_MEMBER);
				POINTER_KIND memberKind = PA.getPointerKindForMemberPointer(baseAndIndex);
				bool hasConstantOffset = PA.getConstantOffsetForMember(baseAndIndex);

				if (init)
					usedValuesFromMap++;
				assert(memberKind != RAW); //TODO: handle RAW pointers?
				if((memberKind == REGULAR || memberKind == SPLIT_REGULAR) && hasConstantOffset)
				{
					if(init)
						compilePointerBase(code, init);
					else
						encodeInst(WasmU32Opcode::GET_GLOBAL, nullArrayGlobal, code);
				}
				else if (memberKind == SPLIT_REGULAR)
				{
					if(init)
						compilePointerBase(code, init);
					else
						encodeInst(WasmU32Opcode::GET_GLOBAL, nullArrayGlobal, code);

					if(init)
						compilePointerOffset(code, init);
					else
						encodeInst(WasmS32Opcode::I32_CONST, 0, code);
				}
				else if (init)
					compilePointerAs(code, init, memberKind);
				else if (memberKind == COMPLETE_OBJECT)
					encodeInst(WasmU32Opcode::REF_NULL, 0x6E, code);
				else if (memberKind == RAW)
				{
					// TODO: test RAW pointers
					assert(false);
					encodeInst(WasmS32Opcode::I32_CONST, 0, code);
				}
				else
					encodeInst(WasmU32Opcode::GET_GLOBAL, nullObjGlobal, code);
			}
			else if(TypeSupport::isSimpleType(elemTy, false))
			{
				if(init)
					usedValuesFromMap++;
				allocateSimpleType(code, elemTy, init);
			}
			else
			{
				errs() << "[allocateComplexType] calling again for elemTy: " << *elemTy << "\n";
				allocateComplexType(code, elemTy, hasDowncastArray, usedValuesFromMap, offsetToValueMap, totalOffset);
			}

			if (types.useWrapperArrayForMember(PA, sTy, i))
			{
				errs() << "[allocateComplexType] allocating wrapper array\n";
				uint32_t typeIdx = linearHelper.getGCTypeIndex(elemTy, SPLIT_REGULAR);
				encodeInst(WasmGCOpcode::ARRAY_NEW_FIXED, typeIdx, 1, code);
			}

			if (hasDowncastArray && i + 1 == downcastIdx)
			{
				errs() << "[allocateComplexType] Allocating the .a and .o at index: " << i << "\n";
				// encode the .a
				encodeInst(WasmS32Opcode::REF_NULL, linearHelper.getSplitRegularObjectIdx(), code);
				// encode the .o
				encodeInst(WasmS32Opcode::I32_CONST, 0, code);
			}
		}
		uint32_t typeIdx = linearHelper.getGCTypeIndex(Ty, COMPLETE_OBJECT, hasDowncastArray);
		errs() << "[allocateComplexType] STRUCT_NEW: " << typeIdx << "\n";
		encodeInst(WasmGCOpcode::STRUCT_NEW, typeIdx, code);

		// TODO: is this correct? JS has it only for objects marked with a LITERAL_OBJ style
		if (globalDeps.needsDowncastArray(sTy))
			callDowncastArrayInit(code, sTy);
	}
	else
	{
		ArrayType* aTy = cast<ArrayType>(Ty);
		Type* elemTy = aTy->getElementType();
		bool elemHasDowncast = false;
		if (auto sTy = dyn_cast<StructType>(elemTy))
			elemHasDowncast = sTy->hasDirectBase() && linearHelper.hasDowncastArray(sTy);
		uint32_t typeIdx = linearHelper.getGCTypeIndex(Ty, COMPLETE_OBJECT);
		for (uint64_t i = 0; i < aTy->getNumElements(); i++)
		{
			uint32_t elementSize = targetData.getTypeAllocSize(aTy->getElementType());
			uint32_t totalOffset = offset + elementSize * i;
			if(TypeSupport::isSimpleType(elemTy, false))
			{
				llvm::Value* init = nullptr;
				if(offsetToValueMap)
				{
					auto it = offsetToValueMap->find(totalOffset);
					if(it != offsetToValueMap->end())
						init = it->second;
				}
				if(init)
					usedValuesFromMap++;
				allocateSimpleType(code, elemTy, init);
			}
			else
				allocateComplexType(code, elemTy, elemHasDowncast, usedValuesFromMap, offsetToValueMap, totalOffset);
		}
		encodeInst(WasmGCOpcode::ARRAY_NEW_FIXED, typeIdx, aTy->getArrayNumElements(), code);
	}
	errs() << "[allocateComplexType] DONE\n";
}

void CheerpWasmWriter::allocateTypeGC(WasmBuffer& code, Type* allocaType, bool hasDowncastArray, const AllocaStoresExtractor::OffsetToValueMap *offsetToValueMap)
{
	errs() << "[allocateTypeGC] Allocating type: " << *allocaType << "\n";

	if (TypeSupport::isSimpleType(allocaType, false))
	{
		Value* init = nullptr;
		if (offsetToValueMap)
		{
			assert(offsetToValueMap->size() == 1);
			auto it = offsetToValueMap->find(0);
			if(it != offsetToValueMap->end())
				init = it->second;
		}
		allocateSimpleType(code, allocaType, init);
	}
	else
	{
		uint32_t usedValuesFromMap = 0;
		allocateComplexType(code, allocaType, hasDowncastArray, usedValuesFromMap, offsetToValueMap, 0);
		if (offsetToValueMap)
			assert(offsetToValueMap->size() == usedValuesFromMap);
	}
}

void CheerpWasmWriter::compileAllocationGC(WasmBuffer& code, const DynamicAllocInfo& info)
{
	assert (info.isValidAlloc());

	Type* Ty = info.getCastedPointedType();
	POINTER_KIND result = PA.getPointerKindAssert(info.getInstruction());
	const ConstantInt* constantOffset = PA.getConstantOffsetForPointer(info.getInstruction());
	bool needsDowncastArray = isa<StructType>(Ty) && globalDeps.needsDowncastArray(cast<StructType>(Ty));
	bool needsRegular = result==REGULAR && !constantOffset;

	errs() << "[compileAllocationGC] Type: " << *Ty << "\n";

	assert(!info.useTypedArray()); // TODO:
	assert(result != BYTE_LAYOUT);
	assert(result != SPLIT_REGULAR || constantOffset);
	int32_t splitRegIdx = linearHelper.getSplitRegularObjectIdx();
	if (info.useCreatePointerArrayFunc())
	{
		assert(globalDeps.needCreatePointerArray());
		if (info.getAllocType() == DynamicAllocInfo::cheerp_reallocate)
		{
			compilePointerBaseTyped(code, info.getMemoryArg(), info.getCastedPointedType());
			compilePointerBaseTyped(code, info.getMemoryArg(), info.getCastedPointedType());
			encodeInst(WasmGCOpcode::ARRAY_LEN, splitRegIdx, code);
		}
		else
		{
			// TODO: can we call compileArraySize twice? it compiles the value again
			compileArraySizeGC(code, info, true);
			encodeInst(WasmGCOpcode::ARRAY_NEW_DEFAULT, splitRegIdx, code);
			// if we have to allocate an array with just null references we can
			// use the array_new_default rather than the createPointerArray func
			if (PA.getPointerKindForStoredType(info.getCastedType()) == COMPLETE_OBJECT)
				return ;
		}

		compileArraySizeGC(code, info, true);

		if (PA.getPointerKindForStoredType(info.getCastedType()) == COMPLETE_OBJECT)
			encodeInst(WasmS32Opcode::REF_NULL, 0x6E, code);
		else
			encodeInst(WasmU32Opcode::GET_GLOBAL, nullObjGlobal, code);

		// parameters are (array, start index, size, init object)
		encodeInst(WasmU32Opcode::CALL, linearHelper.getCreatePointerArrayFuncId(), code);
	}
	else if (info.useCreateArrayFunc())
	{
		errs() << "[compileAllocationGC] Using create array func\n"; 
		if (info.getAllocType() == DynamicAllocInfo::cheerp_reallocate)
		{
			assert(globalDeps.dynResizeArrays().count(Ty));
			auto it = linearHelper.getResizeArrayFuncIds().find(Ty);
			assert(it != linearHelper.getResizeArrayFuncIds().end());
			compilePointerBaseTyped(code, info.getMemoryArg(), info.getCastedPointedType());
			compilePointerBaseTyped(code, info.getMemoryArg(), info.getCastedPointedType());
			encodeInst(WasmGCOpcode::ARRAY_LEN, splitRegIdx, code);
			compileArraySizeGC(code, info, true);
			encodeInst(WasmU32Opcode::CALL, it->second, code);
		}
		else
		{
			compileArraySizeGC(code, info, true);
			// non-aggregate types can be created using ARRAY_NEW_DEFAULT
			if (Ty->isAggregateType())
			{
				auto it = linearHelper.getCreateArrayFuncIds().find(Ty);
				assert(it != linearHelper.getCreateArrayFuncIds().end());
				encodeInst(WasmU32Opcode::CALL, it->second, code);
			}
			else
			{
				int32_t arrayTypeIdx = linearHelper.getGCTypeIndex(Ty, SPLIT_REGULAR);
				encodeInst(WasmGCOpcode::ARRAY_NEW_DEFAULT, arrayTypeIdx, code);
			}
		}

		if (result == COMPLETE_OBJECT)
		{
			encodeInst(WasmS32Opcode::I32_CONST, 0, code);
			encodeInst(WasmGCOpcode::ARRAY_GET, splitRegIdx, code);
		}
	}
	else if (!info.sizeIsRuntime())
	{
		errs() << "[compileAllocationGC] size is not runtime info\n";
		assert(info.getAllocType() != DynamicAllocInfo::cheerp_reallocate);
		uint32_t numElem = compileArraySizeGC(code, info, false);
		assert((result == REGULAR || result == SPLIT_REGULAR) || numElem <= 1);

		for(uint32_t i = 0; i < numElem;i++)
			allocateTypeGC(code, Ty, needsDowncastArray);

		if (numElem == 0)
		{
			encodeInst(WasmU32Opcode::REF_NULL, 0x6E, code);
			numElem = 1;
		}
		if (result == REGULAR || result == SPLIT_REGULAR)
			encodeInst(WasmGCOpcode::ARRAY_NEW_FIXED, splitRegIdx, numElem, code);

		if (needsRegular)
		{
			int32_t typeIdx = linearHelper.getRegularObjectIdx();
			encodeInst(WasmS32Opcode::I32_CONST, 0, code);
			encodeInst(WasmGCOpcode::STRUCT_NEW, typeIdx, code);
		}
	}
	else
	{
		llvm::errs() << "Allocating type " << *Ty << "\n";
		llvm::report_fatal_error("Unsupported type in allocation", false);
	}
}

uint32_t CheerpWasmWriter::compileArraySizeGC(WasmBuffer& code, const DynamicAllocInfo & info, bool encode)
{
	Type * t = info.getCastedPointedType();
	uint32_t typeSize = targetData.getTypeAllocSize(t);
	// TODO: is the use of this correct?
	bool useMul = false;

	uint32_t numElem = 1;
	if(const Value* numberOfElements = info.getNumberOfElementsArg())
	{
		if(isa<ConstantInt>(numberOfElements))
			numElem = getIntFromValue(numberOfElements);
		else
		{
			assert(encode);
			compileOperand(code, numberOfElements);
			useMul = true;
		}
	}
	if(!info.sizeIsRuntime())
	{
		uint32_t allocatedSize = getIntFromValue(info.getByteSizeArg());
		numElem *= (allocatedSize + typeSize - 1);
		if (encode)
		{
			encodeInst(WasmS32Opcode::I32_CONST, numElem / typeSize, code);
			if (useMul)
				encodeInst(WasmOpcode::I32_MUL, code);
		}
		else
			return numElem / typeSize;
	}
	else
	{
		assert(encode);
		compileOperand(code, info.getByteSizeArg());
		if (useMul)
			encodeInst(WasmOpcode::I32_MUL, code);
		encodeInst(WasmS32Opcode::I32_CONST, typeSize, code);
		encodeInst(WasmOpcode::I32_DIV_U, code); // TODO: use DIV_S or DIV_U
	}
	assert(encode);
	return -1;
}

bool CheerpWasmWriter::compileInlineInstruction(WasmBuffer& code, const Instruction& I)
{
	errs() << "[compileInlineInstruction] "; I.dump();
	errs() << "[compileInlineInstruction] Instruction's Opcode: " << I.getOpcode() << "\n";
	switch(I.getOpcode())
	{
		case Instruction::Alloca:
		{
			const AllocaInst* ai = cast<AllocaInst>(&I);
			Type* allocaType = ai->getAllocatedType();
			POINTER_KIND kind = PA.getPointerKindAssert(ai);
			const auto* allocaStores = allocaStoresExtractor.getValuesForAlloca(ai);
			errs() << "[compileInlineInstruction] Alloca for type: " << *allocaType << "\n";
			assert(TypeSupport::isTypeGC(allocaType));
			assert(kind != BYTE_LAYOUT);
			assert(kind != RAW && "Allocas to RAW pointers are removed in the AllocaLowering pass");

			bool needsDowncastArray = false;
			if (auto sTy = dyn_cast<StructType>(allocaType))
				needsDowncastArray = sTy->hasDirectBase() && linearHelper.hasDowncastArray(sTy);
			allocateTypeGC(code, allocaType, needsDowncastArray, allocaStores);

			if (kind == REGULAR)
			{
				encodeInst(WasmS32Opcode::I32_CONST, 0, code);
				encodeInst(WasmGCOpcode::STRUCT_NEW, linearHelper.getRegularObjectIdx(), code);
			}
			else if (kind == SPLIT_REGULAR)
			{
				encodeInst(WasmGCOpcode::STRUCT_NEW, linearHelper.getSplitRegularObjectIdx(), code);
			}
			else if (kind != COMPLETE_OBJECT)
				llvm::report_fatal_error("unexpected pointer kind found");
			return false;
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
			else if (t->isVectorTy())
			{
				const Type* et = dyn_cast<VectorType>(t)->getElementType();
				if (et->isFloatTy())
					encodeInst(WasmSIMDOpcode::F32x4_NEG, code);
				else if (et->isDoubleTy())
					encodeInst(WasmSIMDOpcode::F64x2_NEG, code);
			}
			break;
		}
		case Instruction::BitCast:
		{
			if (I.getType()->isVectorTy())
			{
				const BitCastInst& bci = cast<BitCastInst>(I);
				const FixedVectorType* destVecTy = cast<FixedVectorType>(bci.getDestTy());
				if (bci.getSrcTy()->isVectorTy())
				{
					// If we're casting from a vector to a vector, if the number of
					// elements are the same, we don't need to do anything.
					const FixedVectorType* srcVecTy = cast<FixedVectorType>(bci.getSrcTy());
					if (srcVecTy->getNumElements() == destVecTy->getNumElements())
					{
						compileOperand(code, I.getOperand(0));
						break;
					}
					assert(false && "Bitcasting between different vector formats not supported yet");
				}

				// If this is a bitcast from a non-pointer value to a vector, we have to encode it
				// as with a load on a smaller-than-128 bits vector. We load the value into a vector
				// and then apply a shuffle mask.
				const unsigned vecWidth = getVectorBitwidth(destVecTy);
				encodeVectorConstantZero(code);
				compileOperand(code, I.getOperand(0));
				// Encode the correct replace lane with index 0
				if (vecWidth == 64)
					encodeInst(WasmSIMDU32Opcode::I64x2_REPLACE_LANE, 0, code);
				else if (vecWidth == 32)
					encodeInst(WasmSIMDU32Opcode::I32x4_REPLACE_LANE, 0, code);
				else if (vecWidth == 16)
					encodeInst(WasmSIMDU32Opcode::I16x8_REPLACE_LANE, 0, code);
				else
					llvm::report_fatal_error("Incorrect bitwidth for bitcast to vector");
				encodeLoadingShuffle(code, destVecTy);
				break;
			}
			else if (I.getOperand(0)->getType()->isVectorTy())
				assert(false && "Bitcasting from vector to integer not supported yet");
			Value* operand = I.getOperand(0);
			compileOperand(code, operand);
			errs() << "[compileInlineInstruction] Bitcast found\n";
			errs() << "[compileInlineInstruction] instruction: " << cast<BitCastInst>(I) << "\n";
			errs() << "[compileInlineInstruction] destTy: " << *cast<BitCastInst>(I).getDestTy() << "\n";
			errs() << "[compileInlineInstruction] srcTy: " << *cast<BitCastInst>(I).getSrcTy() << "\n";
			if (TypeSupport::isTypeGC(cast<BitCastInst>(I).getDestTy()) || TypeSupport::isTypeGC(cast<BitCastInst>(I).getSrcTy()))
			{
				// TODO: assert that it is a up/down cast?
				assert(TypeSupport::isTypeGC(cast<BitCastInst>(I).getDestTy()) && TypeSupport::isTypeGC(cast<BitCastInst>(I).getSrcTy()) && "Bitcasting from a GC type to a non-GC type is not supported");
				compileRefCast(code, cast<BitCastInst>(I).getDestTy(), COMPLETE_OBJECT);
			}
			if(I.getType()->isIntegerTy())
			{
				uint32_t bitWidth = I.getType()->getIntegerBitWidth();
				if(bitWidth == 32)
				{
					assert(operand->getType()->isFloatTy());
					encodeInst(WasmOpcode::I32_REINTERPRET_F32, code);
				}
				else
				{
					assert(bitWidth == 64);
					assert(operand->getType()->isDoubleTy());
					encodeInst(WasmOpcode::I64_REINTERPRET_F64, code);
				}
			}
			else if(I.getType()->isFloatTy())
			{
				assert(operand->getType()->isIntegerTy(32));
				encodeInst(WasmOpcode::F32_REINTERPRET_I32, code);
			}
			else if(I.getType()->isDoubleTy())
			{
				assert(operand->getType()->isIntegerTy(64));
				encodeInst(WasmOpcode::F64_REINTERPRET_I64, code);
			}
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

			// TODO: add GC load

			encodeLoad(vi.getType(), 0, code, /*signExtend*/false, /*atomic*/false);

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
			errs() << "[compileInlineInstruction] Compiling a Call instruction\n";
			const CallInst& ci = cast<CallInst>(I);
			const Function * calledFunc = ci.getCalledFunction();
			const Value * calledValue = ci.getCalledOperand();
			const FunctionType* fTy = ci.getFunctionType();
			// const FunctionType* fTy = linearHelper.getExpandedFunctionType(calledFunc);
			if (ci.isInlineAsm())
			{
				// TODO: remove is TMP for GC testing
				encodeInst(WasmU32Opcode::GET_LOCAL, 0, code);
				errs() << "[compileInlineInstruction] Is inlineAsm";
				return true;
			}

			// NOTE: If 'useTailCall' the code _must_ use return_call or insert a return
			//       Returns are not otherwise added in such cases
			const bool useTailCall = isTailCall(ci);
			bool skipFirstParam = false;
			if (calledFunc)
			{
				unsigned intrinsicId = calledFunc->getIntrinsicID();
				switch (intrinsicId)
				{
					case Intrinsic::trap:
					{
						assert(false);
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
					case Intrinsic::get_dynamic_area_offset:
					{
						// The stack pointer is the same as the address of the most recent alloca, so 0 offset
						compileUnsignedInteger(code, ConstantInt::get(ci.getType(), 0));
						return false;
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
					{
						if (TypeSupport::isTypeGC(ci.getParamElementType(0)))
							compileDowncastGC(code, &ci);
						else
							compileDowncast(code, &ci);
						if(useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::cheerp_virtualcast:
					{
						errs() << "[compileInlineInstruction] cheerp_downcast or cheerp_virtualcast\n";
						//TODO: find a test case for GC types when a virtualcast happens
						if (TypeSupport::isTypeGC(ci.getParamElementType(0)))
							compileVirtualcastGC(code, &ci);
						else
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
						errs() << "[compileInlineInstruction] cheerp_downcast_current\n";
						if (TypeSupport::isTypeGC(ci.getOperand(0)->getType()))
						{
							// TODO: find a test case for GC types
							assert(false);
							compileCompleteObject(code, ci.getOperand(0));
							StructType* sTy = cast<StructType>(ci.getOperand(0)->getType()->getNonOpaquePointerElementType());
							int32_t typeIdx = linearHelper.getGCTypeIndex(sTy, COMPLETE_OBJECT, true);
							// Retrieve the .o
							encodeInst(WasmGCOpcode::STRUCT_GET, typeIdx, downcastArrayIndices[sTy] + 1, code);
						}
						else
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
						errs() << "[compileInlineInstruction] cheerp_upcast_collapsed\n";
						if (TypeSupport::isTypeGC(ci.getOperand(0)->getType()))
						{
							// TODO: find a test case
							assert(false);
							compilePointerAs(code, ci.getOperand(0), PA.getPointerKindAssert(&ci));
						}
						else
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
						errs() << "[compileInlineInstruction] cheerp_upcast_user\n";
						if(ci.use_empty())
							return true;
						if (TypeSupport::isTypeGC(ci.getOperand(0)->getType()))
						{
							// TODO: find a test case
							assert(false);
							// JS writer does a compileBitCast here
						}
						else
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
					case Intrinsic::cheerp_memory_init:
					{
						compileOperand(code, ci.getOperand(1));
						compileOperand(code, ci.getOperand(2));
						compileOperand(code, ci.getOperand(3));
						llvm::ConstantInt *constInt = cast<llvm::ConstantInt>(ci.getOperand(0));
						// The second immediate for MEMORY_INIT is the memory index.
						// As there is currently no support for multiple memories, this has to be 0.
						encodeInst(WasmFCU32U32Opcode::MEMORY_INIT, constInt->getZExtValue(), 0, code);
						return true;
					}
					case Intrinsic::cheerp_data_drop:
					{
						llvm::ConstantInt *constInt = cast<llvm::ConstantInt>(ci.getOperand(0));
						encodeInst(WasmFCU32Opcode::DATA_DROP, constInt->getZExtValue(), code);
						return true;
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
					case Intrinsic::abs:
					{
						//Implementing ( X >= 0 ) ? X : -X
						//Note that abs takes 2 arguments, the actual value (X) + a flag that dictates
						//what the output should be for X == INT_MIN (=-2^31).
						//Since in one case it's -2^31 and the other in Undefined, we consider both
						//at the same time rendering -2^31 (note that it's surprisingly a negative value)
						auto* operand = ci.op_begin()->get();
						const llvm::Type* type = calledFunc->getReturnType();
						const bool isI64 = type->isIntegerTy(64);

						if (type->isVectorTy())
						{
							const llvm::Type* elementType = cast<VectorType>(type)->getElementType();
							compileOperand(code, operand);
							if (elementType->isIntegerTy(8))
								encodeInst(WasmSIMDOpcode::I8x16_ABS, code);
							else if (elementType->isIntegerTy(16))
								encodeInst(WasmSIMDOpcode::I16x8_ABS, code);
							else if (elementType->isIntegerTy(32))
								encodeInst(WasmSIMDOpcode::I32x4_ABS, code);
							else if (elementType->isIntegerTy(64))
								encodeInst(WasmSIMDOpcode::I64x2_ABS, code);
							else
								assert(false && "Unsupported bit width for integer abs");
							if (useTailCall)
							{
								encodeInst(WasmOpcode::RETURN, code);
								return true;
							}
							return false;
						}

						//Put on the stack the operand
						compileOperand(code, operand);

						//Put on the stack the negation of the operand
						compileTypedZero(code, type);
						compileSignedInteger(code, operand, /*forComparison*/false);
						encodeInst(isI64 ? WasmOpcode::I64_SUB : WasmOpcode::I32_SUB, code);

						//Do the comparison between operand and 0
						compileSignedInteger(code, operand, /*forComparison*/true);
						compileTypedZero(code, type);
						encodePredicate(type, CmpInst::ICMP_SGE, code);

						//Select
						encodeInst(WasmOpcode::SELECT, code);

						if(useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
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
						// check for WasmGC allocation
						Type* baseType = I.getOperand(0)->getType();
						if (TypeSupport::isTypeGC(baseType))
						{
							DynamicAllocInfo da(&cast<CallBase>(ci), &targetData, false);
							compileAllocationGC(code, da);
							// POINTER_KIND kind = PA.getPointerKind(&I);
							// bool needsRegular = kind==REGULAR && PA.getConstantOffsetForPointer(&I);
							errs() << "[cheerp_allocate_array] allocating for instruction: "; I.dump();
							errs() << "[cheerp_allocate_array] baseType: "; baseType->dump();
							errs() << "[cheerp_allocate_array] Operand(0): "; I.getOperand(0)->dump();
							// errs() << "[cheerp_allocate_array] ptrKind of Instruction: "; printPtrKind(kind);
							// allocateGC(code, baseType, kind, needsRegular, compileArraySizeGC(da));
							errs() << "[cheerp_allocate_array] DONE\n";
							return false;
						}
						skipFirstParam = true;
						calledFunc = module.getFunction("malloc");
						if (!calledFunc)
							llvm::report_fatal_error("missing malloc definition");
						break;
					}
					case Intrinsic::cheerp_reallocate:
					{
						Type* baseType = I.getOperand(0)->getType();
						// TODO: find a way to check if the realloc is called on GC arrays of non-GC types
						if (TypeSupport::isTypeGC(baseType))
						{
							DynamicAllocInfo da(&cast<CallBase>(ci), &targetData, false);
							compileAllocationGC(code, da);
							break;
						}
						calledFunc = module.getFunction("realloc");
						if (!calledFunc)
							llvm::report_fatal_error("missing realloc definition");
						break;
					}
					case Intrinsic::cheerp_deallocate:
					{
						if (TypeSupport::isTypeGC(I.getOperand(0)->getType()))
							return false;
						calledFunc = module.getFunction("free");
						if (!calledFunc)
							llvm::report_fatal_error("missing free definition");
						break;
					}
					case Intrinsic::eh_typeid_for:
					{
						auto& local = landingPadTable.getLocalTypeIdMap(currentFun);
						int id = local.getTypeIdFor(ci.getOperand(0), linearHelper);
						encodeInst(WasmS32Opcode::I32_CONST, id, code);
						return false;
					}
					case Intrinsic::wasm_shuffle:
					{
						compileOperand(code, ci.getOperand(0));
						compileOperand(code, ci.getOperand(1));
						encodeInst(WasmSIMDOpcode::I8x16_SHUFFLE, code);
						assert(ci.getNumOperands() > 17);
						for (int i = 2; i < 18; i++)
						{
							const Value* v = ci.getOperand(i);
							assert(isa<ConstantInt>(v));
							const ConstantInt *num = dyn_cast<ConstantInt>(v);
							code << static_cast<char>(num->getZExtValue());
						}
						if (useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::wasm_swizzle:
					{
						compileOperand(code, ci.getOperand(0));
						compileOperand(code, ci.getOperand(1));
						encodeInst(WasmSIMDOpcode::I8x16_SWIZZLE, code);
						if (useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::wasm_anytrue:
					{
						compileOperand(code, ci.getOperand(0));
						encodeInst(WasmSIMDOpcode::V128_ANY_TRUE, code);
						if (useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::wasm_bitselect:
					{
						compileOperand(code, ci.getOperand(0));
						compileOperand(code, ci.getOperand(1));
						compileOperand(code, ci.getOperand(2));
						encodeInst(WasmSIMDOpcode::V128_BITSELECT, code);
						if (useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::minimum:
					{
						assert(ci.getOperand(0)->getType()->isVectorTy());
						Type* et = cast<VectorType>(ci.getOperand(0)->getType())->getElementType();
						assert(et->isFloatTy() || et->isDoubleTy());
						compileOperand(code, ci.getOperand(0));
						compileOperand(code, ci.getOperand(1));
						if (et->isFloatTy())
							encodeInst(WasmSIMDOpcode::F32x4_MIN, code);
						else
							encodeInst(WasmSIMDOpcode::F64x2_MIN, code);
						if (useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::maximum:
					{
						assert(ci.getOperand(0)->getType()->isVectorTy());
						Type *et = cast<VectorType>(ci.getOperand(0)->getType())->getElementType();
						assert(et->isFloatTy() || et->isDoubleTy());
						compileOperand(code, ci.getOperand(0));
						compileOperand(code, ci.getOperand(1));
						if (et->isFloatTy())
							encodeInst(WasmSIMDOpcode::F32x4_MAX, code);
						else
							encodeInst(WasmSIMDOpcode::F64x2_MAX, code);
						if (useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::wasm_avgr_unsigned:
					{
						assert(ci.getOperand(0)->getType()->isVectorTy());
						Type *et = cast<VectorType>(ci.getOperand(0)->getType())->getElementType();
						assert(et->isIntegerTy(8) || et->isIntegerTy(16));
						compileOperand(code, ci.getOperand(0));
						compileOperand(code, ci.getOperand(1));
						if (et->isIntegerTy(8))
							encodeInst(WasmSIMDOpcode::I8x16_AVGR_U, code);
						else
							encodeInst(WasmSIMDOpcode::I16x8_AVGR_U, code);
						if (useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::cheerp_wasm_shl:
					{
						assert(ci.getOperand(0)->getType()->isVectorTy());
						Type* et = cast<VectorType>(ci.getOperand(0)->getType())->getElementType();
						compileOperand(code, ci.getOperand(0));
						compileOperand(code, ci.getOperand(1));
						if (et->isIntegerTy(8))
							encodeInst(WasmSIMDOpcode::I8x16_SHL, code);
						else if (et->isIntegerTy(16))
							encodeInst(WasmSIMDOpcode::I16x8_SHL, code);
						else if (et->isIntegerTy(32))
							encodeInst(WasmSIMDOpcode::I32x4_SHL, code);
						else if (et->isIntegerTy(64))
							encodeInst(WasmSIMDOpcode::I64x2_SHL, code);
						else
							llvm::report_fatal_error("Unsupported bitwidth for vector shl");
						if (useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::cheerp_wasm_shr_s:
					{
						Type* et = cast<VectorType>(ci.getOperand(0)->getType())->getElementType();
						compileOperand(code, ci.getOperand(0));
						compileOperand(code, ci.getOperand(1));
						if (et->isIntegerTy(8))
							encodeInst(WasmSIMDOpcode::I8x16_SHR_S, code);
						else if (et->isIntegerTy(16))
							encodeInst(WasmSIMDOpcode::I16x8_SHR_S, code);
						else if (et->isIntegerTy(32))
							encodeInst(WasmSIMDOpcode::I32x4_SHR_S, code);
						else if (et->isIntegerTy(64))
							encodeInst(WasmSIMDOpcode::I64x2_SHR_S, code);
						else
							llvm::report_fatal_error("Unsupported bitwidth for vector shr_s");
						if (useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::cheerp_wasm_shr_u:
					{
						Type* et = cast<VectorType>(ci.getOperand(0)->getType())->getElementType();
						compileOperand(code, ci.getOperand(0));
						compileOperand(code, ci.getOperand(1));
						if (et->isIntegerTy(8))
							encodeInst(WasmSIMDOpcode::I8x16_SHR_U, code);
						else if (et->isIntegerTy(16))
							encodeInst(WasmSIMDOpcode::I16x8_SHR_U, code);
						else if (et->isIntegerTy(32))
							encodeInst(WasmSIMDOpcode::I32x4_SHR_U, code);
						else if (et->isIntegerTy(64))
							encodeInst(WasmSIMDOpcode::I64x2_SHR_U, code);
						else
							llvm::report_fatal_error("Unsupported bitwidth for vector shr_u");
						if (useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::cheerp_wasm_splat:
					{
						Type* ty = ci.getOperand(0)->getType();
						compileOperand(code, ci.getOperand(0));
						if (ty->isIntegerTy(8))
							encodeInst(WasmSIMDOpcode::I8x16_SPLAT, code);
						else if (ty->isIntegerTy(16))
							encodeInst(WasmSIMDOpcode::I16x8_SPLAT, code);
						else if (ty->isIntegerTy(32) || ty->isPointerTy())
							encodeInst(WasmSIMDOpcode::I32x4_SPLAT, code);
						else if (ty->isIntegerTy(64))
							encodeInst(WasmSIMDOpcode::I64x2_SPLAT, code);
						else if (ty->isFloatTy())
							encodeInst(WasmSIMDOpcode::F32x4_SPLAT, code);
						else if (ty->isDoubleTy())
							encodeInst(WasmSIMDOpcode::F64x2_SPLAT, code);
						else
							llvm::report_fatal_error("Unsupported type for splat");
						if (useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::umax:
					case Intrinsic::umin:
					case Intrinsic::smax:
					case Intrinsic::smin:
					{
						const FixedVectorType* vecType = cast<FixedVectorType>(ci.getOperand(0)->getType());
						Type* ty = vecType->getElementType();
						// These intrinsics are not supported for 64-bit integers.
						assert(!ty->isIntegerTy(64));
						compileOperand(code, ci.getOperand(0));
						compileOperand(code, ci.getOperand(1));
						switch (intrinsicId)
						{
#define MINMAX(Ty, name) \
							case Intrinsic::Ty: \
								if (ty->isIntegerTy(8)) \
									encodeInst(WasmSIMDOpcode::I8x16_##name, code); \
								else if (ty->isIntegerTy(16)) \
									encodeInst(WasmSIMDOpcode::I16x8_##name, code);\
								else \
									encodeInst(WasmSIMDOpcode::I32x4_##name, code); \
								break;
							MINMAX(umax, MAX_U);
							MINMAX(umin, MIN_U);
							MINMAX(smax, MAX_S);
							MINMAX(smin, MIN_S);
#undef MINMAX
							default:
								llvm::report_fatal_error("This should be unreachable");
						}
						if (useTailCall)
						{
							encodeInst(WasmOpcode::RETURN, code);
							return true;
						}
						return false;
					}
					case Intrinsic::ctlz:
					case Intrinsic::cttz:
					case Intrinsic::ctpop:
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
			const unsigned int endParam = fTy->getNumParams() - TypedBuiltinInstr::numExtraParameters(calledFunc);
			const unsigned int startParam = skipFirstParam ? 1 : 0;
			errs() << "[compileInlineInstruction] Calling function using callInstruction: " << ci << "\n";
			compileMethodArgs(code, ci.op_begin() + startParam, ci.op_begin() + endParam, ci);
			if (calledFunc)
			{
				if (ci.getOperand(0)->getType()->isVectorTy())
				{
					Intrinsic::ID id = calledFunc->getIntrinsicID();
					if (id == Intrinsic::ctpop)
					{
						encodeInst(WasmSIMDOpcode::I8x16_POPCNT, code);
						return false;
					}
					else if (id == Intrinsic::fabs)
					{
						Type* et = cast<VectorType>(ci.getOperand(0)->getType())->getElementType();
						assert(et->isFloatTy() || et->isDoubleTy());
						if (et->isFloatTy())
							encodeInst(WasmSIMDOpcode::F32x4_ABS, code);
						else
							encodeInst(WasmSIMDOpcode::F64x2_ABS, code);
						return false;
					}
					else if (id == Intrinsic::sqrt)
					{
						Type* et = cast<FixedVectorType>(ci.getOperand(0)->getType())->getElementType();
						assert(et->isFloatTy() || et->isDoubleTy());
						if (et->isFloatTy())
							encodeInst(WasmSIMDOpcode::F32x4_SQRT, code);
						else
							encodeInst(WasmSIMDOpcode::F64x2_SQRT, code);
						return false;
					}
				}
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
						assert(false);
						encodeInst(WasmOpcode::UNREACHABLE, code);
						// Make sure that we leave a value on the stack anyway (old Edge's validation get unhappy otherwise)
						if(!fTy->getReturnType()->isVoidTy())
							compileTypedZero(code, fTy->getReturnType());
					}
				}
				else
				{
					llvm::errs() << "warning: Undefined function " << calledFunc->getName() << " called\n";
					// assert(false);
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
					assert(false);
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
		case Instruction::ExtractValue:
		{
			const auto& EV = cast<ExtractValueInst>(I);
			assert(EV.getNumIndices() == 1);
			compileAggregateElem(code, EV.getAggregateOperand(), EV.getIndices()[0]);
			break;
		}
		case Instruction::InsertValue:
		{
			const auto& IV = cast<InsertValueInst>(I);
			assert(IV.getNumIndices() == 1);
			uint32_t newIdx = IV.getIndices()[0];
			for(const auto& ie: getInstElems(&IV, PA))
			{
				if(ie.structIdx == newIdx)
				{
					compileOperand(code, IV.getOperand(1));
				}
				else
				{
					compileAggregateElem(code, IV.getAggregateOperand(), ie.structIdx);
				}
			}
			break;
		}
		case Instruction::Load:
		{
			const LoadInst& li = cast<LoadInst>(I);
			if(GlobalVariable* ptrGV = dyn_cast<GlobalVariable>(li.getOperand(0)))
			{
				errs() << "[compileInlineInstruction] Found a global variable for a load instruction: " << *ptrGV << "\n"; 
				auto it = globalizedGlobalsIDs.find(ptrGV);
				if(it != globalizedGlobalsIDs.end())
				{
					// We can encode this as a get_global
					encodeInst(WasmU32Opcode::GET_GLOBAL, it->second, code);
					const Type* loadTy = li.getType();
					if (TypeSupport::isTypeGC(loadTy))
					{
						POINTER_KIND kind = COMPLETE_OBJECT;
						if (loadTy->isPointerTy())
							kind = PA.getPointerKind(&li);
						errs() << "[compileInlineInstruction] Casting global " << *loadTy << " to ptr kind: "; printPtrKind(kind);
						compileRefCast(code, loadTy, kind);
					}
					break;
				}
				errs() << "[compileInlineInstruction] Could not find global variable id\n";
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
			compileStore(code, si);
			break;
		}
		case Instruction::Switch:
			break;
		case Instruction::Trunc:
		{
			compileOperand(code, I.getOperand(0));
			if (I.getOperand(0)->getType()->isIntegerTy(64))
				encodeInst(WasmOpcode::I32_WRAP_I64, code);
			else if (I.getOperand(0)->getType()->isVectorTy())
				encodeVectorTruncation(code, I);
			break;
		}
		case Instruction::Ret:
		{
			errs() << "[compileInlineInstruction] compiling return instruction\n";
			const ReturnInst& ri = cast<ReturnInst>(I);
			Value* retVal = ri.getReturnValue();
			if(retVal)
			{
				errs() << "[compileInlineInstruction] retVal: " << *retVal << "\n";
				// NOTE: If the retValue is inlineable we must render it here
				//       If 'isReturnPartOfTailCall' return true then retVal must be a CallInst
				//       so blindly casting it to Instruction is safe
				
				// TODO: Can the return of a special pointer kind be inlineable? 
				if(isReturnPartOfTailCall(ri) && !isInlineable(*cast<Instruction>(retVal)))
					break;

				// errs() << "[compileInlineInstruction] return instruction is not inlineable\n";
				if (TypeSupport::isTypeGC(retVal->getType()))
				{
					// TODO: Can the return value be a non-pointer?
					assert(retVal->getType()->isPointerTy());
					POINTER_KIND kind = PA.getPointerKindForReturn(ri.getParent()->getParent());
					if (kind == SPLIT_REGULAR)
					{
						compilePointerOffset(code, retVal);
						encodeInst(WasmU32Opcode::SET_GLOBAL, oSlotGlobal, code);
					}
					assert(kind != REGULAR);
					if (kind == SPLIT_REGULAR)
						compilePointerBase(code, retVal);
					else
						compilePointerAs(code, retVal, kind);
				}
				else
					compileOperand(code, I.getOperand(0));
			}
			break;
		}
		case Instruction::Select:
		{
			const SelectInst& si = cast<SelectInst>(I);
			auto* Ty = si.getType();
			auto* STy = dyn_cast<StructType>(Ty);
			for(const auto& ie: getInstElems(&si, PA))
			{
				if(STy)
				{
					compileAggregateElem(code, si.getTrueValue(), ie.structIdx);
					compileAggregateElem(code, si.getFalseValue(), ie.structIdx);
				}
				else
				{
					compileOperand(code, si.getTrueValue());
					compileOperand(code, si.getFalseValue());
				}
				errs() << "Calling compileCondition: select false\n";
				compileCondition(code, si.getCondition(), /*booleanInvert*/false);
				if (si.getCondition()->getType()->isVectorTy())
					encodeInst(WasmSIMDOpcode::V128_BITSELECT, code);
				else if (TypeSupport::isTypeGC(Ty))
				{
					encodeInst(WasmOpcode::SELECT_VAL, code);
					// encode the vector for the result type, length must be 1
					encodeULEB128(1, code);
					// encode the result type
					encodeULEB128(0x63, code);
					encodeSLEB128(linearHelper.getGCTypeIndex(Ty, COMPLETE_OBJECT), code);
				}
				else
					encodeInst(WasmOpcode::SELECT, code);
				errs() << "[SELECT] " << si << "\n";
			}
			break;
		}
		case Instruction::SExt:
		{
			const Value* op = I.getOperand(0);
			compileOperand(code, op);
			if(!I.getOperand(0)->getType()->isVectorTy() && !isSignedLoad(op))
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
			assert(I.getOperand(0)->getType()->isIntegerTy() || I.getOperand(0)->getType()->isVectorTy());
			compileOperand(code, I.getOperand(0));
			if (I.getOperand(0)->getType()->isVectorTy())
			{
				const FixedVectorType* opType = cast<FixedVectorType>(I.getOperand(0)->getType());
				const FixedVectorType* resultType = cast<FixedVectorType>(I.getType());
				assert(opType->getElementType()->isIntegerTy(32));
				if (resultType->getElementType()->isFloatTy())
					encodeInst(WasmSIMDOpcode::F32x4_CONVERT_I32x4_S, code);
				else if (resultType->getElementType()->isDoubleTy())
					encodeInst(WasmSIMDOpcode::F64x2_CONVERT_LOW_I32x4_S, code);
				else
				{
					llvm::errs() << I << "\n";
					llvm::report_fatal_error("Instruction not supported");
				}
				break;
			}
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
			assert(I.getOperand(0)->getType()->isIntegerTy() || I.getOperand(0)->getType()->isVectorTy());
			compileOperand(code, I.getOperand(0));
			if (I.getOperand(0)->getType()->isVectorTy())
			{
				const FixedVectorType* opType = cast<FixedVectorType>(I.getOperand(0)->getType());
				const FixedVectorType* resultType = cast<FixedVectorType>(I.getType());
				assert(opType->getElementType()->isIntegerTy(32));
				if (resultType->getElementType()->isFloatTy())
					encodeInst(WasmSIMDOpcode::F32x4_CONVERT_I32x4_U, code);
				else if (resultType->getElementType()->isDoubleTy())
					encodeInst(WasmSIMDOpcode::F64x2_CONVERT_LOW_I32x4_U, code);
				else
				{
					llvm::errs() << I << "\n";
					llvm::report_fatal_error("Instruction not supported");
				}
				break;
			}
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
			if (I.getType()->isVectorTy())
			{
				const FixedVectorType* srcVecType = cast<FixedVectorType>(I.getOperand(0)->getType());
				const FixedVectorType* destVecType = cast<FixedVectorType>(I.getType());
				assert(destVecType->getElementType()->isFloatTy());
				assert(srcVecType->getElementType()->isDoubleTy());
				compileOperand(code, I.getOperand(0));
				encodeInst(WasmSIMDOpcode::F32x4_DEMOTE_F64x2_ZERO, code);
				encodeLoadingShuffle(code, destVecType);
				break;
			}
			assert(I.getType()->isFloatTy());
			assert(I.getOperand(0)->getType()->isDoubleTy());
			compileOperand(code, I.getOperand(0));
			encodeInst(WasmOpcode::F32_DEMOTE_F64, code);
			break;
		}
		case Instruction::FPExt:
		{
			if (I.getType()->isVectorTy())
			{
				const FixedVectorType* srcVecType = cast<FixedVectorType>(I.getOperand(0)->getType());
				const FixedVectorType* destVecType = cast<FixedVectorType>(I.getType());
				assert(srcVecType->getElementType()->isFloatTy());
				assert(destVecType->getElementType()->isDoubleTy());
				compileOperand(code, I.getOperand(0));
				encodeStoringShuffle(code, srcVecType);
				encodeInst(WasmSIMDOpcode::F64x2_PROMOTE_LOW_F32x4, code);
				break;
			}
			assert(I.getType()->isDoubleTy());
			assert(I.getOperand(0)->getType()->isFloatTy());
			compileOperand(code, I.getOperand(0));
			encodeInst(WasmOpcode::F64_PROMOTE_F32, code);
			break;
		}
		case Instruction::ZExt:
		{
			if (I.getType()->isVectorTy())
			{
				compileOperand(code, I.getOperand(0));
				break;
			}
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
		case Instruction::ExtractElement:
		{
			encodeExtractLane(code, cast<ExtractElementInst>(I));
			break;
		}
		case Instruction::InsertElement:
		{
			encodeReplaceLane(code, cast<InsertElementInst>(I));
			break;
		}
		case Instruction::ShuffleVector:
		{
			const ShuffleVectorInst& svi = cast<ShuffleVectorInst>(I);
			compileOperand(code, svi.getOperand(0));
			compileOperand(code, svi.getOperand(1));
			encodeInst(WasmSIMDOpcode::I8x16_SHUFFLE, code);
			const FixedVectorType* vecType = cast<FixedVectorType>(svi.getOperand(0)->getType());
			int numElements = vecType->getNumElements();
			int scaleFactor = 16 / numElements;
			for (int i = 0; i < numElements; i++)
			{
				for (int j = 0; j < scaleFactor; j++)
				{
					int result = svi.getMaskValue(i) * scaleFactor + j;
					if (svi.getMaskValue(i) == llvm::UndefMaskElem)
						result = 0;
					assert(result < 32 && result >= 0);
					code << static_cast<char>(result);
				}
			}
			break;
		}
		case Instruction::AtomicRMW:
		{
			const AtomicRMWInst& ai = cast<AtomicRMWInst>(I);
			const Type* opType = ai.getOperand(1)->getType();
			assert(opType->isIntegerTy());
			const Value* ptrOp = ai.getPointerOperand();
			const Value* valOp = ai.getValOperand();
			uint32_t offset = compileLoadStorePointer(code, ptrOp);
			compileOperand(code, valOp);
			switch (ai.getOperation())
			{
#define ATOMICBINOP(Ty, name) \
				case AtomicRMWInst::Ty: \
				{ \
					if (opType->isIntegerTy(64)) \
						encodeInst(WasmThreadsU32U32Opcode::I64_ATOMIC_RMW_##name, 0x3, offset, code); \
					else if (opType->isIntegerTy(32)) \
						encodeInst(WasmThreadsU32U32Opcode::I32_ATOMIC_RMW_##name, 0x2, offset, code); \
					else if (opType->isIntegerTy(16)) \
						encodeInst(WasmThreadsU32U32Opcode::I32_ATOMIC_RMW16_##name##_U, 0x1, offset, code); \
					else if (opType->getPrimitiveSizeInBits() <= 8) \
						encodeInst(WasmThreadsU32U32Opcode::I32_ATOMIC_RMW8_##name##_U, 0x0, offset, code); \
					else \
						llvm::report_fatal_error("Unknown bitwidth for AtomicRMW inst"); \
					break; \
				}
				ATOMICBINOP( Add,  ADD)
				ATOMICBINOP( Sub,  SUB)
				ATOMICBINOP( And,  AND)
				ATOMICBINOP(  Or,   OR)
				ATOMICBINOP( Xor,  XOR)
				ATOMICBINOP(Xchg, XCHG)
#undef ATOMICBINOP
				default:
				{
					llvm::report_fatal_error("Atomic opcode not supported");
				}
			}
			break;
		}
		case Instruction::AtomicCmpXchg:
		{
			const AtomicCmpXchgInst& ai = cast<AtomicCmpXchgInst>(I);
			const Value* ptrOp = ai.getPointerOperand();
			const Value* compareOp = ai.getCompareOperand();
			const Value* newValOp = ai.getNewValOperand();
			const Type* t = compareOp->getType();
			uint32_t offset = compileLoadStorePointer(code, ptrOp);
			// We use compileOperand on the compareOp twice, but this is safe because
			// the compareOp of a cmpxchg instruction will never be inlined.
			compileOperand(code, compareOp);
			compileOperand(code, newValOp);
			if (t->isIntegerTy(8))
				encodeInst(WasmThreadsU32U32Opcode::I32_ATOMIC_RMW8_CMPXCHG_U, 0x0, offset, code);
			else if (t->isIntegerTy(16))
				encodeInst(WasmThreadsU32U32Opcode::I32_ATOMIC_RMW16_CMPXCHG_U, 0x1, offset, code);
			else if (t->isIntegerTy(32))
				encodeInst(WasmThreadsU32U32Opcode::I32_ATOMIC_RMW_CMPXCHG, 0x2, offset, code);
			else if (t->isIntegerTy(64))
				encodeInst(WasmThreadsU32U32Opcode::I64_ATOMIC_RMW_CMPXCHG, 0x3, offset, code);
			else
				llvm::report_fatal_error("Atomic cmpxchg only allowed on integers");
			// Do not duplicate the result and render the comparison if this instruction has no uses.
			// We do however have to push a garbage constant onto the stack, because
			// compileInstructionAndSet will drop 2 times (since there are 2 registers for this).
			if (I.use_empty())
			{
				encodeInst(WasmS32Opcode::I32_CONST, 0, code);
				break;
			}
			// Now compile the second part of this two-register operation, the comparison between
			// the original (loaded) value and the comparison value.
			uint32_t idx = registerize.getRegisterId(&ai, 0, edgeContext);
			uint32_t localId = localMap.at(idx);
			encodeInst(WasmU32Opcode::TEE_LOCAL, localId, code);
			encodeInst(WasmU32Opcode::GET_LOCAL, localId, code);
			compileOperand(code, compareOp);
			if (t->isIntegerTy(64))
				encodeInst(WasmOpcode::I64_EQ, code);
			else
				encodeInst(WasmOpcode::I32_EQ, code);
			break;
		}
		case Instruction::Fence:
		{
			// The FENCE opcode currently requires an immediate set to 0.
			encodeInst(WasmThreadsU32Opcode::ATOMIC_FENCE, 0, code);
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
	errs() << "[compileInstructionAndSet] Compiling instruction: " << I << "\n";
	if (compiled.count(&I) || I.getParent() != currentBB)
	{
		if (compiled.count(&I))
			errs() << "[compileInstructionAndSet] already compiled, skipping\n";
		else
			errs() << "[compileInstructionAndSet] not inside current BasicBlock, skipping\n"; 
		return;
	}
	if (isa<PHINode>(&I) || isInlineable(I))
	{
		if (isa<PHINode>(&I))
			errs() << "[compileInstructionAndSet] is a PHI node, skipping\n";
		else
			errs() << "[compileInstructionAndSet] is inlineable, skipping\n";
		return;
	}
	if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(&I))
	{
		//Skip some kind of intrinsics
		if(II->getIntrinsicID()==Intrinsic::lifetime_start ||
			II->getIntrinsicID()==Intrinsic::lifetime_end ||
			II->getIntrinsicID()==Intrinsic::invariant_start ||
			II->getIntrinsicID()==Intrinsic::invariant_end ||
			II->getIntrinsicID()==Intrinsic::dbg_declare ||
			II->getIntrinsicID()==Intrinsic::dbg_value ||
			II->getIntrinsicID()==Intrinsic::dbg_label ||
			II->getIntrinsicID()==Intrinsic::assume ||
			II->getIntrinsicID()==Intrinsic::experimental_noalias_scope_decl)
		{
			return;
		}
	}
	errs() << "\n[compileInstructionAndSet] START\n";

	const bool needsSubStack = teeLocals.needsSubStack(code);

	if (needsSubStack)
		teeLocals.addIndentation(code);

	auto lastUsedCandidate = teeLocals.lastUsed();

	flushMemoryDependencies(code, I);

	assert(compiled.count(&I) == 0);
	compiled.insert(&I);
	errs() << "[compileInstructionAndSet] compiling instruction: " << I << "\n";
	const bool ret = compileInstruction(code, I);

	flushSetLocalDependencies(code, I);

	teeLocals.removeConsumed(lastUsedCandidate);

	if (needsSubStack)
		teeLocals.decreaseIndentation(code, /*performCheck*/false);

	if(!ret && !I.getType()->isVoidTy())
	{
		bool splitRegReturn = I.getType()->isPointerTy() && PA.getPointerKind(&I) == SPLIT_REGULAR;
		SmallVector<InstElem, 2> elems(getInstElems(&I, PA));
		for(auto it = elems.rbegin(); it != elems.rend(); ++it)
		{
			if(I.use_empty()) {
				encodeInst(WasmOpcode::DROP, code);
			} else {
				// We check if there is a call/Invoke instruction and if it returns a split regular.
				// If then the register we're trying to store into is an integer,
				// we are trying to store the offset so we retrieve the oSlot global.
				Registerize::REGISTER_KIND regKind = registerize.getRegKindFromInstElem(*it, false, &PA);
				bool isCallOrInvoke = (I.getOpcode() == Instruction::Call || I.getOpcode() == Instruction::Invoke);
				bool useOSlot = (splitRegReturn && isCallOrInvoke && regKind == Registerize::REGISTER_KIND::INTEGER);
				uint32_t reg = registerize.getRegisterId(&I, it->totalIdx, edgeContext);
				uint32_t local = localMap.at(reg);
				// TODO: figure out how to deal with tee locals and aggregates
				if(!I.getType()->isStructTy() && !useOSlot)
					teeLocals.addCandidate(&I, /*isInstructionAssigment*/true, local, code.tell());

				// Retrieve the oSlot so we can set it inside the local
				if (useOSlot)
					encodeInst(WasmU32Opcode::GET_GLOBAL, oSlotGlobal, code);

				encodeInst(WasmU32Opcode::SET_LOCAL, local, code);
			}
		}
	}
	teeLocals.instructionStart(code);
	errs() << "[compileInstructionAndSet] done compiling instruction: " << I << "\n";
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

		const uint32_t reg = registerize.getRegisterId(phiHandledAsResult, 0, EdgeContext::emptyContext());
		const uint32_t local = localMap.at(reg);
		teeLocals.addCandidate(phiHandledAsResult, /*isInstructionAssigment*/true, local, code.tell());
		errs() << "[compileBB] Setting local: " << local << "\n";
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
					if (registerize.hasRegisters(next))
					{
						const uint32_t ID = registerize.getRegisterId(next, 0, edgeContext);
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
		if (registerize.hasRegisters(&*I))
		{
			assert(!isInlineable(*I));

			const uint32_t ID = registerize.getRegisterId(&*I, 0, edgeContext);

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
		{
			if (currentFun->getName().find("test") != std::string::npos)
				errs() << "\n\n\nNew Instruction: ", I->dump();
			compileInstructionAndSet(code, *I);
		}
	}
	for (const llvm::Instruction* I : deferred)
	{
		if (currentFun->getName().find("test") != std::string::npos)
			errs() << "\n\n\nNew Instruction: ", I->dump();
		compileInstructionAndSet(code, *I);
	}
}

void CheerpWasmWriter::compileAccessToElement(WasmBuffer& code, Type* tp, ArrayRef< const Value* > indices, bool compileLastWrapperArray, const bool compileFullAccess)
{
	errs() << "[compileAccessToElement] START\n";
	// If compileFullAccess is set to false the final element we want to access is used inside a load or a store.
	// The last reference and offset are left the stack, the access is done through a GET or a SET within the load or store function.
	for(uint32_t i = 0; i < indices.size(); i++)
	{
		errs() << "[compileAccessToElement] compiling indice: " << *indices[i] << "\n";
		assert(!TypeSupport::hasByteLayout(tp));
		if(StructType* st = dyn_cast<StructType>(tp))
		{
			assert(isa<ConstantInt>(indices[i]));
			const APInt& index = cast<Constant>(indices[i])->getUniqueInteger();

			uint32_t structIndex = linearHelper.getGCTypeIndex(tp, COMPLETE_OBJECT);
			const Type* elemTy = st->getStructElementType(index.getLimitedValue());
			const bool hasDowncastArray = linearHelper.hasDowncastArray(st);
			POINTER_KIND elemPtrKind = COMPLETE_OBJECT;
			uint64_t elemIdx = index.getLimitedValue();

			elemIdx = getExpandedStructElemIdx(st, elemIdx);
			if (!compileFullAccess && i == indices.size()-1)
			{
				// If we should leave the last reference on the stack but
				// it is a wrapper array, load the array and encode the offset (0)
				if (types.useWrapperArrayForMember(PA, st, index.getLimitedValue()))
				{
					encodeInst(WasmGCOpcode::STRUCT_GET, structIndex, elemIdx, code);
					compileRefCast(code, elemTy, SPLIT_REGULAR);
					encodeInst(WasmS32Opcode::I32_CONST, 0, code);
				}
				return ;
			}

			errs() << "[compileAccessToElement] STRUCT_GET on index: " << elemIdx << "\n";
			errs() << "[compileAccessToElement] accessing struct type: " << *tp << " of index: " << structIndex << " compiling as COMPLETE OBJECT\n";
			encodeInst(WasmGCOpcode::STRUCT_GET, structIndex, elemIdx, code);
			if (elemTy->isPointerTy())
			{
				TypeAndIndex b = {st, (uint32_t)index.getLimitedValue(), TypeAndIndex::STRUCT_MEMBER};
				elemPtrKind = PA.getPointerKindForMemberPointer(b);
			}

			if((i!=indices.size()-1 || compileLastWrapperArray) && types.useWrapperArrayForMember(PA, st, index.getLimitedValue()))
			{
				compileRefCast(code, elemTy, SPLIT_REGULAR);
				int32_t typeIdx = linearHelper.getGCTypeIndex(elemTy, SPLIT_REGULAR);
				errs() << "[compileAccessToElement] Compiling wrapper array: elemTy: " << *elemTy << "\n";
				errs() << "[compileAccessToElement] ptrKind of element (non wrapper): "; printPtrKind(elemPtrKind);

				encodeInst(WasmS32Opcode::I32_CONST, 0, code);
				encodeInst(WasmGCOpcode::ARRAY_GET, typeIdx, code);
				errs() << "[compileAccessToElement] ARRAY_GET on type index: " << typeIdx << " for pointer type: "; printPtrKind(SPLIT_REGULAR);
			}
			// TODO: is this the correct compileRefCast if we've used a wrapper array?
			compileRefCast(code, elemTy, elemPtrKind);
			tp = st->getElementType(index.getZExtValue());
		}
		else if(const ArrayType* at = dyn_cast<ArrayType>(tp))
		{
			compileOperand(code, indices[i]);

			// We still compile the operand so the offset is on the stack and we can decide to to a load/store later
			if (!compileFullAccess && i == indices.size()-1)
				return ;

			errs() << "[compileAccessToElement] ARRAY_GET on an array type\n";
			uint32_t arrayIndex = linearHelper.getGCTypeIndex(tp, COMPLETE_OBJECT);
			errs() << "[compileAccessToElement] ARRAY_GET on type index: " << arrayIndex << " for pointer type: "; printPtrKind(COMPLETE_OBJECT);
			encodeInst(WasmGCOpcode::ARRAY_GET, arrayIndex, code);
			compileRefCast(code, at->getArrayElementType(), COMPLETE_OBJECT);
			tp = at->getElementType();
		}
		else
		{
			llvm::errs() << "Unexpected type during GEP access " << *tp<< "\n";
			llvm::report_fatal_error("Unsupported code found, please report a bug", false);
		}
	}
}

void CheerpWasmWriter::compileGEPGC(WasmBuffer& code, const User* gep_inst, POINTER_KIND kind, const bool compileFullAccess)
{
	assert(kind != RAW);
	assert(kind != BYTE_LAYOUT);
	errs() << "[compileGEPGC] START\n";
	errs() << "[compileGEPGC] inst: " << *gep_inst << "\n";
	errs() << "[compileGEPGC] kind: "; printPtrKind(kind);
	errs() << "[compileGEPGC] kind from gep_inst: "; printPtrKind(PA.getPointerKind(gep_inst));
	SmallVector< const Value*, 8 > indices(std::next(gep_inst->op_begin()), gep_inst->op_end());
	Type* baseType = cast<GetElementPtrInst>(gep_inst)->getSourceElementType();
	errs() << "[compileGEPGC] baseType: "; baseType->dump();

	StructType* containerStructType = dyn_cast<StructType>(GetElementPtrInst::getIndexedType(baseType,
			makeArrayRef(const_cast<Value* const*>(indices.begin()),
				     const_cast<Value* const*>(indices.end() - 1))));
	if (containerStructType && indices.size() > 1)
	{
		assert(isa<ConstantInt>(indices.back()));
	}

	// If the next operand is a GEP or if we have indices left to compile
	// we need to fully compile the next object.
	bool fullyCompileNextObject = compileFullAccess;
	if (compileFullAccess == false && isGEP(gep_inst->getOperand(0)) || (indices.size() - 1 > 0))
		fullyCompileNextObject = true;

	// TODO: we need this hack because PointerAnalyzer cannot correctly assign
	// the RAW kind to null pointers
	if (isa<ConstantPointerNull>(gep_inst->getOperand(0)))
	{
		const int32_t idx = linearHelper.getGCTypeIndex(gep_inst->getOperand(0)->getType(), kind);
		encodeInst(WasmS32Opcode::REF_NULL, idx, code);
	}
	else if (kind == COMPLETE_OBJECT)
	{
		const llvm::Instruction* I = dyn_cast<Instruction>(gep_inst->getOperand(0));
		errs() << "[compileGEPGC] is the GEP inlineable: " << ((I && isInlineable(*I)) ? "yes" : "no") << "\n";
		errs() << "[compileGEPGC] gep_inst: " << *gep_inst << "\n";
		if (I && !isInlineable(*I) && (isGEP(I) || isBitCast(I)) && PA.getPointerKindAssert(I) == COMPLETE_OBJECT)
		{
			assert(!isBitCast(I)); // TODO: bitcasts?
			errs() << "[compileGEPGC] compileGetLocal\n";
			compileGetLocal(code, I, 0);
		} else {
			errs() << "[compileGEPGC] compileCompleteObject\n";
			compileCompleteObject(code, gep_inst->getOperand(0), indices.front(), fullyCompileNextObject);
		}
		errs() << "[compileGEPGC] compileAccessToElement ind size: " << indices.size() - 1 << "\n";
		errs() << "[compileGEPGC] compiling access to element: " << *cast<GEPOperator>(gep_inst)->getSourceElementType() << "\n";
		compileAccessToElement(code, baseType, makeArrayRef(std::next(indices.begin()), indices.end()), true, compileFullAccess);
	}
	else
	{
		if (PA.getConstantOffsetForPointer(gep_inst))
		{
			errs() << "[compileGEPGC] Has constant offset for pointer\n";
			compileCompleteObject(code, gep_inst->getOperand(0), indices.front(), fullyCompileNextObject);

			compileAccessToElement(code, baseType, makeArrayRef(std::next(indices.begin()), std::prev(indices.end())), false, compileFullAccess);
			errs() << "[compileGEPGC] DONE\n\n\n";
			return;
		}

		errs() << "[compileGEPGC] no constant offset for pointer\n";
		compilePointerBaseTyped(code, gep_inst, cast<GEPOperator>(gep_inst)->getResultElementType()); // compile .d
		compilePointerOffset(code, gep_inst); // compile .o
		
		const int32_t regularObjectIdx = linearHelper.getRegularObjectIdx();
		encodeInst(WasmGCOpcode::STRUCT_NEW, regularObjectIdx, code); // create the regular object {.d, .o}
	}
	errs() << "[compileGEPGC] DONE\n\n\n";
}

void CheerpWasmWriter::compileOffsetForGEP(WasmBuffer& code, Type* pointedOperandType, ArrayRef< const Value* > indices)
{
	// FIXME This will not compile cause getIndexedType is not const-correct
	/*
	 * Type * tp = GetElementPtrInst::getIndexedType( pointedOperandType->getPointerTo(), indices.slice(0, indices.size() - 1 ) );
	 */

	Type* tp = GetElementPtrInst::getIndexedType(pointedOperandType,
	                makeArrayRef(const_cast<Value* const*>(indices.begin()),
	                             const_cast<Value* const*>(indices.end() - 1)));

	if(tp->isStructTy())
	{
		// Literal index
		assert(isa<ConstantInt>(indices.back()));
		const ConstantInt* idx = cast<ConstantInt>(indices.back());
		(void)idx;
		assert(types.useWrapperArrayForMember(PA, cast<StructType>(tp), idx->getZExtValue()));

		encodeInst(WasmS32Opcode::I32_CONST, 0, code);
	}
	else
	{
		compileOperand(code, indices.back());
	}
}

void CheerpWasmWriter::compileGEPOffset(WasmBuffer& code, const User* gep_inst)
{
	errs() << "[compileGEPOffset]\n";
	SmallVector< const Value*, 8 > indices(std::next(gep_inst->op_begin()), gep_inst->op_end());
	Type* basePointerType = gep_inst->getOperand(0)->getType();

	StructType* containerStructType = dyn_cast<StructType>(getGEPContainerType(gep_inst));
	bool useDownCastArray = false;
	if(containerStructType && indices.size() > 1)
	{
		assert(isa<ConstantInt>(indices.back()));
		const ConstantInt* idx = cast<ConstantInt>(indices.back());
		uint32_t lastOffsetConstant = idx->getZExtValue();
		useDownCastArray = !types.useWrapperArrayForMember(PA, containerStructType, lastOffsetConstant);
	}

	assert(PA.getPointerKindAssert(gep_inst) != BYTE_LAYOUT);
	if (indices.size() == 1)
	{
		bool isOffsetConstantZero = isa<Constant>(indices.front()) && cast<Constant>(indices.front())->isNullValue();

		// Just another pointer from this one
		compilePointerOffset(code, gep_inst->getOperand(0));

		if(!isOffsetConstantZero)
		{
			compileOperand(code, indices.front());
			encodeInst(WasmOpcode::I32_ADD, code);
		}
	}
	else
	{
		if (useDownCastArray)
		{
			Type* basePointedType = basePointerType->getPointerElementType();
			compileCompleteObject(code, gep_inst->getOperand(0), indices.front());
			compileAccessToElement(code, basePointedType, makeArrayRef(std::next(indices.begin()), indices.end()), true, true);
			assert(false && "Should this be a get on a regularObject?");
			encodeInst(WasmGCOpcode::STRUCT_GET, linearHelper.getRegularObjectIdx(), 1, code); // access .o
			errs() << "[compileGEPOffset] STRUCT_GET (.o)\n";
		}
		else
			compileOffsetForGEP(code, cast<GEPOperator>(gep_inst)->getSourceElementType(), indices);
	}
}

void CheerpWasmWriter::compileGEPBase(WasmBuffer& code, const llvm::User* gep_inst)
{
	errs() << "[compileGEPBase] START\n";
	SmallVector< const Value*, 8 > indices(std::next(gep_inst->op_begin()), gep_inst->op_end());
	Type* basePointerType = gep_inst->getOperand(0)->getType();

	StructType* containerStructType = dyn_cast<StructType>(getGEPContainerType(gep_inst));
	bool useDownCastArray = false;
	if(containerStructType && indices.size() > 1)
	{
		errs() << "[compileGEPBase] containedStructType = true\n";
		assert(isa<ConstantInt>(indices.back()));
		const ConstantInt* idx = cast<ConstantInt>(indices.back());
		uint32_t lastOffsetConstant = idx->getZExtValue();
		useDownCastArray = !types.useWrapperArrayForMember(PA, containerStructType, lastOffsetConstant);
	}

	assert(PA.getPointerKindAssert(gep_inst) != BYTE_LAYOUT);
	Type* elementTypeBaseOperand = cast<GEPOperator>(gep_inst)->getSourceElementType();
	if (indices.size() == 1)
	{
		// Just another pointer from this one
		errs() << "[compileGEPBase] callingCompilePointerBaseTyped indices is of size 1\n";
		errs() << "[compileGEPBase] indices:\n";
		for (auto i : indices)
			errs() << "[compileGEPBase] ", i->dump();
		errs() << "[compileGEPBase] gepInst: "; gep_inst->dump();
		compilePointerBaseTyped(code, gep_inst->getOperand(0), elementTypeBaseOperand);
	}
	else
	{
		errs() << "[compileGEPBase] indices size: " << indices.size() << "\n";
		// HACK: Accessing members of NULL is invalid, but it is used to compute an offset from the start of a structure
		// TODO: We need to detect and block this on the clang side. In the mean time, just compile an invalid null access
		if( isa<ConstantPointerNull>(gep_inst->getOperand(0)) )
		{
			errs() << "[compileGEPBase] compiling NULL pointer access: " << *gep_inst << "\n";
			errs() << "[compileGEPBase] operand(0): " << *gep_inst->getOperand(0) << "\n";
			assert(false);  
			encodeInst(WasmOpcode::UNREACHABLE, code);
			return;
		}

		compileCompleteObject(code, gep_inst->getOperand(0), indices.front());
		Type* basePointedType = basePointerType->getPointerElementType();
		if (useDownCastArray)
		{
			compileAccessToElement(code, basePointedType, makeArrayRef(std::next(indices.begin()),indices.end()), true, true);
			// TODO: should this be handled here or inside the load/store?
			// stream << ".a";
			errs() << "Should compile Wasm equivalent of the JS's .a access here\n";
			assert(false);
		}
		else if(containerStructType)
		{
			compileAccessToElement(code, basePointedType, makeArrayRef(std::next(indices.begin()),indices.end()), false, true);
		}
		else
		{
			compileAccessToElement(code, basePointedType, makeArrayRef(std::next(indices.begin()),std::prev(indices.end())), true, true);
		}
	}
}

void CheerpWasmWriter::compilePointerAs(WasmBuffer& code, const llvm::Value* p, POINTER_KIND kind)
{
	errs() << "[compilePointerAs] START\n";
	errs() << "[compilePointerAs] Compiling: " << *p << " as a "; printPtrKind(kind);
	assert(p->getType()->isPointerTy());
	assert(kind != SPLIT_REGULAR);
	assert(kind != CONSTANT);
	assert(kind != BYTE_LAYOUT);
	POINTER_KIND valueKind = PA.getPointerKind(p);
	assert(valueKind != BYTE_LAYOUT);

	switch(kind)
	{
		case RAW:
		{
			// TODO: is this correct?
			assert(valueKind == RAW || valueKind == CONSTANT);
			compileOperand(code, p);
			errs() << "[compilePointerAs] RAW\n";
			break;
		}
		case COMPLETE_OBJECT:
		{
			compileCompleteObject(code, p);
			errs() << "[compilePointerAs] compileCompleteObject END\n";
			break;
		}
		case REGULAR:
		{
			if (valueKind == CONSTANT)
			{
				encodeInst(WasmU32Opcode::GET_GLOBAL, nullObjGlobal, code);
				errs() << "[compilePointerAs] compile ConstNull END\n";
			}
			else if (PA.getConstantOffsetForPointer(p) || valueKind == SPLIT_REGULAR)
			{
				assert(isa<Instruction>(p));
				compilePointerBase(code, p); // compile .d
				compilePointerOffset(code, p); // compile .o

				const int32_t regularIndex = linearHelper.getRegularObjectIdx();
				encodeInst(WasmGCOpcode::STRUCT_NEW, regularIndex, code); // create the regular object
				errs() << "[compilePointerAs] compile Regular object END\n";
			}
			else
			{
				compileOperand(code, p);
				errs() << "[compilePointerAs] compileOperand END\n";
			}
			break;
		}
		default:
		{
			llvm::report_fatal_error("Unexpected pointer kind. This is a bug");
		}
	}
}

void CheerpWasmWriter::compileCompleteObject(WasmBuffer& code, const Value* p, const Value* offset, const bool compileFullAccess)
{
	errs() << "[compileCompleteObject] START\n";
	errs() << "[compileCompleteObject] Value: " << *p << "\n";
	// Special handle for undefined pointers
	if(isa<UndefValue>(p))
	{
		compileOperand(code, p);
		errs() << "[compileCompleteObject] DONE\n";
		return;
	}
	if(isa<ConstantPointerNull>(p))
	{
		// NULL reference of 'any' type
		encodeInst(WasmU32Opcode::REF_NULL, 0x6E, code);
		if (offset) // TODO: test if this passes validation WasmWriter dereferences the NULL
		{	
			errs() << "[compileCompleteObject] offset for NULL pointer: " << *offset << " encoding unreachable\n";
			assert(false);
			encodeInst(WasmOpcode::UNREACHABLE, code);
		}
		return;
	}

	const llvm::Instruction* I = dyn_cast<Instruction>(p);
	if (I && !isInlineable(*I) && isGEP(I) && PA.getPointerKindAssert(I) == COMPLETE_OBJECT)
	{
		assert(!isBitCast(I));
		errs() << "[compileCompleteObject] compileGetLocal\n";
		compileGetLocal(code, I, 0);
		errs() << "[compileCompleteObject] DONE\n";
		return;
	}

	bool isOffsetConstantZero = offset == nullptr || (isa<Constant>(offset) && cast<Constant>(offset)->isZeroValue());

	if(isOffsetConstantZero)
	{
		if(isGEP(p))
		{
			errs() << "[compileCompleteObject] calling compileGEPGC\n";
			compileGEPGC(code, cast<User>(p), COMPLETE_OBJECT, compileFullAccess);
			errs() << "[compileCompleteObject] DONE\n";
			return;
		}
	}

	POINTER_KIND kind = PA.getPointerKindAssert(p);
	errs() << "[compileCompleteObject] kind of p: "; printPtrKind(kind); 

	if(kind == REGULAR || kind == SPLIT_REGULAR)
	{
		compilePointerBase(code, p);

		const ConstantInt* c1 = dyn_cast_or_null<ConstantInt>(PA.getConstantOffsetForPointer(p));
		const ConstantInt* c2 = dyn_cast_or_null<ConstantInt>(offset);
		if(c1 && c2)
			encodeInst(WasmS32Opcode::I32_CONST, c1->getSExtValue() + c2->getSExtValue(), code);
		else if(c1 && c1->isZeroValue() && offset)
			compileOperand(code, offset);
		else
		{
			compilePointerOffset(code, p);

			if(!isOffsetConstantZero)
			{
				compileOperand(code, offset);
				encodeInst(WasmOpcode::I32_ADD, code);
			}
		}

		// If this is an operand used by a load/store we do not access the array, but instead leave it on the stack.
		// The load/store instruction will be accessing the reference.
		if (compileFullAccess)
		{
			uint32_t typeIdx = linearHelper.getGCTypeIndex(p->getType(), SPLIT_REGULAR);
			encodeInst(WasmGCOpcode::ARRAY_GET, typeIdx, code);
			compileRefCast(code, p->getType(), COMPLETE_OBJECT); // TODO: Is it possible for the array to contain other pointer kinds?
			errs() << "[compileCompleteObject] Accessing (split) reg array, no load store found\n";
			errs() << "[compileCompleteObject] ARRAY_GET on type index: " << typeIdx << " for pointer type: "; printPtrKind(SPLIT_REGULAR);
		}
	}
	else
	{
		errs() << "[compileCompleteObject] comilingOperand end of func\n";
		compileOperand(code, p);

		if(!isOffsetConstantZero)
		{
			llvm::errs() << "Can not access a " << int(kind) << " pointer with non zero offset:" << *offset << "\n";
			llvm::report_fatal_error("Unsupported code found, please report a bug", false);
		}
	}
	errs() << "[compileCompleteObject] DONE\n";
}

void CheerpWasmWriter::compilePointerOffset(WasmBuffer& code, const Value* ptr)
{
	POINTER_KIND kind = PA.getPointerKind(ptr);
	assert(kind != BYTE_LAYOUT);
	assert(kind != RAW);
	if ( kind == COMPLETE_OBJECT && !isGEP(ptr) )
	{
		// This may still happen when doing ptrtoint of a function
		encodeInst(WasmS32Opcode::I32_CONST, 0, code);
	}
	// null must be handled first
	else if(kind == CONSTANT || isa<UndefValue>(ptr))
	{
		if (const IntToPtrInst* ITP = dyn_cast<IntToPtrInst>(ptr))
		{
			ConstantInt* CI = cast<ConstantInt>(ITP->getOperand(0));
			encodeInst(WasmS32Opcode::I32_CONST, CI->getSExtValue(), code);
		}
		else
		{
			encodeInst(WasmS32Opcode::I32_CONST, 0, code);
		}
	}
	else if(isGEP(ptr) && (!isa<Instruction>(ptr) || isInlineable(*cast<Instruction>(ptr))))
	{
		compileGEPOffset(code, cast<User>(ptr));
	}
	else if(isBitCast(ptr) && (!isa<Instruction>(ptr) || isInlineable(*cast<Instruction>(ptr))))
	{
		// Since we can't have a pointerkind of byte_layout here
		// we can compile the pointer offset instead of a bitCastOffset
		compilePointerOffset(code, cast<User>(ptr)->getOperand(0));
	}
	else if (const ConstantInt* CI = PA.getConstantOffsetForPointer(ptr))
	{
		// Check if the offset has been constantized for this pointer
		compileConstant(code, CI, true);
	}
	else if((isa<SelectInst>(ptr) && isInlineable(*cast<Instruction>(ptr))) || \
			(isa<ConstantExpr>(ptr) && cast<ConstantExpr>(ptr)->getOpcode() == Instruction::Select))
	{
		const User* u = cast<User>(ptr);
		compileOperand(code, u->getOperand(0));
		compilePointerOffset(code, u->getOperand(1));
		compilePointerOffset(code, u->getOperand(2));
		encodeInst(WasmOpcode::SELECT, code);
	}
	else if((!isa<Instruction>(ptr) || !isInlineable(*cast<Instruction>(ptr))) && kind == SPLIT_REGULAR)
	{
		if(const Argument* arg=dyn_cast<Argument>(ptr))
		{
			errs() << "[compilePointerOffset] compiling argument: "; arg->dump();
			if (hasPutTeeLocalOnStack(code, arg))
			{
				//TODO: can we use hasPutTeeLocalOnStack here?
				errs() << "add offset of +1 to localId\n";
				assert(false);
				return;
			}

			// TODO: cache this inside a helper function
			uint32_t local = arg->getArgNo();
			uint32_t splitRegsInArgs = 0;
			for (size_t i = 0; i < local; i++)
			{
				Argument* prevArg = arg->getParent()->getArg(i);
				if (prevArg->getType()->isPointerTy() && PA.getPointerKindForArgument(prevArg) == SPLIT_REGULAR)
					splitRegsInArgs++;
			}
			encodeInst(WasmU32Opcode::GET_LOCAL, local + splitRegsInArgs + 1, code);
			errs() << "[compilePointerOffset] getLocal: " << local + splitRegsInArgs + 1<< "\n";
		}
		else
		{
			compileGetLocal(code, cast<Instruction>(ptr), 1);
		}
	}
	else if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(ptr))
	{
		errs() << "[compilePointerOffset] Intrinsic ID: " << II->getIntrinsicID() << "\n";
		errs() << "[compilePointerOffset] ptr: " << *ptr << "\n";
		// Handle intrinsics
		switch(II->getIntrinsicID())
		{
			case Intrinsic::cheerp_upcast_collapsed:
			case Intrinsic::cheerp_cast_user:
				compilePointerOffset(code, II->getOperand(0));
				return;
			case Intrinsic::cheerp_make_regular:
				compileOperand(code, II->getOperand(1));
				break;
			default:
				compileOperand(code, ptr);
				// .o access
				const int32_t regTypeIndex = linearHelper.getRegularObjectIdx();
				encodeInst(WasmGCOpcode::STRUCT_GET, regTypeIndex, 1, code);
		}
	} else {
		compileOperand(code, ptr);
		// .o access
		const int32_t regTypeIndex = linearHelper.getRegularObjectIdx();
		encodeInst(WasmGCOpcode::STRUCT_GET, regTypeIndex, 1, code);
	}
}

void CheerpWasmWriter::compilePointerBase(WasmBuffer& code, const Value* p, const uint32_t elemIdx)
{
	compilePointerBaseTyped(code, p, p->getType()->getPointerElementType(), elemIdx);
}

void CheerpWasmWriter::compilePointerBaseTyped(WasmBuffer& code, const Value* ptr, const Type* elemTy, const uint32_t elemIdx)
{
	assert(ptr->getType()->isPointerTy());
	POINTER_KIND kind = PA.getPointerKind(ptr);
	assert(kind != RAW);
	assert(kind != BYTE_LAYOUT);


	errs() << "[compilePointerBaseTyped] START for ptr: " << *ptr << " and elemTy: " << *elemTy << "\n";
	if(isGEP(ptr))
	{
		errs() << "[compilePointerBaseTyped] compilingGEPBase\n";
		const User* gepInst = cast<User>(ptr);
		assert(gepInst->getNumOperands() > 1);
		compileGEPBase(code, gepInst);
		return ;
	}

	if(kind == CONSTANT)
	{
		encodeInst(WasmU32Opcode::GET_GLOBAL, nullArrayGlobal, code);
		return;
	}

	if(kind == COMPLETE_OBJECT)
	{
		llvm::errs() << "compilePointerBase with COMPLETE_OBJECT pointer:" << *ptr << '\n' << "In function: " << *currentFun << '\n';
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}

	// Handle intrinsics
	if(const IntrinsicInst* II = dyn_cast<IntrinsicInst>(ptr))
	{
		switch(II->getIntrinsicID())
		{
			case Intrinsic::cheerp_upcast_collapsed:
			case Intrinsic::cheerp_cast_user:
				return compilePointerBase(code, II->getOperand(0));
			case Intrinsic::cheerp_make_regular:
				return compileCompleteObject(code, II->getOperand(0));
			default:
				break;
		}
	}

	if(isa<UndefValue>(ptr))
	{
		assert(false);
		return;
	}

	errs() << "[compilePointerBaseTyped] Before isa<> ptr is: " << *ptr << "\n";

	if((isa<SelectInst> (ptr) && isInlineable(*cast<Instruction>(ptr))) || \
		(isa<ConstantExpr>(ptr) && cast<ConstantExpr>(ptr)->getOpcode() == Instruction::Select))
	{
		const User* u = cast<User>(ptr);
		compileOperand(code, u->getOperand(0));
		compilePointerBase(code, u->getOperand(1), 1);
		compilePointerBase(code, u->getOperand(2), 2);
		encodeInst(WasmOpcode::SELECT, code);
		assert(false);
		return;
	}

	if(const Argument* arg=dyn_cast<Argument>(ptr))
	{
		errs() << "[compilePointerBaseType] compiling argument: "; arg->dump();
		if (hasPutTeeLocalOnStack(code, arg))
		{
			errs() << "[compilePointerBaseType] Has put tee_local on stack\n";
			return;
		}

		uint32_t local = arg->getArgNo();
		uint32_t splitRegsInArgs = 0;
		for (size_t i = 0; i < local; i++)
		{
			Argument* prevArg = arg->getParent()->getArg(i);
			if (prevArg->getType()->isPointerTy() && PA.getPointerKindForArgument(prevArg) == SPLIT_REGULAR)
				splitRegsInArgs++;
		}
		encodeInst(WasmU32Opcode::GET_LOCAL, local + splitRegsInArgs, code);
		errs() << "[compilePointerBaseType] getLocal: " << local + splitRegsInArgs << "\n";

		// If the get_local is called on a WasmGC type we need to cast it from anyref to the right reftype
		POINTER_KIND kind = COMPLETE_OBJECT;
		if (arg->getType()->isPointerTy())
			kind = PA.getPointerKindForArgument(arg);
		compileRefCast(code, arg->getType(), kind);
		return ;
	}
	else if((!isa<Instruction>(ptr) || !isInlineable(*cast<Instruction>(ptr))) && kind == SPLIT_REGULAR)
	{
		// TODO: check condition and compare with WasmWriter
		errs() << "[compilePointerBaseTyped] trying to do a getLocal on: " << *ptr << "\n";

		assert(isa<Instruction>(ptr));
		compileGetLocal(code, cast<Instruction>(ptr), elemIdx);
		return;
	}

	errs() << "[compilePointerBase] compiling operand for .d\n";
	// If value has not been generated from a GEP, just compile it and ask for .d
	compileOperand(code, ptr);
	if(!PA.getConstantOffsetForPointer(ptr))
	{
		const Type* sTy = ptr->getType();
		uint32_t idx = linearHelper.getGCTypeIndex(sTy, REGULAR);
		encodeInst(WasmGCOpcode::STRUCT_GET, idx, 0, code); // get the .d
		compileRefCast(code, sTy, SPLIT_REGULAR);
		errs() << "[compilePointerBaseType] ptr: " << *ptr << " sTy: " << *sTy << " kind: "; printPtrKind(kind); errs() << "\n\n";
	}
}

void CheerpWasmWriter::compileMethodArgs(WasmBuffer& code, User::const_op_iterator it, User::const_op_iterator itE, const CallBase& callV)
{
	errs() << "[compileMethodArgs] Compiling method args\n";
	const Function* F = callV.getCalledFunction();

	Function::const_arg_iterator arg_it;

	// Check if we have a direct call
	if(F && it != itE)
	{
		// Set arg_it to the argument relative to it.
		arg_it = F->arg_begin();
		unsigned argNo = callV.getArgOperandNo(it);

		// Check if it is a variadic argument
		if(argNo < F->arg_size())
		{
			std::advance(arg_it, callV.getArgOperandNo(it));
		}
		else
		{
			arg_it = F->arg_end();
		}
	}
	
	uint32_t opCount = 0;
	for (User::const_op_iterator cur = it; cur != itE; ++cur, ++opCount)
	{
		Type* Ty = (*cur)->getType();
		errs() << "[compileMethodArgs] Compiling argument " << opCount << " of type: " << *Ty << "\n";


		if(Ty->isPointerTy() && !TypeSupport::isRawPointer(Ty, false))
		{
			POINTER_KIND argKind = UNKNOWN;
			// Calling convention:
			// If this is a direct call and the argument is not a variadic one,
			// we pass the kind decided by getPointerKind(arg_it).
			// If it's variadic we use the base kind derived from the type
			// If it's indirect we use a kind good for any argument of a given type at a given position
			if (!F)
			{
				TypeAndIndex typeAndIndex(Ty->getPointerElementType(), opCount, TypeAndIndex::ARGUMENT);
				argKind = PA.getPointerKindForArgumentTypeAndIndex(typeAndIndex);
			}
			else if (arg_it != F->arg_end())
				argKind = PA.getPointerKindForArgument(&*arg_it);
			else
			{
				if(isa<ConstantPointerNull>(*cur) && (cur+1)==itE && cur!=it)
				{
					// Special case for NULL which are the last variadic parameter, copy the previous type
					Type* prevType = (*(cur-1))->getType();
					if(prevType->isPointerTy())
						Ty = prevType;
				}
				if(StructType* st = dyn_cast<StructType>(Ty->getPointerElementType()))
				{
					while(st->getDirectBase())
						st = st->getDirectBase();
					Ty = st->getPointerTo();
				}
				errs() << "[compileMethodArgs] compilingPointerAs\n";
				compilePointerAs(code, *cur, PA.getPointerKindForStoredType(Ty));
			}

			POINTER_KIND curKind = PA.getPointerKind(cur->get());
			assert(argKind != REGULAR);
			assert(argKind != BYTE_LAYOUT);
			assert(curKind != BYTE_LAYOUT);
			// TODO: is this still nececcary since we won't support BYTE_LAYOUT?
			// The second condition is for when the function is only declared
			// And the passed pointer is BYTE_LAYOUT. We decide to compile it as
			// SPLIT_REGULAR, since the code will crash here anyway
			if(argKind == SPLIT_REGULAR)
			{
				if(F && PA.getConstantOffsetForPointer(&*arg_it))
				{
					errs() << "[compileMethodArgs] compiling pointer base\n";
					compilePointerBase(code, *cur, opCount);
				}
				else
				{
					errs() << "[compileMethodArgs] compiling pointer base ',' pointer offset\n";
					compilePointerBase(code, *cur, opCount);
					compilePointerOffset(code, *cur);
				}
			}
			else if(argKind != UNKNOWN)
			{
				errs() << "[compileMethodArgs] compiling pointer as argkind\n";
				compilePointerAs(code, *cur, argKind);
			}
		}
		else
		{
			errs() << "[compileMethodArgs] compiling operand\n";
			compileOperand(code, *cur);
		}
	}
}

// TODO: is this still needed once we switch from hacking JS functions into wasm
// to having attributes for GC?
POINTER_KIND CheerpWasmWriter::getLocalPointerKind(const Value* v)
{
	POINTER_KIND kind = COMPLETE_OBJECT;
	if (v->getType()->isPointerTy())
	{
		kind = PA.getPointerKind(v);
		errs() << "[getLocalPointerKind] getPointerKind: "; printPtrKind(kind);
		if (auto ci = dyn_cast<CallInst>(v))
		{
			errs() << "[getLocalPointerKind] value is a call Instruction";
			const Function* calledFunc = ci->getCalledFunction();
			unsigned intrId = calledFunc->getIntrinsicID();

			if (intrId == Intrinsic::cheerp_allocate || intrId == Intrinsic::cheerp_allocate_array)
			{
				errs() << "[getLocalPointerKind] Found a cheerp alloca intrinsic\n";
				if (kind == REGULAR)
					return (SPLIT_REGULAR);
			}
		}
	}
	return (kind);
}

void CheerpWasmWriter::compileMethodLocals(WasmBuffer& code, const vector<int>& locals)
{
	uint32_t groups = (uint32_t) locals.at(Registerize::INTEGER) > 0;
	groups += (uint32_t) locals.at(Registerize::INTEGER64) > 0;
	groups += (uint32_t) locals.at(Registerize::DOUBLE) > 0;
	groups += (uint32_t) locals.at(Registerize::FLOAT) > 0;
	groups += (uint32_t) locals.at(Registerize::OBJECT) > 0;
	groups += (uint32_t) locals.at(Registerize::VECTOR) > 0;

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

	if (locals.at(Registerize::VECTOR)) {
		encodeULEB128(locals.at(Registerize::VECTOR), code);
		encodeRegisterKind(Registerize::VECTOR, code);
	}
}

void CheerpWasmWriter::compileMethodParams(WasmBuffer& code, const FunctionType* fTy)
{
	uint32_t numArgs = fTy->getNumParams();
		encodeULEB128(numArgs, code);

	for(uint32_t i = 0; i < numArgs; i++)
		encodeValType(fTy->getParamType(i), code);
}

static bool isReturnTypeGC(const Type* Ty)
{
	errs() << "[isReturnTypeGC] checking Type: " << *Ty << "\n";
	if (auto sTy = dyn_cast<StructType>(Ty))
		return (!sTy->hasAsmJS());
	if (auto aTy = dyn_cast<ArrayType>(Ty))
		return (true); // TODO: use address spaces
	return (false);
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
		if (isReturnTypeGC(ty))
		{
			// Nullable reference
			encodeULEB128(0x63, code);
			encodeSLEB128(linearHelper.getGCTypeIndex(ty, COMPLETE_OBJECT), code);
		}
		else
			encodeValType(ty, code);
	}
}

void CheerpWasmWriter::compileCondition(WasmBuffer& code, const llvm::Value* cond, bool booleanInvert)
{
	errs() << "[compileCondition] compiling condition: " << *cond << "\n";
	bool canInvertCond = isa<Instruction>(cond) && isInlineable(*cast<Instruction>(cond));

	if (cond->getType()->isVectorTy() && isa<ICmpInst>(cond))
	{
		const ICmpInst* ci = cast<ICmpInst>(cond);
		compileOperand(code, ci->getOperand(0));
		compileOperand(code, ci->getOperand(1));
		CmpInst::Predicate p = ci->getPredicate();
		encodePredicate(ci->getOperand(0)->getType(), p, code);
	}
	else if(canInvertCond && isa<ICmpInst>(cond))
	{
		errs() << "[compileCondition] canInvertCond and isa ICmpInst\n";
		const ICmpInst* ci = cast<ICmpInst>(cond);
		CmpInst::Predicate p = ci->getPredicate();
		if(booleanInvert)
			p = CmpInst::getInversePredicate(p);
		Value* op0 = ci->getOperand(0);
		Value* op1 = ci->getOperand(1);

		if (isTypeGC(op0->getType()) || isTypeGC(op1->getType())) // TODO: can just check one of the two?
		{
			errs() << "[compileCondition] op0:" << *op0 << "\n[compileCondition] op1: " << *op1 << "\n";
			errs() << "[compileCondition] Should invert: " << (p == CmpInst::ICMP_NE ? "true" : "false") << "\n";
			// Optimization for NULL reference comparison
			if (isa<ConstantPointerNull>(op0) || isa<ConstantPointerNull>(op1))
			{
				if (isa<ConstantPointerNull>(op0))
					std::swap(op0, op1);
				compileOperand(code, op0);
				encodeInst(WasmOpcode::REF_IS_NULL, code);
				if (p == CmpInst::ICMP_NE)
					encodeInst(WasmOpcode::I32_EQZ, code);
				teeLocals.removeConsumed();
				return;
			}

			compileOperand(code, op0);
			compileOperand(code, op1);
			encodeInst(WasmOpcode::REF_EQ, code);
			if (p == CmpInst::ICMP_NE)
				encodeInst(WasmOpcode::I32_EQZ, code);
			teeLocals.removeConsumed();
			return;
		}

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
		errs() << "[compileCondition] canInvertCond and isa Fcmpinst\n";
		const CmpInst* ci = cast<CmpInst>(cond);
		CmpInst::Predicate p = ci->getPredicate();
		if(booleanInvert)
			p = CmpInst::getInversePredicate(p);
		compileFCmp(ci->getOperand(0), ci->getOperand(1), p, code);
	}
	else
	{
		errs() << "[compileCondition] compilingOperand\n";
		compileOperand(code, cond);
		if (booleanInvert) {
			errs() << "[compileCondition] inverting Condition\n";
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
	if (bitWidth != 32 && needsUnsignedTruncation(si->getCondition(), /*asmjs*/true))
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
	//	NONE -> 0x40 and {I32, I64, F32, F64, V128} -> {0x7F, 0x7E, 0x7D, 0x7C, 0x7B}
	if (kind != 0x40 && (kind > 0x7F || kind < 0x7B))
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
				errs() << "Calling compileCondition: TK_Condition false\n";
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
				encodeBranchHint(bi, IfNot, code);
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
				encodeBranchHint(bi, IfNot, code);
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
				assert(Depth || T.getMatch()->getKind() == Token::TK_Loop);
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
			case Token::TK_Try:
			case Token::TK_Catch:
				report_fatal_error("Try and Catch not handled");
				break;
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
			if (phi.use_empty())
				continue;
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

// TODO: maybe cache this since we loop through it inside the linearMemoryHelper already
uint32_t CheerpWasmWriter::getDowncastArraySize(StructType* sTy, uint32_t size) const
{
	if (sTy->hasDirectBase())
	{
		size = getDowncastArraySize(sTy->getDirectBase(), size);
		if(!TypeSupport::hasBasesInfoMetadata(sTy, module))
			return size;
	}
	else
		size++;
	
	uint32_t firstBase;
	uint32_t baseCount;
	if(!types.getBasesInfo(sTy, firstBase, baseCount))
		return size;

	for (uint32_t i = firstBase; i< (firstBase + baseCount); i++)
	{
		if (!sTy->getElementType(i)->isStructTy())
			continue ;
		size = getDowncastArraySize(cast<StructType>(sTy->getElementType(i)), size);
	}
	return size;
}

uint32_t CheerpWasmWriter::compileDowncastInitializerRecursive(WasmBuffer& code, Chunk<128> currClassAccess, StructType* currTy, uint32_t baseCount)
{
	int32_t typeIdx = linearHelper.getGCTypeIndex(currTy, COMPLETE_OBJECT, true);

	if (currTy->hasDirectBase())
	{
		baseCount = compileDowncastInitializerRecursive(code, currClassAccess, currTy->getDirectBase(), baseCount);
		if(!TypeSupport::hasBasesInfoMetadata(currTy, module))
			return baseCount;
	}
	else
	{
		assert(downcastArrayIndices.find(currTy) != downcastArrayIndices.end());
		assert(linearHelper.hasDowncastArray(currTy));

		// store the current class into the the tmp local and downcast array
		encodeInst(WasmU32Opcode::GET_LOCAL, 1, code);
		encodeInst(WasmS32Opcode::I32_CONST, baseCount, code);
		code << currClassAccess.str();
		encodeInst(WasmU32Opcode::TEE_LOCAL, 2, code);
		encodeInst(WasmGCOpcode::ARRAY_SET, linearHelper.getSplitRegularObjectIdx(), code);

		// store the downcast array
		encodeInst(WasmU32Opcode::GET_LOCAL, 2, code);
		encodeInst(WasmGCOpcode::REF_CAST_NULL, typeIdx, code);
		encodeInst(WasmU32Opcode::GET_LOCAL, 1, code);
		encodeInst(WasmGCOpcode::STRUCT_SET, typeIdx, downcastArrayIndices[currTy], code);

		// store the offset
		encodeInst(WasmU32Opcode::GET_LOCAL, 2, code);
		encodeInst(WasmGCOpcode::REF_CAST_NULL, typeIdx, code);
		encodeInst(WasmS32Opcode::I32_CONST, baseCount, code);
		encodeInst(WasmGCOpcode::STRUCT_SET, typeIdx, downcastArrayIndices[currTy] + 1, code);

		baseCount++;
	}

	uint32_t firstBase;
	uint32_t localBaseCount;
	if (!types.getBasesInfo(currTy, firstBase, localBaseCount))
		return baseCount;

	for (uint32_t i = firstBase; i < (firstBase + localBaseCount); i++)
	{
		if (!currTy->getElementType(i)->isStructTy())
			continue ;

		Chunk<128> nextClassAccess(currClassAccess);
		encodeInst(WasmGCOpcode::STRUCT_GET, typeIdx, getExpandedStructElemIdx(currTy, i), nextClassAccess);
		encodeInst(WasmGCOpcode::REF_CAST_NULL, linearHelper.getGCTypeIndex(currTy->getElementType(i), COMPLETE_OBJECT, true), nextClassAccess);
		baseCount = compileDowncastInitializerRecursive(code, nextClassAccess, cast<StructType>(currTy->getElementType(i)), baseCount);
	}
	return baseCount;
}

void CheerpWasmWriter::compileDowncastInitializer(WasmBuffer& code, StructType* sTy)
{
	errs() << "[compileDowncastInitializerRecursive] creating the initializer function for sTy: " << *sTy << "\n";
	const int32_t splitRegIdx = linearHelper.getSplitRegularObjectIdx();

	// encode the local group count
	encodeULEB128(2, code);
	// the local downcast array
	encodeULEB128(1, code);
	encodeULEB128(0x63, code);
	encodeSLEB128(splitRegIdx, code);
	// a tmp anyref so we don't have to encode every struct_get over again
	encodeULEB128(1, code);
	encodeULEB128(0x6E, code);

	// allocate the array and initialize the size
	uint32_t size = getDowncastArraySize(sTy, 0);
	encodeInst(WasmU32Opcode::REF_NULL, 0x6E, code);
	encodeInst(WasmS32Opcode::I32_CONST, size, code);
	encodeInst(WasmGCOpcode::ARRAY_NEW, splitRegIdx, code);
	encodeInst(WasmU32Opcode::SET_LOCAL, 1, code);

	// compile the logic:
	Chunk<128> tmpBuffer;
	encodeInst(WasmU32Opcode::GET_LOCAL, 0, tmpBuffer);
	compileDowncastInitializerRecursive(code, tmpBuffer, sTy, 0);

	// make sure to return the class
	encodeInst(WasmU32Opcode::GET_LOCAL, 0, code);
	encodeULEB128(0x0b, code);
}

/**
 * The logic of this function boils down to this:
 * 	array* resizeArray(oldArray, oldSize, newSize)	
 * 	{
 * 		array* newArray = new array[newSize];
 * 		int i = 0;
 *
 * 		while (i != oldSize && i != newSize)
 * 			newArray[i] = oldArray[i];
 * 		while (i != newSize)
 * 			newArray[i] = type{};
 * 		return newArray;
 * 	}
 */
void CheerpWasmWriter::compileResizeArrayFunc(WasmBuffer& code, Type* Ty)
{
	int32_t arrayTypeIdx = linearHelper.getGCTypeIndex(Ty, SPLIT_REGULAR);
	// encode the local group count
	encodeULEB128(2, code);
	// encode the index
	encodeULEB128(1, code);
	encodeULEB128(0x7f, code);
	// encode the new array local
	encodeULEB128(1, code);
	encodeULEB128(0x63, code);
	encodeSLEB128(arrayTypeIdx, code);


	// create an array with the new size
	encodeInst(WasmU32Opcode::GET_LOCAL, 2, code);
	encodeInst(WasmGCOpcode::ARRAY_NEW_DEFAULT, arrayTypeIdx, code);
	encodeInst(WasmU32Opcode::SET_LOCAL, 4, code);

	// create the first loop and it's exit block
	encodeInst(WasmU32Opcode::BLOCK, 0x40, code);
	encodeInst(WasmU32Opcode::LOOP, 0x40, code);

	// loop till (index == oldSize || index == newSize)
	encodeInst(WasmU32Opcode::GET_LOCAL, 3, code);	// index
	encodeInst(WasmU32Opcode::GET_LOCAL, 1, code);	// oldSize
	encodeInst(WasmOpcode::I32_EQ, code);			// check equal
	encodeInst(WasmU32Opcode::GET_LOCAL, 3, code);	// index
	encodeInst(WasmU32Opcode::GET_LOCAL, 2, code);	// newSize
	encodeInst(WasmOpcode::I32_EQ, code);			// check equal
	encodeInst(WasmOpcode::I32_OR, code);			// OR comparison
	encodeInst(WasmU32Opcode::BR_IF, 1, code);		// break out of the loop

	// copy from the old array into the new array
	encodeInst(WasmU32Opcode::GET_LOCAL, 4, code);	// new array
	encodeInst(WasmU32Opcode::GET_LOCAL, 3, code);	// index
	encodeInst(WasmU32Opcode::GET_LOCAL, 0, code);	// old array
	encodeInst(WasmU32Opcode::GET_LOCAL, 3, code);	// index
	encodeInst(WasmGCOpcode::ARRAY_GET, arrayTypeIdx, code);
	encodeInst(WasmGCOpcode::ARRAY_SET, arrayTypeIdx, code);

	// increase index iterator
	encodeInst(WasmU32Opcode::GET_LOCAL, 3, code);
	encodeInst(WasmS32Opcode::I32_CONST, 1, code);
	encodeInst(WasmOpcode::I32_ADD, code);
	encodeInst(WasmU32Opcode::SET_LOCAL, 3, code);

	// continue the loop
	encodeInst(WasmU32Opcode::BR, 0, code);

	// end the exit block and loop
	encodeULEB128(0x0b, code);
	encodeULEB128(0x0b, code);

	// if the allocated type is not an aggregate we can return right away,
	// all the values are 0 initialized through ARRAY_NEW_DEFAULT
	if (!Ty->isAggregateType())
	{
		encodeInst(WasmU32Opcode::GET_LOCAL, 4, code);
		encodeULEB128(0x0b, code);
		return ;
	}

	// create the second loop and it's exit block
	encodeInst(WasmU32Opcode::BLOCK, 0x40, code);
	encodeInst(WasmU32Opcode::LOOP, 0x40, code);

	// loop till index == newSize
	encodeInst(WasmU32Opcode::GET_LOCAL, 3, code);	// index
	encodeInst(WasmU32Opcode::GET_LOCAL, 2, code);	// newSize
	encodeInst(WasmOpcode::I32_EQ, code);			// equal
	encodeInst(WasmU32Opcode::BR_IF, 1, code);		// break out of the loop

	// insert a new object at the current array index
	encodeInst(WasmU32Opcode::GET_LOCAL, 4, code);
	encodeInst(WasmU32Opcode::GET_LOCAL, 3, code);
	allocateTypeGC(code, Ty, false);
	encodeInst(WasmGCOpcode::ARRAY_SET, arrayTypeIdx, code);

	// increase index iterator
	encodeInst(WasmU32Opcode::GET_LOCAL, 3, code);
	encodeInst(WasmS32Opcode::I32_CONST, 1, code);
	encodeInst(WasmOpcode::I32_ADD, code);
	encodeInst(WasmU32Opcode::SET_LOCAL, 3, code);

	// continue the loop
	encodeInst(WasmU32Opcode::BR, 0, code);

	// end the exit block and loop
	encodeULEB128(0x0b, code);
	encodeULEB128(0x0b, code);

	// make sure to return the array
	encodeInst(WasmU32Opcode::GET_LOCAL, 4, code);
	encodeULEB128(0x0b, code);
}

void CheerpWasmWriter::compileCreateDynamicAllocArrayFunc(WasmBuffer& code, Type* Ty)
{
	int32_t arrayTypeIdx = linearHelper.getSplitRegularObjectIdx();
	// non-aggregate types should be handled as an ARRAY_NEW_DEFAULT instead of a function call
	assert (Ty->isAggregateType());
	// encode the local group count
	encodeULEB128(2, code);
	// encode the index
	encodeULEB128(1, code);
	encodeULEB128(0x7f, code);
	// encode the array local
	encodeULEB128(1, code);
	encodeULEB128(0x63, code);
	encodeSLEB128(arrayTypeIdx, code);


	// create an array with the required size
	encodeInst(WasmU32Opcode::GET_LOCAL, 0, code);
	encodeInst(WasmGCOpcode::ARRAY_NEW_DEFAULT, arrayTypeIdx, code);
	encodeInst(WasmU32Opcode::SET_LOCAL, 2, code);

	// create the loop and it's exit block
	encodeInst(WasmU32Opcode::BLOCK, 0x40, code);
	encodeInst(WasmU32Opcode::LOOP, 0x40, code);

	// loop till index == size
	encodeInst(WasmU32Opcode::GET_LOCAL, 1, code);
	encodeInst(WasmU32Opcode::GET_LOCAL, 0, code);
	encodeInst(WasmOpcode::I32_EQ, code);
	encodeInst(WasmU32Opcode::BR_IF, 1, code);

	// retrieve the array and the offset to store into
	encodeInst(WasmU32Opcode::GET_LOCAL, 2, code);
	encodeInst(WasmU32Opcode::GET_LOCAL, 1, code);

	// allocate the type
	allocateTypeGC(code, Ty, false);

	// store the newly allocated type inside the array
	encodeInst(WasmGCOpcode::ARRAY_SET, arrayTypeIdx, code);

	// increase index iterator
	encodeInst(WasmU32Opcode::GET_LOCAL, 1, code);
	encodeInst(WasmS32Opcode::I32_CONST, 1, code);
	encodeInst(WasmOpcode::I32_ADD, code);
	encodeInst(WasmU32Opcode::SET_LOCAL, 1, code);

	// continue the loop
	encodeInst(WasmU32Opcode::BR, 0, code);

	// end the exit block and loop
	encodeULEB128(0x0b, code);
	encodeULEB128(0x0b, code);

	// make sure to return the array
	encodeInst(WasmU32Opcode::GET_LOCAL, 2, code);
	encodeULEB128(0x0b, code);
}

// parameter order -> the array, start index, size, init object
void CheerpWasmWriter::compileCreatePointerArrayFunc(WasmBuffer& code)
{
	int32_t arrayTypeIdx = linearHelper.getSplitRegularObjectIdx();
	// encode the local group count
	encodeULEB128(0, code);


	// create the loop and it's exit block
	encodeInst(WasmU32Opcode::BLOCK, 0x40, code);
	encodeInst(WasmU32Opcode::LOOP, 0x40, code);

	// loop till index == size
	encodeInst(WasmU32Opcode::GET_LOCAL, 1, code);
	encodeInst(WasmU32Opcode::GET_LOCAL, 2, code);
	encodeInst(WasmOpcode::I32_EQ, code);
	encodeInst(WasmU32Opcode::BR_IF, 1, code);

	// store the given init object into the array at the current index
	encodeInst(WasmU32Opcode::GET_LOCAL, 0, code);
	encodeInst(WasmU32Opcode::GET_LOCAL, 1, code);
	encodeInst(WasmU32Opcode::GET_LOCAL, 3, code);
	encodeInst(WasmGCOpcode::ARRAY_SET, arrayTypeIdx, code);

	// increase index iterator
	encodeInst(WasmU32Opcode::GET_LOCAL, 1, code);
	encodeInst(WasmS32Opcode::I32_CONST, 1, code);
	encodeInst(WasmOpcode::I32_ADD, code);
	encodeInst(WasmU32Opcode::SET_LOCAL, 1, code);

	// continue the loop
	encodeInst(WasmU32Opcode::BR, 0, code);

	// end the exit block and loop
	encodeULEB128(0x0b, code);
	encodeULEB128(0x0b, code);

	// make sure to return the array
	encodeInst(WasmU32Opcode::GET_LOCAL, 0, code);
	encodeULEB128(0x0b, code);
}

void CheerpWasmWriter::compileMethod(WasmBuffer& code, const Function& F)
{
	assert(code.tell() == 0);

	assert(!F.empty());
	currentFun = &F;

	static int i = numberOfImportedFunctions;
	errs() << "---------------------------------------------------------------------------\nCompiling func " << i << " " << F.getName() << "\n";
	errs() << F << "\n";

	uint32_t numArgs = linearHelper.getExpandedFunctionType(&F)->getFunctionNumParams();
	const llvm::BasicBlock* lastDepth0Block = nullptr;

	const std::vector<Registerize::RegisterInfo>& regsInfo = registerize.getRegistersForFunction(&F);
	const uint32_t localCount = regsInfo.size();

	vector<int> locals(6, 0);
	localMap.assign(localCount, 0);
	uint32_t reg = 0;

	// Make lookup table for registers to locals.
	for(const Registerize::RegisterInfo& regInfo: regsInfo)
	{
		// Save the current local index
		localMap.at(reg) = numArgs + locals.at((int)regInfo.regKind);
		locals.at((int)regInfo.regKind)++;
		reg++;
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
			case Registerize::VECTOR:
				offset += locals.at((int)Registerize::INTEGER);
				offset += locals.at((int)Registerize::INTEGER64);
				offset += locals.at((int)Registerize::DOUBLE);
				offset += locals.at((int)Registerize::FLOAT);
				offset += locals.at((int)Registerize::OBJECT);
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
		{
			DominatorTree &DT = FAM.getResult<DominatorTreeAnalysis>(const_cast<Function&>(F));
			LoopInfo &LI = FAM.getResult<LoopAnalysis>(const_cast<Function&>(F));
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



	if (F.getName().find("test") != std::string::npos)
		errs() << "Done compiling func " << i << " " << F.getName() << "\n---------------------------------------------------------------------------\n";
	i++;
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

static bool isNumberType(const llvm::Type* t)
{
	if (t->isIntegerTy(64))
		return true;
	else if (t->isIntegerTy() || TypeSupport::isRawPointer(t, true))
		return true;
	else if (t->isFloatTy())
		return true;
	else if (t->isDoubleTy())
		return true;
	return false;
}

void CheerpWasmWriter::compileStorageType(Section& section, const Type* Ty)
{
	errs() << "[compileStorageType] storageType: "<< *Ty << "\n";
	if (isNumberType(Ty) || Ty->isVectorTy())
		encodeValType(Ty, section);
	else if (Ty->isAggregateType() || Ty->isPointerTy())
	{
		// nullable anyref
		encodeULEB128(0x6e, section);
	}
	else
	{
		llvm::report_fatal_error("Unsopported strorage type");
	}
	// encoding mutability: Currently setting everything to mutable
	encodeULEB128(0x01, section);
}

void CheerpWasmWriter::compilePackedType(Section& section, const Type* Ty)
{
	if (Ty->isIntegerTy(8))
		encodeULEB128(0x78, section);
	else if (Ty->isIntegerTy(16))
		encodeULEB128(0x77, section);
	else
		llvm::report_fatal_error("invalid packed type");

	// encoding mutability: Currently setting everything to mutable
	encodeULEB128(0x01, section);
}

// TODO: move to a utils file
bool CheerpWasmWriter::needsOffsetAsElement(const StructType* sTy, uint32_t elemIdx)
{
	if (sTy->getElementType(elemIdx)->isPointerTy())
	{
		TypeAndIndex baseAndIndex(sTy, elemIdx, TypeAndIndex::STRUCT_MEMBER);
		if (PA.getPointerKindForMemberPointer(baseAndIndex) == SPLIT_REGULAR)
		{
			if (PA.getConstantOffsetForMember(baseAndIndex) == NULL)
				errs() << "[needsoffsetAsElement] Type: " << *sTy << " needs an offset at index: " << elemIdx << "\n";
			return (PA.getConstantOffsetForMember(baseAndIndex) == NULL);
		}
	}
	return false;
}

// TODO: move to a utils file
bool CheerpWasmWriter::needsExpandedStruct(const StructType* sTy)
{
	// The root struct will have the downcast array appended to the end
	// meaning that all of the indices still line up
	if (linearHelper.hasDowncastArray(sTy) && sTy->hasDirectBase())
		return true;
	
	for (uint32_t i = 0; i < sTy->getStructNumElements(); i++)
	{
		if (needsOffsetAsElement(sTy, i))
			return true;
	}
	return false;
}

// TODO: move to a utils file
uint32_t CheerpWasmWriter::getExpandedStructElemIdx(const StructType* sTy, uint32_t elemIdx)
{
	auto it = structElemIdxCache.find(sTy);
	if (it == structElemIdxCache.end())
		return (elemIdx);
	return (it->second[elemIdx]);
}

// TODO: move to a utils file
uint32_t CheerpWasmWriter::calculateAndCacheElemInfo(const StructType* sTy)
{
	errs() << "[calculateAndCacheElemInfo] calulating sTy: " << *sTy << "\n";
	assert(structElemIdxCache.find(sTy) == structElemIdxCache.end());
	// TODO: is it clear by the function names/vars that this happens:
	// Caching only the structs that need to be expanded. Should save on memory
	// and make lookup faster. Does mean we have to loop through it twice,
	// here and inside needsExpandedStruct
	// This function also returns the size of the expanded struct
	if (!needsExpandedStruct(sTy))
	{
		errs() << "[calculateAndCacheElemInfo] no expansion needed\n";
		return (sTy->getStructNumElements());
	}

	uint32_t currIndex = 0;
	uint32_t downcastArrayIndex = 0;
	// The root type will have a second version of itself with the offset and array appended to it
	bool hasDowncastArray = (linearHelper.hasDowncastArray(sTy) && sTy->hasDirectBase());
	if (hasDowncastArray)
	{
		assert(downcastArrayIndices.find(sTy) != downcastArrayIndices.end());
		assert(linearHelper.hasDowncastArray(sTy));
		downcastArrayIndex = downcastArrayIndices[sTy];
	}

	const uint32_t numElems = sTy->getStructNumElements();
	for (uint32_t i = 0; i < numElems; i++)
	{
		structElemIdxCache[sTy][i] = currIndex;
		if (needsOffsetAsElement(sTy, i))
			currIndex++;
		if (hasDowncastArray && i + 1 == downcastArrayIndex)
			currIndex += 2;
		currIndex++;
	}
	errs() << "[calculateAndCacheElemInfo] final index: " << currIndex << "\n";
	return (currIndex);
}

uint32_t CheerpWasmWriter::cacheDowncastOffset(const llvm::StructType* sTy)
{
	assert(linearHelper.hasDowncastArray(sTy));
	auto it = downcastArrayIndices.find(sTy);
	if (it != downcastArrayIndices.end())
	{
		errs() << "[cacheDowncastOffset] found a cached offset: " << it->second << "\n";
		return (it->second);
	}

	uint32_t rootSize = 0;
	if (sTy->hasDirectBase())
	{
		rootSize = cacheDowncastOffset(sTy->getDirectBase());
		downcastArrayIndices[sTy] = rootSize;
		errs() << "[cacheDowncastOffset] recursive outcome: " << rootSize << "\n";
		return rootSize;
	}

	// Calculates the size of the root struct,
	// The downcast array and offset are appended to the end of it
	for (uint32_t i = 0; i < sTy->getStructNumElements(); i++)
	{
		if (needsOffsetAsElement(sTy, i))
			rootSize++;
		rootSize++;
	}
	downcastArrayIndices[sTy] = rootSize;
	errs() << "[cacheDowncastOffset] offset: " << rootSize << "\n";
	return rootSize;
}

void CheerpWasmWriter::compileArrayType(Section& section, const ArrayType* aTy)
{
	errs() << "[compileArrayType] Encoding array type\n";
	encodeULEB128(0x5E, section);
	compileStorageType(section, aTy->getArrayElementType());
}

 void CheerpWasmWriter::compileStructType(Section& section, const StructType* sTy)
{
	if (sTy->hasName())
		errs() << "[compileStructType] compiling struct type: " << sTy->getName() << "\n";
	bool hasDowncastArray = (linearHelper.hasDowncastArray(sTy) && sTy->hasDirectBase());
	uint32_t elemCount = calculateAndCacheElemInfo(sTy);
	uint32_t downcastIndex = 0;

	encodeULEB128(0x5F, section);
	encodeULEB128(elemCount, section);


	errs() << "[compileStructType] sTy: " << *sTy << " is" << (hasDowncastArray ? " marked " : " not marked ") << "for downcast\n";
	if (hasDowncastArray)
	{
		assert(downcastArrayIndices.find(sTy) != downcastArrayIndices.end());
		assert(linearHelper.hasDowncastArray(sTy));
		downcastIndex = downcastArrayIndices[sTy];
	}
	errs() << "[compileStructType] sTy: " << *sTy << " has downcastIndex: " << downcastIndex << "\n";
	for (size_t i = 0; i < sTy->getStructNumElements(); i++)
	{
		Type* elemTy = sTy->getStructElementType(i);
		if (types.useWrapperArrayForMember(PA, const_cast<StructType*>(sTy), i))
			compileStorageType(section, ArrayType::get(elemTy, 1));
		else
			compileStorageType(section, elemTy);

		// If a pointer does not have a constant offset, add the offset into the struct
		if (needsOffsetAsElement(sTy, i))
		{
			errs() << "[compileStructType] Encoding extra offset element\n";
			encodeULEB128(0x7f, section);
			encodeULEB128(0x01, section);
		}

		// Adds the downcast array and its offset at the current index
		if (hasDowncastArray && i + 1 == downcastIndex)
		{
			// encode the .a as a split regular, an array of anyrefs
			encodeULEB128(0x63, section);
			encodeSLEB128(linearHelper.getSplitRegularObjectIdx(), section);
			encodeULEB128(0x01, section);

			// encode the .o as a mutable i32
			encodeULEB128(0x7f, section);
			encodeULEB128(0x01, section);
		}
	}
}

void CheerpWasmWriter::compileSubType(Section& section, const StructType* sTy)
{
	int tmpTypeIdx = linearHelper.getGCTypeIndex(sTy, COMPLETE_OBJECT);
	errs() << "[compileSubType] compiling subType: " << sTy->getName() << "\n";
	errs() << "[compileSubType] TypeIdx: " << tmpTypeIdx << "\n";
	// non-final subtype
	encodeULEB128(0x50, section);

	// If the struct has a direct base, encode that as its super type
	// else it is a sub-type of another type, so we encode the super-type vector as empty
	if (sTy->hasDirectBase())
	{
		const StructType* db = sTy->getDirectBase();
		const bool hasDowncastArray = linearHelper.hasDowncastArray(sTy);
		const int32_t directBaseIdx = linearHelper.getGCTypeIndex(db, COMPLETE_OBJECT, hasDowncastArray);
		encodeULEB128(1, section);
		encodeSLEB128(directBaseIdx, section);
	}
	else
	{
		encodeULEB128(0, section);
		assert(linearHelper.isSuperType(sTy));
	}

	// compiling the struct itself
	compileStructType(section, sTy);
}

void CheerpWasmWriter::compileRootStructWithDowncastArray(Section& section, const StructType* sTy)
{
	// non-final subtype
	encodeULEB128(0x50, section);

	// inherits from the root
	encodeULEB128(1, section);
	encodeSLEB128(linearHelper.getGCTypeIndex(sTy, COMPLETE_OBJECT), section);

	// encode the size of the struct, adding space for the offset and array
	uint32_t elemCount = calculateAndCacheElemInfo(sTy);
	encodeULEB128(0x5F, section);
	encodeULEB128(elemCount + 2, section);

	// unroll all of the root struct's types
	for (size_t i = 0; i < sTy->getStructNumElements(); i++)
	{
		Type* elemTy = sTy->getStructElementType(i);
		if (types.useWrapperArrayForMember(PA, const_cast<StructType*>(sTy), i))
			compileStorageType(section, ArrayType::get(elemTy, 1));
		else
			compileStorageType(section, elemTy);
		
		// If a pointer does not have a constant offset, add the offset into the struct
		if (needsOffsetAsElement(sTy, i))
		{
			encodeULEB128(0x7f, section);
			encodeULEB128(0x01, section);
		}
	}
	// encode the .a as a split regular, an array of anyrefs
	encodeULEB128(0x63, section);
	encodeSLEB128(linearHelper.getSplitRegularObjectIdx(), section);
	encodeULEB128(0x01, section);

	// encode the .o as a mutable i32
	encodeULEB128(0x7f, section);
	encodeULEB128(0x01, section);
}

void CheerpWasmWriter::compileTypeSection()
{
	if (linearHelper.getFunctionTypes().empty() && \
		linearHelper.getGCTypes().empty())
		return;

	Section section(0x01, "Type", this);
	uint32_t currentTypeIdx = 0;

	// Encode number of entries in the type section.
	errs() << "[compileTypeSection] encodingType count\n";
	// TODO: Right now we take all of the needed functions from the global deps,
	// this needs to be seperated into JS and WASM
	size_t typeCount = linearHelper.getGCTypeCount() + \
						linearHelper.getFunctionTypes().size() + \
						globalDeps.classesWithBaseInfo().size() + \
						globalDeps.needCreatePointerArray() + \
						(linearHelper.getTypesThatRequireCreateArrayFunc().size() > 0) + \
						linearHelper.getTypesUsedForDynResizeFuncTypes().size();
	encodeULEB128(typeCount, section);

	errs() << "[compileTypeSection] encodingAggregateTypes\n";
	// Define GC types
	for (const auto& Ty : linearHelper.getGCTypes())
	{
		errs() << "[compileTypeSection] encoding type idx: " << currentTypeIdx << ": " << *Ty << "\n";
		auto it = linearHelper.getGCTypeIndices().find(Ty);
		assert(it != linearHelper.getGCTypeIndices().end());
		assert(currentTypeIdx == (uint32_t) it->second);
		if (const StructType* sTy = dyn_cast<StructType>(Ty))
		{
			if (linearHelper.hasDowncastArray(sTy))
				cacheDowncastOffset(sTy);

			if (sTy->hasDirectBase() || linearHelper.isSuperType(sTy))
				compileSubType(section, sTy);
			else
				compileStructType(section, sTy);

			// A root class marked with a downcast array needs to have a second
			// version of itself that inherits from the root and appends
			// the offset and downcast array to it.
			//
			// This allows for classes that don't use a downcast to still inherit
			// from the root without the extra variables
			//
			// These extended classes are added right after the root,
			// allowing the use the index of the root + 1 to retrieve them
			if (!sTy->hasDirectBase() && linearHelper.hasDowncastArray(sTy))
			{
				currentTypeIdx++;
				assert(currentTypeIdx == (uint32_t) linearHelper.getGCTypeIndex(Ty, COMPLETE_OBJECT, true));
				compileRootStructWithDowncastArray(section, sTy);
			}
		}
		else if (const ArrayType* aTy = dyn_cast<ArrayType>(Ty))
			compileArrayType(section, aTy);
		else
			report_fatal_error("Unexpected type", false);
		currentTypeIdx++;
	}
	assert(currentTypeIdx == (uint32_t) linearHelper.getGCTypeCount());

	// Define function type variables
	errs() << "[compileTypeSection] encodingFunctionTypes\n";
	for (const auto& fTy : linearHelper.getFunctionTypes())
	{
		auto it = linearHelper.getFunctionTypeIndices().find(fTy);
		assert(it != linearHelper.getFunctionTypeIndices().end());
		assert(currentTypeIdx == it->second);
		errs() << "[compileTypeSection] adding functiontype: " << *fTy << "\n";
		encodeULEB128(0x60, section);
		compileMethodParams(section, fTy);
		compileMethodResult(section, fTy->getReturnType());
		currentTypeIdx++;
	}

	// Define the function types used for the downcast array initializers
	for (const auto& sTy : globalDeps.classesWithBaseInfo())
	{
		auto it = linearHelper.getDowncastFuncTypeIndices().find(sTy);
		assert(it != linearHelper.getDowncastFuncTypeIndices().end());
		assert(currentTypeIdx == it->second);
		const int32_t structTypeIdx = linearHelper.getGCTypeIndex(sTy, COMPLETE_OBJECT, true);
		encodeULEB128(0x60, section);
		// encode param
		encodeULEB128(1, section);
		encodeULEB128(0x63, section);
		encodeSLEB128(structTypeIdx, section);

		// encode result
		encodeULEB128(1, section);
		encodeULEB128(0x63, section);
		encodeSLEB128(structTypeIdx, section);
		currentTypeIdx++;
	}

	// Define the function type used for the CreatePointerArrayFunc function
	if (globalDeps.needCreatePointerArray())
	{
		assert(currentTypeIdx == (uint32_t) linearHelper.getCreatePointerArrayFuncTypeIndex());
		encodeULEB128(0x60, section);
		// encode params: (ref array, i32 startIndex, i32 size, ref initValue)
		encodeULEB128(4, section);
		encodeULEB128(0x63, section);
		encodeSLEB128(linearHelper.getSplitRegularObjectIdx(), section);
		encodeULEB128(0x7F, section);
		encodeULEB128(0x7F, section);
		encodeULEB128(0x6E, section);

		// encode the result
		encodeULEB128(1, section);
		encodeULEB128(0x63, section);
		encodeSLEB128(linearHelper.getSplitRegularObjectIdx(), section);
		currentTypeIdx++;
	}

	// Define the function type used for the CreateArrayFunc functions
	if (linearHelper.getCreateArrayFuncIds().size() > 0)
	{
		assert(currentTypeIdx == (uint32_t) linearHelper.getCreateArrayFuncTypeIndex());
		encodeULEB128(0x60, section);
		// encode param
		encodeULEB128(1, section);
		encodeULEB128(0x7F, section);

		// encode the result
		encodeULEB128(1, section);
		encodeULEB128(0x63, section);
		encodeSLEB128(linearHelper.getSplitRegularObjectIdx(), section);
		currentTypeIdx++;
	}

	for (auto Ty : linearHelper.getTypesUsedForDynResizeFuncTypes())
	{
		auto it = linearHelper.getDynResizeFuncTypeIndices().find(Ty);
		assert(it != linearHelper.getDynResizeFuncTypeIndices().end());
		assert(currentTypeIdx == it->second);
		int32_t arrayTypeIdx = linearHelper.getGCTypeIndex(Ty, SPLIT_REGULAR);
		encodeULEB128(0x60, section);
		// encode params: (ref oldArray, i32 oldSize, i32 newSize)
		encodeULEB128(3, section);
		encodeULEB128(0x63, section);
		encodeSLEB128(arrayTypeIdx, section);
		encodeULEB128(0x7F, section);
		encodeULEB128(0x7F, section);

		// encode the result
		encodeULEB128(1, section);
		encodeULEB128(0x63, section);
		encodeSLEB128(arrayTypeIdx, section);
		currentTypeIdx++;
	}

	section.encode();
}

void CheerpWasmWriter::compileImport(WasmBuffer& code, StringRef funcName, FunctionType* fTy)
{
	std::string fieldName;
	std::string moduleName;
	if (TypeSupport::isWasiFuncName(funcName))
	{
		 fieldName = TypeSupport::getWasiFuncName(funcName);
		 moduleName = "wasi_snapshot_preview1";
	}
	else
	{
		fieldName = funcName;
		moduleName = "i";
	}

	// Encode the module name.
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

void CheerpWasmWriter::compileImportMemory(WasmBuffer& code)
{
	// Define the memory for the module in WasmPage units. The heap size is
	// defined in MiB and the wasm page size is 64 KiB. Thus, the wasm heap
	// max size parameter is defined as: heapSize << 20 >> 16 = heapSize << 4.
	uint32_t maxMemory = heapSize << 4;
	uint32_t minMemory = (linearHelper.getHeapStart() + 65535) >> 16;
	if (noGrowMemory)
		minMemory = maxMemory;

	// Encode the module name.
	std::string moduleName = "i";
	encodeULEB128(moduleName.size(), code);
	code.write(moduleName.data(), moduleName.size());

	// Encode the field name.
	StringRef fieldName = namegen.getBuiltinName(NameGenerator::MEMORY);
	encodeULEB128(fieldName.size(), code);
	code.write(fieldName.data(), fieldName.size());

	// Encode kind as 'Memory'.
	encodeULEB128(0x02, code);

	// Encode limits of the memory.
	// from the spec:
	//limits ::= 0x00 n:u32          => {min n, max e, unshared}
	//           0x01 n:u32 m:u32    => {min n, max m, unshared}
	//           0x03 n:u32 m:u32    => {min n, max m, shared}
	// We use 0x01 and 0x03 only for now
	int memType = sharedMemory ? 0x03 : 0x01;
	encodeULEB128(memType, code);
	// Encode minimum and maximum memory parameters.
	encodeULEB128(minMemory, code);
	encodeULEB128(maxMemory, code);
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

	numberOfImportedFunctions = importedTotal;

	if (!useWasmLoader && importedTotal == 0)
		return;

	Section section(0x02, "Import", this);

	// Encode number of entries in the import section.
	// We import the memory in all cases, except when the target is WASI.
	encodeULEB128(importedTotal + useWasmLoader, section);

	// Import the memory if target is not WASI.
	if (useWasmLoader)
		compileImportMemory(section);

	for (const Function* F : globalDeps.asmJSImports())
	{
		StringRef name = useWasmLoader ? namegen.getName(F, 0) : F->getName();
		compileImport(section, name, F->getFunctionType());
	}

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

	section.encode();
}

void CheerpWasmWriter::compileFunctionSection()
{
	if (linearHelper.getFunctionTypes().empty())
		return;

	Section section(0x03, "Function", this);

	// TODO: filter with only the WasmGC types for all the globalDeps
	uint32_t count = linearHelper.functions().size() + \
						globalDeps.classesWithBaseInfo().size() + \
						globalDeps.needCreatePointerArray() + \
						globalDeps.dynAllocArrays().size() + \
						globalDeps.dynResizeArrays().size();
	count = std::min(count, COMPILE_METHOD_LIMIT); // TODO

	// Encode number of entries in the function section.
	encodeULEB128(count, section);

	// Define function type ids
	size_t i = numberOfImportedFunctions;
	for (const Function* F : linearHelper.functions()) {
		const FunctionType* fTy = linearHelper.getExpandedFunctionType(F);
		const auto& typeIt = linearHelper.getFunctionTypeIndices().find(fTy);
		const auto& indexIt = linearHelper.getFunctionIds().find(F);

		assert(typeIt != linearHelper.getFunctionTypeIndices().end());
		assert(indexIt != linearHelper.getFunctionIds().end());
		errs() << "[compileFunctionSection] Function: " << F->getName() << " should have index: " << indexIt->second << " i is currently: " << i << "\n";
		// TODO: Currently does not work since we hack in functions from the JS side,
		// Should work once we stop doing so.
		// assert(i == indexIt->second);

		encodeULEB128(typeIt->second, section);

		if (++i >= COMPILE_METHOD_LIMIT)
			break; // TODO
	}

	// Define the function type ids for the downcast initializer functions
	for (auto sTy : globalDeps.classesWithBaseInfo())
	{
		const auto& typeIt = linearHelper.getDowncastFuncTypeIndices().find(sTy);
		const auto& indexIt = linearHelper.getDowncastFuncIds().find(sTy);
		assert(typeIt != linearHelper.getDowncastFuncTypeIndices().end());
		assert(indexIt != linearHelper.getDowncastFuncIds().end());
		assert(i == indexIt->second);

		encodeULEB128(typeIt->second, section);
		if (++i >= COMPILE_METHOD_LIMIT)
			break; // TODO
	}

	// Define the function type id for the create pointer array function
	if (globalDeps.needCreatePointerArray())
	{
		assert(i == linearHelper.getCreatePointerArrayFuncId());
		encodeULEB128(linearHelper.getCreatePointerArrayFuncTypeIndex(), section);
		++i; // TODO
	}

	// Define the function type ids for the createDynamicArrayAlloc functions
	for (auto Ty : globalDeps.dynAllocArrays())
	{
		const auto& indexIt = linearHelper.getCreateArrayFuncIds().find(Ty);
		assert(indexIt != linearHelper.getCreateArrayFuncIds().end());
		assert(i == indexIt->second);

		encodeULEB128(linearHelper.getCreateArrayFuncTypeIndex(), section);
		if (++i >= COMPILE_METHOD_LIMIT)
			break; // TODO
	}

	// Define the function type ids for the resize array functions
	for (auto Ty : globalDeps.dynResizeArrays())
	{
		const auto& typeIt = linearHelper.getDynResizeFuncTypeIndices().find(Ty);
		const auto& indexIt = linearHelper.getResizeArrayFuncIds().find(Ty);
		assert(typeIt != linearHelper.getDynResizeFuncTypeIndices().end());
		assert(indexIt != linearHelper.getResizeArrayFuncIds().end());
		assert(i == indexIt->second);

		encodeULEB128(typeIt->second, section);
		if (++i >= COMPILE_METHOD_LIMIT)
			break; // TODO	
	}

	section.encode();
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
	if (exportedTable)
	{
		// Use a 'open ended limit' (= 0x00) with only a minimum value.
		encodeULEB128(0x00, section);
		encodeULEB128(count, section);
	}
	else
	{
		// Use 'range limit' (= 0x01) with equal minimum and maximum value (the table is static).
		encodeULEB128(0x01, section);
		encodeULEB128(count, section);
		encodeULEB128(count, section);
	}

	section.encode();
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

void CheerpWasmWriter::compileMemorySection()
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
		section.encode();
	}
}

void CheerpWasmWriter::compileGlobalSection()
{
	errs() << "[compileGlobalSection] START\n";
	// Temporary map for the globalized constants. We update the global one at the end, to avoid
	// global constants referencing each other
	std::unordered_map<const llvm::Constant*, std::pair<uint32_t, GLOBAL_CONSTANT_ENCODING>> globalizedConstantsTmp;
	std::unordered_map<const llvm::Constant*, uint32_t> orderOfInsertion;
	const LinearMemoryHelper::GlobalUsageMap& globalizedGlobalsUsage(linearHelper.getGlobalizedGlobalUsage());

	for (auto G : linearHelper.globals())
	{
		errs() << "[compileGlobalSection] " << (globalizedGlobalsUsage.count(G) ? "Adding" : "Skipping") <<" Global: " << G->getName() << " of type: " << *G->getType() << "\n";
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
						errs() << "[compileGlobalSection] the whole global " << C->getName() << " is globalized, dont globalize the address\n";
						// The whole global is globalized, there is no point in globalizing the address
						continue;
					}
					errs() << "[compileGlobalSection] adding constant: " << *C << "\n";
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
	uint32_t globalId = 4; // Skip stackTopGlobal, oSlotGlobal, nullArrayGlobal and nullObjGlobal
	for(uint32_t i=0;i<orderedConstants.size();i++)
	{
		GlobalConstant& GC = orderedConstants[i];
		errs() << "[compileGlobalSection] Global: " << *GC.C << "\n";
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
		int32_t stackTop = linearHelper.getStackStart();

		// The stack, oSlot, nullArray, nullObj and the globalized constants
		encodeULEB128(4 + globalizedConstantsTmp.size() + globalizedGlobalsIDs.size(), section);

		// The stack top global has type i32 (0x7f) and is mutable (0x01).
		encodeULEB128(0x7f, section);
		encodeULEB128(0x01, section);
		// The global value is a 'i32.const' literal.
		encodeLiteralType(Type::getInt32Ty(Ctx), section);
		encodeSLEB128(stackTop, section);
		// Encode the end of the instruction sequence.
		encodeULEB128(0x0b, section);

		// The oSlot global, i32 and mutable
		oSlotGlobal = usedGlobals++;
		encodeULEB128(0x7f, section);
		encodeULEB128(0x01, section);
		// Initialize it to 0
		encodeLiteralType(Type::getInt32Ty(Ctx), section);
		encodeSLEB128(0, section);
		// Encode the end of the instruction sequence.
		encodeULEB128(0x0b, section);

		// The nullArray global, reference of an array containing a null reference
		nullArrayGlobal = usedGlobals++;
		const int32_t splitRegIdx = linearHelper.getSplitRegularObjectIdx();
		// Immutable reference type
		encodeULEB128(0x63, section);
		encodeSLEB128(splitRegIdx, section);
		encodeULEB128(0x00, section);
		// Create the array
		encodeInst(WasmU32Opcode::REF_NULL, 0x6E, section);
		encodeInst(WasmGCOpcode::ARRAY_NEW_FIXED, splitRegIdx, 1, section);
		// Encode the end of the instruction sequence.
		encodeULEB128(0x0b, section);

		// The nullObj global, reference of a regular pointer kind struct containing the null array
		nullObjGlobal = usedGlobals++;
		const int32_t regularIdx = linearHelper.getRegularObjectIdx();
		// Immutable reference type
		encodeULEB128(0x63, section);
		encodeSLEB128(regularIdx, section);
		encodeULEB128(0x00, section);
		// Create the struct
		encodeInst(WasmU32Opcode::GET_GLOBAL, nullArrayGlobal, section); // .d
		encodeInst(WasmS32Opcode::I32_CONST, 0, section); // .o
		encodeInst(WasmGCOpcode::STRUCT_NEW, regularIdx, section);
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
				errs() << "[compileGlobalSection] Encoding global: " << GV->getName() << "\n";
				encodeValType(GV->getValueType(), section);
				// Mutable -> 1
				encodeULEB128(0x01, section);
				assert(GV->hasInitializer());
				errs() << "[compileGlobalSection] compilingConstant for global: " << GV->getName() << "\n";
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

		section.encode();
	}
	globalizedConstants = std::move(globalizedConstantsTmp);
	for (auto it = globalizedConstants.begin(); it != globalizedConstants.end(); it++)
		errs() << "[compileGLobalSection] In list: " << *it->first << "\n";
	errs() << "[compileGLobalSection] END\n";
}

void CheerpWasmWriter::compileExportSection()
{
	Section section(0x07, "Export", this);
	std::vector<const llvm::Function*> exports;

	// Add the list of asmjs-exported functions.
	exports.insert(exports.end(), globalDeps.asmJSExports().begin(),
			globalDeps.asmJSExports().end());

	// We may need to export the table and/or the memory.
	bool exportMemory = WasmExportedMemory || !useWasmLoader;
	uint32_t extraExports = uint32_t(exportedTable) + uint32_t(exportMemory);
	encodeULEB128(exports.size() + extraExports, section);

	if (exportMemory)
	{
		// Encode the memory.
		StringRef name = useWasmLoader? namegen.getBuiltinName(NameGenerator::MEMORY) : "memory";
		encodeULEB128(name.size(), section);
		section.write(name.data(), name.size());
		encodeULEB128(0x02, section);
		encodeULEB128(0, section);
	}

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
		StringRef name = useWasmLoader? namegen.getName(F, 0) : F->getName();

		encodeULEB128(name.size(), section);
		section.write(name.data(), name.size());

		// Encode the function index (where '0x00' means that this export is a
		// function).
		encodeULEB128(0x00, section);
		encodeULEB128(linearHelper.getFunctionIds().find(F)->second, section);
	}

	section.encode();
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

	section.encode();
}

void CheerpWasmWriter::compileDataCountSection()
{
	Section dataCountSection(0x0c, "Data Count", this);
	encodeULEB128(linearHelper.getAmountChunks(), dataCountSection);
	dataCountSection.encode();
}

void CheerpWasmWriter::compileCodeSection()
{
	Section codeSection(0x0a, "Code", this);
	Section branchHintsSection(0x0, "metadata.code.branch_hint", this);

	// TODO: filter with only the WasmGC types for all the globalDeps
	uint32_t count = linearHelper.functions().size() + \
						globalDeps.classesWithBaseInfo().size() + \
						globalDeps.needCreatePointerArray() +\
						linearHelper.getTypesThatRequireCreateArrayFunc().size() + \
						globalDeps.dynResizeArrays().size();
	count = std::min(count, COMPILE_METHOD_LIMIT);
	encodeULEB128(count, codeSection);		//Encode the number of Wasm functions
	uint32_t countHinted = 0;
	Chunk<128> branchHintsChunk;
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

		std::vector<std::pair<uint32_t, bool>> branchHintsVec;

		filterNop(method.buf(), [&branchHintsVec](uint32_t location, char byte)->void{
			const bool dir = (byte == (char)WasmInvalidOpcode::BRANCH_LIKELY);
			branchHintsVec.push_back({location, dir});
		});
		nopLocations.clear();

		if (!branchHintsVec.empty())
		{
			countHinted++;
			encodeULEB128(i+numberOfImportedFunctions, branchHintsChunk);		//Encode the ID of the current function

			encodeULEB128(branchHintsVec.size(), branchHintsChunk);		//Encode number of hints (possibly 0)
			for (auto x : branchHintsVec)
			{
				encodeULEB128(x.first, branchHintsChunk);			//Encode Instruction offset (in bytes, from the start of the function)
				encodeULEB128(0x01, branchHintsChunk);			//Encode length code annotation (that has to be the ULEB 1, possibly padded)
				encodeULEB128(x.second ? 0x01 : 0x00, branchHintsChunk);	//Encode direction of hint (that has to be the byte 1, with NO padding)
			}
		}

#if WASM_DUMP_METHOD_DATA
		llvm::errs() << "method length: " << method.tell() << '\n';
		llvm::errs() << "method: " << string_to_hex(method.str()) << '\n';
#endif
		encodeULEB128(method.tell(), codeSection);
		codeSection << method.str();

		if (++i == COMPILE_METHOD_LIMIT)
			break; // TODO
	}

	// add the downcast initalizer functions
	for (auto sTy : globalDeps.classesWithBaseInfo())
	{
		Chunk<128> downcastInitMethod;

		compileDowncastInitializer(downcastInitMethod, sTy);

#if WASM_DUMP_METHOD_DATA
		llvm::errs() << "downcastInitMethod length: " << downcastInitMethod.tell() << '\n';
		llvm::errs() << "downcastInitMethod: " << string_to_hex(downcastInitMethod.str()) << '\n';
#endif
		encodeULEB128(downcastInitMethod.tell(), codeSection);
		codeSection << downcastInitMethod.str();

		if (++i == COMPILE_METHOD_LIMIT)
			break; // TODO
	}

	// add the createPointerArray function
	if (globalDeps.needCreatePointerArray())
	{
		Chunk<128> createPointerArray;

		compileCreatePointerArrayFunc(createPointerArray);

#if WASM_DUMP_METHOD_DATA
		llvm::errs() << "createPointerArray length: " << createPointerArray.tell() << '\n';
		llvm::errs() << "createPointerArray: " << string_to_hex(createPointerArray.str()) << '\n';
#endif
		encodeULEB128(createPointerArray.tell(), codeSection);
		codeSection << createPointerArray.str();

		++i; // TODO
	}

	// add the createDynamicArrayAlloc initalizer functions
	for (auto Ty : linearHelper.getTypesThatRequireCreateArrayFunc())
	{
		Chunk<128> createDynAllocArray;

		compileCreateDynamicAllocArrayFunc(createDynAllocArray, Ty);

#if WASM_DUMP_METHOD_DATA
		llvm::errs() << "createDynAllocArray length: " << createDynAllocArray.tell() << '\n';
		llvm::errs() << "createDynAllocArray: " << string_to_hex(createDynAllocArray.str()) << '\n';
#endif
		encodeULEB128(createDynAllocArray.tell(), codeSection);
		codeSection << createDynAllocArray.str();

		if (++i == COMPILE_METHOD_LIMIT)
			break; // TODO
	}

	// add the resizeArray functions
	for (auto Ty : globalDeps.dynResizeArrays())
	{
		Chunk<128> resizeArrayFunc;

		compileResizeArrayFunc(resizeArrayFunc, Ty);

#if WASM_DUMP_METHOD_DATA
		llvm::errs() << "resizeArrayFunc length: " << resizeArrayFunc.tell() << '\n';
		llvm::errs() << "resizeArrayFunc: " << string_to_hex(resizeArrayFunc.str()) << '\n';
#endif
		encodeULEB128(resizeArrayFunc.tell(), codeSection);
		codeSection << resizeArrayFunc.str();

		if (++i == COMPILE_METHOD_LIMIT)
			break; // TODO
	}


	encodeULEB128(countHinted, branchHintsSection);	//Encode the number of Wasm functions
	branchHintsSection << branchHintsChunk.str();

	if (WasmBranchHints && countHinted > 0)
		branchHintsSection.encode();
	else
		branchHintsSection.discard();

	codeSection.encode();
}

void CheerpWasmWriter::encodeDataSectionChunk(WasmBuffer& data, uint32_t address, StringRef buf)
{
	// In the current version of WebAssembly, 0x00 encodes an active data segment on memory 0
	// (0x01 encodes passive data segment, and 0x02 an active data segment followed by actual memory index)
	// We encode the memory as passive, and initialize them in a custom function called by _start.
	encodeULEB128(1, data);
	// Prefix the number of bytes to the bytes vector.
	encodeULEB128(buf.size(), data);
	data.write(buf.data(), buf.size());
}

void CheerpWasmWriter::compileDataSection()
{
	Section section(0x0b, "Data", this);

	uint32_t amountChunks = linearHelper.getAmountChunks();
	encodeULEB128(amountChunks, section);

	for (uint32_t i = 0; i < amountChunks; i++)
	{
		const LinearMemoryHelper::GlobalDataChunk &chunk = linearHelper.getGlobalDataChunk(i);
		std::string buf(reinterpret_cast<const char *>(&chunk.view[0]), chunk.view.size());
		encodeDataSectionChunk(section, chunk.address, buf);
	}

	section.encode();
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

	section.encode();
}

void CheerpWasmWriter::compileModule()
{
	// return ;
	errs() << "\n\n\n\n\nCompiling Wasm Module\n";

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

	errs() << "\nCompiling Type Section\n";
	compileTypeSection();

	errs() << "\nCompiling Import Section\n";
	compileImportSection();

	errs() << "\nCompiling Function Section\n";
	compileFunctionSection();

	errs() << "\nCompiling Table Section\n";
	compileTableSection();

	if (!useWasmLoader)
		compileMemorySection();

	errs() << "\nCompiling GLobal Section\n";
	compileGlobalSection();

	errs() << "\nCompiling Export Section\n";
	compileExportSection();

	errs() << "\nCompiling Element Section\n";
	compileElementSection();

	errs() << "\nCompiling DataCount Section\n";
	compileDataCountSection();

	errs() << "\nCompiling Code Section\n";
	compileCodeSection();

	errs() << "\nCompiling Data Section\n";
	compileDataSection();

	if (prettyCode) {
		errs() << "\nCompiling Name Section\n";
		compileNameSection();
	}
}

void CheerpWasmWriter::makeWasm()
{
	compileModule();
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
	errs() << "\n[compileValues] start\n";
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
			constantPart += c/multiplier;
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
		if (v.multiplier == 0)
			continue;
		if (V2.empty() || V2.back().multiplier != v.multiplier)
		{
			V2.push_back(GroupedValuesToAdd(v.multiplier));
		}
		V2.back().add(v);
	}

	auto initializeYetToBeEncodedOffset = [this, &positiveOffsetAllowed](std::vector<GroupedValuesToAdd>& V2, bool& first) -> uint32_t
	{
		if (!positiveOffsetAllowed)
		{
			uint32_t toBeHandled = constPart;
			//In these cases constantPart should not be used, so it has to be merged in the GroupedValuesToAdd's vector
			if (toBeHandled != 0)
			{
				for (auto& grouped : V2)
				{
					if (toBeHandled % grouped.multiplier == 0)
					{
						grouped.addConstant(toBeHandled);
						toBeHandled = 0;
						break;
					}
				}
			}
			if (toBeHandled != 0)
			{
				V2.push_back(GroupedValuesToAdd(1));
					V2.back().addConstant(toBeHandled);
				toBeHandled = 0;
			}
			assert(toBeHandled == 0);
			return 0;
		}
		else if (std::none_of(V2.begin(), V2.end(), [](const GroupedValuesToAdd& g)->bool{return g.hasPositive();}))
		{
			//If we have to put a 0, it's always beneficial to put the maximum value that comes in the single byte encoding
			writer.encodeInst(WasmS32Opcode::I32_CONST, 0, code);
			first = false;
			return constPart;
		}
		else
		{
			return constPart;
		}
	};

	bool first = true;
	//The call to the lambda will properly initialize yetToBeEncodedOffset AND set first to true if something has been written in the stack
	const uint32_t yetToBeEncodedOffset = initializeYetToBeEncodedOffset(V2, first);

	std::stable_partition(V2.begin(), V2.end(), [](const GroupedValuesToAdd& g) -> bool { return g.hasPositive(); });

	if (first)
		assert(!V2.empty() && V2.front().hasPositive());

	for (GroupedValuesToAdd& p : V2)
	{
		p.sort(writer);
	}

	bool singlePositiveValue = true;
	for (const GroupedValuesToAdd& p : V2)
	{
		const bool invertSign = (p.hasPositive() == false) && (first == false);
		bool is_first = true;

		for (const llvm::Value* v : p.valuesToAdd)
		{
			writer.compileOperand(code, v);
			if (writer.needsUnsignedTruncation(v, /*asmjs*/true))
				singlePositiveValue = false;
			if (!is_first)
			{
				writer.encodeInst(WasmOpcode::I32_ADD, code);
				singlePositiveValue = false;
			}
			is_first = false;
		}

		if (p.constantPart)
		{
			writer.encodeInst(WasmS32Opcode::I32_CONST, p.constantPart, code);
			if (!is_first)
				writer.encodeInst(WasmOpcode::I32_ADD, code);
			singlePositiveValue = false;
			is_first = false;
		}

		for (const llvm::Value* v : p.valuesToInvert)
		{
			writer.compileOperand(code, v);
			if (!is_first)
			{
				if (invertSign)
					writer.encodeInst(WasmOpcode::I32_ADD, code);
				else
					writer.encodeInst(WasmOpcode::I32_SUB, code);
			}
			singlePositiveValue = false;
			is_first = false;
		}

		const uint32_t sizeCurr = p.multiplier * (invertSign ? -1 : 1);
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
			singlePositiveValue = false;
		}

		if(!first)
		{
			if (invertSign || p.hasPositive())
				writer.encodeInst(WasmOpcode::I32_ADD, code);
			else
				writer.encodeInst(WasmOpcode::I32_SUB, code);
		}
		first = false;
	}

	//In any case we have put something on the stack
	assert(!first);

	// We assume no access to the first 4k is legal, so a small offset can be safely encoded in the load/store
	if(!singlePositiveValue && yetToBeEncodedOffset > 4096)
	{
		writer.encodeInst(WasmS32Opcode::I32_CONST, yetToBeEncodedOffset, code);
		writer.encodeInst(WasmOpcode::I32_ADD, code);
		return 0;
	}

	return yetToBeEncodedOffset;
}

void CheerpWasmWriter::WasmGepWriter::addConst(int64_t v)
{
	assert(v);
	// Just make sure that the constant part of the offset is not too big
	// TODO: maybe use i64.const here instead of crashing
	assert(v>=std::numeric_limits<int32_t>::min());
	assert(v<=std::numeric_limits<uint32_t>::max());

	constPart += v;
}

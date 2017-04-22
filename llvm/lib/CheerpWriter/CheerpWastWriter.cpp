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

#include "Relooper.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/Cheerp/WastWriter.h"

using namespace cheerp;
using namespace llvm;
using namespace std;

class CheerpWastRenderInterface: public RenderInterface
{
private:
	CheerpWastWriter* writer;
	// IF must be the last one, we use values above it to count the amount of else ifs
	enum BLOCK_TYPE { WHILE1 = 0, DO, IF };
	uint32_t labelLocal;
	std::vector<BLOCK_TYPE> blockTypes;
	void renderCondition(const BasicBlock* B, int branchId);
	void indent();
public:
	const BasicBlock* lastDepth0Block;
	CheerpWastRenderInterface(CheerpWastWriter* w, uint32_t labelLocal):writer(w),labelLocal(labelLocal),lastDepth0Block(nullptr)
	{
	}
	void renderBlock(const void* privateBlock);
	void renderLabelForSwitch(int labelId);
	void renderSwitchOnLabel();
	void renderCaseOnLabel(int labelId);
	void renderSwitchBlockBegin(const void* privateBranchVar);
	void renderCaseBlockBegin(const void* privateBlock, int branchId);
	void renderDefaultBlockBegin();
	void renderIfBlockBegin(const void* privateBlock, int branchId, bool first);
	void renderIfBlockBegin(const void* privateBlock, const vector<int>& branchId, bool first);
	void renderElseBlockBegin();
	void renderBlockEnd();
	void renderBlockPrologue(const void* privateBlockTo, const void* privateBlockFrom);
	bool hasBlockPrologue(const void* privateBlockTo, const void* privateBlockFrom) const;
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

void CheerpWastRenderInterface::renderBlock(const void* privateBlock)
{
	const BasicBlock* bb=(const BasicBlock*)privateBlock;
	if(blockTypes.empty())
		lastDepth0Block = bb;
	else
		lastDepth0Block = nullptr;
	writer->compileBB(*bb);
}

void CheerpWastRenderInterface::indent()
{
	for(uint32_t i=0;i<blockTypes.size();i++)
		writer->stream << "  ";
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
		writer->compileOperand(bi->getCondition());
	}
	else if(isa<SwitchInst>(term))
	{
		const SwitchInst* si=cast<SwitchInst>(term);
		assert(branchId > 0);
		SwitchInst::ConstCaseIt it=si->case_begin();
		for(int i=1;i<branchId;i++)
			++it;
		const BasicBlock* dest=it.getCaseSuccessor();
		writer->compileOperand(si->getCondition());
		writer->stream << '\n';
		writer->compileOperand(it.getCaseValue());
		writer->stream << "\ni32.eq";
		//We found the destination, there may be more cases for the same
		//destination though
		for(++it;it!=si->case_end();++it)
		{
			if(it.getCaseSuccessor()==dest)
			{
				//Also add this condition
				writer->stream << '\n';
				writer->compileOperand(si->getCondition());
				writer->stream << '\n';
				writer->compileOperand(it.getCaseValue());
				writer->stream << "\ni32.eq\ni32.or";
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
assert(false);
#if 0
	writer->stream << 'L' << labelId << ':';
#endif
}

void CheerpWastRenderInterface::renderSwitchOnLabel()
{
assert(false);
#if 0
	writer->stream << "switch(label";
	if (asmjs)
		writer->stream << "|0";
	writer->stream << "){" << NewLine;
#endif
}

void CheerpWastRenderInterface::renderCaseOnLabel(int labelId)
{
assert(false);
#if 0
	writer->stream << "case ";
	writer->stream << labelId << ":{" << NewLine;
#endif
}

void CheerpWastRenderInterface::renderSwitchBlockBegin(const void* privateBranchVar)
{
assert(false);
#if 0
	const Value* cond = (const Value*)privateBranchVar;
	writer->stream << "switch(";
	CheerpWriter::PARENT_PRIORITY myPrio = asmjs?CheerpWriter::BIT_OR:CheerpWriter::LOWEST;
	writer->compileOperand(cond,myPrio);
	if (asmjs)
		writer->stream << "|0";
	writer->stream << "){" << NewLine;
#endif
}

void CheerpWastRenderInterface::renderCaseBlockBegin(const void* privateBlock, int branchId)
{
assert(false);
#if 0
	const BasicBlock* bb=(const BasicBlock*)privateBlock;
	const TerminatorInst* term = bb->getTerminator();
	assert(isa<SwitchInst>(term));
	const SwitchInst* si=cast<SwitchInst>(term);
	assert(branchId > 0);
	SwitchInst::ConstCaseIt it=si->case_begin();
	for(int i=1;i<branchId;i++)
		++it;
	writer->stream << "case ";
	writer->compileOperand(it.getCaseValue());
	writer->stream << ':' << NewLine;
	//We found the destination, there may be more cases for the same
	//destination though
	const BasicBlock* dest=it.getCaseSuccessor();
	for(++it;it!=si->case_end();++it)
	{
		if(it.getCaseSuccessor()==dest)
		{
			writer->stream << "case ";
			writer->compileOperand(it.getCaseValue());
			writer->stream << ':' << NewLine;
		}
	}
	writer->stream << '{' << NewLine;
#endif
}

void CheerpWastRenderInterface::renderDefaultBlockBegin()
{
assert(false);
#if 0
	writer->stream << "default:{" << NewLine;
#endif
}

void CheerpWastRenderInterface::renderIfBlockBegin(const void* privateBlock, int branchId, bool first)
{
	const BasicBlock* bb=(const BasicBlock*)privateBlock;
	if(!first)
	{
		indent();
		writer->stream << "else\n";
	}
	// The condition goes first
	renderCondition(bb, branchId);
	writer->stream << '\n';
	indent();
	writer->stream << "if\n";
	if(first)
		blockTypes.push_back(IF);
	else
	{
		assert(blockTypes.back()>=IF);
		blockTypes.back() = BLOCK_TYPE(blockTypes.back() + 1);
	}
}

void CheerpWastRenderInterface::renderIfBlockBegin(const void* privateBlock, const std::vector<int>& skipBranchIds, bool first)
{
	const BasicBlock* bb=(const BasicBlock*)privateBlock;
	// The condition goes first
	for(uint32_t i=0;i<skipBranchIds.size();i++)
	{
		if(i!=0)
		{
assert(false);
#if 0
			writer->stream << "||";
#endif
		}
		renderCondition(bb, skipBranchIds[i]);
		writer->stream << '\n';
	}
	// Invert result
	writer->stream << "i32.const 1\n";
	writer->stream << "i32.xor\n";
	assert(first);
#if 0
	if(!first)
		writer->stream << "}else ";
#endif
	indent();
	writer->stream << "if\n";
	blockTypes.push_back(IF);
}

void CheerpWastRenderInterface::renderElseBlockBegin()
{
	assert(!blockTypes.empty());
	assert(blockTypes.back() >= IF);
	indent();
	writer->stream << "else\n";
}

void CheerpWastRenderInterface::renderBlockEnd()
{
	assert(!blockTypes.empty());
	BLOCK_TYPE bt = blockTypes.back();
	blockTypes.pop_back();
	if(bt == WHILE1)
	{
		// TODO: Why do we even need to fake value
		writer->stream << "i32.const 0\n";
		writer->stream << "br 1\n";
		writer->stream << "end\n";
		writer->stream << "end\n";
	}
	else if(bt >= IF)
	{
		for(uint32_t i=0;i<(bt - IF)+1;i++)
		{
			indent();
			writer->stream << "end\n";
		}
	}
	else
		assert(false);
}

void CheerpWastRenderInterface::renderBlockPrologue(const void* privateBlockTo, const void* privateBlockFrom)
{
	const BasicBlock* bbTo=(const BasicBlock*)privateBlockTo;
	const BasicBlock* bbFrom=(const BasicBlock*)privateBlockFrom;
	writer->compilePHIOfBlockFromOtherBlock(bbTo, bbFrom);
}

bool CheerpWastRenderInterface::hasBlockPrologue(const void* privateBlockTo, const void* privateBlockFrom) const
{
	const BasicBlock* to=(const BasicBlock*)privateBlockTo;
	const BasicBlock* from=(const BasicBlock*)privateBlockFrom;

	if (to->getFirstNonPHI()==&to->front())
		return false;

	// We can avoid assignment from the same register if no pointer kind
	// conversion is required
	return writer->needsPointerKindConversionForBlocks(to, from);
}

void CheerpWastRenderInterface::renderWhileBlockBegin()
{
	// Wrap a block in a loop so that:
	// br 1 -> break
	// br 2 -> continue
	indent();
	writer->stream << "loop\n";
	indent();
	writer->stream << "block\n";
	blockTypes.push_back(WHILE1);
}

void CheerpWastRenderInterface::renderWhileBlockBegin(int blockLabel)
{
	// Wrap a block in a loop so that:
	// br 1 -> break
	// br 2 -> continue
	indent();
	writer->stream << "loop $c" << blockLabel << "\n";
	indent();
	writer->stream << "block $" << blockLabel << "\n";
	blockTypes.push_back(WHILE1);
}

void CheerpWastRenderInterface::renderDoBlockBegin()
{
	indent();
	writer->stream << "block\n";
	blockTypes.push_back(DO);
}

void CheerpWastRenderInterface::renderDoBlockBegin(int blockLabel)
{
	indent();
	writer->stream << "block $" << blockLabel << "\n";
	blockTypes.push_back(DO);
}

void CheerpWastRenderInterface::renderDoBlockEnd()
{
	assert(!blockTypes.empty());
#ifndef NDEBUG
	BLOCK_TYPE bt = blockTypes.back();
#endif
	blockTypes.pop_back();
	assert(bt == DO);
	indent();
	writer->stream << "end\n";
}

void CheerpWastRenderInterface::renderBreak()
{
	writer->stream << "br 1\n";
}

void CheerpWastRenderInterface::renderBreak(int labelId)
{
	// DO blocks only have one label
	// WHILE1 blocks have the "block" without a prefix
	writer->stream << "br $" << labelId << '\n';
}

void CheerpWastRenderInterface::renderContinue()
{
	// Find the first while block
	uint32_t breakIndex = 0;
	for(uint32_t i=0;i<blockTypes.size();i++)
	{
		if(blockTypes[blockTypes.size() - i - 1] == DO)
			breakIndex++;
		else if(blockTypes[blockTypes.size() - i - 1] == WHILE1)
			break;
	}
	writer->stream << "br " << (breakIndex+2) << "\n";
}

void CheerpWastRenderInterface::renderContinue(int labelId)
{
	writer->stream << "br $c" << labelId << '\n';
}

void CheerpWastRenderInterface::renderLabel(int labelId)
{
	writer->stream << "i32.const " << labelId << '\n';
	writer->stream << "set_local " << labelLocal << '\n';
}

void CheerpWastRenderInterface::renderIfOnLabel(int labelId, bool first)
{
	// TODO: Use first to optimize dispatch
	writer->stream << "i32.const " << labelId << '\n';
	writer->stream << "get_local " << labelLocal << '\n';
	writer->stream << "i32.eq\n";
	indent();
	writer->stream << "if\n";
	blockTypes.push_back(IF);
}

bool CheerpWastWriter::needsPointerKindConversion(const Instruction* phi, const Value* incoming)
{
	const Instruction* incomingInst=dyn_cast<Instruction>(incoming);
	if(!incomingInst)
		return true;
	return isInlineable(*incomingInst, PA) ||
		registerize.getRegisterId(phi)!=registerize.getRegisterId(incomingInst);
}


bool CheerpWastWriter::needsPointerKindConversionForBlocks(const BasicBlock* to, const BasicBlock* from)
{
	class PHIHandler: public EndOfBlockPHIHandler
	{
	public:
		PHIHandler(CheerpWastWriter& w):EndOfBlockPHIHandler(w.PA),needsPointerKindConversion(false),writer(w)
		{
		}
		~PHIHandler()
		{
		}
		bool needsPointerKindConversion;
	private:
		CheerpWastWriter& writer;
		void handleRecursivePHIDependency(const Instruction* incoming) override
		{
		}
		void handlePHI(const Instruction* phi, const Value* incoming) override
		{
			needsPointerKindConversion |= writer.needsPointerKindConversion(phi, incoming);
		}
	};

	auto handler = PHIHandler(*this);
	handler.runOnEdge(registerize, from, to);
	return handler.needsPointerKindConversion;
}

void CheerpWastWriter::compilePHIOfBlockFromOtherBlock(const BasicBlock* to, const BasicBlock* from)
{
	class WriterPHIHandler: public EndOfBlockPHIHandler
	{
	public:
		WriterPHIHandler(CheerpWastWriter& w, const BasicBlock* f, const BasicBlock* t):EndOfBlockPHIHandler(w.PA),writer(w),fromBB(f),toBB(t)
		{
		}
		~WriterPHIHandler()
		{
		}
	private:
		CheerpWastWriter& writer;
		const BasicBlock* fromBB;
		const BasicBlock* toBB;
		void handleRecursivePHIDependency(const Instruction* incoming) override
		{
			assert(incoming);
			writer.stream << "get_local " << (1 + writer.currentFun->arg_size() + writer.registerize.getRegisterId(incoming)) << '\n';
			writer.stream << "set_local " << (1 + writer.currentFun->arg_size() + writer.registerize.getRegisterIdForEdge(incoming, fromBB, toBB)) << '\n';
		}
		void handlePHI(const Instruction* phi, const Value* incoming) override
		{
			// We can avoid assignment from the same register if no pointer kind conversion is required
			if(!writer.needsPointerKindConversion(phi, incoming))
				return;
			// 1) Put the value on the stack
			writer.registerize.setEdgeContext(fromBB, toBB);
			writer.compileOperand(incoming);
			writer.registerize.clearEdgeContext();
			// 2) Save the value in the phi
			writer.stream << "\nset_local " << (1 + writer.currentFun->arg_size() + writer.registerize.getRegisterId(phi)) << '\n';
		}
	};
	WriterPHIHandler(*this, from, to).runOnEdge(registerize, from, to);
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

void CheerpWastWriter::compileGEP(const llvm::User* gep_inst)
{
	WastGepWriter gepWriter(*this);
	const llvm::Value *p = linearHelper.compileGEP(gep_inst, &gepWriter);
	compileOperand(p);
	if(!gepWriter.first)
		stream << "\ni32.add";
}

void CheerpWastWriter::compileConstantExpr(const ConstantExpr* ce)
{
	switch(ce->getOpcode())
	{
		case Instruction::GetElementPtr:
		{
			compileGEP(ce);
			break;
		}
		case Instruction::BitCast:
		{
			assert(ce->getOperand(0)->getType()->isPointerTy());
			compileOperand(ce->getOperand(0));
			break;
		}
		case Instruction::IntToPtr:
		{
			compileOperand(ce->getOperand(0));
			break;
		}
		case Instruction::ICmp:
		{
			compileOperand(ce->getOperand(0));
			stream << '\n';
			compileOperand(ce->getOperand(1));
			stream << '\n';
			stream << getTypeString(ce->getOperand(0)->getType()) << '.' << getIntegerPredicate((CmpInst::Predicate)ce->getPredicate());
			break;
		}
#if 0
		case Instruction::PtrToInt:
		{
			compilePtrToInt(ce->getOperand(0));
			break;
		}
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
			stream << "undefined";
			llvm::errs() << "warning: Unsupported constant expr " << ce->getOpcodeName() << '\n';
	}
}

void CheerpWastWriter::compileConstant(const Constant* c)
{
	if(const ConstantExpr* CE = dyn_cast<ConstantExpr>(c))
	{
		compileConstantExpr(CE);
	}
	else if(const ConstantInt* i=dyn_cast<ConstantInt>(c))
	{
		stream << getTypeString(i->getType()) << ".const ";
		if(i->getBitWidth()==32)
			stream << i->getSExtValue();
		else
			stream << i->getZExtValue();
	}
	else if(const ConstantFP* f=dyn_cast<ConstantFP>(c))
	{
		stream << getTypeString(f->getType()) << ".const ";
		if(f->getValueAPF().isInfinity())
		{
			if(f->getValueAPF().isNegative())
				stream << '-';
			stream << "infinity";
		}
		else if(f->getValueAPF().isNaN())
		{
			stream << "nan";
		}
		else
		{
			APFloat apf = f->getValueAPF();
			char buf[40];
			// TODO: Figure out the right amount of hexdigits
			unsigned charCount = apf.convertToHexString(buf, f->getType()->isFloatTy() ? 8 : 16, false, APFloat::roundingMode::rmNearestTiesToEven);
			assert(charCount < 40);
			stream << buf;
		}
	}
	else if(const GlobalVariable* GV = dyn_cast<GlobalVariable>(c))
	{
		stream << "i32.const " << linearHelper.getGlobalVariableAddress(GV);
	}
	else if(isa<ConstantPointerNull>(c))
	{
		stream << "i32.const 0";
	}
	else
	{
		c->dump();
		assert(false);
	}
}

void CheerpWastWriter::compileOperand(const llvm::Value* v)
{
	if(const Constant* c=dyn_cast<Constant>(v))
		compileConstant(c);
	else if(const Instruction* it=dyn_cast<Instruction>(v))
	{
		if(isInlineable(*it, PA))
			compileInstruction(*it);
		else
			stream << "get_local " << (1 + currentFun->arg_size() + registerize.getRegisterId(it));
	}
	else if(const Argument* arg=dyn_cast<Argument>(v))
	{
		stream << "get_local " << arg->getArgNo();
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
		case CmpInst::ICMP_SGT:
			return "gt_s";
		case CmpInst::ICMP_SLT:
			return "lt_s";
		case CmpInst::ICMP_UGE:
			return "ge_u";
		case CmpInst::ICMP_UGT:
			return "gt_u";
		case CmpInst::ICMP_ULT:
			return "lt_u";
		default:
			llvm::errs() << "Handle predicate " << p << "\n";
			break;
	}
	return "";
}

bool CheerpWastWriter::compileInstruction(const Instruction& I)
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
			assert (!ai->isArrayAllocation());
			assert((alignment & (alignment-1)) == 0 && "alignment must be power of 2");
			// We grow the stack down for now
			// 1) Push the current stack pointer
			stream << "get_global " << stackTopGlobal << '\n';
			// 2) Push the allocation size
			stream << "i32.const " << size << '\n';
			// 3) Substract the size
			stream << "i32.sub\n";
			// 3.1) Optionally align the stack down
			if(size % alignment)
			{
				stream << "i32.const " << uint32_t(0-alignment) << '\n';
				stream << "i32.and\n";
			}
			// 4) Write the location to the local, but preserve the value
			stream << "tee_local " << (1 + currentFun->arg_size() + registerize.getRegisterId(&I)) << '\n';
			// 5) Save the new stack position
			stream << "set_global " << stackTopGlobal << '\n';
			return true;
		}
		case Instruction::Add:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".add";
			break;
		}
		case Instruction::And:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".and";
			break;
		}
		case Instruction::AShr:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".shr_s";
			break;
		}
		case Instruction::BitCast:
		{
			assert(I.getType()->isPointerTy());
			compileOperand(I.getOperand(0));
			break;
		}
		case Instruction::Br:
			break;
		case Instruction::Call:
		{
			const CallInst& ci = cast<CallInst>(I);
			const Function * calledFunc = ci.getCalledFunction();
			const Value * calledValue = ci.getCalledValue();
			const PointerType* pTy = cast<PointerType>(calledValue->getType());
			const FunctionType* fTy = cast<FunctionType>(pTy->getElementType());
			assert(!ci.isInlineAsm());
			assert(!fTy->isVarArg());
			assert(calledFunc);
			assert(functionIds.count(calledFunc));
			for(unsigned i=0;i<ci.getNumArgOperands();i++)
			{
				compileOperand(ci.getOperand(i));
				stream << '\n';
			}
			stream << "call " << functionIds[calledFunc];
			if(ci.getType()->isVoidTy())
			{
				stream << '\n';
				return true;
			}
			break;
		}
		case Instruction::FAdd:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".add";
			break;
		}
		case Instruction::FCmp:
		{
			const CmpInst& ci = cast<CmpInst>(I);
			compileOperand(ci.getOperand(0));
			stream << '\n';
			compileOperand(ci.getOperand(1));
			stream << '\n';
			stream << getTypeString(ci.getOperand(0)->getType()) << '.';
			switch(ci.getPredicate())
			{
				// TODO: Handle ordered vs unordered
				case CmpInst::FCMP_OEQ:
					stream << "eq";
					break;
				case CmpInst::FCMP_OGT:
					stream << "gt";
					break;
				case CmpInst::FCMP_OLE:
					stream << "le";
					break;
				case CmpInst::FCMP_OLT:
					stream << "lt";
					break;
				case CmpInst::FCMP_UNE:
					stream << "ne";
					break;
				default:
					llvm::errs() << "Handle predicate for " << ci << "\n";
					break;
			}
			break;
		}
		case Instruction::FDiv:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".div";
			break;
		}
		case Instruction::FMul:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".mul";
			break;
		}
		case Instruction::FSub:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".sub";
			break;
		}
		case Instruction::GetElementPtr:
		{
			compileGEP(&I);
			break;
		}
		case Instruction::ICmp:
		{
			const CmpInst& ci = cast<CmpInst>(I);
			compileOperand(ci.getOperand(0));
			stream << '\n';
			compileOperand(ci.getOperand(1));
			stream << '\n';
			stream << getTypeString(ci.getOperand(0)->getType()) << '.' << getIntegerPredicate(ci.getPredicate());
			break;
		}
		case Instruction::Load:
		{
			const LoadInst& li = cast<LoadInst>(I);
			const Value* ptrOp=li.getPointerOperand();
			// 1) The pointer
			compileOperand(ptrOp);
			stream << '\n';
			// 2) Load
			stream << getTypeString(li.getType()) << ".load";
			if(li.getType()->isIntegerTy())
			{
				uint32_t bitWidth = li.getType()->getIntegerBitWidth();
				if(bitWidth<32)
				{
					assert(bitWidth == 8 || bitWidth == 16);
					// Currently assume unsigned, like Cheerp. We may optimize this be looking at a following sext or zext instruction.
					stream << bitWidth << "_u";
				}
			}
			break;
		}
		case Instruction::LShr:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".shr_u";
			break;
		}
		case Instruction::Mul:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".mul";
			break;
		}
		case Instruction::Or:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".or";
			break;
		}
		case Instruction::PtrToInt:
		{
			compileOperand(I.getOperand(0));
			break;
		}
		case Instruction::Shl:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".shl";
			break;
		}
		case Instruction::Store:
		{
			const StoreInst& si = cast<StoreInst>(I);
			const Value* ptrOp=si.getPointerOperand();
			const Value* valOp=si.getValueOperand();
			// 1) The pointer
			compileOperand(ptrOp);
			stream << '\n';
			// 2) The value
			compileOperand(valOp);
			stream << '\n';
			// 3) Store
			stream << getTypeString(valOp->getType()) << ".store";
			// When storing values with size less than 32-bit we need to truncate them
			if(valOp->getType()->isIntegerTy())
			{
				uint32_t bitWidth = valOp->getType()->getIntegerBitWidth();
				if(bitWidth<32)
				{
					assert(bitWidth == 8 || bitWidth == 16);
					stream << bitWidth;
				}
			}
			stream << '\n';
			break;
		}
		case Instruction::Sub:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".sub";
			break;
		}
		case Instruction::Switch:
			break;
		case Instruction::Trunc:
		{
			// TODO: We need to mask the value
			compileOperand(I.getOperand(0));
			break;
		}
		case Instruction::Ret:
		{
			const ReturnInst& ri = cast<ReturnInst>(I);
			Value* retVal = ri.getReturnValue();
			if(retVal)
			{
				compileOperand(I.getOperand(0));
				stream << '\n';
			}
			// Restore old stack
			stream << "get_local " << currentFun->arg_size() << "\n";
			stream << "set_global " << stackTopGlobal << '\n';
			stream << "return\n";
			break;
		}
		case Instruction::SDiv:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".div_s";
			break;
		}
		case Instruction::Select:
		{
			const SelectInst& si = cast<SelectInst>(I);
			compileOperand(si.getTrueValue());
			stream << '\n';
			compileOperand(si.getFalseValue());
			stream << '\n';
			compileOperand(si.getCondition());
			stream << '\n';
			stream << "select";
			break;
		}
		case Instruction::SExt:
		{
			uint32_t bitWidth = I.getOperand(0)->getType()->getIntegerBitWidth();
			compileOperand(I.getOperand(0));
			stream << "\ni32.const " << (32-bitWidth) << '\n';
			stream << "i32.shl\n";
			stream << "i32.const " << (32-bitWidth) << '\n';
			stream << "i32.shr_s";
			break;
		}
		case Instruction::SIToFP:
		{
			assert(I.getOperand(0)->getType()->isIntegerTy(32));
			compileOperand(I.getOperand(0));
			stream << '\n' << getTypeString(I.getType()) << ".convert_s/" << getTypeString(I.getOperand(0)->getType());
			break;
		}
		case Instruction::Xor:
		{
			compileOperand(I.getOperand(0));
			stream << '\n';
			compileOperand(I.getOperand(1));
			stream << '\n';
			stream << getTypeString(I.getType()) << ".xor";
			break;
		}
		case Instruction::ZExt:
		{
			uint32_t bitWidth = I.getOperand(0)->getType()->getIntegerBitWidth();
			compileOperand(I.getOperand(0));
			stream << "\ni32.const " << getMaskForBitWidth(bitWidth) << '\n';
			stream << "i32.and";
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

void CheerpWastWriter::compileBB(const BasicBlock& BB)
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
		if(I->isTerminator() || !I->use_empty() || I->mayHaveSideEffects())
		{
			if(!compileInstruction(*I) && !I->getType()->isVoidTy() && !I->use_empty())
				stream << "\nset_local " << (currentFun->arg_size() + registerize.getRegisterId(I)) << '\n';
		}
	}
}

void CheerpWastWriter::compileMethodLocals(const Function& F, bool needsLabel)
{
	uint32_t numArgs = F.arg_size();
	const std::vector<Registerize::RegisterInfo>& regsInfo = registerize.getRegistersForFunction(&F);
	uint32_t numRegs = regsInfo.size();
	if(numArgs == 0 && numRegs == 0)
		return;
	// The first local after ther params stores the previous stack address
	stream << "(local i32";
	// Emit the registers, careful as the registerize id is offset by the number of args
	for(const Registerize::RegisterInfo& regInfo: regsInfo)
	{
		stream << ' ';
		assert(regInfo.regKind != Registerize::OBJECT);
		assert(!regInfo.needsSecondaryName);
		switch(regInfo.regKind)
		{
			case Registerize::DOUBLE:
				stream << "f64";
				break;
			case Registerize::FLOAT:
				stream << "f32";
				break;
			case Registerize::INTEGER:
				stream << "i32";
				break;
			default:
				assert(false);
		}
	}
	// If needed, label is the very last local
	if(needsLabel)
		stream << " i32";
	stream << ")\n";
}

void CheerpWastWriter::compileMethod(const Function& F)
{
	currentFun = &F;
	stream << "(func ";
	// TODO: We should not export them all
	stream << "(export \"" << F.getName() << "\")";
	uint32_t numArgs = F.arg_size();
	if(numArgs)
	{
		stream << "(param";
		llvm::FunctionType* FTy = F.getFunctionType();
		for(uint32_t i = 0; i < numArgs; i++)
			stream << ' ' << getTypeString(FTy->getParamType(i));
		stream << ')';
	}
	if(!F.getReturnType()->isVoidTy())
		stream << "(result " << getTypeString(F.getReturnType()) << ')';
	stream << '\n';
	const llvm::BasicBlock* lastDepth0Block = nullptr;
	if(F.size() == 1)
	{
		compileMethodLocals(F, false);
		// TODO: Only save the stack address if required
		stream << "get_global " << stackTopGlobal << '\n';
		stream << "set_local " << numArgs << "\n";
		compileBB(*F.begin());
		lastDepth0Block = &(*F.begin());
	}
	else
	{
		Relooper* rl = CheerpWriter::runRelooperOnFunction(F);
		compileMethodLocals(F, rl->needsLabel());
		// TODO: Only save the stack address if required
		stream << "get_global " << stackTopGlobal << '\n';
		stream << "set_local " << numArgs << "\n";
		uint32_t numArgs = F.arg_size();
		const std::vector<Registerize::RegisterInfo>& regsInfo = registerize.getRegistersForFunction(&F);
		uint32_t numRegs = regsInfo.size();
		// label is the very last local
		CheerpWastRenderInterface ri(this, 1+numArgs+numRegs);
		rl->Render(&ri);
		lastDepth0Block = ri.lastDepth0Block;
	}
	// A function has to terminate with a return instruction
	if(!lastDepth0Block || !isa<ReturnInst>(lastDepth0Block->getTerminator()))
	{
		// Add a fake return
		if(!F.getReturnType()->isVoidTy())
			stream << getTypeString(F.getReturnType()) << ".const 0\n";
		stream << "return\n";
	}
	stream << ")\n";
}

void CheerpWastWriter::compileDataSection()
{
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
			stream << "(data (i32.const " << linearHelper.getGlobalVariableAddress(&GV) << ") \"";
			WastBytesWriter bytesWriter(stream);
			linearHelper.compileConstantAsBytes(init,/* asmjs */ true, &bytesWriter);
			stream << "\")\n";
		}
	}
}

void CheerpWastWriter::makeWast()
{
	// Emit S-expressions for the module
	stream << "(module\n";

	// Define the memory for the module (these should be parameter, they are min and max in WasmPage units)
	uint32_t minMemory = 1;
	uint32_t maxMemory = 2;
	stream << "(memory " << minMemory << ' ' << maxMemory << ")\n";

	// Assign globals in the module, these are used for codegen they are not part of the user program
	stackTopGlobal = usedGlobals++;
	// Start the stack from the end of default memory
	stream << "(global (mut i32) (i32.const " << (minMemory*WasmPage) << "))\n";
	
	// First run, assing required Ids to functions and globals
	for ( const Function & F : module.getFunctionList() )
	{
		if (!F.empty() && F.getSection() == StringRef("asmjs"))
		{
			functionIds.insert(std::make_pair(&F, functionIds.size()));
		}
	}

	// Experimental entry point for wast code
	llvm::Function* wastStart = module.getFunction("_Z9wastStartv");
	if(wastStart)
	{
		assert(functionIds.count(wastStart));
		stream << "(start " << functionIds[wastStart] << ")\n";
	}

	for ( const GlobalVariable & GV : module.getGlobalList() )
	{
		if (GV.getSection() != StringRef("asmjs"))
			continue;
		linearHelper.addGlobalVariable(&GV);
	}

	// Second run, actually compile the code
	for ( const Function & F : module.getFunctionList() )
	{
		if (!F.empty() && F.getSection() == StringRef("asmjs"))
		{
			compileMethod(F);
		}
	}

	compileDataSection();
	
	stream << ')';
}

void CheerpWastWriter::WastBytesWriter::addByte(uint8_t byte)
{
	char buf[4];
	snprintf(buf, 4, "\\%02x", byte);
	stream << buf;
}

void CheerpWastWriter::WastGepWriter::addValue(const llvm::Value* v, uint32_t size)
{
	writer.compileOperand(v);
	writer.stream << '\n';
	if(size != 1)
	{
		writer.stream << "i32.const " << size << '\n';
		writer.stream << "i32.mul\n";
	}
	if(!first)
		writer.stream << "i32.add\n";
	first = false;
}

void CheerpWastWriter::WastGepWriter::addConst(uint32_t v)
{
	assert(v);
	writer.stream << "i32.const " << v << '\n';
	if(!first)
		writer.stream << "i32.add\n";
	first = false;
}

//===-- CheerpWriter.cpp - The Cheerp JavaScript generator -------------===//
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
	enum BLOCK_TYPE { WHILE1 = 0, IF };
	std::vector<BLOCK_TYPE> blockTypes;
	void renderCondition(const BasicBlock* B, int branchId);
public:
	CheerpWastRenderInterface(CheerpWastWriter* w):writer(w)
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
	writer->compileBB(*bb);
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
assert(false);
#if 0
		const SwitchInst* si=cast<SwitchInst>(term);
		assert(branchId > 0);
		SwitchInst::ConstCaseIt it=si->case_begin();
		for(int i=1;i<branchId;i++)
			++it;
		const BasicBlock* dest=it.getCaseSuccessor();
		writer->compileOperandForIntegerPredicate(si->getCondition(), CmpInst::ICMP_EQ, CheerpWriter::COMPARISON);
		if (asmjs)
			writer->stream << "==";
		else
			writer->stream << "===";
		writer->compileOperandForIntegerPredicate(it.getCaseValue(), CmpInst::ICMP_EQ, CheerpWriter::COMPARISON);
		//We found the destination, there may be more cases for the same
		//destination though
		for(++it;it!=si->case_end();++it)
		{
			if(it.getCaseSuccessor()==dest)
			{
				//Also add this condition
				if (asmjs)
					writer->stream << '|';
				else
					writer->stream << "||";
				writer->compileOperandForIntegerPredicate(si->getCondition(), CmpInst::ICMP_EQ, CheerpWriter::COMPARISON);
				if (asmjs)
					writer->stream << "==";
				else
					writer->stream << "===";
				writer->compileOperandForIntegerPredicate(it.getCaseValue(), CmpInst::ICMP_EQ, CheerpWriter::COMPARISON);
			}
		}
#endif
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
assert(false);
#if 0
	const BasicBlock* bb=(const BasicBlock*)privateBlock;
	if(!first)
		writer->stream << "}else ";
	writer->stream << "if(";
	renderCondition(bb, branchId, CheerpWriter::LOWEST);
	writer->stream << "){" << NewLine;
#endif
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
	writer->stream << "if\n";
	blockTypes.push_back(IF);
}

void CheerpWastRenderInterface::renderElseBlockBegin()
{
assert(false);
#if 0
	writer->stream << "}else{" << NewLine;
#endif
}

void CheerpWastRenderInterface::renderBlockEnd()
{
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
	else if(bt == IF)
		writer->stream << "end\n";
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
	writer->stream << "loop\n";
	writer->stream << "block\n";
	blockTypes.push_back(WHILE1);
}

void CheerpWastRenderInterface::renderWhileBlockBegin(int blockLabel)
{
assert(false);
#if 0
	writer->stream << 'L' << blockLabel << ':';
	renderWhileBlockBegin();
#endif
}

void CheerpWastRenderInterface::renderDoBlockBegin()
{
assert(false);
#if 0
	writer->stream << "do{" << NewLine;
#endif
}

void CheerpWastRenderInterface::renderDoBlockBegin(int blockLabel)
{
assert(false);
#if 0
	writer->stream << 'L' << blockLabel << ':';
	renderDoBlockBegin();
#endif
}

void CheerpWastRenderInterface::renderDoBlockEnd()
{
assert(false);
#if 0
	writer->stream << "}while(0);" << NewLine;
#endif
}

void CheerpWastRenderInterface::renderBreak()
{
	// TODO: We have to count the block types
	writer->stream << "br 1\n";
}

void CheerpWastRenderInterface::renderBreak(int labelId)
{
assert(false);
#if 0
	writer->stream << "break L" << labelId << ';' << NewLine;
#endif
}

void CheerpWastRenderInterface::renderContinue()
{
assert(false);
#if 0
	writer->stream << "continue;" << NewLine;
#endif
}

void CheerpWastRenderInterface::renderContinue(int labelId)
{
assert(false);
#if 0
	writer->stream << "continue L" << labelId << ';' << NewLine;
#endif
}

void CheerpWastRenderInterface::renderLabel(int labelId)
{
assert(false);
#if 0
	writer->stream << "label=" << labelId << "|0;" << NewLine;
#endif
}

void CheerpWastRenderInterface::renderIfOnLabel(int labelId, bool first)
{
assert(false);
#if 0
	if(first==false)
		writer->stream << "else ";
	if (asmjs)
		writer->stream << "if(label>>>0==" << labelId << ">>>0){" << NewLine;
	else
		writer->stream << "if(label===" << labelId << "){" << NewLine;
#endif
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
assert(false);
#if 0
			needsPointerKindConversion |= writer.needsPointerKindConversion(phi, incoming);
#endif
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
assert(false);
			writer.stream << "__RECURSIVE__\n";
#if 0
			writer.namegen.setEdgeContext(fromBB, toBB);
			writer.stream << writer.namegen.getNameForEdge(incoming);
			writer.namegen.clearEdgeContext();
			writer.stream << '=' << writer.namegen.getName(incoming) << ';' << writer.NewLine;
#endif
		}
		void handlePHI(const Instruction* phi, const Value* incoming) override
		{
assert(false);
#if 0
			// We can avoid assignment from the same register if no pointer kind conversion is required
			if(!writer.needsPointerKindConversion(phi, incoming))
				return;
			Type* phiType=phi->getType();
			writer.stream << writer.namegen.getName(phi) << '=';
			writer.namegen.setEdgeContext(fromBB, toBB);
			writer.compileOperand(incoming, LOWEST);
			writer.stream << ';' << writer.NewLine;
			writer.namegen.clearEdgeContext();
#endif
		}
	};
	WriterPHIHandler(*this, from, to).runOnEdge(registerize, from, to);
}

const char* CheerpWastWriter::getTypeString(Type* t)
{
	if(t->isIntegerTy(32))
		return "i32";
	else
	{
		llvm::errs() << "Unsupported type " << *t << "\n";
		llvm_unreachable("Unsuppored type");
	}
}

void CheerpWastWriter::compileConstant(const Constant* c)
{
	if(const ConstantInt* i=dyn_cast<ConstantInt>(c))
	{
		stream << getTypeString(i->getType()) << ".const ";
		if(i->getBitWidth()==32)
			stream << i->getSExtValue();
		else
			stream << i->getZExtValue();
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
			stream << "get_local " << registerize.getRegisterId(it);
	}
	else
	{
v->dump();
		assert(false);
	}
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
			assert((size % alignment) == 0);
			// We grow the stack down for now
			// 1) Push the current stack pointer
			stream << "get_global " << stackTopGlobal << '\n';
			// 2) Push the allocation size
			stream << "i32.const " << size << '\n';
			// 3) Substract the size
			stream << "i32.sub\n";
			// 4) Write the location to the local, but preserve the value
			stream << "tee_local " << registerize.getRegisterId(&I) << '\n';
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
		case Instruction::Br:
			break;
		case Instruction::Call:
			stream << "__CALL__";
			break;
		case Instruction::ICmp:
		{
			const CmpInst& ci = cast<CmpInst>(I);
			// TODO: Check order
			compileOperand(ci.getOperand(0));
			stream << '\n';
			compileOperand(ci.getOperand(1));
			stream << '\n';
			stream << getTypeString(ci.getOperand(0)->getType()) << '.';
			switch(ci.getPredicate())
			{
				case CmpInst::ICMP_SLT:
					stream << "lt_s";
					break;
				default:
					llvm::errs() << "Handle predicate for " << ci << "\n";
					break;
			}
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
			stream << getTypeString(valOp->getType()) << ".store\n";
			break;
		}
		case Instruction::Ret:
		{
			// TODO: Restore old stack
			const ReturnInst& ri = cast<ReturnInst>(I);
			Value* retVal = ri.getReturnValue();
			if(retVal)
			{
				compileOperand(I.getOperand(0));
				stream << '\n';
			}
			stream << "return";
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
				stream << "\nset_local " << registerize.getRegisterId(I) << '\n';
		}
	}
}

void CheerpWastWriter::compileMethodLocals(const Function& F)
{
	// Declare are all used locals in the beginning
	enum LOCAL_STATE { NOT_DONE, NAME_DONE, SECONDARY_NAME_DONE };
	// Keept track of already found registers
	std::vector<std::pair<Registerize::REGISTER_KIND, bool>> localsFound;
	for(const BasicBlock& BB: F)
	{
		for(const Instruction& I: BB)
		{
			if (isInlineable(I, PA) || I.use_empty())
				continue;
			// Get the register
			uint32_t regId = registerize.getRegisterId(&I);
			if(localsFound.size() <= regId)
				localsFound.resize(regId+1);
			if(localsFound[regId].second)
				continue;
			localsFound[regId] = std::make_pair(registerize.getRegKindFromType(I.getType(), false),true);
		}
		// Handle the special names required for the edges between blocks
		class LocalsPHIHandler: public EndOfBlockPHIHandler
		{
		public:
			LocalsPHIHandler(CheerpWastWriter& w):EndOfBlockPHIHandler(w.PA)
			{
			}
			~LocalsPHIHandler()
			{
			}
		private:
			void handleRecursivePHIDependency(const Instruction* incoming) override
			{
				// TODO: Migrate tmpphis to registerize
				assert(false);
			}
			void handlePHI(const Instruction* phi, const Value* incoming) override
			{
			}
		};
		const TerminatorInst* term=BB.getTerminator();
		for(uint32_t i=0;i<term->getNumSuccessors();i++)
		{
			const BasicBlock* succBB=term->getSuccessor(i);
			LocalsPHIHandler(*this).runOnEdge(registerize, &BB, succBB);
		}
	}
	if(localsFound.empty())
		return;
	stream << "(local";
	for(auto& it: localsFound)
	{
		assert(it.second);
		stream << ' ';
		switch(it.first)
		{
			case Registerize::INTEGER:
			// TODO: This should be fixed in Registerize
			case Registerize::OBJECT:
				stream << "i32";
				break;
			default:
				assert(false);
		}
	}
	stream << ")\n";
}

void CheerpWastWriter::compileMethod(const Function& F)
{
	stream << "(func ";
	// TODO: We should not export them all
	stream << "(export \"" << F.getName() << "\")";
	if(!F.getReturnType()->isVoidTy())
		stream << "(result " << getTypeString(F.getReturnType()) << ')';
	stream << '\n';
	compileMethodLocals(F);
	if(F.size() == 1)
		compileBB(*F.begin());
	else
	{
		Relooper* rl = CheerpWriter::runRelooperOnFunction(F);
		CheerpWastRenderInterface ri(this);
		rl->Render(&ri);
	}
	stream << ')';
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
	
	for ( const Function & F : module.getFunctionList() )
	{
		if (!F.empty() && F.getSection() == StringRef("asmjs"))
		{
			compileMethod(F);
		}
	}
	
	stream << ')';
}

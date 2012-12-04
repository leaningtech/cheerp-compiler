//===-- duetto-compiler.cpp - The Duetto JavaScript generator -------------===//
//
//	Copyright 2011-2012 Leaning Technlogies
//===----------------------------------------------------------------------===//

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Pass.h"
#include "llvm/ADT/Triple.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/CodeGen/LinkAllAsmWriterComponents.h"
#include "llvm/CodeGen/LinkAllCodegenComponents.h"
#include "llvm/MC/SubtargetFeature.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/PluginLoader.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/ToolOutputFile.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include <memory>
#include <map>
#include <iostream>
using namespace llvm;
using namespace std;

// General options for llc.  Other pass-specific options are specified
// within the corresponding llc passes, and target-specific options
// and back-end code generation options are specified with the target machine.
//
static cl::opt<std::string>
InputFilename(cl::Positional, cl::desc("<input bitcode>"), cl::init("-"));

static cl::opt<std::string>
OutputFilename("o", cl::desc("Output filename"), cl::value_desc("filename"));

// Determine optimization level.
static cl::opt<char>
OptLevel("O",
         cl::desc("Optimization level. [-O0, -O1, -O2, or -O3] "
                  "(default = '-O2')"),
         cl::Prefix,
         cl::ZeroOrMore,
         cl::init(' '));

static cl::opt<std::string>
TargetTriple("mtriple", cl::desc("Override target triple for module"));

static cl::opt<std::string>
MArch("march", cl::desc("Architecture to generate code for (see --version)"));

static cl::opt<std::string>
MCPU("mcpu",
  cl::desc("Target a specific cpu type (-mcpu=help for details)"),
  cl::value_desc("cpu-name"),
  cl::init(""));

static cl::list<std::string>
MAttrs("mattr",
  cl::CommaSeparated,
  cl::desc("Target specific attributes (-mattr=help for details)"),
  cl::value_desc("a1,+a2,-a3,..."));

static cl::opt<Reloc::Model>
RelocModel("relocation-model",
             cl::desc("Choose relocation model"),
             cl::init(Reloc::Default),
             cl::values(
            clEnumValN(Reloc::Default, "default",
                       "Target default relocation model"),
            clEnumValN(Reloc::Static, "static",
                       "Non-relocatable code"),
            clEnumValN(Reloc::PIC_, "pic",
                       "Fully relocatable, position independent code"),
            clEnumValN(Reloc::DynamicNoPIC, "dynamic-no-pic",
                       "Relocatable external references, non-relocatable code"),
            clEnumValEnd));

static cl::opt<llvm::CodeModel::Model>
CMModel("code-model",
        cl::desc("Choose code model"),
        cl::init(CodeModel::Default),
        cl::values(clEnumValN(CodeModel::Default, "default",
                              "Target default code model"),
                   clEnumValN(CodeModel::Small, "small",
                              "Small code model"),
                   clEnumValN(CodeModel::Kernel, "kernel",
                              "Kernel code model"),
                   clEnumValN(CodeModel::Medium, "medium",
                              "Medium code model"),
                   clEnumValN(CodeModel::Large, "large",
                              "Large code model"),
                   clEnumValEnd));

static cl::opt<bool>
RelaxAll("mc-relax-all",
  cl::desc("When used with filetype=obj, "
           "relax all fixups in the emitted object file"));

cl::opt<TargetMachine::CodeGenFileType>
FileType("filetype", cl::init(TargetMachine::CGFT_AssemblyFile),
  cl::desc("Choose a file type (not all types are supported by all targets):"),
  cl::values(
       clEnumValN(TargetMachine::CGFT_AssemblyFile, "asm",
                  "Emit an assembly ('.s') file"),
       clEnumValN(TargetMachine::CGFT_ObjectFile, "obj",
                  "Emit a native object ('.o') file [experimental]"),
       clEnumValN(TargetMachine::CGFT_Null, "null",
                  "Emit nothing, for performance testing"),
       clEnumValEnd));

cl::opt<bool> NoVerify("disable-verify", cl::Hidden,
                       cl::desc("Do not verify input module"));

cl::opt<bool> DisableDotLoc("disable-dot-loc", cl::Hidden,
                            cl::desc("Do not use .loc entries"));

cl::opt<bool> DisableCFI("disable-cfi", cl::Hidden,
                         cl::desc("Do not use .cfi_* directives"));

cl::opt<bool> EnableDwarfDirectory("enable-dwarf-directory", cl::Hidden,
    cl::desc("Use .file directives with an explicit directory."));

static cl::opt<bool>
DisableRedZone("disable-red-zone",
  cl::desc("Do not emit code that uses the red zone."),
  cl::init(false));

static cl::opt<bool>
EnableFPMAD("enable-fp-mad",
  cl::desc("Enable less precise MAD instructions to be generated"),
  cl::init(false));

static cl::opt<bool>
PrintCode("print-machineinstrs",
  cl::desc("Print generated machine code"),
  cl::init(false));

static cl::opt<bool>
DisableFPElim("disable-fp-elim",
  cl::desc("Disable frame pointer elimination optimization"),
  cl::init(false));

static cl::opt<bool>
DisableFPElimNonLeaf("disable-non-leaf-fp-elim",
  cl::desc("Disable frame pointer elimination optimization for non-leaf funcs"),
  cl::init(false));

static cl::opt<bool>
DisableExcessPrecision("disable-excess-fp-precision",
  cl::desc("Disable optimizations that may increase FP precision"),
  cl::init(false));

static cl::opt<bool>
EnableUnsafeFPMath("enable-unsafe-fp-math",
  cl::desc("Enable optimizations that may decrease FP precision"),
  cl::init(false));

static cl::opt<bool>
EnableNoInfsFPMath("enable-no-infs-fp-math",
  cl::desc("Enable FP math optimizations that assume no +-Infs"),
  cl::init(false));

static cl::opt<bool>
EnableNoNaNsFPMath("enable-no-nans-fp-math",
  cl::desc("Enable FP math optimizations that assume no NaNs"),
  cl::init(false));

static cl::opt<bool>
EnableHonorSignDependentRoundingFPMath("enable-sign-dependent-rounding-fp-math",
  cl::Hidden,
  cl::desc("Force codegen to assume rounding mode can change dynamically"),
  cl::init(false));

static cl::opt<bool>
GenerateSoftFloatCalls("soft-float",
  cl::desc("Generate software floating point library calls"),
  cl::init(false));

static cl::opt<llvm::FloatABI::ABIType>
FloatABIForCalls("float-abi",
  cl::desc("Choose float ABI type"),
  cl::init(FloatABI::Default),
  cl::values(
    clEnumValN(FloatABI::Default, "default",
               "Target default float ABI type"),
    clEnumValN(FloatABI::Soft, "soft",
               "Soft float ABI (implied by -soft-float)"),
    clEnumValN(FloatABI::Hard, "hard",
               "Hard float ABI (uses FP registers)"),
    clEnumValEnd));

static cl::opt<bool>
DontPlaceZerosInBSS("nozero-initialized-in-bss",
  cl::desc("Don't place zero-initialized symbols into bss section"),
  cl::init(false));

static cl::opt<bool>
EnableGuaranteedTailCallOpt("tailcallopt",
  cl::desc("Turn fastcc calls into tail calls by (potentially) changing ABI."),
  cl::init(false));

static cl::opt<bool>
DisableTailCalls("disable-tail-calls",
  cl::desc("Never emit tail calls"),
  cl::init(false));

static cl::opt<unsigned>
OverrideStackAlignment("stack-alignment",
  cl::desc("Override default stack alignment"),
  cl::init(0));

static cl::opt<bool>
EnableRealignStack("realign-stack",
  cl::desc("Realign stack if needed"),
  cl::init(true));

static cl::opt<bool>
DisableSwitchTables(cl::Hidden, "disable-jump-tables",
  cl::desc("Do not generate jump tables."),
  cl::init(false));

static cl::opt<std::string>
TrapFuncName("trap-func", cl::Hidden,
  cl::desc("Emit a call to trap function rather than a trap instruction"),
  cl::init(""));

static cl::opt<bool>
EnablePIE("enable-pie",
  cl::desc("Assume the creation of a position independent executable."),
  cl::init(false));

static cl::opt<bool>
SegmentedStacks("segmented-stacks",
  cl::desc("Use segmented stacks if possible."),
  cl::init(false));


// GetFileNameRoot - Helper function to get the basename of a filename.
static inline std::string
GetFileNameRoot(const std::string &InputFilename) {
  std::string IFN = InputFilename;
  std::string outputFilename;
  int Len = IFN.length();
  if ((Len > 2) &&
      IFN[Len-3] == '.' &&
      ((IFN[Len-2] == 'b' && IFN[Len-1] == 'c') ||
       (IFN[Len-2] == 'l' && IFN[Len-1] == 'l'))) {
    outputFilename = std::string(IFN.begin(), IFN.end()-3); // s/.bc/.s/
  } else {
    outputFilename = IFN;
  }
  return outputFilename;
}

class JSWriter
{
private:
	Module* module;
	raw_fd_ostream& stream;
	uint32_t getIntFromValue(Value* v) const;
	std::set<const Value*> completeObjects;
	bool isValidTypeCast(Type* src, Type* dst) const;
	bool isClientType(Type* t) const;
	bool isI32Type(Type* t) const;
	bool isInlineable(const Instruction& I) const;
	void compileTerminatorInstruction(const TerminatorInst& I,
			const std::map<const BasicBlock*, uint32_t>& blocksMap);
	bool compileInlineableInstruction(const Instruction& I);
	bool compileNotInlineableInstruction(const Instruction& I);
	void compilePHIOfBlockFromOtherBlock(const BasicBlock* to, const BasicBlock* from) const;
	void compileRecursiveAccessToGEP(const GetElementPtrInst& gep, const Type* curType,
		GetElementPtrInst::const_op_iterator it);
	void compilePredicate(CmpInst::Predicate p);
	uint32_t compileType(Type* t);
	uint32_t getTypeSize(Type* t) const;
	uint32_t getStructOffsetFromElement(const StructType* st, uint32_t elem) const;
	void compileDereferencePointer(const Value* v);
public:
	JSWriter(Module* m, raw_fd_ostream& s):module(m),stream(s)
	{
	}
	void makeJS();
	void compileMethod(Function& F);
	void compileBB(BasicBlock& BB, const std::map<const BasicBlock*, uint32_t>& blocksMap);
	void compileOperand(const Value* v);
	void compileConstant(const Constant* c) const;
	void compileConstantExpr(const ConstantExpr* ce) const;
	void compileRecursiveGEP(const ConstantExpr* ce, const Constant* base, uint32_t level) const;
};

void JSWriter::compilePredicate(CmpInst::Predicate p)
{
	switch(p)
	{
		case CmpInst::ICMP_EQ:
			stream << " == ";
			break;
		case CmpInst::ICMP_NE:
			stream << " != ";
			break;
		case CmpInst::ICMP_SGT:
			stream << " > ";
			break;
		case CmpInst::ICMP_SGE:
			stream << " >= ";
			break;
		case CmpInst::ICMP_SLT:
			stream << " < ";
			break;
		case CmpInst::ICMP_SLE:
			stream << " <= ";
			break;
		default:
			cerr << "Support predicate " << p << endl;
	}
}

void JSWriter::compileDereferencePointer(const Value* v)
{
	assert(v->getType()->isPointerTy());
	compileOperand(v);
	stream << ".d[";
	compileOperand(v);
	stream << ".p+";
	compileOperand(v);
	stream << ".o]";
}

uint32_t JSWriter::getIntFromValue(Value* v) const
{
	assert(ConstantInt::classof(v));
	ConstantInt* i=cast<ConstantInt>(v);
	return i->getZExtValue();
}

void JSWriter::compileRecursiveAccessToGEP(const GetElementPtrInst& gep,
		const Type* curType,
		GetElementPtrInst::const_op_iterator it)
{
	//Before this the base name has been already printed
	if(it==gep.idx_end())
		return;
	assert(curType->isStructTy());
	const StructType* st=static_cast<const StructType*>(curType);
	//Special handling for constant offsets
	assert(ConstantInt::classof(*it));
	uint32_t elementIndex = getIntFromValue(*it);
	stream << ".a" << getStructOffsetFromElement(st, elementIndex);
	compileRecursiveAccessToGEP(gep, st->getElementType(elementIndex), ++it);
}

void JSWriter::compileRecursiveGEP(const ConstantExpr* ce, const Constant* base, uint32_t level) const
{
	//TODO: Support multiple dereferece in GEP
	assert(ce->getNumOperands()==level+3);
	stream << "{ d: ";
	compileConstant(base);
	//TODO: this should include the size of the fields
	stream << ", o: " << getIntFromValue(ce->getOperand(level+2));
	stream << ", p: ";
	assert(!ConstantStruct::classof(base));
	stream << "'' }";
}

bool JSWriter::isClientType(Type* t) const
{
	return (t->isStructTy() && 
		strncmp(t->getStructName().data(), "class.client::", 14)==0);
}

bool JSWriter::isValidTypeCast(Type* srcPtr, Type* dstPtr) const
{
	//Only pointer casts are possible anyway
	assert(srcPtr->isPointerTy() && dstPtr->isPointerTy());
	Type* src=cast<PointerType>(srcPtr)->getElementType();
	Type* dst=cast<PointerType>(dstPtr)->getElementType();
	//Conversion between client objects is free
	if(isClientType(src) && isClientType(dst))
		return true;
	//Conversion between any function pointers are ok
	if(src->isFunctionTy() && dst->isFunctionTy())
		return true;
	src->dump();
	cerr << endl;
	dst->dump();
	cerr << endl;
	return false;
}

void JSWriter::compileConstantExpr(const ConstantExpr* ce) const
{
	switch(ce->getOpcode())
	{
		case Instruction::GetElementPtr:
		{
			assert(ce->getNumOperands()>=3);
			Value* base = ce->getOperand(0);
			assert(GlobalVariable::classof(base));
			//GlobalVariables never changes, they are assumed to be
			//in the form { d: [<objPointed>], o: 0, p: '' }
			assert(cast<GlobalVariable>(base)->hasInitializer());
			//TODO: Support external global variables
			Constant* initializer = cast<GlobalVariable>(base)->getInitializer();
			//NOTE: the first dereference must be 0, they point to a single object
			Value* first=ce->getOperand(1);
			assert(getIntFromValue(first)==0);
			compileRecursiveGEP(ce, initializer, 0);
			break;
		}
		case Instruction::BitCast:
		{
			assert(ce->getNumOperands()==1);
			Value* val=ce->getOperand(0);
			Type* src=ce->getType();
			Type* dst=val->getType();
			assert(isValidTypeCast(src, dst));
			assert(val->hasName());
			stream << val->getName().data();
			break;
		}
		default:
			cerr << "Unsupported constant expr " << ce->getOpcodeName() << endl;
	}
}

void JSWriter::compileConstant(const Constant* c) const
{
	if(ConstantExpr::classof(c))
		compileConstantExpr(cast<ConstantExpr>(c));
	else if(ConstantDataSequential::classof(c))
	{
		const ConstantDataSequential* d=cast<const ConstantDataSequential>(c);
		assert(d->isString());
		//TODO: Support \x escapes
		stream << '"';
		stream << d->getAsString().data();
		stream << '"';
	}
	else if(ConstantFP::classof(c))
	{
		const ConstantFP* f=cast<const ConstantFP>(c);
		stream << f->getValueAPF().convertToDouble();
	}
	else if(ConstantInt::classof(c))
	{
		const ConstantInt* i=cast<const ConstantInt>(c);
		assert(i->getBitWidth()<=32);
		stream << i->getSExtValue();
	}
	else
	{
		cerr << "Unsupported constant type ";
		c->dump();
	}
}

void JSWriter::compileOperand(const Value* v)
{
	//Check the inline map first
	const Constant* c=dyn_cast<const Constant>(v);
	if(c)
		compileConstant(c);
	else if(dyn_cast<Instruction>(v) && isInlineable(*cast<Instruction>(v)))
		compileInlineableInstruction(*cast<Instruction>(v));
	else if(v->hasName())
		stream << v->getName().data();
	else
	{
		cerr << "No name for value ";
		v->dump();
	}
}

/*
 * Keep in sync with compileType
 */
uint32_t JSWriter::getTypeSize(Type* t) const
{
	switch(t->getTypeID())
	{
		case Type::IntegerTyID:
		case Type::PointerTyID:
			return 4;
		case Type::StructTyID:
		{
			StructType* st=static_cast<StructType*>(t);
			StructType::element_iterator E=st->element_begin();
			StructType::element_iterator EE=st->element_end();
			uint32_t offset=0;
			for(;E!=EE;++E)
				offset+=getTypeSize(*E);
			return offset;
		}
		default:
			cerr << "Support type ";
			t->dump();
			cerr << endl;
			return 0;
	}
}

uint32_t JSWriter::getStructOffsetFromElement(const StructType* st, uint32_t elem) const
{
	uint32_t ret=0;
	for(uint32_t i=0;i<elem;i++)
		ret+=getTypeSize(st->getElementType(i));
	return ret;
}

uint32_t JSWriter::compileType(Type* t)
{
	switch(t->getTypeID())
	{
		case Type::IntegerTyID:
		{
			IntegerType* it=static_cast<IntegerType*>(t);
			//We only really have 32bit integers.
			//We will allow anything shorter.
			//NOTE: Only bit operations are allowed on shorter types
			//this is enforced on a per-operation basis
			assert(it->getBitWidth()<=32);
			//Print out a '0'. To let the engine know this is an integer
			stream << '0';
			return 4;
		}
		case Type::StructTyID:
		{
			stream << "{ ";
			StructType* st=static_cast<StructType*>(t);
			StructType::element_iterator E=st->element_begin();
			StructType::element_iterator EE=st->element_end();
			uint32_t offset=0;
			for(;E!=EE;++E)
			{
				if(offset!=0)
					stream << ", ";
				stream << "a" << offset << ": ";
				offset+=compileType(*E);
			}
			stream << " }";
			return offset;
		}
		case Type::PointerTyID:
		{
			stream << "{ d:null, o: 0, p: '' }";
			return 4;
		}
		default:
			cerr << "Support type ";
			t->dump();
			cerr << endl;
			return 0;
	}
}

void JSWriter::compilePHIOfBlockFromOtherBlock(const BasicBlock* to, const BasicBlock* from) const
{
	BasicBlock::const_iterator I=to->begin();
	BasicBlock::const_iterator IE=to->end();
	for(;I!=IE;++I)
	{
		const PHINode* phi=dyn_cast<const PHINode>(I);
		assert(phi==NULL);
	}
}

/*
 * This method is fragile, each opcode must handle the phis in the correct place
 */
void JSWriter::compileTerminatorInstruction(const TerminatorInst& I,
		const std::map<const BasicBlock*, uint32_t>& blocksMap)
{
	switch(I.getOpcode())
	{
		case Instruction::Ret:
		{
			const ReturnInst& ri=static_cast<const ReturnInst&>(I);
			Value* retVal = ri.getReturnValue();
			stream << "return ";
			if(retVal)
				compileOperand(retVal);
			stream << ");\n";
			assert(I.getNumSuccessors()==0);
			break;
		}
		case Instruction::Invoke:
		{
			const InvokeInst& ci=static_cast<const InvokeInst&>(I);
			//TODO: Support unwind
			//For now, pretend it's a regular call
			stream << ci.getCalledFunction()->getName().data() << '(';
			for(uint32_t i=0;i<ci.getNumArgOperands();i++)
			{
				if(i!=0)
					stream << ", ";
				compileOperand(ci.getArgOperand(i));
			}
			stream << ");\n";
			//For each successor output the variables for the phi nodes
			for(uint32_t i=0;i<I.getNumSuccessors();i++)
			{
				BasicBlock* b=I.getSuccessor(i);
				compilePHIOfBlockFromOtherBlock(b, I.getParent());
			}
			//Add code to jump to the next block
			stream << "__block = " << blocksMap.find(ci.getNormalDest())->second << ";\n";
			break;
		}
		case Instruction::Resume:
		{
			//TODO: support exceptions
			break;
		}
		case Instruction::Br:
		{
			const BranchInst& bi=static_cast<const BranchInst&>(I);
			if(bi.isUnconditional())
				stream << "__block = " << blocksMap.find(bi.getSuccessor(0))->second << ";\n";
			else
			{
				stream << "if( ";
				compileOperand(bi.getCondition());
				stream << ") __block = " << blocksMap.find(bi.getSuccessor(0))->second <<
					"; else __block = " << blocksMap.find(bi.getSuccessor(1))->second <<
					";\n";
			}
			break;
		}
		default:
			cerr << "\tImplement terminator inst " << I.getOpcodeName() << endl;
			break;
	}
}

bool JSWriter::compileNotInlineableInstruction(const Instruction& I)
{
	switch(I.getOpcode())
	{
		case Instruction::Call:
		{
			const CallInst& ci=static_cast<const CallInst&>(I);
			stream << ci.getCalledFunction()->getName().data() << '(';
			for(uint32_t i=0;i<ci.getNumArgOperands();i++)
			{
				if(i!=0)
					stream << ", ";
				compileOperand(ci.getArgOperand(i));
			}
			stream << ")";
			return true;
		}
		case Instruction::LandingPad:
		{
			//TODO: Support exceptions
			//Do not continue block
			return false;
		}
		case Instruction::PHI:
		{
			//Phis are handled at each terminator, so nothing to do here
			return true;
		}
		case Instruction::InsertValue:
		{
			const InsertValueInst& ivi=static_cast<const InsertValueInst&>(I);
			const Value* aggr=ivi.getAggregateOperand();
			Type* t=aggr->getType();
			assert(t->isStructTy());
			StructType* st=static_cast<StructType*>(t);
			if(UndefValue::classof(aggr))
			{
				//We have to assemble the type object from scratch
				compileType(t);
			}
			stream << ";\n";
			//Also assign the element
			assert(ivi.getNumIndices()==1);
			//Find the offset to the pointed element
			uint32_t offset=getStructOffsetFromElement(st, ivi.getIndices()[0]);
			assert(ivi.hasName());
			stream << ivi.getName() << ".a" << offset << " = ";
			compileOperand(ivi.getInsertedValueOperand());
			return true;
		}
		case Instruction::Store:
		{
			const StoreInst& si=static_cast<const StoreInst&>(I);
			const Value* ptrOp=si.getPointerOperand();
			const Value* valOp=si.getValueOperand();
			compileOperand(ptrOp);
			compileOperand(valOp);
			return true;
		}
		default:
			return compileInlineableInstruction(I);
	}
}

bool JSWriter::isI32Type(Type* t) const
{
	return t->isIntegerTy() && static_cast<IntegerType*>(t)->getBitWidth()==32;
}

/*
 * This can be used for both named instructions and inlined ones
 * NOTE: Call, Ret, Invoke are NEVER inlined
 */
bool JSWriter::compileInlineableInstruction(const Instruction& I)
{
	switch(I.getOpcode())
	{
		case Instruction::BitCast:
		{
			const BitCastInst& bi=static_cast<const BitCastInst&>(I);
			Type* srcPtr=bi.getSrcTy();
			Type* dstPtr=bi.getDestTy();
			assert(isValidTypeCast(srcPtr, dstPtr));
			compileOperand(bi.getOperand(0));
			return true;
		}
		case Instruction::Alloca:
		{
			const AllocaInst& ai=static_cast<const AllocaInst&>(I);
			//Alloca is supposed to return a pointer. We will cheat and return
			//an object.
			assert(ai.hasName());
			compileType(ai.getAllocatedType());
			//Take note that this is a complete object
			completeObjects.insert(&I);
			return true;
		}
		case Instruction::FPToSI:
		{
			const CastInst& ci=static_cast<const CastInst&>(I);
			//Check that the in and out types are sane
			Type* srcT = ci.getSrcTy();
			Type* dstT = ci.getDestTy();
			assert(srcT->isDoubleTy());
			assert(isI32Type(dstT));

			assert(ci.hasName());
			stream << "(";
			compileOperand(ci.getOperand(0));
			//Seems to be the fastest way
			//http://jsperf.com/math-floor-vs-math-round-vs-parseint/33
			stream << " >> 0)";
			return true;
		}
		case Instruction::GetElementPtr:
		{
			const GetElementPtrInst& gep=static_cast<const GetElementPtrInst&>(I);
			Type* t=gep.getOperand(0)->getType();
			assert(t->isPointerTy());
			PointerType* ptrT=static_cast<PointerType*>(t);
			if(isClientType(ptrT->getElementType()))
			{
				//Client objects are just passed through
				compileOperand(gep.getOperand(0));
			}
			else
			{
				GetElementPtrInst::const_op_iterator it=gep.idx_begin();
				assert(ConstantInt::classof(*it));
				assert(getIntFromValue(*it)==0);
				//First dereference the pointer
				compileDereferencePointer(gep.getOperand(0));
				//TODO: properly implement GEP
				//If this has one use and the user
				//is a store, optimize the code
				assert(gep.hasOneUse());
				const User* u=*gep.use_begin();
				assert(StoreInst::classof(u));
				compileRecursiveAccessToGEP(gep, ptrT->getElementType(), ++it);
				//NOTE: here, we are assuming that we are being inlined by the store
				//is this always true?
				stream << " = ";
			}
			return true;
		}
		case Instruction::Sub:
		{
			//Integer subtraction
			//TODO: optimize negation
			assert(I.getNumOperands()==2);
			assert(isI32Type(I.getOperand(0)->getType()));
			assert(isI32Type(I.getOperand(1)->getType()));
			stream << "((";
			compileOperand(I.getOperand(0));
			stream << " - ";
			compileOperand(I.getOperand(1));
			stream << ") >> 0)";
			return true;
		}
		case Instruction::ZExt:
		{
			const BitCastInst& bi=static_cast<const BitCastInst&>(I);
			Type* src=bi.getSrcTy();
			Type* dst=bi.getDestTy();
			assert(src->isIntegerTy() && dst->isIntegerTy());
			IntegerType* srcI=static_cast<IntegerType*>(src);
			IntegerType* dstI=static_cast<IntegerType*>(dst);
			assert(srcI->getBitWidth()<=32);
			assert(dstI->getBitWidth()<=32);
			assert(srcI->getBitWidth()<=dstI->getBitWidth());
			//The operation is a NOP
			compileOperand(bi.getOperand(0));
			return true;
		}
		case Instruction::SDiv:
		{
			//Integer signed division
			assert(I.getNumOperands()==2);
			assert(isI32Type(I.getOperand(0)->getType()));
			assert(isI32Type(I.getOperand(1)->getType()));
			stream << "((";
			compileOperand(I.getOperand(0));
			stream << " / ";
			compileOperand(I.getOperand(1));
			stream << ") >> 0)";
			return true;
		}
		case Instruction::ICmp:
		{
			//Integer comparison
			const CmpInst& ci=static_cast<const CmpInst&>(I);
			assert(ci.getNumOperands()==2);
			//Check that the operation is JS safe
			switch(ci.getPredicate())
			{
				case CmpInst::ICMP_EQ:
				case CmpInst::ICMP_NE:
					//Those are safe in any size
					break;
				default:
					assert(isI32Type(ci.getOperand(0)->getType()));
					assert(isI32Type(ci.getOperand(1)->getType()));
			}
			stream << "(";
			compileOperand(ci.getOperand(0));
			compilePredicate(ci.getPredicate());
			compileOperand(ci.getOperand(1));
			stream << ")";
			return true;
		}
		default:
			cerr << "\tImplement inst " << I.getOpcodeName() << endl;
			return false;
	}
}

bool JSWriter::isInlineable(const Instruction& I) const
{
	//Beside a few cases, instructions without name
	//or with a single use may be inlined
	if(I.hasName()==false || I.hasOneUse())
	{
		//A few opcodes needs to be executed anyway as they
		//do not operated on registers
		switch(I.getOpcode())
		{
			case Instruction::Call:
			case Instruction::Invoke:
			case Instruction::Ret:
			case Instruction::LandingPad:
			case Instruction::PHI:
			case Instruction::Store:
			case Instruction::InsertValue:
			case Instruction::Resume:
			case Instruction::Br:
				return false;
			case Instruction::FPToSI:
			case Instruction::Sub:
			case Instruction::SDiv:
			case Instruction::BitCast:
			case Instruction::GetElementPtr:
			case Instruction::FCmp:
			case Instruction::ICmp:
			case Instruction::ZExt:
			case Instruction::Load:
				return true;
			default:
				cerr << "Is " << I.getOpcodeName() << " inlineable?" << endl;
				assert(false);
		}
	}
	return false;
}

void JSWriter::compileBB(BasicBlock& BB, const std::map<const BasicBlock*, uint32_t>& blocksMap)
{
	BasicBlock::iterator I=BB.begin();
	BasicBlock::iterator IE=BB.end();
	for(;I!=IE;++I)
	{
		if(isInlineable(*I))
			continue;
		if(I->hasName())
			stream << "var " << I->getName().data() << " = ";
		if(I->isTerminator())
		{
			compileTerminatorInstruction(*dyn_cast<TerminatorInst>(I), blocksMap);
		}
		else
		{
			bool ret=compileNotInlineableInstruction(*I);
			stream << ";\n";
			if(ret==false)
			{
				//Stop basic block compilation
				return;
			}
		}
	}
	//At the end of the block
}

void JSWriter::compileMethod(Function& F)
{
	if(F.empty())
		return;
	std::cerr << (string)F.getName() << std::endl;
	stream << "function " << (string)F.getName() << "(";
	Function::const_arg_iterator A=F.arg_begin();
	Function::const_arg_iterator AE=F.arg_end();
	int i=0;
	for(;A!=AE;++A)
	{
		if(i!=0)
			stream << ", ";
		i++;
		stream << "arg" << A->getArgNo();
	}
	stream << ") {\n";
	std::map<const BasicBlock*, uint32_t> blocksMap;
	if(F.size()==1)
		compileBB(*F.begin(), blocksMap);
	else
	{
		Function::iterator B=F.begin();
		Function::iterator BE=F.end();
		//Build a map from basicblocks to ids
		uint32_t blockId=0;
		for(;B!=BE;++B)
		{
			blocksMap.insert(make_pair(&(*B), blockId));
			blockId++;
		}
		//Rest the iterator
		B=F.begin();

		//Create an emscripten style switch for now
		stream << "var __block=0;\nswitch(__block){\n";
		for(;B!=BE;++B)
		{
			stream << "case " << blocksMap.find(&(*B))->second << ":\n";
			compileBB(*B, blocksMap);
			stream << "break;\n";
		}
		stream << "}\n";
	}

	stream << "}\n";
}

void JSWriter::makeJS()
{
	Module::iterator F=module->begin();
	Module::iterator FE=module->end();
	for (; F != FE; ++F)
	{
		compileMethod(*F);
	}
}

// main - Entry point for the duetto double target compiler.
//
int main(int argc, char **argv) {
  sys::PrintStackTraceOnErrorSignal();
  PrettyStackTraceProgram X(argc, argv);

  // Enable debug stream buffering.
  EnableDebugBuffering = true;

  LLVMContext &Context = getGlobalContext();
  llvm_shutdown_obj Y;  // Call llvm_shutdown() on exit.

  // Initialize targets first, so that --version shows registered targets.
  InitializeAllTargets();
  InitializeAllTargetMCs();
  InitializeAllAsmPrinters();
  InitializeAllAsmParsers();

  // Register the target printer for --version.
  cl::AddExtraVersionPrinter(TargetRegistry::printRegisteredTargetsForVersion);

  cl::ParseCommandLineOptions(argc, argv, "llvm system compiler\n");

  // Load the module to be compiled...
  SMDiagnostic Err;
  std::unique_ptr<Module> M(parseIRFile(InputFilename, Err, Context));
  if (M.get() == 0) {
    Err.print(argv[0], errs());
    return 1;
  }
  Module* clientMod = M.get();

  std::string JSOutputFilename = GetFileNameRoot(InputFilename) + ".js";

  std::error_code errorInfo;
  raw_fd_ostream JSOut(JSOutputFilename.c_str(),errorInfo,sys::fs::F_None);
  if(errorInfo)
  {
	std::cerr << errorInfo << std::endl;
	return -1;
  }

  JSWriter writer(clientMod, JSOut);
  writer.makeJS();

  JSOut.close();

  return 0;
}

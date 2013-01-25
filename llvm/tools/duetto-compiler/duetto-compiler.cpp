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
	uint32_t getIntFromValue(const Value* v) const;
	//std::set<const Value*> completeObjects;
	bool isFunctionPointerPointerType(Type* t) const;
	bool isValidTypeCast(const Value* cast, const Value* castOp, Type* src, Type* dst) const;
	bool isVTableCast(Type* src, Type* dst) const;
	bool isClientType(Type* t) const;
	bool isI32Type(Type* t) const;
	bool isInlineable(const Instruction& I) const;
	void compileTerminatorInstruction(const TerminatorInst& I,
			const std::map<const BasicBlock*, uint32_t>& blocksMap);
	bool compileInlineableInstruction(const Instruction& I);
	bool compileNotInlineableInstruction(const Instruction& I);
	void compilePHIOfBlockFromOtherBlock(const BasicBlock* to, const BasicBlock* from);
	const Type* compileRecursiveAccessToGEP(const Type* curType, const Use* it, const Use* const itE);
	void compilePredicate(CmpInst::Predicate p);
	void compileType(Type* t);
	void compileDereferencePointer(const Value* v, int byteOffset);
	void compileDereferencePointer(const Value* v, const Value* offset);
	void compileFastGEPDereference(const GetElementPtrInst& gep);
	void compileGEP(const Value* val, const Use* it, const Use* const itE);
	void printLLVMName(const StringRef& s) const;
	void handleBuiltinNamespace(const char* ident, User::const_op_iterator it,
			User::const_op_iterator itE);
	bool handleBuiltinCall(const char* ident, User::const_op_iterator it,
			User::const_op_iterator itE);
	bool safeCallForNewedMemory(const CallInst* ci) const;
	std::map<const Value*, int> unnamedValueMap;
public:
	JSWriter(Module* m, raw_fd_ostream& s):module(m),stream(s)
	{
	}
	void makeJS();
	void compileMethod(Function& F);
	void compileBB(BasicBlock& BB, const std::map<const BasicBlock*, uint32_t>& blocksMap);
	void compileOperand(const Value* v);
	void compileConstant(const Constant* c);
	void compileConstantExpr(const ConstantExpr* ce);
};

void JSWriter::handleBuiltinNamespace(const char* ident, User::const_op_iterator it,
			User::const_op_iterator itE)
{
	//Read the class name
	char* className;
	int classLen = strtol(ident,&className,10);
	ident = className + classLen;

	//Read the function name
	char* funcName;
	int funcNameLen=strtol(ident,&funcName,10);

	//The first arg should be the object
	assert(it!=itE);
	if(strncmp(funcName,"get_",4)==0 && (itE-it)==1)
	{
		//Getter
		compileOperand(*it);
		stream << ".";
		stream.write(funcName+4,funcNameLen-4);
	}
	else if(strncmp(funcName,"set_",4)==0 && (itE-it)==2)
	{
		//Setter
		compileOperand(*it);
		++it;
		stream << ".";
		stream.write(funcName+4,funcNameLen-4);
		stream << " = ";
		compileOperand(*it);
	}
	else
	{
		//Regular call
		compileOperand(*it);
		++it;
		stream << ".";
		stream.write(funcName,funcNameLen);
		stream << "(";
		for(User::const_op_iterator cur=it;it!=itE;++it)
		{
			if(cur!=it)
				stream << ",";
			compileOperand(*it);
		}
		stream << ")";
	}
}

bool JSWriter::handleBuiltinCall(const char* ident, User::const_op_iterator it,
			User::const_op_iterator itE)
{
	if(strcmp(ident,"_ZN6client5ArrayixEi")==0 ||
		strcmp(ident,"_ZNK6client6ObjectcvdEv")==0)
	{
		//Do not touch method that are implemented in native JS code
		return false;
	}
	else if(strncmp(ident,"_ZN6client6Client",17)==0)
	{
		//Handle getters in Client
		const char* rest=ident+17;
		char* functionName;
		int functionNameLen=strtol(rest,&functionName,10);
		assert(strncmp(functionName,"get_",4)==0);
		stream.write(functionName+4,functionNameLen-4);
		return true;
	}
	else if(strncmp(ident,"_ZN6client",10)==0)
	{
		handleBuiltinNamespace(ident+10,it,itE);
		return true;
	}
	else if(strncmp(ident,"_ZNK6client",11)==0)
	{
		handleBuiltinNamespace(ident+11,it,itE);
		return true;
	}
	else if(strncmp(ident,"default_duettoCreateBuiltin_",28)==0)
	{
		assert(it==itE);
		//Default handling of builtin constructors
		stream << "new " << (ident+28) << "()";
		return true;
	}
	return false;
}

void JSWriter::compilePredicate(CmpInst::Predicate p)
{
	switch(p)
	{
		case CmpInst::ICMP_EQ:
			stream << " === ";
			break;
		case CmpInst::FCMP_UNE: //The undordered case correspond to the usual JS operator
					//See ECMA-262, Section 11.9.6
		case CmpInst::ICMP_NE:
			stream << " !== ";
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

void JSWriter::printLLVMName(const StringRef& s) const
{
	const char* data=s.data();
	//Add an '_' to skip reserved names
	stream.write("_",1);
	for(uint32_t i=0;i<s.size();i++)
	{
		if(data[i]=='.')
			stream.write("_",1);
		else
			stream.write(data+i,1);
	}
}

void JSWriter::compileDereferencePointer(const Value* v, const Value* offset)
{
	assert(v->getType()->isPointerTy());
	compileOperand(v);
	stream << ".d[";
	compileOperand(v);
	stream << ".p+";
	stream << '(';
	compileOperand(v);
	stream << ".o+";
	compileOperand(offset);
	stream << ")]";
}

void JSWriter::compileDereferencePointer(const Value* v, int byteOffset)
{
	assert(v->getType()->isPointerTy());
	compileOperand(v);
	stream << ".d[";
	compileOperand(v);
	stream << ".p+";
	if(byteOffset==0)
	{
		compileOperand(v);
		stream << ".o]";
	}
	else
	{
		stream << '(';
		compileOperand(v);
		stream << ".o+" << byteOffset << ")]";
	}
}

uint32_t JSWriter::getIntFromValue(const Value* v) const
{
	assert(ConstantInt::classof(v));
	const ConstantInt* i=cast<const ConstantInt>(v);
	return i->getZExtValue();
}

const Type* JSWriter::compileRecursiveAccessToGEP(const Type* curType, const Use* it, const Use* const itE)
{
	//Before this the base name has been already printed
	if(it==itE)
		return curType;
	const Type* subType = NULL;
	if(curType->isStructTy())
	{
		const StructType* st=static_cast<const StructType*>(curType);
		//Special handling for constant offsets
		assert(ConstantInt::classof(*it));
		uint32_t elementIndex = getIntFromValue(*it);
		stream << ".a" << elementIndex;
		subType = st->getElementType(elementIndex);
	}
	else if(curType->isArrayTy())
	{
		const ArrayType* at=static_cast<const ArrayType*>(curType);
		stream << '[';
		//Special handling for constant offsets
		if(ConstantInt::classof(*it))
		{
			uint32_t elementIndex = getIntFromValue(*it);
			stream << elementIndex;
		}
		else
			compileOperand(*it);
		stream << ']';
		subType = at->getElementType();
	}
	else
		assert(false);
	return compileRecursiveAccessToGEP(subType, ++it, itE);
}

bool JSWriter::isClientType(Type* t) const
{
	return (t->isStructTy() && 
		strncmp(t->getStructName().data(), "class.client::", 14)==0);
}

bool JSWriter::safeCallForNewedMemory(const CallInst* ci) const
{
	//We allow the unsafe cast to i8* only
	//if the usage is memcpy, memset, free or delete
	//or one of the lifetime intrinsics
	return (ci && (ci->getCalledFunction()->getName()=="llvm.memcpy.p0i8.p0i8.i32" ||
		ci->getCalledFunction()->getName()=="llvm.memset.p0i8.i32" ||
		ci->getCalledFunction()->getName()=="llvm.memset.p0i8.i64" ||
		ci->getCalledFunction()->getName()=="free" ||
		ci->getCalledFunction()->getName()=="_ZdlPv" ||
		ci->getCalledFunction()->getName()=="llvm.lifetime.start" ||
		ci->getCalledFunction()->getName()=="llvm.lifetime.end" ||
		//Allow unsafe casts for a limited number of functions that accepts callback args
		//TODO: find a nicer approach for this
		ci->getCalledFunction()->getName()=="__cxa_atexit"));
}

bool JSWriter::isFunctionPointerPointerType(Type* t) const
{
	if(!t->isPointerTy())
		return false;
	Type* innerDst=cast<PointerType>(t)->getElementType();
	if(!innerDst->isPointerTy())
		return false;
	Type* innerDst2=cast<PointerType>(innerDst)->getElementType();
	if(!innerDst2->isFunctionTy())
		return false;
	return true;
}

bool JSWriter::isVTableCast(Type* srcPtr, Type* dstPtr) const
{
	//Only pointer casts are possible anyway
	assert(srcPtr->isPointerTy() && dstPtr->isPointerTy());
	Type* src=cast<PointerType>(srcPtr)->getElementType();
	Type* dst=cast<PointerType>(dstPtr)->getElementType();
	//Support getting the vtable from the object
	if(!src->isStructTy() || !dst->isPointerTy())
		return false;

	if(!isFunctionPointerPointerType(dst))
		return false;

	//There is no easy way of convincing clang to emit proper
	//access to the actual vtable pointer. It uses a direct bitcast
	//Support this by iteratively getting the first member of any struct
	//until we find a vtable compatible type
	StructType* innerSrc=static_cast<StructType*>(src);
	while(true)
	{
		Type* tmp=innerSrc->getElementType(0);
		if(isFunctionPointerPointerType(tmp))
			return true;
		if(!tmp->isStructTy())
			return false;
		innerSrc=static_cast<StructType*>(tmp);
	}
	return false;
}

bool JSWriter::isValidTypeCast(const Value* castI, const Value* castOp, Type* srcPtr, Type* dstPtr) const
{
	//Only pointer casts are possible anyway
	assert(srcPtr->isPointerTy() && dstPtr->isPointerTy());
	Type* src=cast<PointerType>(srcPtr)->getElementType();
	Type* dst=cast<PointerType>(dstPtr)->getElementType();
	//Conversion between client objects is free
	if(isClientType(src) && isClientType(dst))
		return true;
	//Conversion between any function pointer are ok
	if(src->isFunctionTy() && dst->isFunctionTy())
		return true;
	if(dst->isIntegerTy(8))
	{
		Value::const_use_iterator it=castI->use_begin();
		Value::const_use_iterator itE=castI->use_end();
		bool safeUsage=true;
		for(;it!=itE;++it)
		{
			const CallInst* ci=dyn_cast<const CallInst>(*it);
			if(!safeCallForNewedMemory(ci))
			{
				safeUsage=false;
				break;
			}
		}
		if(safeUsage)
			return true;
	}
	//Support getting functions back from the Vtable
	if(src->isPointerTy() && dst->isPointerTy())
	{
		Type* innerSrc=cast<PointerType>(src)->getElementType();
		Type* innerDst=cast<PointerType>(dst)->getElementType();
		if(innerSrc->isIntegerTy(8) || innerDst->isFunctionTy())
		{
			const GetElementPtrInst* gep=dyn_cast<const GetElementPtrInst>(castOp);
			if(gep)
				assert(false);
			const ConstantExpr* constGep=dyn_cast<const ConstantExpr>(castOp);
			if(constGep && constGep->getOpcode()==Instruction::GetElementPtr)
			{
				const Value* sourceVal = constGep->getOperand(0);
				if(sourceVal->hasName() &&
					strncmp(sourceVal->getName().data(),"_ZTV",4)==0)
				{
					//This casts ultimately comes from a VTable, it's ok
					return true;
				}
			}
		}
	}
	//Also allow the unsafe cast from i8* only when casting from new, malloc
	//NOTE: The fresh memory may be passed uncasted to memset to zero new memory
	//NOTE: The fresh memory may be passed uncasted to memcpy (it optimizes another cast to i8*)
	//NOTE: The fresh memory may be passed uncasted to icmp to test against null
	if(src->isIntegerTy(8))
	{
		bool comesFromNew = false;
		bool allowedRawUsages = true;
		const CallInst* newCall=dyn_cast<const CallInst>(castOp);
		if(newCall && (newCall->getCalledFunction()->getName()=="_Znwj"
				|| newCall->getCalledFunction()->getName()=="malloc"))
		{
			comesFromNew = true;
		}
		else
		{
			//Try invoke
			//TODO: Disable throw in new, it's nonsense in JS context
			const InvokeInst* newCall=dyn_cast<const InvokeInst>(castOp);
			if(newCall && (newCall->getCalledFunction()->getName()=="_Znwj"
					|| newCall->getCalledFunction()->getName()=="malloc"))
			{
				comesFromNew = true;
			}
		}
		Value::const_use_iterator it=castOp->use_begin();
		Value::const_use_iterator itE=castOp->use_end();
		for(;it!=itE;++it)
		{
			//Check that the other use is a memset or an icmp
			if((*it)==castI)
				continue;
			const CallInst* ci=dyn_cast<const CallInst>(*it);
			if(!(ICmpInst::classof(*it) || safeCallForNewedMemory(ci)))
				allowedRawUsages = false;
		}
		if(comesFromNew && allowedRawUsages)
			return true;
	}
	//Support upcasting. This is safe in javascript since at most we will access
	//undefined stuff. Start from the destination and see check the first element
	//recursively until the source type is found
	Type* currentType=dst;
	while(currentType->isStructTy())
	{
		StructType* t=static_cast<StructType*>(currentType);
		currentType=t->getElementType(0);
		if(currentType==src)
			return true;
	}
	src->dump();
	cerr << endl;
	dst->dump();
	cerr << endl;
	return false;
}

void JSWriter::compileConstantExpr(const ConstantExpr* ce)
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
			compileGEP(base, ce->op_begin()+1, ce->op_end()-1);
			break;
		}
		case Instruction::BitCast:
		{
			assert(ce->getNumOperands()==1);
			Value* val=ce->getOperand(0);
			Type* dst=ce->getType();
			Type* src=val->getType();
			//Special case guard variables, they are defined as 64bit,
			//but only the first byte is specified and probably used
			//Guard variables are identified by their mangling prefix
			if(val->hasName() && strncmp("_ZGV",val->getName().data(),4)==0)
			{
				compileOperand(val);
				break;
			}
			assert(isValidTypeCast(ce, val, src, dst));
			compileOperand(val);
			break;
		}
		default:
			cerr << "Unsupported constant expr " << ce->getOpcodeName() << endl;
	}
}

void JSWriter::compileConstant(const Constant* c)
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
		//Must compare pointers, it seems
		if(&f->getValueAPF().getSemantics()==&APFloat::IEEEsingle)
			stream << f->getValueAPF().convertToFloat();
		else if(&f->getValueAPF().getSemantics()==&APFloat::IEEEdouble)
			stream << f->getValueAPF().convertToDouble();
		else
			assert(false);
	}
	else if(ConstantInt::classof(c))
	{
		const ConstantInt* i=cast<const ConstantInt>(c);
		//TODO: Restore when 64bit are forbidden by the frontend
		//assert(i->getBitWidth()<=32);
		stream << i->getSExtValue();
	}
	else if(Function::classof(c))
	{
		assert(c->hasName());
		//printLLVMName already add '_' to the name
		printLLVMName(c->getName());
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
		printLLVMName(v->getName());
	else
	{
		cerr << "No name for value ";
		v->dump();
	}
}

void JSWriter::compileType(Type* t)
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
			//TODO: out assertion back
			//assert(it->getBitWidth()<=32);
			//Print out a '0'. To let the engine know this is an integer
			stream << '0';
			break;
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
				compileType(*E);
				offset++;
			}
			stream << " }";
			break;
		}
		case Type::PointerTyID:
			stream << "{ d:null, o: 0, p: '' }";
			break;
		default:
			cerr << "Support type ";
			t->dump();
			cerr << endl;
	}
}

void JSWriter::compilePHIOfBlockFromOtherBlock(const BasicBlock* to, const BasicBlock* from)
{
	BasicBlock::const_iterator I=to->begin();
	BasicBlock::const_iterator IE=to->end();
	for(;I!=IE;++I)
	{
		const PHINode* phi=dyn_cast<const PHINode>(I);
		if(phi==NULL)
			continue;
		const Value* val=phi->getIncomingValueForBlock(from);
		//TODO: verify that 'var' works
		stream << "var ";
		if(phi->hasName())
			printLLVMName(phi->getName());
		else
		{
			std::map<const Value*,int>::iterator it=unnamedValueMap.find(phi);
			if(it==unnamedValueMap.end())
				it=unnamedValueMap.insert(make_pair(phi, unnamedValueMap.size())).first;
			stream << "phi" << it->second;
		}
		stream << " = ";
		compileOperand(val);
		stream << ";\n";
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
			stream << ";\n";
			assert(I.getNumSuccessors()==0);
			break;
		}
		case Instruction::Invoke:
		{
			const InvokeInst& ci=static_cast<const InvokeInst&>(I);
			//TODO: Support unwind
			//For now, pretend it's a regular call
			if(ci.getCalledFunction())
			{
				//Direct call
				const char* funcName=ci.getCalledFunction()->getName().data();
				if(handleBuiltinCall(funcName,ci.op_begin(),ci.op_begin()+ci.getNumArgOperands()))
				{
					stream << ";\n";
					//Only consider the normal successor for PHIs here
					//For each successor output the variables for the phi nodes
					compilePHIOfBlockFromOtherBlock(ci.getNormalDest(), I.getParent());
					//Add code to jump to the next block
					stream << "__block = " << blocksMap.find(ci.getNormalDest())->second << ";\n";
					break;
				}
				else
					stream << '_' << funcName;
			}
			else
			{
				//Indirect call
				compileOperand(ci.getCalledValue());
			}

			stream << '(';
			for(uint32_t i=0;i<ci.getNumArgOperands();i++)
			{
				if(i!=0)
					stream << ", ";
				compileOperand(ci.getArgOperand(i));
			}
			stream << ");\n";
			//Only consider the normal successor for PHIs here
			//For each successor output the variables for the phi nodes
			compilePHIOfBlockFromOtherBlock(ci.getNormalDest(), I.getParent());
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
			//For each successor output the variables for the phi nodes
			const BranchInst& bi=static_cast<const BranchInst&>(I);
			if(bi.isUnconditional())
			{
				//Generate the PHIs
				compilePHIOfBlockFromOtherBlock(bi.getSuccessor(0), I.getParent());
				stream << "__block = " << blocksMap.find(bi.getSuccessor(0))->second << ";\n";
			}
			else
			{
				//In each branch generate the right PHIs
				stream << "if( ";
				compileOperand(bi.getCondition());
				stream << ") { ";
				compilePHIOfBlockFromOtherBlock(bi.getSuccessor(0), I.getParent());
				stream << "__block = " << blocksMap.find(bi.getSuccessor(0))->second <<
					"; } else {";
				compilePHIOfBlockFromOtherBlock(bi.getSuccessor(1), I.getParent());
				stream << "__block = " << blocksMap.find(bi.getSuccessor(1))->second <<
					";}\n";
			}
			break;
		}
		default:
			stream << "alert('Unsupported code');\n";
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
			if(ci.getCalledFunction())
			{
				//Direct call
				const char* funcName=ci.getCalledFunction()->getName().data();
				if(handleBuiltinCall(funcName,ci.op_begin(),ci.op_begin()+ci.getNumArgOperands()))
					return true;
				stream << '_' << funcName;
			}
			else
			{
				//Indirect call
				compileOperand(ci.getCalledValue());
			}
			stream << '(';
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
			stream << " alert('Exceptions not supported')";
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
			if(UndefValue::classof(aggr))
			{
				//We have to assemble the type object from scratch
				compileType(t);
				stream << ";\n";
				//Also assign the element
				assert(ivi.getNumIndices()==1);
				//Find the offset to the pointed element
				assert(ivi.hasName());
				printLLVMName(ivi.getName());
			}
			else
			{
				//Optimize for the assembly of the aggregate values
				assert(aggr->hasOneUse());
				assert(aggr->hasName());
				printLLVMName(aggr->getName());
			}
			uint32_t offset=ivi.getIndices()[0];
			stream << ".a" << offset << " = ";
			compileOperand(ivi.getInsertedValueOperand());
			return true;
		}
		case Instruction::Store:
		{
			const StoreInst& si=static_cast<const StoreInst&>(I);
			const Value* ptrOp=si.getPointerOperand();
			const Value* valOp=si.getValueOperand();
			//If the ptrOp has a single use and it'a a GEP
			//we can optimize it
			if(ptrOp->hasOneUse() && GetElementPtrInst::classof(ptrOp))
			{
				const GetElementPtrInst& gep=static_cast<const GetElementPtrInst&>(*ptrOp);
				compileFastGEPDereference(gep);
			}
			else
				compileDereferencePointer(ptrOp, 0);
			stream << " = ";
			compileOperand(valOp);
			return true;
		}
		default:
			return compileInlineableInstruction(I);
	}
}

bool JSWriter::isI32Type(Type* t) const
{
	//TODO: To compile bullet 64 bit integers
	//are needed. We don't want to support them
	//but for first round support we will cheat
	//as emscripten does and allow them
	return t->isIntegerTy();// && static_cast<IntegerType*>(t)->getBitWidth()==32;
}

void JSWriter::compileFastGEPDereference(const GetElementPtrInst& gep)
{
	GetElementPtrInst::const_op_iterator it=gep.idx_begin();
	const GetElementPtrInst::const_op_iterator itE=gep.idx_end();
	Type* t=gep.getOperand(0)->getType();
	assert(t->isPointerTy());
	PointerType* ptrT=static_cast<PointerType*>(t);
	if(ConstantInt::classof(*it))
	{
		uint32_t firstElement = getIntFromValue(*it);
		//First dereference the pointer
		compileDereferencePointer(gep.getOperand(0), firstElement);
	}
	else
	{
		//First dereference the pointer
		compileDereferencePointer(gep.getOperand(0), *it);
	}
	compileRecursiveAccessToGEP(ptrT->getElementType(), ++it, itE);
}

void JSWriter::compileGEP(const Value* val, const Use* it, const Use* const itE)
{
	Type* t=val->getType();
	assert(t->isPointerTy());
	PointerType* ptrT=static_cast<PointerType*>(t);
	stream << "{ d: ";
	if(it==itE)
	{
		//Same level access, we are just computing another pointer from this pointer
		compileOperand(val);
		stream << ".d, o: ";
		compileOperand(val);
		stream << ".o+";
		//Compute the offset
		if(ConstantInt::classof(*it))
		{
			uint32_t firstElement = getIntFromValue(*it);
			stream << firstElement;
		}
		else
			compileOperand(*it);
		stream << ", p: ";
		compileOperand(val);
		stream << ".p }";
	}
	else
	{
		if(ConstantInt::classof(*it))
		{
			uint32_t firstElement = getIntFromValue(*it);
			//First dereference the pointer
			compileDereferencePointer(val, firstElement);
		}
		else
			compileDereferencePointer(val, *it);
		const Type* lastType=compileRecursiveAccessToGEP(ptrT->getElementType(), ++it, itE);
		//Now add the offset for the desired element
		if(ConstantInt::classof(*itE))
		{
			uint32_t elementIndex = getIntFromValue(*itE);
			stream << ", o: " << elementIndex << ", p: ";
			if(StructType::classof(lastType))
				stream << "'a' }";
			else if(ArrayType::classof(lastType))
				stream << "'' }";
			else
				assert(false);
		}
		else
		{
			//Only arrays are accepted
			assert(ArrayType::classof(lastType));
			stream << ", o: ";
			compileOperand(*itE);
			stream << ", p: '' }";
		}
	}
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
			bi.dump();
			cerr << endl;
			bool vtableCast = isVTableCast(srcPtr, dstPtr);
			assert(vtableCast || isValidTypeCast(&bi, bi.getOperand(0), srcPtr, dstPtr));
			if(vtableCast)
			{
				//TODO: Implement recursive access to vtable
			}
			else
				compileOperand(bi.getOperand(0));
			return true;
		}
		case Instruction::Alloca:
		{
			const AllocaInst& ai=static_cast<const AllocaInst&>(I);
			//Alloca must return a pointer, create a 1 element array
			assert(ai.hasName());
			stream << "{ d: [";
			compileType(ai.getAllocatedType());
			stream << "], o: 0, p: ''}";
			//Take note that this is a complete object
			//completeObjects.insert(&I);
			return true;
		}
		case Instruction::FPToSI:
		{
			const CastInst& ci=static_cast<const CastInst&>(I);
			//Check that the in and out types are sane
			Type* srcT = ci.getSrcTy();
			Type* dstT = ci.getDestTy();
			//TODO: Restore type check, now relax it
			//assert(srcT->isDoubleTy());
			assert(isI32Type(dstT));

			stream << "(";
			compileOperand(ci.getOperand(0));
			//Seems to be the fastest way
			//http://jsperf.com/math-floor-vs-math-round-vs-parseint/33
			stream << " >> 0)";
			return true;
		}
		case Instruction::SIToFP:
		{
			const CastInst& ci=static_cast<const CastInst&>(I);
			//Check that the in and out types are sane
			Type* srcT = ci.getSrcTy();
			Type* dstT = ci.getDestTy();
			assert(isI32Type(srcT));
			assert(dstT->isDoubleTy());
			//It's a NOP, values are logically FP anyway in JS
			compileOperand(ci.getOperand(0));
			//Seems to be the fastest way
			//http://jsperf.com/math-floor-vs-math-round-vs-parseint/33
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
				const Value* val=gep.getOperand(0);
				GetElementPtrInst::const_op_iterator it=gep.idx_begin();
				//We compile as usual till the last level
				GetElementPtrInst::const_op_iterator itE=gep.idx_end()-1;
				compileGEP(val, it, itE);
			}
			return true;
		}
		case Instruction::Add:
		{
			//Integer addition
			assert(I.getNumOperands()==2);
			assert(isI32Type(I.getOperand(0)->getType()));
			assert(isI32Type(I.getOperand(1)->getType()));
			assert(isI32Type(I.getType()));
			stream << "((";
			compileOperand(I.getOperand(0));
			stream << " + ";
			compileOperand(I.getOperand(1));
			stream << ") >> 0)";
			return true;
		}
		case Instruction::Sub:
		{
			//Integer subtraction
			//TODO: optimize negation
			assert(I.getNumOperands()==2);
			assert(isI32Type(I.getOperand(0)->getType()));
			assert(isI32Type(I.getOperand(1)->getType()));
			assert(isI32Type(I.getType()));
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
			//TODO: put asserts back
			//assert(srcI->getBitWidth()<=32);
			//assert(dstI->getBitWidth()<=32);
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
			assert(isI32Type(I.getType()));
			stream << "((";
			compileOperand(I.getOperand(0));
			stream << " / ";
			compileOperand(I.getOperand(1));
			stream << ") >> 0)";
			return true;
		}
		case Instruction::Mul:
		{
			//Integer signed multiplication
			assert(I.getNumOperands()==2);
			assert(isI32Type(I.getOperand(0)->getType()));
			assert(isI32Type(I.getOperand(1)->getType()));
			assert(isI32Type(I.getType()));
			stream << "((";
			compileOperand(I.getOperand(0));
			stream << " * ";
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
		case Instruction::FCmp:
		{
			//Integer comparison
			const CmpInst& ci=static_cast<const CmpInst&>(I);
			assert(ci.getNumOperands()==2);
			//Check that the operation is JS safe
			//TOTO: Verify that any float type is ok
			/*switch(ci.getPredicate())
			{
				default:
					assert(ci.getOperand(0)->getType()->isDoubleTy());
					assert(ci.getOperand(1)->getType()->isDoubleTy());
			}*/
			stream << "(";
			compileOperand(ci.getOperand(0));
			compilePredicate(ci.getPredicate());
			compileOperand(ci.getOperand(1));
			stream << ")";
			return true;
		}
		case Instruction::Load:
		{
			const LoadInst& li=static_cast<const LoadInst&>(I);
			const Value* ptrOp=li.getPointerOperand();
			stream << "(";
			//If the ptrOp has a single use and it'a a GEP
			//we can optimize it
			if(ptrOp->hasOneUse() && GetElementPtrInst::classof(ptrOp))
			{
				const GetElementPtrInst& gep=static_cast<const GetElementPtrInst&>(*ptrOp);
				compileFastGEPDereference(gep);
			}
			else
				compileDereferencePointer(ptrOp, 0);
			stream << ")";
			return true;

		}
		default:
			stream << "alert('Unsupported code');\n";
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
			case Instruction::Alloca:
			case Instruction::Switch:
				return false;
			case Instruction::Add:
			case Instruction::Sub:
			case Instruction::Mul:
			case Instruction::And:
			case Instruction::Or:
			case Instruction::Xor:
			case Instruction::Trunc:
			case Instruction::FPToSI:
			case Instruction::SIToFP:
			case Instruction::SDiv:
			case Instruction::SRem:
			case Instruction::Shl:
			case Instruction::AShr:
			case Instruction::LShr:
			case Instruction::BitCast:
			case Instruction::GetElementPtr:
			case Instruction::FAdd:
			case Instruction::FDiv:
			case Instruction::FSub:
			case Instruction::FPTrunc:
			case Instruction::FPExt:
			case Instruction::FMul:
			case Instruction::FCmp:
			case Instruction::ICmp:
			case Instruction::ZExt:
			case Instruction::SExt:
			case Instruction::Load:
			case Instruction::Select:
			case Instruction::ExtractValue:
			//Unsigned opcodes are a problem, where do they come
			case Instruction::URem:
			case Instruction::UIToFP:
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
		if(I->hasName() && I->getOpcode()!=Instruction::PHI) //Phys are manually handled
		{
			stream << "var ";
			printLLVMName(I->getName());
			stream << " = ";
		}
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
	stream << "function _" << F.getName().data() << "(";
	const Function::const_arg_iterator A=F.arg_begin();
	const Function::const_arg_iterator AE=F.arg_end();
	for(Function::const_arg_iterator curArg=A;curArg!=AE;++curArg)
	{
		if(curArg!=A)
			stream << ", ";
		if(curArg->hasName())
			printLLVMName(curArg->getName());
		else
			stream << "arg" << curArg->getArgNo();
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
		stream << "var __block=0;\nwhile(true){\nswitch(__block){\n";
		for(;B!=BE;++B)
		{
			stream << "case " << blocksMap.find(&(*B))->second << ":\n";
			compileBB(*B, blocksMap);
			stream << "break;\n";
		}
		stream << "}}\n";
	}

	stream << "}\n";
}

void JSWriter::makeJS()
{
	//Header: output a guard variable to to set the environment
	stream << "var __duetto_compiler = true;\n";
	Module::iterator F=module->begin();
	Module::iterator FE=module->end();
	for (; F != FE; ++F)
	{
		compileMethod(*F);
	}
	//Invoke the webMain function
	stream << "__Z7webMainv();\n";
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

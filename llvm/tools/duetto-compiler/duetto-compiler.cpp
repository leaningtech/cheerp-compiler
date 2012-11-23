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
	uint32_t getIntFromValue(Value* v);
	std::map<const Value*, std::string> inlineOperandMap;
	std::set<const Value*> completeObjects;
public:
	JSWriter(Module* m, raw_fd_ostream& s):module(m),stream(s)
	{
	}
	void makeJS();
	void compileMethod(Function& F);
	void compileBB(BasicBlock& BB, const std::map<const BasicBlock*, uint32_t>& blocksMap);
	void compileOperand(Value* v);
	void compileConstant(Constant* c);
	void compileConstantExpr(ConstantExpr* ce);
	void compileRecursiveGEP(ConstantExpr* ce, Constant* base, uint32_t level);
	uint32_t compileType(Type* t);
	bool isValidTypeCast(Type* src, Type* dst) const;
};

uint32_t JSWriter::getIntFromValue(Value* v)
{
	assert(ConstantInt::classof(v));
	ConstantInt* i=cast<ConstantInt>(v);
	return i->getZExtValue();
}

void JSWriter::compileRecursiveGEP(ConstantExpr* ce, Constant* base, uint32_t level)
{
	//TODO: Support multiple dereferece in GEP
	assert(ce->getNumOperands()==level+3);
	stream << "{ d: ";
	compileConstant(base);
	stream << ", o: " << getIntFromValue(ce->getOperand(level+2));
	stream << ", p: ";
	assert(!ConstantStruct::classof(base));
	stream << "'' }";
}

bool JSWriter::isValidTypeCast(Type* srcPtr, Type* dstPtr) const
{
	//Only pointer casts are possible anyway
	assert(srcPtr->isPointerTy() && dstPtr->isPointerTy());
	Type* src=cast<PointerType>(srcPtr)->getElementType();
	Type* dst=cast<PointerType>(dstPtr)->getElementType();
	//Conversion between client objects is free
	if(src->isStructTy() && dst->isStructTy() &&
		strncmp(src->getStructName().data(), "class.client::", 14)==0 &&
		strncmp(dst->getStructName().data(), "class.client::", 14)==0)
	{
		return true;
	}
	//Conversion between any function pointers are ok
	if(src->isFunctionTy() && dst->isFunctionTy())
		return true;
	src->dump();
	cerr << endl;
	dst->dump();
	cerr << endl;
	return false;
}

void JSWriter::compileConstantExpr(ConstantExpr* ce)
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

void JSWriter::compileConstant(Constant* c)
{
	if(ConstantExpr::classof(c))
		compileConstantExpr(cast<ConstantExpr>(c));
	else if(ConstantDataSequential::classof(c))
	{
		ConstantDataSequential* d=cast<ConstantDataSequential>(c);
		assert(d->isString());
		//TODO: Support \x escapes
		stream << '"';
		stream << d->getAsString().data();
		stream << '"';
	}
	else if(ConstantFP::classof(c))
	{
		ConstantFP* f=cast<ConstantFP>(c);
		stream << f->getValueAPF().convertToDouble();
	}
	else if(ConstantInt::classof(c))
	{
		ConstantInt* i=cast<ConstantInt>(c);
		assert(i->getBitWidth()>=32);
		stream << i->getSExtValue();
	}
	else
	{
		cerr << "Unsupported constant type ";
		c->dump();
	}
}

void JSWriter::compileOperand(Value* v)
{
	//Check the inline map first
	map<const Value*, std::string>::iterator it=inlineOperandMap.find(v);
	if(it!=inlineOperandMap.end())
	{
		stream << it->second;
		return;
	}

	Constant* c=dyn_cast<Constant>(v);
	if(c)
		compileConstant(c);
	else if(v->hasName())
		stream << v->getName().data();
	else
	{
		cerr << "No name for value ";
		v->dump();
	}
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
		default:
			cerr << "Support type ";
			t->dump();
			cerr << endl;
			return 0;
	}
}

void JSWriter::compileBB(BasicBlock& BB, const std::map<const BasicBlock*, uint32_t>& blocksMap)
{
	BasicBlock::iterator I=BB.begin();
	BasicBlock::iterator IE=BB.end();
	for(;I!=IE;++I)
	{
		switch(I->getOpcode())
		{
			case Instruction::Call:
			{
				const CallInst& ci=static_cast<const CallInst&>(*I);
				bool isVoid=ci.getType()->isVoidTy();
				if(isVoid==false)
					stream << "var " << ci.getName().data() << " = ";
				stream << ci.getCalledFunction()->getName().data() << '(';
				for(uint32_t i=0;i<ci.getNumArgOperands();i++)
				{
					if(i!=0)
						stream << ", ";
					compileOperand(ci.getArgOperand(i));
				}
				stream << ");\n";
				break;
			}
			case Instruction::Ret:
			{
				const ReturnInst& ri=static_cast<const ReturnInst&>(*I);
				Value* retVal = ri.getReturnValue();
				stream << "return ";
				if(retVal)
					compileOperand(retVal);
				stream << ";\n";
				break;
			}
			case Instruction::Invoke:
			{
				const InvokeInst& ci=static_cast<const InvokeInst&>(*I);
				//TODO: Support unwind
				//For now, pretend it's a regular call
				bool isVoid=ci.getType()->isVoidTy();
				if(isVoid==false)
					stream << "var " << ci.getName().data() << " = ";
				stream << ci.getCalledFunction()->getName().data() << '(';
				for(uint32_t i=0;i<ci.getNumArgOperands();i++)
				{
					if(i!=0)
						stream << ", ";
					compileOperand(ci.getArgOperand(i));
				}
				stream << ");\n";
				//Add code to jump to the next block
				stream << "__block = " << blocksMap.find(ci.getNormalDest())->second << ";\n";
				break;
			}
			case Instruction::BitCast:
			{
				const BitCastInst& bi=static_cast<const BitCastInst&>(*I);
				Type* srcPtr=bi.getSrcTy();
				Type* dstPtr=bi.getDestTy();
				assert(isValidTypeCast(srcPtr, dstPtr));
				//HACK?: Some bitcast seems to have no name
				//Since they are nop, just put in the inline operand map
				//the name of the source if so
				if(bi.hasName())
				{
					stream << "var " << bi.getName().data() << " = " <<
						bi.getOperand(0)->getName().data() << ";\n";
				}
				else
				{
					const Value* srcVal = bi.getOperand(0);
					assert(srcVal->hasName());
					inlineOperandMap.insert(make_pair(&bi, srcVal->getName()));
				}
				break;
			}
			case Instruction::LandingPad:
			{
				//TODO: Support exceptions
				return;
			}
			case Instruction::Alloca:
			{
				const AllocaInst& ai=static_cast<const AllocaInst&>(*I);
				//Alloca is supposed to return a pointer. We will cheat and return
				//an object.
				assert(ai.hasName());
				stream << "var " << ai.getName().data() << " = ";
				compileType(ai.getAllocatedType());
				stream << ";\n";
				//Take note that this is a complete object
				completeObjects.insert(&(*I));
				break;
			}
			case Instruction::FPToSI:
			{
				const CastInst& ci=static_cast<CastInst&>(*I);
				//Check that the in and out types are sane
				Type* srcT = ci.getSrcTy();
				Type* dstT = ci.getDestTy();
				assert(srcT->isDoubleTy());
				assert(dstT->isIntegerTy());
				IntegerType* dstIntT = static_cast<IntegerType*>(dstT);
				assert(dstIntT->getBitWidth()==32);

				assert(ci.hasName());
				stream << "var " << ci.getName().data() << " = ";
				compileOperand(ci.getOperand(0));
				//Seems to be the fastest way
				//http://jsperf.com/math-floor-vs-math-round-vs-parseint/33
				stream << " >> 0;\n";
				break;
			}
			default:
				cerr << "\tImplement inst " << I->getOpcodeName() << endl;
				return;
		}
	}
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

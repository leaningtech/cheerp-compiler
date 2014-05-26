//===-- DuettoWriter.cpp - The Duetto JavaScript generator -------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "Relooper.h"
#include "llvm/Duetto/Utility.h"
#include "llvm/Duetto/Writer.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;
using namespace std;
using namespace duetto;

//De-comment this to debug why a global is included in the JS
//#define DEBUG_GLOBAL_DEPS

class DuettoRenderInterface: public RenderInterface
{
private:
	DuettoWriter* writer;
	const NewLineHandler& NewLine;
	void renderCondition(const BasicBlock* B, int branchId);
public:
	DuettoRenderInterface(DuettoWriter* w, const NewLineHandler& n):writer(w),NewLine(n)
	{
	}
	void renderBlock(const void* privateBlock);
	void renderIfBlockBegin(const void* privateBlock, int branchId, bool first);
	void renderIfBlockBegin(const void* privateBlock, const vector<int>& branchId, bool first);
	void renderElseBlockBegin();
	void renderBlockEnd();
	void renderBlockPrologue(const void* privateBlockTo, const void* privateBlockFrom);
	bool hasBlockPrologue(const void* privateBlockTo) const;
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

raw_ostream& duetto::operator<<(raw_ostream& s, const NewLineHandler& handler)
{
	s << "\n";
	handler.sourceMapGenerator.finishLine();
	return s;
}

void DuettoWriter::handleBuiltinNamespace(const char* identifier, const llvm::Function* calledFunction,
			User::const_op_iterator it, User::const_op_iterator itE)
{
	const char* ident = identifier;
	//Read the class name
	char* className;
	int classLen = strtol(ident,&className,10);
	if(classLen == 0)
	{
		llvm::report_fatal_error(Twine("Unexpected C++ mangled name: ", StringRef(identifier)), false);
		return;
	}
	ident = className + classLen;

	//Read the function name
	char* funcName;
	int funcNameLen=strtol(ident,&funcName,10);
	if(funcNameLen==0)
	{
		//This means that we are parsing a fuction which is not in a class
		//and we already parsed the function name
		funcName = className;
		funcNameLen = classLen;
		className = NULL;
		classLen = 0;
	}
	//This condition is necessarily true
	assert(funcNameLen!=0);

	bool isClientStatic = calledFunction->hasFnAttribute(Attribute::Static);
	//The first arg should be the object
	if(strncmp(funcName,"get_",4)==0 && (itE-it)==1)
	{
		//Getter
		if(className == NULL)
		{
			llvm::report_fatal_error(Twine("Unexpected getter without class: ", StringRef(identifier)), false);
			return;
		}
		compileOperand(*it);
		stream << ".";
		stream.write(funcName+4,funcNameLen-4);
	}
	else if(strncmp(funcName,"set_",4)==0 && (itE-it)==2)
	{
		//Setter
		if(className == NULL)
		{
			llvm::report_fatal_error(Twine("Unexpected setter without class: ", StringRef(identifier)), false);
			return;
		}
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
		if(className)
		{
			if(isClientStatic)
				stream.write(className,classLen);
			else if(it == itE)
			{
				llvm::report_fatal_error(Twine("At least 'this' parameter was expected: ",
					StringRef(identifier)), false);
				return;
			}
			else
			{
				compileOperand(*it);
				++it;
			}
			stream << ".";
		}
		stream.write(funcName,funcNameLen);
		compileMethodArgs(it,itE);
	}
}

void DuettoWriter::compileCopyRecursive(const std::string& baseName, const Value* baseDest,
		const Value* baseSrc, const Type* currentType, const char* namedOffset)
{
	switch(currentType->getTypeID())
	{
		case Type::IntegerTyID:
		case Type::FloatTyID:
		case Type::DoubleTyID:
		case Type::PointerTyID:
		{
			compileDereferencePointer(baseDest, NULL, namedOffset);
			stream << baseName << " = ";
			compileDereferencePointer(baseSrc, NULL, namedOffset);
			stream << baseName << ';' << NewLine;
			break;
		}
		case Type::StructTyID:
		{
			if(TypeSupport::isUnion(currentType))
			{
				stream << "var __tmp__=new Int8Array(";
				compileDereferencePointer(baseDest, NULL, namedOffset);
				stream << baseName << ");" << NewLine;
				stream << "__tmp__.set(";
				stream << "new Int8Array(";
				compileDereferencePointer(baseSrc, NULL, namedOffset);
				stream << baseName << "));" << NewLine;
				break;
			}
			const StructType* st=static_cast<const StructType*>(currentType);
			StructType::element_iterator E=st->element_begin();
			StructType::element_iterator EE=st->element_end();
			uint32_t offset=0;
			for(;E!=EE;++E)
			{
				char buf[16];
				snprintf(buf,16,".a%u",offset);
				compileCopyRecursive(baseName+buf, baseDest, baseSrc, *E, namedOffset);
				offset++;
			}
			break;
		}
		case Type::ArrayTyID:
		{
			const ArrayType* at=static_cast<const ArrayType*>(currentType);
			char buf[16];
			for(uint64_t i=0;i<at->getNumElements();i++)
			{
				snprintf(buf,16,"[%lu]",i);
				compileCopyRecursive(baseName+buf, baseDest, baseSrc, at->getElementType(), namedOffset);
			}
			break;
		}
		default:
			llvm::errs() << "Support type in copy ";
			currentType->dump();
			llvm::errs() << '\n';
	}
}

void DuettoWriter::compileResetRecursive(const std::string& baseName, const Value* baseDest,
		const Value* resetValue, const Type* currentType, const char* namedOffset)
{
	switch(currentType->getTypeID())
	{
		case Type::IntegerTyID:
		{
			compileDereferencePointer(baseDest, NULL, namedOffset);
			stream << baseName << " = ";
			if(Constant::classof(resetValue))
			{
				uint8_t constResetValue = getIntFromValue(resetValue);
				char buf[11];
				buf[10]=0;
				if(currentType->getIntegerBitWidth()==8)
					snprintf(buf,10,"0x%x",constResetValue);
				else if(currentType->getIntegerBitWidth()==16)
					snprintf(buf,10,"0x%x%x",constResetValue,constResetValue);
				else if(currentType->getIntegerBitWidth()==32)
				{
					snprintf(buf,10,"0x%x%x%x%x",
						constResetValue,constResetValue,constResetValue,constResetValue);
				}
				else
					llvm::report_fatal_error("Unsupported values for memset", false);
				stream << buf;
			}
			else
			{
				if(currentType->getIntegerBitWidth()!=8)
					llvm::report_fatal_error("Unsupported values for memset", false);
				compileOperand(resetValue);
			}
			stream << ';' << NewLine;
			break;
		}
		case Type::FloatTyID:
		case Type::DoubleTyID:
		{
			compileDereferencePointer(baseDest, NULL, namedOffset);
			if(!Constant::classof(resetValue) || getIntFromValue(resetValue) != 0)
				llvm::report_fatal_error("Unsupported values for memset", false);
			stream << baseName << " = 0;" << NewLine;
			break;
		}
		case Type::PointerTyID:
		{
			compileDereferencePointer(baseDest, NULL, namedOffset);
			if(!Constant::classof(resetValue) || getIntFromValue(resetValue) != 0)
				llvm::report_fatal_error("Unsupported values for memset", false);
			//Pointers to client objects must use a normal null
			const Type* pointedType = currentType->getPointerElementType();
			stream << baseName << " = ";
			if(types.isClientType(pointedType))
				stream << "null";
			else
				stream << "nullObj";
			stream << ';' << NewLine;
			break;
		}
		case Type::StructTyID:
		{
			if(TypeSupport::isUnion(currentType))
			{
				stream << "var __tmp__=new Int8Array(";
				compileDereferencePointer(baseDest, NULL, namedOffset);
				stream << baseName << ");" << NewLine;
				stream << "for(var __i__=0;__i__<__tmp__.length;__i__++) __tmp__[__i__]=0;" << NewLine;
				break;
			}
			const StructType* st=static_cast<const StructType*>(currentType);
			StructType::element_iterator E=st->element_begin();
			StructType::element_iterator EE=st->element_end();
			uint32_t offset=0;
			for(;E!=EE;++E)
			{
				char buf[16];
				snprintf(buf,16,".a%u",offset);
				compileResetRecursive(baseName+buf, baseDest, resetValue, *E, namedOffset);
				offset++;
			}
			break;
		}
		case Type::ArrayTyID:
		{
			const ArrayType* at=static_cast<const ArrayType*>(currentType);
			char buf[16];
			for(uint64_t i=0;i<at->getNumElements();i++)
			{
				snprintf(buf,16,"[%lu]",i);
				compileResetRecursive(baseName+buf, baseDest, resetValue, at->getElementType(), namedOffset);
			}
			break;
		}
		default:
			llvm::errs() << "Support type in reset ";
			currentType->dump();
			llvm::errs() << '\n';
	}
}

void DuettoWriter::compileDowncast(const Value* src, uint32_t baseOffset)
{
	Type* t=src->getType()->getPointerElementType();
	if(types.isClientType(t) || baseOffset==0)
		compileOperand(src);
	else
	{
		//Do a runtime downcast
		stream << "{d:";
		compileDereferencePointer(src, NULL);
		stream << ".a,o:";
		compileDereferencePointer(src, NULL);
		stream << ".o-" << baseOffset << "}";
	}
}

void DuettoWriter::compileMove(const Value* dest, const Value* src, const Value* size)
{
	//TODO: Optimize the checks if possible
	//Check if they are inside the same memory island
	stream << "if(";
	Type* lastTypeDest=compileObjectForPointer(dest, NORMAL);
	stream << "===";
	Type* lastTypeSrc=compileObjectForPointer(src, NORMAL);
	//If so they may overlap, check and use reverse copy if needed
	stream << "&&";
	bool notFirst=compileOffsetForPointer(dest,lastTypeDest);
	if(!notFirst)
		stream << '0';
	stream << ">";
	notFirst=compileOffsetForPointer(src,lastTypeSrc);
	if(!notFirst)
		stream << '0';
	stream << "){" << NewLine;
	//Destination is after source, copy backward
	compileMemFunc(dest, src, size, BACKWARD);
	stream << "}else{";
	//Destination is before source, copy forward
	compileMemFunc(dest, src, size, FORWARD);
	stream << "}" << NewLine;
}


/* Method that handles memcpy, memset and memmove.
 * If src is not NULL present a copy operation is done using the supplied direction.
 * memset is handled by passing a NULL src and setting resetValue as needed. direction should be FORWARD */
void DuettoWriter::compileMemFunc(const Value* dest, const Value* src, const Value* size,
		COPY_DIRECTION copyDirection)
{
	Type* destType=dest->getType();

	Type* pointedType = static_cast<PointerType*>(destType)->getElementType();
	if(TypeSupport::isUnion(pointedType))
	{
		//We can use the natural i8*, since the union will have already an allocated
		//typed array when it has been casted to i8*
		pointedType = destType->getPointerElementType();
	}
	uint32_t typeSize = targetData.getTypeAllocSize(pointedType);

	//Check that the number of element is not zero
	if(ConstantInt::classof(size))
	{
		uint32_t allocatedSize = getIntFromValue(size);
		uint32_t numElem = (allocatedSize+typeSize-1)/typeSize;
		if(numElem==0)
			return;
	}
	else
	{
		//Compute number of elements at runtime
		stream << "var __numElem__=";
		compileOperand(size);
		stream << '/' << typeSize;
		//Make sure to close this if below
		stream << ';' << NewLine << "if(__numElem__!=0)" << NewLine << '{';
	}

	//The first element is copied directly, to support complete objects
	//In the BACKWARD case we need to copy the first as the last element
        //and we do this below
	if(copyDirection==RESET)
		compileResetRecursive("", dest, src, pointedType, NULL);
	else if(copyDirection==FORWARD)
		compileCopyRecursive("", dest, src, pointedType, NULL);

	//The rest is compiled using a for loop, or native TypedArray set operator

	//NOTE: For constant values we can stop code generation here
	//For the dynamic case we still need to close the if below
	if(ConstantInt::classof(size))
	{
		uint32_t allocatedSize = getIntFromValue(size);
		uint32_t numElem = (allocatedSize+typeSize-1)/typeSize;

		if(numElem==1)
			return;
	}

	Type* lastTypeSrc = NULL;
	Type* lastTypeDest = NULL;
	//Prologue: Construct the first part, up to using the size
	if(copyDirection!=RESET && types.isTypedArrayType(pointedType))
	{
		// The semantics of set is memmove like, no need to care about direction
		lastTypeDest=compileObjectForPointer(dest, NORMAL);
		stream << ".set(";
		lastTypeSrc=compileObjectForPointer(src, NORMAL);
		//We need to get a subview of the source
		stream << ".subarray(";
		bool notFirst=compileOffsetForPointer(src,lastTypeSrc);
		if(!notFirst)
			stream << '0';
		stream << ',';
		notFirst=compileOffsetForPointer(src,lastTypeSrc);
		if(notFirst)
			stream << '+';
	}
	else
	{
		//memset is always handled using the for loop
		if(copyDirection == FORWARD || copyDirection == RESET)
			stream << "for(var __i__=1;__i__<";
		else
			stream << "for(var __i__=";
	}

	// Use the size
	if(ConstantInt::classof(size))
	{
		uint32_t allocatedSize = getIntFromValue(size);
		uint32_t numElem = (allocatedSize+typeSize-1)/typeSize;

		stream << numElem;
	}
	else
	{
		stream << "__numElem__";
	}

	//Epilogue: Write the code after the size
	if(copyDirection!=RESET && types.isTypedArrayType(pointedType))
	{
		stream << "),";
		bool notFirst=compileOffsetForPointer(dest,lastTypeDest);
		if(!notFirst)
			stream << '0';
		stream << ");" << NewLine;
	}
	else
	{
		if(copyDirection == FORWARD || copyDirection == RESET)
			stream	<< ";__i__++){" << NewLine;
		else
			stream << "-1;__i__>0;__i__--){" << NewLine;

		if(copyDirection==RESET)
			compileResetRecursive("", dest, src, pointedType,"__i__");
		else
			compileCopyRecursive("", dest, src, pointedType,"__i__");
		stream << NewLine << '}';
	}

	//The first element must be copied last in the backward case
	if(copyDirection==BACKWARD)
		compileCopyRecursive("", dest, src, pointedType, NULL);

	if(!ConstantInt::classof(size))
	{
		//Close the if for the '0' case
		stream << NewLine << '}';
	}
}

void DuettoWriter::compileAllocation(const DynamicAllocInfo & info)
{
	assert (info.isValidAlloc());

	Type * t = info.getCastedType()->getElementType();

	uint32_t typeSize = targetData.getTypeAllocSize(t);
	
	if (info.useTypedArray())
	{
		stream << "new ";
		compileTypedArrayType(t);
		stream << '(';
		
		if(info.getNumberOfElementsArg())
			compileOperand(info.getNumberOfElementsArg());
		else if( !info.sizeIsRuntime() )
		{
			uint32_t allocatedSize = getIntFromValue( info.getByteSizeArg() );
			uint32_t numElem = (allocatedSize+typeSize-1)/typeSize;
			stream << numElem;
		}
		else
		{
			compileOperand( info.getByteSizeArg() );
			stream << '/' << typeSize;
		}
		stream << ')';
	}
	else if (info.useCreateArrayFunc() )
	{
		assert( t->isStructTy() );
		StructType* st = cast<StructType>(t);
		
		assert( globalDeps.dynAllocArrays().count(st) );
		
		stream << "createArray";
		printLLVMName(st->getName(), GLOBAL);
		stream << '(';
		if( info.getNumberOfElementsArg() )
			compileOperand( info.getNumberOfElementsArg() );
		else
		{
			compileOperand( info.getByteSizeArg() );
			stream << '/' << typeSize;
		}
		stream << ')';
	}
	else if (info.useCreatePointerArrayFunc() )
	{
		stream << "createPointerArray(";
		if( info.getNumberOfElementsArg() )
			compileOperand( info.getNumberOfElementsArg() );
		else
		{
			compileOperand( info.getByteSizeArg() );
			stream << '/' << typeSize;
		}
		stream << ')';
	
		assert( globalDeps.needCreatePointerArray() );
	}
	else if (!info.sizeIsRuntime() )
	{
		// Create a plain array
		const Value * numberOfElems = info.getNumberOfElementsArg();
		
		//NOTE should we use uint32_t here? Probably not, but need to fix getIntFromValue too!
		uint32_t numElem;
		
		if (numberOfElems)
			numElem = getIntFromValue( numberOfElems );
		else
		{
			assert( isa<ConstantInt>( info.getByteSizeArg() ) );
			uint32_t allocatedSize = getIntFromValue( info.getByteSizeArg() );

			numElem = (allocatedSize+typeSize-1)/typeSize;
		}
		
		stream << '[';
		for(uint32_t i = 0; i < numElem;i++)
		{
			compileType(t, LITERAL_OBJ);
			if((i+1) < numElem)
				stream << ",";
		}
		stream << ']';
	}
	else
	{
		llvm::errs() << "Allocating type " << *t << "\n";
		llvm::report_fatal_error("Unsupported type in allocation", false);
	}
}

void DuettoWriter::compileFree(const Value* obj)
{
	//TODO: Clean up class related data structures
}

DuettoWriter::COMPILE_INSTRUCTION_FEEDBACK DuettoWriter::handleBuiltinCall(ImmutableCallSite callV, const Function * func)
{
	assert( callV.isCall() || callV.isInvoke() );
	assert( func );
	assert( (func == callV.getCalledFunction() ) || !(callV.getCalledFunction()) );
	
	bool userImplemented = !func->empty();
	
	ImmutableCallSite::arg_iterator it = callV.arg_begin(), itE = callV.arg_end();
	
	const char* ident = func->getName().data();
	unsigned instrinsicId = func->getIntrinsicID();
	//First handle high priority builtins, they will be used even
	//if an implementation is available from the user
	if(instrinsicId==Intrinsic::memmove)
	{
		compileMove(*(it), *(it+1), *(it+2));
		return COMPILE_EMPTY;
	}
	else if(instrinsicId==Intrinsic::memcpy)
	{
		compileMemFunc(*(it), *(it+1), *(it+2), FORWARD);
		return COMPILE_EMPTY;
	}
	else if(instrinsicId==Intrinsic::memset)
	{
		//TODO: memset on allocate memory may be optimized
		compileMemFunc(*(it), *(it+1), *(it+2), RESET);
		return COMPILE_EMPTY;
	}
	else if(instrinsicId==Intrinsic::invariant_start)
	{
		//TODO: Try to optimize using this, for now just pass the second arg
		compileOperand(*(it+1));
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::invariant_end)
		return COMPILE_EMPTY;
	else if(instrinsicId==Intrinsic::vastart)
	{
		compileDereferencePointer(*it, NULL);
		stream << " = { d:arguments, o:_" << currentFun->getName() << ".length }";
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::vaend)
	{
		compileDereferencePointer(*it, NULL);
		stream << "=null";
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::duetto_downcast)
	{
		compileDowncast(*(it), getIntFromValue(*(it+1)));
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::duetto_upcast_collapsed)
	{
		compileOperand(*it);
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::duetto_cast_user)
	{
		compileOperand(*it);
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::duetto_pointer_base)
	{
		compileObjectForPointer(*it, NORMAL);
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::duetto_pointer_offset)
	{
		Type* lastType = compileObjectForPointer(*it, DRY_RUN);
		bool ret=compileOffsetForPointer(*it, lastType);
		if(!ret)
			stream << '0';
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::duetto_create_closure)
	{
		assert( globalDeps.needCreateClosure() );

		//We use an helper method to create closures without
		//keeping all local variable around. The helper
		//method is printed on demand depending on a flag
		stream << "duettoCreateClosure";
		compileMethodArgs(it, itE);
		return COMPILE_OK;
	}
	else if(strcmp(ident,"free")==0 ||
		strcmp(ident,"_ZdlPv")==0 ||
		strcmp(ident,"_ZdaPv")==0)
	{
		compileFree(*it);
		return COMPILE_OK;
	}
	else if(strcmp(ident,"fmod")==0)
	{
		// Handle this internally, C++ does not have float mod operation
		stream << '(';
		compileOperand(*(it));
		stream << '%';
		compileOperand(*(it+1));
		stream << ')';
		return COMPILE_OK;
	}
	else
	{
		DynamicAllocInfo da(callV);
		
		if (da.isValidAlloc())
		{
			compileAllocation(da);
			return COMPILE_OK;
		}
	}

	//If the method is implemented by the user, stop here
	if(userImplemented)
		return COMPILE_UNSUPPORTED;

	if(strncmp(ident,"_ZN6client",10)==0)
	{
		handleBuiltinNamespace(ident+10,callV.getCalledFunction(),it,itE);
		return COMPILE_OK;
	}
	else if(strncmp(ident,"_ZNK6client",11)==0)
	{
		handleBuiltinNamespace(ident+11,callV.getCalledFunction(),it,itE);
		return COMPILE_OK;
	}
	else if(strncmp(ident,"duettoCreate_ZN6client",22)==0)
	{
		//Default handling of builtin constructors
		char* typeName;
		int typeLen=strtol(ident+22,&typeName,10);
		//For builtin String, do not use new
		if(strncmp(typeName, "String", 6)!=0)
			stream << "new ";
		stream.write(typeName, typeLen);
		compileMethodArgs(it, itE);
		return COMPILE_OK;
	}
	return COMPILE_UNSUPPORTED;
}

void DuettoWriter::compilePredicate(CmpInst::Predicate p)
{
	switch(p)
	{
		case CmpInst::FCMP_UEQ: //TODO: fix this, if an operand is NaN LLVM expects false,
		case CmpInst::FCMP_OEQ:
		case CmpInst::ICMP_EQ:
			stream << " === ";
			break;
		case CmpInst::FCMP_UNE: //The undordered case correspond to the usual JS operator
					//See ECMA-262, Section 11.9.6
		case CmpInst::ICMP_NE:
			stream << " !== ";
			break;
		case CmpInst::FCMP_OGT: //TODO: fix this, if an operand is NaN LLVM expects false,
		case CmpInst::FCMP_UGT:	//but JS returns undefined. Adding ==true after the whole expression
					//should work
		case CmpInst::ICMP_SGT:
		case CmpInst::ICMP_UGT: //TODO: To support unsigned we need to add casts around the ops
			stream << " > ";
			break;
		case CmpInst::FCMP_UGE:
		case CmpInst::FCMP_OGE:
		case CmpInst::ICMP_SGE:
		case CmpInst::ICMP_UGE:
			stream << " >= ";
			break;
		case CmpInst::FCMP_OLT: //TODO: fix this, if an operand is NaN LLVM expects false,
		case CmpInst::FCMP_ULT:	//but JS returns undefined. Adding ==true after the whole expression
					//should work
		case CmpInst::ICMP_SLT:
		case CmpInst::ICMP_ULT: //TODO: To support unsigned we need to add casts around the ops
			stream << " < ";
			break;
		case CmpInst::FCMP_ULE:
		case CmpInst::FCMP_OLE:
		case CmpInst::ICMP_SLE:
		case CmpInst::ICMP_ULE:
			stream << " <= ";
			break;
		default:
			llvm::errs() << "Support predicate " << p << '\n';
	}
}

void DuettoWriter::compileOperandForIntegerPredicate(const Value* v, CmpInst::Predicate p)
{
	if(CmpInst::isUnsigned(p))
		compileUnsignedInteger(v);
	else
		compileSignedInteger(v);
}

void DuettoWriter::compileEqualPointersComparison(const llvm::Value* lhs, const llvm::Value* rhs, CmpInst::Predicate p)
{
	// Pointers to functions and client objects are compared directly.
	// All other pointers are compared using the base and offset separately
	llvm::Type* pointedType = lhs->getType()->getPointerElementType();
	bool isFunction = pointedType->isFunctionTy();
	bool isClient = types.isClientType(pointedType);

	if(isFunction || isClient)
	{
		//Functions can be compared by reference, the can't be in an array
		//There can be an array of pointer to functions, not an array of functions
		compileOperand(lhs);
		if(p==CmpInst::ICMP_NE)
			stream << "!==";
		else
			stream << "===";
		compileOperand(rhs);
	}
	else
	{
		Type* lastType1=compileObjectForPointer(lhs, NORMAL);
		if(p==CmpInst::ICMP_NE)
			stream << "!==";
		else
			stream << "===";
		Type* lastType2=compileObjectForPointer(rhs, NORMAL);
		if(analyzer.getPointerKind(lhs)==REGULAR ||
			analyzer.getPointerKind(rhs)==REGULAR)
		{
			if(p==CmpInst::ICMP_NE)
				stream << " || ";
			else
				stream << " && ";
			bool notFirst=compileOffsetForPointer(lhs,lastType1);
			if(!notFirst)
				stream << '0';
			if(p==CmpInst::ICMP_NE)
				stream << "!==";
			else
				stream << "===";
			notFirst=compileOffsetForPointer(rhs,lastType2);
			if(!notFirst)
				stream << '0';
		}
	}
}

void DuettoWriter::printLLVMName(const StringRef& s, NAME_KIND nameKind) const
{
	const char* data=s.data();
	//Add an '_' or 'L' to skip reserved names
	stream.write((nameKind==GLOBAL)?"_":"L",1);
	for(uint32_t i=0;i<s.size();i++)
	{
		//We need to escape invalid chars
		switch(data[i])
		{
			case '.':
				stream.write("_p",2);
				break;
			case '-':
				stream.write("_m",2);
				break;
			case ':':
				stream.write("_c",2);
				break;
			case '<':
				stream.write("_l",2);
				break;
			case '>':
				stream.write("_r",2);
				break;
			case ' ':
				stream.write("_s",2);
				break;
			case '_':
				//NOTE: This may cause collisions
				stream.write("_",1);
				break;
			default:
				stream.write(data+i,1);
		}
	}
}

void DuettoWriter::compileDereferencePointer(const Value* v, const Value* offset, const char* namedOffset)
{
	assert(v->getType()->isPointerTy());
	POINTER_KIND k=analyzer.getPointerKind(v);

	bool isOffsetConstantZero = false;
	if(offset==NULL || (ConstantInt::classof(offset) && getIntFromValue(offset)==0))
		isOffsetConstantZero = true;

	//If we know that no offset should be applied we can ask for the object directly.
	//If v is a GEP this optimizes away the separate access to the base and the offset
	//which would then be conbined dynamically. The idea is as follow
	//obj.a0["a1"] -> obj.a0.a1
	Type* lastType=compileObjectForPointer(v, (isOffsetConstantZero && !namedOffset)?GEP_DIRECT:NORMAL);
	//If a type has been returned (i.e. the value is a GEP) and we asked for direct access,
	//we can just stop
	if(k==COMPLETE_OBJECT || (lastType && isOffsetConstantZero && !namedOffset))
		return;
	stream << '[';
	if(k==COMPLETE_ARRAY)
	{
		bool notFirst=false;
		if(namedOffset)
		{
			stream << namedOffset;
			notFirst = true;
		}

		if(isOffsetConstantZero)
		{
			if(!notFirst)
				stream << '0';
		}
		else
		{
			if(notFirst)
				stream << '+';
			compileOperand(offset);
		}
	}
	else
	{
		// We need to explicitly avoid adding 0 to the offset. It has no
		// effect on numeric offsets, but screws up string offsets.
		// When there are string offset the pointer is for a struct member,
		// in this case it is only valid to dereference the exact pointer.
		// It is also possible to add 1, but in such case only comparing is allowed
		// so doing the string concatenation is ok.
		// NOTE: It actually breaks if we do +1 -1
		// NOTE: namedOffset is only used in memcpy/memove which already handle the 0th
		// case in a special way
		// TODO: We need to add a pointer kind which allows arithmetic
		if(!isOffsetConstantZero)
		{
			assert(namedOffset==NULL);
			compileOperand(offset);
			stream << "===0?";
			bool notFirst=compileOffsetForPointer(v, lastType);
			//TODO: Optimize this
			if(!notFirst)
				stream << '0';
			stream << ':';
		}
		bool notFirst=compileOffsetForPointer(v, lastType);
		if(!isOffsetConstantZero)
		{
			if(notFirst)
				stream << '+';
			compileOperand(offset);
			notFirst = true;
		}
		if(namedOffset)
		{
			if(notFirst)
				stream << '+';
			stream << namedOffset;
			notFirst = true;
		}
		if(!notFirst)
			stream << '0';
	}
	stream << ']';
}

Type* DuettoWriter::compileRecursiveAccessToGEP(Type* curType, const Use* it, const Use* const itE,
							COMPILE_FLAG flag)
{
	//Before this the base name has been already printed
	if(it==itE)
		return curType;
	Type* subType = NULL;
	if(curType->isStructTy())
	{
		const StructType* st=static_cast<const StructType*>(curType);
		//Special handling for constant offsets
		assert(ConstantInt::classof(*it));
		uint32_t elementIndex = getIntFromValue(*it);
		if(flag!=DRY_RUN)
			stream << ".a" << elementIndex;
		subType = st->getElementType(elementIndex);
	}
	else if(curType->isArrayTy())
	{
		const ArrayType* at=static_cast<const ArrayType*>(curType);
		if(flag!=DRY_RUN)
		{
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
		}
		subType = at->getElementType();
	}
	else
	{
		llvm::errs() << "Unexpected type during GEP access " << *curType << "\n";
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
		return curType;
	}
	return compileRecursiveAccessToGEP(subType, ++it, itE, flag);
}

void DuettoWriter::compileConstantExpr(const ConstantExpr* ce)
{
	switch(ce->getOpcode())
	{
		case Instruction::GetElementPtr:
		{
			Value* base = ce->getOperand(0);
			compileGEP(base, ce->op_begin()+1, ce->op_end()-1);
			break;
		}
		case Instruction::BitCast:
		{
			Value* val=ce->getOperand(0);
			Type* dst=ce->getType();
			Type* src=val->getType();
			if(!types.isValidTypeCast(val, dst) )
			{
				llvm::errs() << "Between:\n\t" << *src << "\n\t" << *dst << "\n";
				llvm::errs() << "warning: Type conversion is not safe, expect issues. And report a bug.\n";
			}
			compileOperand(val);
			break;
		}
		case Instruction::IntToPtr:
		{
			// NOTE: This is necessary for virtual inheritance. It should be made type safe.
			compileOperand(ce->getOperand(0));
			break;
		}
		case Instruction::PtrToInt:
		{
			compilePtrToInt(ce->getOperand(0));
			break;
		}
		case Instruction::ICmp:
		{
			compileIntegerComparison(ce->getOperand(0), ce->getOperand(1), (CmpInst::Predicate)ce->getPredicate());
			break;
		}
		case Instruction::Sub:
		{
			compileSubtraction(ce->getOperand(0), ce->getOperand(1));
			break;
		}
		default:
			llvm::errs() << "warning: Unsupported constant expr " << ce->getOpcodeName() << '\n';
	}
}

void DuettoWriter::compileConstant(const Constant* c)
{
	if(ConstantExpr::classof(c))
		compileConstantExpr(cast<ConstantExpr>(c));
	else if(ConstantDataSequential::classof(c))
	{
		const ConstantDataSequential* d=cast<const ConstantDataSequential>(c);
		Type* t=d->getElementType();
		stream << "new ";
		compileTypedArrayType(t);
		stream << "([";
		for(uint32_t i=0;i<d->getNumElements();i++)
		{
			compileConstant(d->getElementAsConstant(i));
			if((i+1)<d->getNumElements())
				stream << ",";
		}
		stream << "])";
	}
	else if(ConstantArray::classof(c))
	{
		const ConstantArray* d=cast<const ConstantArray>(c);
		stream << '[';
		assert(d->getType()->getNumElements() == d->getNumOperands());
		for(uint32_t i=0;i<d->getNumOperands();i++)
		{
			compileOperand(d->getOperand(i), REGULAR);
			if((i+1)<d->getNumOperands())
				stream << ",";
		}
		stream << ']';
	}
	else if(ConstantStruct::classof(c))
	{
		const ConstantStruct* d=cast<const ConstantStruct>(c);
		stream << '{';
		assert(d->getType()->getNumElements() == d->getNumOperands());
		for(uint32_t i=0;i<d->getNumOperands();i++)
		{
			stream << 'a' << i << ':';
			compileOperand(d->getOperand(i), REGULAR);
			if((i+1)<d->getNumOperands())
				stream << ",";
		}
		stream << '}';
	}
	else if(ConstantFP::classof(c))
	{
		const ConstantFP* f=cast<const ConstantFP>(c);
		//Must compare pointers for semantics, it seems
		if(f->getValueAPF().isInfinity())
		{
			if(f->getValueAPF().isNegative())
				stream << '-';
			stream << "Infinity";
		}
		else if(&f->getValueAPF().getSemantics()==&APFloat::IEEEsingle)
			stream << f->getValueAPF().convertToFloat();
		else if(&f->getValueAPF().getSemantics()==&APFloat::IEEEdouble)
			stream << f->getValueAPF().convertToDouble();
		else
			llvm::report_fatal_error("Unsupported float type, please report a bug", false);
	}
	else if(ConstantInt::classof(c))
	{
		const ConstantInt* i=cast<const ConstantInt>(c);
		if(i->getBitWidth()==1)
			stream << (i->isZero()?"false":"true");
		else
			stream << i->getSExtValue();
	}
	else if(ConstantPointerNull::classof(c))
	{
		const Type* pointedType = c->getType()->getPointerElementType();
		if(types.isClientType(pointedType))
			stream << "null";
		else
			stream << "nullObj";
	}
	else if(UndefValue::classof(c))
	{
		stream << "undefined";
	}
	else if(GlobalAlias::classof(c))
	{
		const GlobalAlias* a=cast<const GlobalAlias>(c);
		compileConstant(a->getAliasee());
	}
	else if(GlobalValue::classof(c))
	{
		assert(c->hasName());
		//Check if this is a client global value, if so skip mangling
		const char* mangledName = c->getName().data();
		if(isClientGlobal(mangledName))
		{
			//Client value
			char* objName;
			int nameLen=strtol(mangledName+10,&objName,10);
			stream.write(objName, nameLen);
		}
		else
			printLLVMName(c->getName(), GLOBAL);
	}
	else if(ConstantAggregateZero::classof(c))
	{
		compileType(c->getType(), LITERAL_OBJ);
	}
	else
	{
		llvm::errs() << "Unsupported constant type ";
		c->dump();
		stream << "null";
	}
}

void DuettoWriter::compilePointer(const Value* v, POINTER_KIND toKind)
{
	assert(toKind != UNDECIDED);
	
	POINTER_KIND fromKind = analyzer.getPointerKind(v);
	assert(fromKind != UNDECIDED);
	
	if(fromKind == toKind || UndefValue::classof(v))
	{
		//Nothing to do, forward
		compileOperandImpl(v);
		return;
	}
	
	// Impossible conversions:
	assert( ! (fromKind == REGULAR && toKind == COMPLETE_ARRAY) );
	assert( ! (fromKind == COMPLETE_OBJECT && toKind == COMPLETE_ARRAY) );
	
	// Syntetize a REGULAR pointer from a COMPLETE_ARRAY or a COMPLETE_OBJECT
	if (toKind == REGULAR)
	{
		stream << "{ d: ";
		Type* lastType = compileObjectForPointer(v, NORMAL);
		stream << ", o: ";
		bool notEmpty = compileOffsetForPointer(v, lastType);
		if (!notEmpty)
			stream << '0';
		stream << '}';
	}
	else
	{
		// Syntetize a promotion from a REGULAR or COMPLETE_ARRAY to a COMPLETE_OBJECT
		assert(toKind == COMPLETE_OBJECT && fromKind != COMPLETE_OBJECT);
		compileDereferencePointer(v, NULL);
	}
}

void DuettoWriter::compileOperandImpl(const Value* v)
{
	if(const Constant* c=dyn_cast<const Constant>(v))
		compileConstant(c);
	else if(const Instruction* it=dyn_cast<Instruction>(v))
	{
		if(isInlineable(*it))
			compileInlineableInstruction(*cast<Instruction>(v));
		else
			printVarName(it);
	}
	else if(const Argument* arg=dyn_cast<const Argument>(v))
		printArgName(arg);
	else if(const InlineAsm* a=dyn_cast<const InlineAsm>(v))
	{
		assert(a->getConstraintString().empty());
		stream << a->getAsmString();
	}
	else
	{
		llvm::errs() << "No name for value ";
		v->dump();
	}
}

void DuettoWriter::compileOperand(const Value* v, POINTER_KIND requestedPointerKind)
{
	if (!currentFun && isa<GlobalVariable>(v) && !compiledGVars.count( cast<GlobalVariable>(v)) )
	{
		// If we are compiling a constant expr for a GVar, and v has not been defined yet
		// just print undefined
		stream << "undefined";
		return;
	}

	//First deal with complete objects, but never expand pointers to client objects
	if(v->getType()->isPointerTy() &&
		requestedPointerKind!=UNDECIDED &&
		!types.isClientType(v->getType()->getPointerElementType()))
	{
		compilePointer(v, requestedPointerKind);
	}
	else
		compileOperandImpl(v);
}

void DuettoWriter::printVarName(const Value* val)
{
	if(val->hasName())
		printLLVMName(val->getName(), GlobalValue::classof(val)?GLOBAL:LOCAL);
	else
		stream << "tmp" << namegen.getUniqueIndexForValue(val);
}

void DuettoWriter::printArgName(const Argument* val) const
{
	if(val->hasName())
		printLLVMName(val->getName(), LOCAL);
	else
		stream << "arg" << val->getArgNo();
}

void DuettoWriter::compilePHIOfBlockFromOtherBlock(const BasicBlock* to, const BasicBlock* from)
{
	BasicBlock::const_iterator I=to->begin();
	BasicBlock::const_iterator IE=to->end();
	SmallVector<uint32_t, 4> tmps;
	//Phase 1, use temporaries to store the results of PHIs
	for(;I!=IE;++I)
	{
		const PHINode* phi=dyn_cast<const PHINode>(I);
		//TODO: I think that after the first non-phi node we can stop
		if(phi==NULL)
			continue;
		const Value* val=phi->getIncomingValueForBlock(from);
		uint32_t tmpIndex = namegen.getUniqueIndexForPHI( currentFun );
		stream << "var tmpphi" << tmpIndex << " = ";
		tmps.push_back(tmpIndex);
		POINTER_KIND k=phi->getType()->isPointerTy()? analyzer.getPointerKind(phi):UNDECIDED;
		compileOperand(val, k);
		stream << ';' << NewLine;
	}
	//Phase 2, actually assign the values
	I=to->begin();
	for(uint32_t tmpI=0;I!=IE;++I,tmpI++)
	{
		const PHINode* phi=dyn_cast<const PHINode>(I);
		if(phi==NULL)
			continue;
		stream << "var ";
		printVarName(phi);
		stream << " = tmpphi" << tmps[tmpI] << ';' << NewLine;
	}
}

void DuettoWriter::compileMethodArgs(const llvm::User::const_op_iterator it, const llvm::User::const_op_iterator itE)
{
	stream << '(';
	for(llvm::User::const_op_iterator cur=it;cur!=itE;++cur)
	{
		if(cur!=it)
			stream << ", ";
		compileOperand(*cur, REGULAR);
	}
	stream << ')';
}

void DuettoWriter::compileMethodArgsForDirectCall(const llvm::User::const_op_iterator it,
						const llvm::User::const_op_iterator itE,
						llvm::Function::const_arg_iterator arg_it)
{
	stream << '(';
	
	for(llvm::User::const_op_iterator cur=it;cur!=itE;++cur, ++arg_it)
	{
		if(cur!=it)
			stream << ", ";
		if ( arg_it->getType()->isPointerTy() )
			compileOperand(*cur, analyzer.getPointerKind(&(*arg_it)));
		else
			compileOperand(*cur, REGULAR);
	}
	stream << ')';
}

/*
 * This method is fragile, each opcode must handle the phis in the correct place
 */
DuettoWriter::COMPILE_INSTRUCTION_FEEDBACK DuettoWriter::compileTerminatorInstruction(const TerminatorInst& I)
{
	switch(I.getOpcode())
	{
		case Instruction::Ret:
		{
			const ReturnInst& ri=static_cast<const ReturnInst&>(I);
			assert(I.getNumSuccessors()==0);
			Value* retVal = ri.getReturnValue();
			stream << "return ";
			if(retVal)
				compileOperand(retVal, REGULAR);
			stream << ';' << NewLine;
			return COMPILE_OK;
		}
		case Instruction::Invoke:
		{
			const InvokeInst& ci=static_cast<const InvokeInst&>(I);
			//TODO: Support unwind
			//For now, pretend it's a regular call
			if(ci.getCalledFunction())
			{
				//Direct call
				COMPILE_INSTRUCTION_FEEDBACK cf=handleBuiltinCall(&ci, ci.getCalledFunction());
				assert(cf!=COMPILE_EMPTY);
				if(cf==COMPILE_OK)
				{
					stream << ';' << NewLine;
					//Only consider the normal successor for PHIs here
					//For each successor output the variables for the phi nodes
					compilePHIOfBlockFromOtherBlock(ci.getNormalDest(), I.getParent());
					return COMPILE_OK;
				}
				else
					stream << '_' << ci.getCalledFunction()->getName();
			}
			else
			{
				//Indirect call
				compileOperand(ci.getCalledValue());
			}

			compileMethodArgs(ci.op_begin(),ci.op_begin()+ci.getNumArgOperands());
			stream << ';' << NewLine;
			//Only consider the normal successor for PHIs here
			//For each successor output the variables for the phi nodes
			compilePHIOfBlockFromOtherBlock(ci.getNormalDest(), I.getParent());
			return COMPILE_OK;
		}
		case Instruction::Resume:
		{
			//TODO: support exceptions
			return COMPILE_OK;
		}
		case Instruction::Br:
		case Instruction::Switch:
		case Instruction::Unreachable:
			return COMPILE_OK;
		default:
			stream << "alert('Unsupported code');" << NewLine;
			llvm::errs() << "\tImplement terminator inst " << I.getOpcodeName() << '\n';
	}
	return COMPILE_UNSUPPORTED;
}

DuettoWriter::COMPILE_INSTRUCTION_FEEDBACK DuettoWriter::compileTerminatorInstruction(const TerminatorInst& I,
		const std::map<const BasicBlock*, uint32_t>& blocksMap)
{
	COMPILE_INSTRUCTION_FEEDBACK cf=compileTerminatorInstruction(I);
	switch(I.getOpcode())
	{
		case Instruction::Ret:
		case Instruction::Unreachable:
			break;
		case Instruction::Invoke:
		{
			//TODO: Support unwind
			const InvokeInst& ci=static_cast<const InvokeInst&>(I);
			stream << "__block = " << blocksMap.find(ci.getNormalDest())->second << ';' << NewLine;
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
				stream << "__block = " << blocksMap.find(bi.getSuccessor(0))->second << ';' << NewLine;
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
					";}" << NewLine;
			}
			break;
		}
		case Instruction::Switch:
		{
			//Create a JS switch
			const SwitchInst& si=static_cast<const SwitchInst&>(I);
			stream << "switch (";
			compileOperand(si.getCondition());
			stream << "){";
			SwitchInst::ConstCaseIt it=si.case_begin();
			for(;it!=si.case_end();++it)
			{
				stream << "case ";
				compileConstant(it.getCaseValue());
				stream << ':' << NewLine << "__block = " << blocksMap.find(it.getCaseSuccessor())->second <<
					"; break;";
			}
			if(si.getDefaultDest())
			{
				stream << "default:" << NewLine << "__block = " <<
					blocksMap.find(si.getDefaultDest())->second << ';';
			}
			stream << '}' << NewLine;
			break;
		}
		default:
			stream << "alert('Unsupported code');" << NewLine;
			llvm::errs() << "\tImplement terminator inst " << I.getOpcodeName() << '\n';
			break;
	}
	return cf;
}

DuettoWriter::COMPILE_INSTRUCTION_FEEDBACK DuettoWriter::compileNotInlineableInstruction(const Instruction& I)
{
	switch(I.getOpcode())
	{
		case Instruction::Alloca:
		{
			const AllocaInst * ai = cast<AllocaInst>(&I);
			
			if( analyzer.getPointerKind(ai) == COMPLETE_ARRAY )
			{
				stream << '[';
				compileType( ai->getAllocatedType(), LITERAL_OBJ);
				stream << ']';
			}
			else 
				compileType( ai->getAllocatedType(), LITERAL_OBJ);

			if ( isa<StructType>( ai->getAllocatedType()) && globalDeps.classesWithBaseInfo().count(cast<StructType>(ai->getAllocatedType())) )
				return COMPILE_OK;
			else
				return analyzer.hasSelfMember(ai) ? COMPILE_ADD_SELF : COMPILE_OK;
		}
		case Instruction::Call:
		{
			const CallInst& ci=static_cast<const CallInst&>(I);
			const Function * calledFunc = ci.getCalledFunction();
	
			if(calledFunc)
			{
				//Direct call
				COMPILE_INSTRUCTION_FEEDBACK cf=handleBuiltinCall(&ci, calledFunc);
				if(cf!=COMPILE_UNSUPPORTED)
					return cf;
				stream << '_' << calledFunc->getName();
			}
			else
			{
				//Indirect call
				compileOperand(ci.getCalledValue());
			}
			//If we are dealing with inline asm we are done
			if(!ci.isInlineAsm())
			{
				if ( analyzer.hasNonRegularArgs(calledFunc) )
				{
					assert( calledFunc->getArgumentList().size() == ci.getNumArgOperands() );
					compileMethodArgsForDirectCall(ci.op_begin(),ci.op_begin()+ci.getNumArgOperands(),calledFunc->arg_begin() );
				}
				else
					compileMethodArgs(ci.op_begin(),ci.op_begin()+ci.getNumArgOperands());
			}
			return COMPILE_OK;
		}
		case Instruction::LandingPad:
		{
			//TODO: Support exceptions
			stream << " alert('Exceptions not supported')";
			//Do not continue block
			return COMPILE_UNSUPPORTED;
		}
		case Instruction::InsertValue:
		{
			const InsertValueInst& ivi=static_cast<const InsertValueInst&>(I);
			const Value* aggr=ivi.getAggregateOperand();
			Type* t=aggr->getType();
			if(!t->isStructTy())
			{
				llvm::errs() << "insertvalue: Expected struct, found " << *t << "\n";
				llvm::report_fatal_error("Unsupported code found, please report a bug", false);
				return COMPILE_UNSUPPORTED;
			}
			if(UndefValue::classof(aggr))
			{
				//We have to assemble the type object from scratch
				compileType(t, LITERAL_OBJ);
				stream << ';' << NewLine;
				//Also assign the element
				assert(ivi.getNumIndices()==1);
				//Find the offset to the pointed element
				assert(ivi.hasName());
				printLLVMName(ivi.getName(), LOCAL);
			}
			else
			{
				//Optimize for the assembly of the aggregate values
				assert(aggr->hasOneUse());
				assert(aggr->hasName());
				printLLVMName(aggr->getName(), LOCAL);
			}
			uint32_t offset=ivi.getIndices()[0];
			stream << ".a" << offset << " = ";
			compileOperand(ivi.getInsertedValueOperand());
			return COMPILE_OK;
		}
		case Instruction::Load:
		{
			const LoadInst& li=static_cast<const LoadInst&>(I);
			const Value* ptrOp=li.getPointerOperand();
			stream << "(";
			if(BitCastInst::classof(ptrOp) &&
					TypeSupport::isUnion(cast<BitCastInst>(ptrOp)->getOperand(0)->getType()->getPointerElementType()) &&
					!ArrayType::classof(ptrOp->getType()->getPointerElementType()))
			{
				//Optimize loads of single values from unions
				compileOperand(cast<BitCastInst>(ptrOp)->getOperand(0));
				Type* pointedType=ptrOp->getType()->getPointerElementType();
				if(pointedType->isIntegerTy(8))
					stream << ".getInt8(0)";
				else if(pointedType->isIntegerTy(16))
					stream << ".getInt16(0,true)";
				else if(pointedType->isIntegerTy(32))
					stream << ".getInt32(0,true)";
				else if(pointedType->isFloatTy())
					stream << ".getFloat32(0,true)";
				else if(pointedType->isDoubleTy())
					stream << ".getFloat64(0,true)";
			}
			else
				compileDereferencePointer(ptrOp, NULL);
			stream << ")";
			return COMPILE_OK;
		}
		case Instruction::Store:
		{
			const StoreInst& si=static_cast<const StoreInst&>(I);
			const Value* ptrOp=si.getPointerOperand();
			const Value* valOp=si.getValueOperand();
			if(BitCastInst::classof(ptrOp) &&
					TypeSupport::isUnion(cast<BitCastInst>(ptrOp)->getOperand(0)->getType()->getPointerElementType()) &&
					!ArrayType::classof(ptrOp->getType()->getPointerElementType()))
			{
				//Optimize loads of single values from unions
				compileOperand(cast<BitCastInst>(ptrOp)->getOperand(0));
				Type* pointedType=ptrOp->getType()->getPointerElementType();
				if(pointedType->isIntegerTy(8))
					stream << ".setInt8(0,";
				else if(pointedType->isIntegerTy(16))
					stream << ".setInt16(0,";
				else if(pointedType->isIntegerTy(32))
					stream << ".setInt32(0,";
				else if(pointedType->isFloatTy())
					stream << ".setFloat32(0,";
				else if(pointedType->isDoubleTy())
					stream << ".setFloat64(0,";
				//Special case compilation of operand, the default behavior use =
				compileOperand(valOp);
				if(!pointedType->isIntegerTy(8))
					stream << ",true";
				stream << ')';
				return COMPILE_OK;
			}
			else
				compileDereferencePointer(ptrOp, NULL);
			stream << " = ";
			compileOperand(valOp, REGULAR);
			return COMPILE_OK;
		}
		default:
			return compileInlineableInstruction(I)?COMPILE_OK:COMPILE_UNSUPPORTED;
	}
}

Type* DuettoWriter::compileObjectForPointer(const Value* val, COMPILE_FLAG flag)
{
	assert(val->getType()->isPointerTy());
	if(isGEP(val))
	{
		const User* gep=static_cast<const User*>(val);
		GetElementPtrInst::const_op_iterator it=gep->op_begin()+1;
		GetElementPtrInst::const_op_iterator itE=gep->op_end();
		//When generating a regular pointer, do not compile the last level
		//If the access is direct compile all offsets
		if(flag!=GEP_DIRECT)
			--itE;
		return compileObjectForPointerGEP(gep->getOperand(0), it, itE, flag);
	}
	else if(isBitCast(val))
	{
		const User* b=static_cast<const User*>(val);
		//If it is a union, handle below
		if(!TypeSupport::isUnion(b->getOperand(0)->getType()->getPointerElementType()))
			return compileObjectForPointer(b->getOperand(0), flag);
	}

	if(flag!=DRY_RUN)
	{
		POINTER_KIND k=analyzer.getPointerKind(val);
		compilePointer(val, k);
		if(k==REGULAR)
			stream << ".d";
		else if(k==COMPLETE_OBJECT && flag == NORMAL &&
			StructType::classof(val->getType()->getPointerElementType()) &&
			globalDeps.classesWithBaseInfo().count(cast<StructType>(val->getType()->getPointerElementType())))
		{
			stream << ".a";
		}
	}
	return NULL;
}

bool DuettoWriter::compileOffsetForPointer(const Value* val, Type* lastType)
{
	assert(val->getType()->isPointerTy());
	if(isGEP(val))
	{
		const User* gep=static_cast<const User*>(val);
		GetElementPtrInst::const_op_iterator it=gep->op_begin()+1;
		//We compile as usual till the last level
		GetElementPtrInst::const_op_iterator itE=gep->op_end()-1;
		return compileOffsetForPointerGEP(gep->getOperand(0), it, itE, lastType);
	}
	else if(isBitCast(val))
	{
		const User* b=static_cast<const User*>(val);
		//If it is a union, handle below
		if(!TypeSupport::isUnion(b->getOperand(0)->getType()->getPointerElementType()))
			return compileOffsetForPointer(b->getOperand(0), lastType);
	}

	if(analyzer.getPointerKind(val) == COMPLETE_OBJECT)
	{
		// Objects with the downcast array uses it directly, not the self pointer
		if(StructType::classof(val->getType()->getPointerElementType()) &&
			globalDeps.classesWithBaseInfo().count(cast<StructType>(val->getType()->getPointerElementType())))
		{
			stream << '0';
		}
		else
		{
			//Print the regular "s" offset for complete objects
			assert(analyzer.hasSelfMember(val) );
			stream << "'s'";
		}
		return true;
	}
	else if(analyzer.getPointerKind(val)==COMPLETE_ARRAY)
	{
		//Skip printing 0 offset for complete arrays
		return false;
	}

	//Regular pointer case
	compileOperand(val);
	stream << ".o";
	return true;
}

Type* DuettoWriter::compileObjectForPointerGEP(const Value* val, const Use* it, const Use* const itE, COMPILE_FLAG flag)
{
	Type* t=val->getType();
	if(it==itE)
	{
		//Same level access, we are just computing another pointer from this pointer
		compileObjectForPointer(val, flag);
		return t;
	}
	else
	{
		assert(t->isPointerTy());
		PointerType* ptrT=static_cast<PointerType*>(t);
		//First dereference the pointer
		if(flag!=DRY_RUN)
			compileDereferencePointer(val, *it);
		Type* ret=compileRecursiveAccessToGEP(ptrT->getElementType(), ++it, itE, flag);
		if(flag!=NORMAL)
			return ret;
		//If we are accessing a base class, use the downcast array
		if(StructType* st=dyn_cast<StructType>(ret))
		{
			uint32_t firstBase, baseCount;
			if(types.getBasesInfo(st, firstBase, baseCount) && globalDeps.classesWithBaseInfo().count(st))
			{
				uint32_t lastIndex=getIntFromValue(*itE);
				if(lastIndex>=firstBase && lastIndex<(firstBase+baseCount))
					stream << ".a";
			}
		}
		return ret;
	}
}

bool DuettoWriter::compileOffsetForPointerGEP(const Value* val, const Use* it, const Use* const itE, Type* lastType)
{
	if(it==itE)
	{
		//Same level access, we are just computing another pointer from this pointer
		bool notFirst=compileOffsetForPointer(val, lastType);
		//Compute the offset
		if(ConstantInt::classof(*itE))
		{
			uint32_t firstElement = getIntFromValue(*itE);
			if(firstElement==0)
				return notFirst;
			if(notFirst)
				stream << '+';
			compileOperand(*itE);
		}
		else
		{
			if(notFirst)
				stream << '+';
			compileOperand(*itE);
		}
	}
	else
	{
		//Now add the offset for the desired element
		if(ConstantInt::classof(*itE))
		{
			uint32_t elementIndex = getIntFromValue(*itE);
			if(elementIndex == 0 && !lastType->isStructTy())
				return false;
			bool isStruct=false;
			//If we are accessing a base class, use the downcast array
			if(StructType* st=dyn_cast<StructType>(lastType))
			{
				isStruct=true;
				uint32_t firstBase, baseCount;
				if(types.getBasesInfo(st, firstBase, baseCount) && elementIndex>=firstBase &&
					elementIndex<(firstBase+baseCount) && globalDeps.classesWithBaseInfo().count(st))
				{
					compileDereferencePointer(val, *it);
					compileRecursiveAccessToGEP(val->getType()->getPointerElementType(), ++it, itE, NORMAL);
					stream << ".a";
					stream << elementIndex;
					stream << ".o";
					return true;
				}
			}
			if(isStruct)
				stream << "\"a";
			stream << elementIndex;
			if(isStruct)
				stream << '"';
		}
		else
		{
			assert(!lastType->isStructTy());
			compileOperand(*itE);
		}
	}
	//TODO: Skip some useless offsets when possible
	return true;
}

void DuettoWriter::compileGEP(const Value* val, const Use* it, const Use* const itE)
{
	assert(val->getType()->isPointerTy());
	stream << "{ d: ";
	Type* lastType=compileObjectForPointerGEP(val, it, itE, NORMAL);
	stream << ", o: ";
	bool notFirst=compileOffsetForPointerGEP(val, it, itE,lastType);
	if(!notFirst)
		stream << '0';
	stream << '}';
}

uint32_t DuettoWriter::getMaskForBitWidth(int width)
{
	return (1 << width) - 1;
}

void DuettoWriter::compileSignedInteger(const llvm::Value* v)
{
	//We anyway have to use 32 bits for sign extension to work
	uint32_t shiftAmount = 32-v->getType()->getIntegerBitWidth();
	if(shiftAmount==0)
	{
		//Use simpler code
		stream << '(';
		compileOperand(v);
		stream << ">> 0)";
	}
	else
	{
		stream << "((";
		compileOperand(v);
		stream << "<<" << shiftAmount << ")>>" << shiftAmount << ')';
	}
}

void DuettoWriter::compileUnsignedInteger(const llvm::Value* v)
{
	//We anyway have to use 32 bits for sign extension to work
	uint32_t initialSize = v->getType()->getIntegerBitWidth();
	stream << '(';
	if(initialSize == 32)
	{
		//Use simpler code
		compileOperand(v);
		stream << ">>> 0)";
	}
	else
	{
		compileOperand(v);
		stream << " & " << getMaskForBitWidth(initialSize) << ')';
	}
}

/*
 * This can be used for both named instructions and inlined ones
 * NOTE: Call, Ret, Invoke are NEVER inlined
 */
bool DuettoWriter::compileInlineableInstruction(const Instruction& I)
{
	switch(I.getOpcode())
	{
		case Instruction::BitCast:
		{
			const BitCastInst& bi=static_cast<const BitCastInst&>(I);
			Type* src=bi.getSrcTy();
			Type* dst=bi.getDestTy();
			if(!TypeSupport::isValidTypeCast(bi.getOperand(0), dst))
			{
				llvm::errs() << "Between:\n\t" << *src << "\n\t" << *dst << "\n";
				llvm::errs() << "warning: Type conversion is not safe, expect issues. And report a bug.\n";
			}
			//Special case unions
			if(src->isPointerTy() && TypeSupport::isUnion(src->getPointerElementType()))
			{
				//Find the type
				llvm::Type* elementType = dst->getPointerElementType();
				bool isArray=ArrayType::classof(elementType);
				stream << "new ";
				compileTypedArrayType((isArray)?elementType->getSequentialElementType():elementType);
				stream << '(';
				compileOperand(bi.getOperand(0));
				stream << ".buffer)";
				return true;
			}

			compileOperand(bi.getOperand(0));
			return true;
		}
		case Instruction::FPToSI:
		{
			const CastInst& ci=static_cast<const CastInst&>(I);
			stream << '(';
			compileOperand(ci.getOperand(0));
			//Seems to be the fastest way
			//http://jsperf.com/math-floor-vs-math-round-vs-parseint/33
			stream << " >> 0)";
			return true;
		}
		case Instruction::FPToUI:
		{
			const CastInst& ci=static_cast<const CastInst&>(I);
			stream << '(';
			compileOperand(ci.getOperand(0));
			//Cast to signed anyway
			//ECMA-262 guarantees that (a >> 0) >>> 0
			//is the same as (a >>> 0)
			stream << " >> 0)";
			return true;
		}
		case Instruction::SIToFP:
		{
			const CastInst& ci=static_cast<const CastInst&>(I);
			stream << "(+";
			compileOperand(ci.getOperand(0));
			stream << ')';
			return true;
		}
		case Instruction::UIToFP:
		{
			const CastInst& ci=static_cast<const CastInst&>(I);
			//We need to cast to unsigned before
			stream << "(+(";
			compileOperand(ci.getOperand(0));
			stream << " >>> 0))";
			return true;
		}
		case Instruction::GetElementPtr:
		{
			const GetElementPtrInst& gep=static_cast<const GetElementPtrInst&>(I);
			Type* t=gep.getOperand(0)->getType();
			assert(t->isPointerTy());
			PointerType* ptrT=static_cast<PointerType*>(t);
			if(types.isClientType(ptrT->getElementType()))
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
			stream << "((";
			compileOperand(I.getOperand(0));
			stream << " + ";
			compileOperand(I.getOperand(1));
			stream << ')';
			if(types.isI32Type(I.getType()))
				stream << ">> 0";
			else
				stream << "& " << getMaskForBitWidth(I.getType()->getIntegerBitWidth());
			stream << ')';
			return true;
		}
		case Instruction::FAdd:
		{
			//Double addition
			stream << "(";
			compileOperand(I.getOperand(0));
			stream << " + ";
			compileOperand(I.getOperand(1));
			stream << ")";
			return true;
		}
		case Instruction::Sub:
		{
			compileSubtraction(I.getOperand(0), I.getOperand(1));
			return true;
		}
		case Instruction::FSub:
		{
			//Double subtraction
			//TODO: optimize negation
			stream << "(";
			compileOperand(I.getOperand(0));
			stream << " - ";
			compileOperand(I.getOperand(1));
			stream << ")";
			return true;
		}
		case Instruction::ZExt:
		{
			const BitCastInst& bi=static_cast<const BitCastInst&>(I);
			Type* src=bi.getSrcTy();
#ifndef NDEBUG
			Type* dst=bi.getDestTy();
#endif
			assert(src->isIntegerTy() && dst->isIntegerTy());
			if(src->isIntegerTy(1))
			{
				//If the source type is i1, attempt casting from Boolean
				stream << '(';
				compileOperand(bi.getOperand(0));
				stream << "==false?0:1)";
			}
			else
			{
				//Let's mask out upper bits, to make sure we get zero extension
				//The value might have been initialized with a negative value
				compileUnsignedInteger(I.getOperand(0));
			}
			return true;
		}
		case Instruction::SDiv:
		{
			//Integer signed division
			stream << "((";
			compileSignedInteger(I.getOperand(0));
			stream << " / ";
			compileSignedInteger(I.getOperand(1));
			stream << ") >> 0)";
			return true;
		}
		case Instruction::UDiv:
		{
			//Integer unsigned division
			stream << "((";
			compileUnsignedInteger(I.getOperand(0));
			stream << " / ";
			compileUnsignedInteger(I.getOperand(1));
			stream << ") >>> 0)";
			return true;
		}
		case Instruction::SRem:
		{
			//Integer signed remainder
			stream << "((";
			compileSignedInteger(I.getOperand(0));
			stream << " % ";
			compileSignedInteger(I.getOperand(1));
			stream << ") >> 0)";
			return true;
		}
		case Instruction::URem:
		{
			//Integer unsigned remainder
			stream << "((";
			compileUnsignedInteger(I.getOperand(0));
			stream << " % ";
			compileUnsignedInteger(I.getOperand(1));
			stream << ") >>> 0)";
			return true;
		}
		case Instruction::FDiv:
		{
			//Double division
			stream << "(";
			compileOperand(I.getOperand(0));
			stream << " / ";
			compileOperand(I.getOperand(1));
			stream << ")";
			return true;
		}
		case Instruction::Mul:
		{
			//Integer signed multiplication
			stream << "((";
			compileOperand(I.getOperand(0));
			stream << " * ";
			compileOperand(I.getOperand(1));
			stream << ')';
			if(types.isI32Type(I.getType()))
				stream << ">> 0";
			else
				stream << "& " << getMaskForBitWidth(I.getType()->getIntegerBitWidth());
			stream << ')';
			return true;
		}
		case Instruction::FMul:
		{
			//Double multiplication
			stream << "(";
			compileOperand(I.getOperand(0));
			stream << " * ";
			compileOperand(I.getOperand(1));
			stream << ")";
			return true;
		}
		case Instruction::ICmp:
		{
			//Integer comparison
			const CmpInst& ci=static_cast<const CmpInst&>(I);
			stream << '(';
			compileIntegerComparison(ci.getOperand(0), ci.getOperand(1), ci.getPredicate());
			stream << ')';
			return true;
		}
		case Instruction::FCmp:
		{
			//Integer comparison
			const CmpInst& ci=static_cast<const CmpInst&>(I);
			//Check that the operation is JS safe
			stream << "(";
			//Special case orderedness check
			if(ci.getPredicate()==CmpInst::FCMP_ORD)
			{
				stream << "!isNaN(";
				compileOperand(ci.getOperand(0));
				stream << ") && !isNaN(";
				compileOperand(ci.getOperand(1));
				stream << ')';
			}
			else
			{
				compileOperand(ci.getOperand(0));
				compilePredicate(ci.getPredicate());
				compileOperand(ci.getOperand(1));
			}
			stream << ")";
			return true;
		}
		case Instruction::And:
		{
			//Integer logical and
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << " & ";
			compileOperand(I.getOperand(1));
			stream << ')';
			return true;
		}
		case Instruction::LShr:
		{
			//Integer logical shift right
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << " >>> ";
			compileOperand(I.getOperand(1));
			stream << ')';
			return true;
		}
		case Instruction::AShr:
		{
			//Integer arithmetic shift right
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			if(types.isI32Type(I.getOperand(0)->getType()))
				compileOperand(I.getOperand(0));
			else
				compileSignedInteger(I.getOperand(0));
			stream << " >> ";
			compileOperand(I.getOperand(1));
			stream << ')';
			return true;
		}
		case Instruction::Shl:
		{
			//Integer shift left
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << " << ";
			compileOperand(I.getOperand(1));
			stream << ')';
			return true;
		}
		case Instruction::Or:
		{
			//Integer logical or
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << " | ";
			compileOperand(I.getOperand(1));
			stream << ')';
			return true;
		}
		case Instruction::Xor:
		{
			//Integer logical xor
			//Xor with 1s is used to implement bitwise and logical negation
			//TODO: Optimize the operation with 1s
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << " ^ ";
			compileOperand(I.getOperand(1));
			stream << ')';
			return true;
		}
		case Instruction::Trunc:
		{
			//Well, ideally this should not be used since, since it's a waste of bit to
			//use integers less than 32 bit wide. Still we can support it
			uint32_t finalSize = I.getType()->getIntegerBitWidth();
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << " & " << getMaskForBitWidth(finalSize) << ')';
			return true;
		}
		case Instruction::SExt:
		{
			//We can use a couple of shift to make this work
			compileSignedInteger(I.getOperand(0));
			return true;
		}
		case Instruction::Select:
		{
			const SelectInst& si=static_cast<const SelectInst&>(I);
			stream << "(";
			compileOperand(si.getCondition());
			stream << "?";
			POINTER_KIND k=si.getType()->isPointerTy()?analyzer.getPointerKind(&si):UNDECIDED;
			compileOperand(si.getTrueValue(), k);
			stream << ":";
			compileOperand(si.getFalseValue(), k);
			stream << ")";
			return true;
		}
		case Instruction::ExtractValue:
		{
			const ExtractValueInst& evi=static_cast<const ExtractValueInst&>(I);
			const Value* aggr=evi.getAggregateOperand();
			Type* t=aggr->getType();
			if(!t->isStructTy())
			{
				llvm::errs() << "extractvalue: Expected struct, found " << *t << "\n";
				llvm::report_fatal_error("Unsupported code found, please report a bug", false);
				return true;
			}
			assert(!UndefValue::classof(aggr));

			printVarName(aggr);

			uint32_t offset=evi.getIndices()[0];
			stream << ".a" << offset;
			return true;
		}
		case Instruction::FPExt:
		{
			const Value* src=I.getOperand(0);
			compileOperand(src);
			return true;
		}
		case Instruction::FPTrunc:
		{
			const Value* src=I.getOperand(0);
			compileOperand(src);
			return true;
		}
		case Instruction::PtrToInt:
		{
			const PtrToIntInst& pi=static_cast<const PtrToIntInst&>(I);
			compilePtrToInt(pi.getOperand(0));
			return true;
		}
		case Instruction::VAArg:
		{
			const VAArgInst& vi=static_cast<const VAArgInst&>(I);
			stream << "handleVAArg(";
			compileDereferencePointer(vi.getPointerOperand(), NULL);
			stream << ')';
			
			assert( globalDeps.needHandleVAArg() );
			return true;
		}
		default:
			stream << "alert('Unsupported code')";
			llvm::errs() << "\tImplement inst " << I.getOpcodeName() << '\n';
			return false;
	}
}


/* We add a ".s" member pointing to itself, this can be used to convert complete objects
   to regular pointers on demand with a low overhead. The complete pointer will be
   { d: obj, o: "s" } */
void DuettoWriter::addSelfPointer(const llvm::Value* obj)
{
	printVarName(obj);
	stream << ".s = ";
	printVarName(obj);
	stream << ';' << NewLine;
}

void DuettoWriter::compileBB(const BasicBlock& BB, const std::map<const BasicBlock*, uint32_t>& blocksMap)
{
	BasicBlock::const_iterator I=BB.begin();
	BasicBlock::const_iterator IE=BB.end();
	for(;I!=IE;++I)
	{
		if(isInlineable(*I))
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
		const DebugLoc& debugLoc = I->getDebugLoc();
		if(!debugLoc.isUnknown())
			sourceMapGenerator.setDebugLoc(I->getDebugLoc());
		if(I->getType()->getTypeID()!=Type::VoidTyID)
		{
			stream << "var ";
			printVarName(&(*I));
			stream << " = ";
		}
		if(I->isTerminator())
		{
			//TODO: Keep support for both relooper and swicth generation
			compileTerminatorInstruction(*dyn_cast<TerminatorInst>(I));
			//compileTerminatorInstruction(*dyn_cast<TerminatorInst>(I), blocksMap);
		}
		else
		{
			COMPILE_INSTRUCTION_FEEDBACK ret=compileNotInlineableInstruction(*I);
			if(ret==COMPILE_OK || ret==COMPILE_ADD_SELF)
				stream << ';' << NewLine;
			if(ret==COMPILE_ADD_SELF)
				addSelfPointer(&(*I));
			else if(ret==COMPILE_UNSUPPORTED)
			{
				//Stop basic block compilation
				return;
			}
		}
	}
	//At the end of the block
}

void DuettoRenderInterface::renderBlock(const void* privateBlock)
{
	const BasicBlock* bb=(const BasicBlock*)privateBlock;
	std::map<const BasicBlock*, uint32_t> blocksMap;
	writer->compileBB(*bb, blocksMap);
}

void DuettoRenderInterface::renderCondition(const BasicBlock* bb, int branchId)
{
	const TerminatorInst* term=bb->getTerminator();
	if(BranchInst::classof(term))
	{
		const BranchInst* bi=cast<const BranchInst>(term);
		assert(bi->isConditional());
		//The second branch is the default
		assert(branchId==0);
		writer->compileOperand(bi->getCondition());
	}
	else if(SwitchInst::classof(term))
	{
		const SwitchInst* si=cast<const SwitchInst>(term);
		assert(branchId > 0);
		SwitchInst::ConstCaseIt it=si->case_begin();
		for(int i=1;i<branchId;i++)
			++it;
		const BasicBlock* dest=it.getCaseSuccessor();
		//We found the destination, there may be more cases for the same
		//destination though
		writer->compileOperand(si->getCondition());
		writer->stream << " === ";
		writer->compileConstant(it.getCaseValue());
		for(;it!=si->case_end();++it)
		{
			if(it.getCaseSuccessor()==dest)
			{
				//Also add this condition
				writer->stream << "|| (";
				writer->compileOperand(si->getCondition());
				writer->stream << " === ";
				writer->compileConstant(it.getCaseValue());
				writer->stream << ')';
			}
		}
	}
	else
	{
		term->dump();
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}
}

void DuettoRenderInterface::renderIfBlockBegin(const void* privateBlock, int branchId, bool first)
{
	const BasicBlock* bb=(const BasicBlock*)privateBlock;
	if(!first)
		writer->stream << "} else ";
	writer->stream << "if (";
	renderCondition(bb, branchId);
	writer->stream << ") {" << NewLine;
}

void DuettoRenderInterface::renderIfBlockBegin(const void* privateBlock, const std::vector<int>& skipBranchIds, bool first)
{
	const BasicBlock* bb=(const BasicBlock*)privateBlock;
	if(!first)
		writer->stream << "} else ";
	writer->stream << "if (!(";
	for(uint32_t i=0;i<skipBranchIds.size();i++)
	{
		if(i!=0)
			writer->stream << "||";
		renderCondition(bb, skipBranchIds[i]);
	}
	writer->stream << ")) {" << NewLine;
}

void DuettoRenderInterface::renderElseBlockBegin()
{
	writer->stream << "} else {" << NewLine;
}

void DuettoRenderInterface::renderBlockEnd()
{
	writer->stream << '}' << NewLine;
}

void DuettoRenderInterface::renderBlockPrologue(const void* privateBlockTo, const void* privateBlockFrom)
{
	const BasicBlock* bbTo=(const BasicBlock*)privateBlockTo;
	const BasicBlock* bbFrom=(const BasicBlock*)privateBlockFrom;
	writer->compilePHIOfBlockFromOtherBlock(bbTo, bbFrom);
}

bool DuettoRenderInterface::hasBlockPrologue(const void* privateBlockTo) const
{
	const BasicBlock* bbTo=(const BasicBlock*)privateBlockTo;
	return bbTo->getFirstNonPHI()!=&bbTo->front();
}

void DuettoRenderInterface::renderWhileBlockBegin()
{
	writer->stream << "while(1) {" << NewLine;
}

void DuettoRenderInterface::renderWhileBlockBegin(int blockLabel)
{
	writer->stream << 'L' << blockLabel << ':';
	renderWhileBlockBegin();
}

void DuettoRenderInterface::renderDoBlockBegin()
{
	writer->stream << "do {" << NewLine;
}

void DuettoRenderInterface::renderDoBlockBegin(int blockLabel)
{
	writer->stream << 'L' << blockLabel << ':';
	renderDoBlockBegin();
}

void DuettoRenderInterface::renderDoBlockEnd()
{
	writer->stream << "} while(0);" << NewLine;
}

void DuettoRenderInterface::renderBreak()
{
	writer->stream << "break;" << NewLine;
}

void DuettoRenderInterface::renderBreak(int labelId)
{
	writer->stream << "break L" << labelId << ";" << NewLine;
}

void DuettoRenderInterface::renderContinue()
{
	writer->stream << "continue;" << NewLine;
}

void DuettoRenderInterface::renderContinue(int labelId)
{
	writer->stream << "continue L" << labelId << ';' << NewLine;
}

void DuettoRenderInterface::renderLabel(int labelId)
{
	writer->stream << "label = " << labelId << ';' << NewLine;
}

void DuettoRenderInterface::renderIfOnLabel(int labelId, bool first)
{
	if(first==false)
		writer->stream << "else ";
	writer->stream << "if (label === " << labelId << ") {" << NewLine;
}

void DuettoWriter::compileMethod(const Function& F)
{
	currentFun = &F;
	stream << "function _" << F.getName() << "(";
	const Function::const_arg_iterator A=F.arg_begin();
	const Function::const_arg_iterator AE=F.arg_end();
	for(Function::const_arg_iterator curArg=A;curArg!=AE;++curArg)
	{
		if(curArg!=A)
			stream << ", ";
		printArgName(curArg);
	}
	stream << ") {" << NewLine;
	std::map<const BasicBlock*, uint32_t> blocksMap;
	if(F.size()==1)
		compileBB(*F.begin(), blocksMap);
	else
	{
		stream << "var label = 0;" << NewLine;
		//TODO: Support exceptions
		Function::const_iterator B=F.begin();
		Function::const_iterator BE=F.end();
		//First run, create the corresponding relooper blocks
		std::map<const BasicBlock*, /*relooper::*/Block*> relooperMap;
		for(;B!=BE;++B)
		{
			if(B->isLandingPad())
				continue;
			//Decide if this block should be duplicated instead
			//of actually directing the control flow to reach it
			//Currently we just check if the block ends with a return
			//and its small enough. This should simplify some control flows.
			bool isSplittable = B->size()<3 && ReturnInst::classof(B->getTerminator());
			Block* rlBlock = new Block(&(*B), isSplittable);
			relooperMap.insert(make_pair(&(*B),rlBlock));
		}

		B=F.begin();
		BE=F.end();
		//Second run, add the branches
		for(;B!=BE;++B)
		{
			if(B->isLandingPad())
				continue;
			const TerminatorInst* term=B->getTerminator();
			uint32_t defaultBranchId=-1;
			//Find out which branch id is the default
			if(BranchInst::classof(term))
			{
				const BranchInst* bi=cast<const BranchInst>(term);
				if(bi->isUnconditional())
					defaultBranchId = 0;
				else
					defaultBranchId = 1;
			}
			else if(SwitchInst::classof(term))
			{
#ifndef NDEBUG
				const SwitchInst* si=cast<const SwitchInst>(term);
#endif
				assert(si->getDefaultDest()==si->getSuccessor(0));
				defaultBranchId = 0;
			}
			else if(InvokeInst::classof(term))
			{
#ifndef NDEBUG
				const InvokeInst* ii=cast<const InvokeInst>(term);
#endif
				assert(ii->getNormalDest()==ii->getSuccessor(0));
				defaultBranchId = 0;
			}
			else if(term->getNumSuccessors())
			{
				//Only a problem if there are successors
				term->dump();
				llvm::report_fatal_error("Unsupported code found, please report a bug", false);
			}

			for(uint32_t i=0;i<term->getNumSuccessors();i++)
			{
				if(term->getSuccessor(i)->isLandingPad())
					continue;
				Block* target=relooperMap[term->getSuccessor(i)];
				//Use -1 for the default target
				bool ret=relooperMap[&(*B)]->AddBranchTo(target, (i==defaultBranchId)?-1:i);
				if(ret==false) //More than a path for a single block can only happen for switch
					assert(SwitchInst::classof(term));
			}
		}

		B=F.begin();
		BE=F.end();
		//Third run, add the block to the relooper and run it
		Relooper* rl=new Relooper();
		for(;B!=BE;++B)
		{
			if(B->isLandingPad())
				continue;
			rl->AddBlock(relooperMap[&(*B)]);
		}
		rl->Calculate(relooperMap[&F.getEntryBlock()]);
		
		DuettoRenderInterface ri(this, NewLine);
		rl->Render(&ri);
	}

	stream << '}' << NewLine;
	currentFun = NULL;
}

void DuettoWriter::compileGlobal(const GlobalVariable& G)
{
	assert(G.hasName());
	if(isClientGlobal(G.getName().data()))
	{
		//Global objects in the client namespace are only
		//placeholders for JS calls
		return;
	}
	stream  << "var ";
	printLLVMName(G.getName(), GLOBAL);

	bool addSelf = false;
	if(G.hasInitializer())
	{
		stream << " = ";
		const Constant* C = G.getInitializer();
		
		if( analyzer.getPointerKind(&G) == COMPLETE_ARRAY )
		{
			stream << '[';
			compileOperand(C, REGULAR);
			stream << ']';
		}
		else 
			compileOperand(C, REGULAR);

		if ( isa<StructType>( C->getType() ) && globalDeps.classesWithBaseInfo().count(cast<StructType>(C->getType()) ) )
			addSelf = false;
		else
			addSelf = analyzer.hasSelfMember(&G);
	}
	stream << ';' << NewLine;

	if(addSelf)
		addSelfPointer(&G);
	
	compiledGVars.insert(&G);

	//Now we have defined a new global, check if there are fixups for previously defined globals
	auto fixup_range = globalDeps.fixupVars().equal_range(&G);
	
	for ( auto it = fixup_range.first; it != fixup_range.second; ++it )
	{
		const GlobalDepsAnalyzer::SubExprVec & subExpr = it->second;
		
		assert( !subExpr.empty() );
		assert( isa<GlobalVariable>( subExpr.front()->getUser() ) );
		
		const GlobalVariable* otherGV = cast<GlobalVariable>(subExpr.front()->getUser());
		if(!otherGV->hasInitializer())
		{
			llvm::errs() << "Expected initializer for ";
			otherGV->dump();
			llvm::errs() << "\n";
			llvm::report_fatal_error("Unsupported code found, please report a bug", false);
			continue;
		}

		printLLVMName(otherGV->getName(), GLOBAL);
		if( analyzer.getPointerKind(otherGV) == COMPLETE_ARRAY )
			stream << "[0]";

		for ( auto it = std::next(subExpr.begin()); it != subExpr.end(); ++it )
		{
			const Use * u = *it;

			if ( isa<ConstantArray>( u->getUser() ) )
				stream << '[' << u->getOperandNo() << ']';
			else if ( isa<ConstantStruct>( u->getUser() ) )
				stream << ".a" << u->getOperandNo();
		}

		stream << " = ";
		compileOperand( subExpr.back()->get(), REGULAR);
		stream << ';' << NewLine;
	}
}

void DuettoWriter::compileNullPtrs()
{
	stream << "var nullArray = [null];var nullObj = { d: nullArray, o: 0 };" << NewLine;
}

void DuettoWriter::compileCreateClosure()
{
	stream << "function duettoCreateClosure(func, obj) { return function(e) { func(obj, e); }; }" << NewLine;
}

void DuettoWriter::compileHandleVAArg()
{
	stream << "function handleVAArg(ptr) { var ret=ptr.d[ptr.o]; ptr.o++; return ret; }" << NewLine;
}

void DuettoWriter::makeJS()
{
	sourceMapGenerator.beginFile();
	// Enable strict mode first
	stream << "\"use strict\"" << NewLine;

	compileClassesExportedToJs();
	compileNullPtrs();
	
	for ( const Function * F : globalDeps.functionOrderedList() )
		if (!F->empty())
			compileMethod(*F);
	
	for ( const GlobalVariable * GV : globalDeps.varsOrderedList() )
		compileGlobal(*GV);

	for ( StructType * st : globalDeps.classesWithBaseInfo() )
		compileClassType(st);

	for ( StructType * st : globalDeps.dynAllocArrays() )
		compileArrayClassType(st);

	if ( globalDeps.needCreatePointerArray() )
		compileArrayPointerType();
	
	//Compile the closure creation helper
	if ( globalDeps.needCreateClosure() )
		compileCreateClosure();
	
	//Compile handleVAArg if needed
	if( globalDeps.needHandleVAArg() )
		compileHandleVAArg();
	
	//Call constructors
	for (const Function * F : globalDeps.constructors() )
	{
		printLLVMName(F->getName(), GLOBAL);
		stream << "();" << NewLine;
	}

	//Invoke the webMain function
	stream << "__Z7webMainv();" << NewLine;

	sourceMapGenerator.endFile();
	// Link the source map if necessary
	if (!sourceMapName.empty())
		stream << "//# sourceMappingURL=" << sourceMapName;
	
#ifdef DUETTO_DEBUG_POINTERS
	analyzer.dumpAllFunctions();
	analyzer.dumpAllPointers();
#endif //DUETTO_DEBUG_POINTERS

}

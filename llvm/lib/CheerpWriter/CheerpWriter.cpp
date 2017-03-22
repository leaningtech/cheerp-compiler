//===-- CheerpWriter.cpp - The Cheerp JavaScript generator -------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2015 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "Relooper.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;
using namespace std;
using namespace cheerp;

//TODO: make this a command line parameter
constexpr int32_t functionAddrStart = 0x1000000;

//De-comment this to debug the pointer kind of every function
//#define CHEERP_DEBUG_POINTERS

//De-comment this to debug the source map generation
//#define CHEERP_DEBUG_SOURCE_MAP

class CheerpRenderInterface: public RenderInterface
{
private:
	CheerpWriter* writer;
	const NewLineHandler& NewLine;
	bool asmjs;
	void renderCondition(const BasicBlock* B, int branchId, CheerpWriter::PARENT_PRIORITY parentPrio);
public:
	CheerpRenderInterface(CheerpWriter* w, const NewLineHandler& n, bool asmjs=false):writer(w),NewLine(n),asmjs(asmjs)
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

void CheerpWriter::handleBuiltinNamespace(const char* identifier, llvm::ImmutableCallSite callV)
{
	assert(callV.getCalledFunction());
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

	bool isClientStatic = callV.getCalledFunction()->hasFnAttribute(Attribute::Static);

	//The first arg should be the object
	if(strncmp(funcName,"get_",4)==0)
	{
		//Getter
		assert(callV.arg_size()==1);
		if(className == NULL)
		{
			llvm::report_fatal_error(Twine("Unexpected getter without class: ", StringRef(identifier)), false);
			return;
		}

		compileOperand(callV.getArgument(0), HIGHEST);
		stream << '.' << StringRef( funcName + 4, funcNameLen - 4 );
	}
	else if(strncmp(funcName,"set_",4)==0)
	{
		//Setter
		if(className == NULL)
		{
			llvm::report_fatal_error(Twine("Unexpected setter without class: ", StringRef(identifier)), false);
			return;
		}

		compileOperand(callV.getArgument(0), HIGHEST);
		if(funcNameLen == 4)
		{
			// Generic setter
			assert(callV.arg_size()==3);
			stream << '[';
			compileOperand(callV.getArgument(1), LOWEST);
			stream << "]=";
			compileOperand(callV.getArgument(2), LOWEST);
		}
		else
		{
			assert(callV.arg_size()==2);
			stream << '.' << StringRef( funcName + 4, funcNameLen - 4 ) <<  '=';
			compileOperand(callV.getArgument(1), LOWEST);
		}
	}
	else if(className == NULL && strncmp(funcName,"Objectix",8)==0)
	{
		// operator[]
		assert(callV.arg_size()==2);
		compileOperand(callV.getArgument(0), HIGHEST);
		stream << '[';
		compileOperand(callV.getArgument(1), LOWEST);
		stream << ']';
	}
	else
	{
		User::const_op_iterator it = callV.arg_begin();

		//Regular call
		if(className)
		{
			if(isClientStatic)
				stream << StringRef(className,classLen);
			else if(callV.arg_empty())
			{
				llvm::report_fatal_error(Twine("At least 'this' parameter was expected: ",
					StringRef(identifier)), false);
				return;
			}
			else
			{
				compileOperand(*it, HIGHEST);
				++it;
			}
			stream << '.';
		}
		stream << StringRef(funcName,funcNameLen);
		compileMethodArgs(it,callV.arg_end(), callV, /*forceBoolean*/ true);
	}
}

void CheerpWriter::compileCopyElement(const Value* baseDest,
                                      const Value* baseSrc,
                                      Type* currentType)
{
	switch(currentType->getTypeID())
	{
		case Type::IntegerTyID:
		case Type::FloatTyID:
		case Type::DoubleTyID:
		case Type::PointerTyID:
		{
			compileCompleteObject(baseDest, nullptr);
			stream << '=';
			compileCompleteObject(baseSrc, nullptr);
			stream << ';' << NewLine;
			break;
		}
		case Type::ArrayTyID:
		case Type::StructTyID:
		{
			if(TypeSupport::hasByteLayout(currentType))
			{
				uint64_t typeSize = targetData.getTypeAllocSize(currentType);
				stream << "var __tmp__=new Int8Array(";
				compilePointerBase(baseDest);
				stream << ".buffer,";
				compilePointerOffset(baseDest, LOWEST);
				stream << ',' << typeSize << ");" << NewLine;
				stream << "__tmp__.set(";
				stream << "new Int8Array(";
				compilePointerBase(baseSrc);
				stream << ".buffer,";
				compilePointerOffset(baseSrc, LOWEST);
				stream << ',' << typeSize << "));" << NewLine;
				break;
			}
			// Fallthrough if not byte layout
		}
		default:
			llvm::errs() << "Support type in copy ";
			currentType->dump();
			llvm::errs() << '\n';
	}
}

void CheerpWriter::compileDowncast( ImmutableCallSite callV )
{
	assert( callV.arg_size() == 2 );
	assert( callV.getCalledFunction() && callV.getCalledFunction()->getIntrinsicID() == Intrinsic::cheerp_downcast);

	POINTER_KIND result_kind = PA.getPointerKind(callV.getInstruction());
	const Value * src = callV.getArgument(0);
	const Value * offset = callV.getArgument(1);

	Type* t=src->getType()->getPointerElementType();

	if(TypeSupport::isClientType(t) || (isa<ConstantInt>(offset) && cast<ConstantInt>(offset)->isNullValue()))
	{
		if(result_kind == SPLIT_REGULAR)
		{
			compilePointerBase(src);
			stream << ';' << NewLine;
			stream << namegen.getSecondaryName(callV.getInstruction()) << '=';
			compilePointerOffset(src, LOWEST);
		}
		else
			compilePointerAs(src, result_kind);
	}
	else
	{
		//Do a runtime downcast
		if(result_kind == SPLIT_REGULAR)
		{
			compileCompleteObject(src);
			stream << ".a;" << NewLine;
			stream << namegen.getSecondaryName(callV.getInstruction()) << '=';
			compileCompleteObject(src);
			stream << ".o-";
			compileOperand(offset, HIGHEST);
		}
		else if(result_kind == REGULAR)
		{
			stream << "{d:";
			compileCompleteObject(src);
			stream << ".a,o:";
			compileCompleteObject(src);

			stream << ".o-";
			compileOperand(offset, HIGHEST);
			stream << '}';
		}
		else if(result_kind == RAW)
		{
			stream << '(';
			compileOperand(src, ADD_SUB);
			stream  << '-';
			compileOperand(offset, HIGHEST);
			stream << "|0)";
		}
		else
		{
			compileCompleteObject(src);
			stream << ".a[";
			compileCompleteObject(src);
			stream << ".o-";
			compileOperand(offset, HIGHEST);
			stream << ']';
		}
	}
}

/* Method that handles memcpy and memmove.
 * Since only immutable types are handled in the backend and we use TypedArray.set to make the copy
 * there is not need to handle memmove in a special way
*/
void CheerpWriter::compileMemFunc(const Value* dest, const Value* src, const Value* size)
{
	Type* destType=dest->getType();
	Type* pointedType = cast<PointerType>(destType)->getElementType();
	if(!(TypeSupport::isTypedArrayType(pointedType, /* forceTypedArray*/ true) || TypeSupport::hasByteLayout(pointedType)))
		llvm::report_fatal_error("Unsupported memory intrinsic, please rebuild the code using an updated version of Cheerp", false);

	uint64_t typeSize = TypeSupport::hasByteLayout(pointedType) ? 1 : targetData.getTypeAllocSize(pointedType);

	bool constantNumElements = false;
	uint32_t numElem = 0;

	if(isa<ConstantInt>(size))
	{
		uint32_t allocatedSize = getIntFromValue(size);
		numElem = (allocatedSize+typeSize-1)/typeSize;
		constantNumElements = true;
	}
	else
	{
		//Compute number of elements at runtime
		stream << "var __numElem__=";
		compileOperand(size,MUL_DIV);
		stream << '/' << typeSize;
		//Make sure to close this if below
		stream << ';' << NewLine;
	}


	// Handle the case for multiple elements, it assumes that we can use TypedArray.set
	if(!constantNumElements)
		stream << "if(__numElem__>1)" << NewLine << '{';
	if(!constantNumElements || numElem>1)
	{
		bool byteLayout = PA.getPointerKind(dest) == BYTE_LAYOUT;
		// The semantics of TypedArray.set is memmove-like, no need to care about direction
		if(byteLayout)
			stream << "(new Int8Array(";
		compilePointerBase(dest);
		if(byteLayout)
			stream << ".buffer))";
		stream << ".set(";
		if(byteLayout)
			stream << "(new Int8Array(";
		compilePointerBase(src);
		if(byteLayout)
			stream << ".buffer))";

		//We need to get a subview of the source
		stream << ".subarray(";
		compilePointerOffset(src, LOWEST);
		stream << ',';
		compilePointerOffset(src, ADD_SUB);
		stream << '+';

		// Use the size
		if(constantNumElements)
			stream << numElem;
		else
			stream << "__numElem__";

		stream << "),";
		compilePointerOffset(dest, LOWEST);
		stream << ");" << NewLine;
	}
	// Handle the single element case, do not assume we have a typed array
	if(!constantNumElements)
		stream << NewLine << "}else if(__numElem__===1)" << NewLine << '{';
	if(!constantNumElements || numElem==1)
	{
		compileCopyElement(dest, src, pointedType);
	}
	if(!constantNumElements)
		stream << NewLine << '}';
}

uint32_t CheerpWriter::compileArraySize(const DynamicAllocInfo & info, bool shouldPrint, bool inBytes)
{
	// We assume parenthesis around this code
	Type * t = info.getCastedType()->getElementType();
	uint32_t typeSize = targetData.getTypeAllocSize(t);
	if(inBytes)
		typeSize = 1;

	bool closeMathImul = false;
	uint32_t numElem = 1;
	if(const Value* numberOfElements = info.getNumberOfElementsArg())
	{
		if(isa<ConstantInt>(numberOfElements))
			numElem = getIntFromValue(numberOfElements);
		else
		{
			assert(shouldPrint);
			if(useMathImul)
			{
				stream << namegen.getBuiltinName(NameGenerator::Builtin::IMUL) << '(';
				closeMathImul = true;
			}
			compileOperand(numberOfElements, LOWEST);
			if(useMathImul)
				stream << ',';
			else
				stream << '*';
		}
	}
	if( !info.sizeIsRuntime() )
	{
		uint32_t allocatedSize = getIntFromValue( info.getByteSizeArg() );
		numElem *= (allocatedSize+typeSize-1);
		if(closeMathImul)
		{
			assert(shouldPrint);
			// We need to multiply before we divide
			stream << numElem;
			stream << ")/" << typeSize << "|0";
		}
		else
		{
			if(shouldPrint)
				stream << (numElem / typeSize);
			else
				return numElem / typeSize;
		}
	}
	else
	{
		assert(shouldPrint);
		compileOperand( info.getByteSizeArg(), closeMathImul?LOWEST:MUL_DIV );
		if(closeMathImul)
			stream << ')';
		stream << '/' << typeSize << "|0";
	}
	assert(shouldPrint);
	return -1;
}

void CheerpWriter::compileAllocation(const DynamicAllocInfo & info)
{
	assert (info.isValidAlloc());

	Type * t = info.getCastedType()->getElementType();

	POINTER_KIND result = PA.getPointerKind(info.getInstruction());
	const ConstantInt* constantOffset = PA.getConstantOffsetForPointer(info.getInstruction());
	bool needsDowncastArray = isa<StructType>(t) && globalDeps.needsDowncastArray(cast<StructType>(t));
	bool needsRegular = result==REGULAR && !constantOffset && !needsDowncastArray;
	assert(result != SPLIT_REGULAR || constantOffset);

	if(needsRegular)
	{
		stream << "{d:";
	}

	// To implement cheerp_reallocate we need to strategies:
	// 1) Immutable types are stored in typed array which cannot be resized, we need to make a new one
	//    and copy the old data over
	// 2) Objects and pointers are stored in a regular array and we can just resize them
	if (info.getAllocType() == DynamicAllocInfo::cheerp_reallocate)
	{
		if (info.useTypedArray() || BYTE_LAYOUT == result)
		{
			stream << "(function(){";
			stream << "var __old__=";
			compilePointerBase(info.getMemoryArg());
			stream << ';' << NewLine;
			//Allocated the new array (created below) in a temporary var
			stream << "var __ret__=";
		}
	}

	
	if (info.useTypedArray())
	{
		stream << "new ";
		compileTypedArrayType(t);
		stream << '(';
		compileArraySize(info, /* shouldPrint */true);
		stream << ')';
	}
	else if(BYTE_LAYOUT == result)
	{
		stream << "new DataView(new ArrayBuffer(((";
		compileArraySize(info, /* shouldPrint */true, /* inBytes */true);
		// Round up the size to make sure that any typed array can be initialized from the buffer
		stream << ")+ 7) & (~7)))";
	}
	else if (info.useCreateArrayFunc() )
	{
		assert( globalDeps.dynAllocArrays().count(t) );
		
		stream << namegen.getArrayName(t) << '(';
		if (info.getAllocType() == DynamicAllocInfo::cheerp_reallocate)
		{
			compilePointerBase(info.getMemoryArg());
			stream << ',';
			compilePointerBase(info.getMemoryArg());
			stream << ".length,";
			compileArraySize(info, /* shouldPrint */true);
			stream << ')';
		}
		else
		{
			stream << "[],0,";
			compileArraySize(info, /* shouldPrint */true);
			stream << ')';
		}
	}
	else if (info.useCreatePointerArrayFunc() )
	{
		stream << "createPointerArray(";
		if (info.getAllocType() == DynamicAllocInfo::cheerp_reallocate)
		{
			compilePointerBase(info.getMemoryArg());
			stream << ',';
			compilePointerBase(info.getMemoryArg());
			stream << ".length,";
		}
		else
		{
			stream << "[],0,";
		}
		compileArraySize(info, /* shouldPrint */true);
		stream << ',';
		if(PA.getPointerKindForStoredType(info.getCastedType())==COMPLETE_OBJECT)
			stream << "null";
		else
			stream << "nullObj";
		stream << ')';
		assert( globalDeps.needCreatePointerArray() );
	}
	else if (!info.sizeIsRuntime() )
	{
		assert( info.getAllocType() != DynamicAllocInfo::cheerp_reallocate );
		// Create a plain array
		
		uint32_t numElem = compileArraySize(info, /* shouldPrint */false);
		
		assert((REGULAR == result || SPLIT_REGULAR == result || BYTE_LAYOUT == result) || numElem <= 1);

		if((REGULAR == result || SPLIT_REGULAR == result) && !needsDowncastArray)
			stream << '[';

		for(uint32_t i = 0; i < numElem;i++)
		{
			compileType(t, LITERAL_OBJ, !isInlineable(*info.getInstruction(), PA) ? namegen.getName(info.getInstruction()) : StringRef());
			if((i+1) < numElem)
				stream << ',';
		}

		if(numElem == 0)
			stream << "null";

		if(REGULAR == result || SPLIT_REGULAR == result)
		{
			if(needsDowncastArray)
				stream << ".a";
			else
				stream << ']';
		}
	}
	else
	{
		llvm::errs() << "Allocating type " << *t << "\n";
		llvm::report_fatal_error("Unsupported type in allocation", false);
	}

	if (info.getAllocType() == DynamicAllocInfo::cheerp_reallocate)
	{
		if (info.useTypedArray() || result == BYTE_LAYOUT)
		{
			stream << ';' << NewLine;
			//__ret__ now contains the new array, we need to copy over the data
			if(result == BYTE_LAYOUT)
				stream << "(new Int8Array(__ret__.buffer)).set((new Int8Array(__old__.buffer)).subarray(0, Math.min(__ret__.byteLength,__old__.byteLength)));" << NewLine;
			else
			{
				//The amount of data to copy is limited by the shortest between the old and new array
				stream << "__ret__.set(__old__.subarray(0, Math.min(__ret__.length,__old__.length)));" << NewLine;
			}
			stream << "return __ret__;})()";
		}
	}

	if(needsRegular)
	{
		stream << ",o:0}";
	}
}

void CheerpWriter::compileFree(const Value* obj)
{
	//TODO: Clean up class related data structures
}

CheerpWriter::COMPILE_INSTRUCTION_FEEDBACK CheerpWriter::handleBuiltinCall(ImmutableCallSite callV, const Function * func)
{
	assert( callV.isCall() || callV.isInvoke() );
	assert( func );
	assert( (func == callV.getCalledFunction() ) || !(callV.getCalledFunction()) );
	
	bool userImplemented = !func->empty();
	
	ImmutableCallSite::arg_iterator it = callV.arg_begin(), itE = callV.arg_end();
	
	StringRef ident = func->getName();
	unsigned intrinsicId = func->getIntrinsicID();

	StringRef section  = currentFun->getSection();
	bool asmjs = section == StringRef("asmjs");

	//First handle high priority builtins, they will be used even
	//if an implementation is available from the user
	if(intrinsicId==Intrinsic::memmove ||
		intrinsicId==Intrinsic::memcpy)
	{
		if (!asmjs)
		{
			compileMemFunc(*(it), *(it+1), *(it+2));
			return COMPILE_EMPTY;
		}
		else
		{
			stream << "__asmjs_memmove(";
			compileOperand(*(it),LOWEST);
			stream << ",";
			compileOperand(*(it+1),LOWEST);
			stream << ",";
			compileOperand(*(it+2),LOWEST);
			stream << ")|0";
			return COMPILE_OK;
		}
	}
	else if(intrinsicId==Intrinsic::memset)
	{
		llvm::report_fatal_error("Unsupported memory intrinsic, please rebuild the code using an updated version of Cheerp", false);
		return COMPILE_EMPTY;
	}
	else if(intrinsicId==Intrinsic::invariant_start)
	{
		//TODO: Try to optimize using this, for now just pass the second arg
		if(!callV.getInstruction()->use_empty())
		{
			compileOperand(*(it+1));
			return COMPILE_OK;
		}
		else
			return COMPILE_EMPTY;
	}
	else if(intrinsicId==Intrinsic::invariant_end)
		return COMPILE_EMPTY;
	else if(intrinsicId==Intrinsic::vastart)
	{
		if (asmjs)
		{
			stream << heapNames[HEAP32] << '[';
			compileRawPointer(*it);
			stream << ">>2]=__savedStack|0";
		}
		else
		{
			compileCompleteObject(*it);
			stream << "={d:arguments,o:" << namegen.getName(currentFun) << ".length}";
		}
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::vaend)
	{
		if (asmjs) return COMPILE_EMPTY;

		compileCompleteObject(*it);
		stream << "=null";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_downcast)
	{
		compileDowncast( callV );
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_downcast_current)
	{
		if (asmjs)
		{
			compileOperand(*it);
		}
		else
		{
			compileCompleteObject(*it);
			stream << ".o|0";
		}
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_upcast_collapsed)
	{
		compilePointerAs(*it, PA.getPointerKind(callV.getInstruction()));
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_cast_user)
	{
		if(callV.getInstruction()->use_empty())
			return COMPILE_EMPTY;

		compileBitCast(callV.getInstruction(), PA.getPointerKind(callV.getInstruction()));
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_pointer_base)
	{
		compilePointerBase(*it, true);
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_pointer_offset)
	{
		compilePointerOffset(*it, LOWEST, true);
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_create_closure)
	{
		assert( globalDeps.needCreateClosure() );

		//We use an helper method to create closures without
		//keeping all local variable around. The helper
		//method is printed on demand depending on a flag
		assert( isa<Function>( callV.getArgument(0) ) );
		stream << "cheerpCreateClosure(";
		compileCompleteObject( callV.getArgument(0) );
		stream << ',';
		compilePointerAs( callV.getArgument(1), 
				  PA.getPointerKind( cast<Function>(callV.getArgument(0))->arg_begin() ) );
		stream << ')';
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_make_complete_object)
	{
		compileCompleteObject(*it);
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_make_regular)
	{
		stream << "{d:";
		compileCompleteObject(*it);
		stream << ",o:";
		compileOperand(*(it+1), LOWEST);
		stream << '}';
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::flt_rounds)
	{
		// Rounding mode 1: nearest
		stream << '1';
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::ctlz)
	{
		stream << "Math.clz32(";
		compileOperand(*it, LOWEST);
		stream << ')';
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::expect)
	{
		compileOperand(*it);
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::trap && asmjs)
	{
		//TODO: handle correctly when not in pretty mode
		stream << "__dummy()";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::stacksave && asmjs)
	{
		stream << "__stackPtr";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::stackrestore && asmjs)
	{
		stream <<"__stackPtr=";
		compileOperand(*it, LOWEST);
		return COMPILE_OK;
	}
	else if(!asmjs && (ident=="free" || ident=="_ZdlPv" || ident=="_ZdaPv" || intrinsicId==Intrinsic::cheerp_deallocate))
	{
		compileFree(*it);
		return COMPILE_OK;
	}
	else if(ident=="fmod" || ident=="fmodf")
	{
		// Handle this internally, C++ does not have float mod operation
		stream << '(';
		compileOperand(*(it), MUL_DIV);
		stream << '%';
		compileOperand(*(it+1), nextPrio(MUL_DIV));
		stream << ')';
		return COMPILE_OK;
	}
	else if(useNativeJavaScriptMath)
	{
		const char* Math = asmjs ? "" : "Math.";
		if(ident=="fabs" || ident=="fabsf")
		{
			stream << Math << "abs(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="acos" || ident=="acosf")
		{
			stream << Math << "acos(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="asin" || ident=="asinf")
		{
			stream << Math << "asin(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="atan" || ident=="atanf")
		{
			stream << Math << "atan(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="atan2" || ident=="atan2f")
		{
			stream << Math << "atan2(";
			compileOperand(*(it), LOWEST);
			stream << ',';
			compileOperand(*(it+1), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="ceil" || ident=="ceilf")
		{
			stream << Math << "ceil(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="cos" || ident=="cosf")
		{
			stream << Math << "cos(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="exp" || ident=="expf")
		{
			stream << Math << "exp(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="floor" || ident=="floorf")
		{
			stream << Math << "floor(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="log" || ident=="logf")
		{
			stream << Math << "log(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="pow" || ident=="powf")
		{
			stream << Math << "pow(";
			compileOperand(*(it), LOWEST);
			stream << ',';
			compileOperand(*(it+1), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="round" || ident=="roundf")
		{
			stream << Math << "round(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="sin" || ident=="sinf")
		{
			stream << Math << "sin(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="sqrt" || ident=="sqrtf")
		{
			stream << Math << "sqrt(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="tan" || ident=="tanf")
		{
			stream << Math << "tan(";
			compileOperand(*(it), LOWEST);
			stream << ')';
			return COMPILE_OK;
		}
	}

	DynamicAllocInfo da(callV, &targetData, forceTypedArrays);
	if (da.isValidAlloc())
	{
		compileAllocation(da);
		return COMPILE_OK;
	}
	else if(ident=="cheerpCreate_ZN6client6StringC2EPKc")
	{
		StringRef str;
		if(llvm::getConstantStringInfo(*it, str))
		{
			stream << '"';
			for(uint8_t c: str)
			{
				if(c=='\b')
					stream << "\\b";
				else if(c=='\f')
					stream << "\\f";
				else if(c=='\n')
					stream << "\\n";
				else if(c=='\r')
					stream << "\\r";
				else if(c=='\t')
					stream << "\\t";
				else if(c=='\v')
					stream << "\\v";
				else if(c=='\'')
					stream << "\\'";
				else if(c=='"')
					stream << "\\\"";
				else if(c=='\\')
					stream << "\\\\";
				else if(c>=' ' && c<='~')
				{
					// Printable ASCII after we exscluded the previous one
					stream << c;
				}
				else
				{
					char buf[5];
					snprintf(buf, 5, "\\x%02x", c);
					stream << buf;
				}
			}
			stream << '"';
			return COMPILE_OK;
		}
	}

	//If the method is implemented by the user, stop here
	if(userImplemented)
		return COMPILE_UNSUPPORTED;

	if(ident.startswith("_ZN6client"))
	{
		handleBuiltinNamespace(ident.data()+10,callV);
		return COMPILE_OK;
	}
	else if(ident.startswith("_ZNK6client"))
	{
		handleBuiltinNamespace(ident.data()+11,callV);
		return COMPILE_OK;
	}
	else if(ident.startswith("cheerpCreate_ZN6client"))
	{
		//Default handling of builtin constructors
		char* typeName;
		int typeLen=strtol(ident.data()+22,&typeName,10);
		//For builtin String, do not use new
		if(strncmp(typeName, "String", 6)!=0)
			stream << "new ";
		stream << StringRef(typeName, typeLen);
		compileMethodArgs(it, itE, callV, /*forceBoolean*/ true);
		return COMPILE_OK;
	}
	return COMPILE_UNSUPPORTED;
}

void CheerpWriter::compilePredicate(CmpInst::Predicate p)
{
	bool asmjs = currentFun->getSection() == StringRef("asmjs");
	switch(p)
	{
		case CmpInst::FCMP_UEQ: //TODO: fix this, if an operand is NaN LLVM expects false,
		case CmpInst::FCMP_OEQ:
		case CmpInst::ICMP_EQ:
			if (asmjs)
				stream << "==";
			else
				stream << "===";
			break;
		case CmpInst::FCMP_UNE: //The undordered case correspond to the usual JS operator
					//See ECMA-262, Section 11.9.6
		case CmpInst::FCMP_ONE:
		case CmpInst::ICMP_NE:
			if (asmjs)
				stream << "!=";
			else
				stream << "!==";
			break;
		case CmpInst::FCMP_OGT: //TODO: fix this, if an operand is NaN LLVM expects false,
		case CmpInst::FCMP_UGT:	//but JS returns undefined. Adding ==true after the whole expression
					//should work
		case CmpInst::ICMP_SGT:
		case CmpInst::ICMP_UGT: //TODO: To support unsigned we need to add casts around the ops
			stream << '>';
			break;
		case CmpInst::FCMP_UGE:
		case CmpInst::FCMP_OGE:
		case CmpInst::ICMP_SGE:
		case CmpInst::ICMP_UGE:
			stream << ">=";
			break;
		case CmpInst::FCMP_OLT: //TODO: fix this, if an operand is NaN LLVM expects false,
		case CmpInst::FCMP_ULT:	//but JS returns undefined. Adding ==true after the whole expression
					//should work
		case CmpInst::ICMP_SLT:
		case CmpInst::ICMP_ULT: //TODO: To support unsigned we need to add casts around the ops
			stream << '<';
			break;
		case CmpInst::FCMP_ULE:
		case CmpInst::FCMP_OLE:
		case CmpInst::ICMP_SLE:
		case CmpInst::ICMP_ULE:
			stream << "<=";
			break;
		default:
			llvm::errs() << "Support predicate " << p << '\n';
	}
}

void CheerpWriter::compileOperandForIntegerPredicate(const Value* v, CmpInst::Predicate p, PARENT_PRIORITY parentPrio)
{
	assert(v->getType()->isIntegerTy());
	if(CmpInst::isSigned(p))
		compileSignedInteger(v, /*forComparison*/ true, parentPrio);
	else if(CmpInst::isUnsigned(p) || !v->getType()->isIntegerTy(32))
		compileUnsignedInteger(v, parentPrio);
	else
		compileSignedInteger(v, /*forComparison*/ true, parentPrio);
}

void CheerpWriter::compileEqualPointersComparison(const llvm::Value* lhs, const llvm::Value* rhs, CmpInst::Predicate p)
{
	StringRef compareString;
	bool asmjs = currentFun->getSection() == StringRef("asmjs");
	if (asmjs)
		compareString = (p == CmpInst::ICMP_NE) ? "!=" : "==";
	else
		compareString = (p == CmpInst::ICMP_NE) ? "!==" : "===";

	StringRef joinString;
	joinString = (p == CmpInst::ICMP_NE) ? "||" : "&&";

	POINTER_KIND lhsKind = PA.getPointerKind(lhs);
	POINTER_KIND rhsKind = PA.getPointerKind(rhs);

	// in asmjs mode all the pointers are RAW pointers
	if(asmjs)
	{
		stream << "(";
		compileRawPointer(lhs);
		stream << "|0)";
		stream << compareString;
		stream << "(";
		compileRawPointer(rhs);
		stream << "|0)";
	}
	else if((lhsKind == REGULAR || lhsKind == SPLIT_REGULAR || (isGEP(lhs) && cast<User>(lhs)->getNumOperands()==2)) &&
		(rhsKind == REGULAR || rhsKind == SPLIT_REGULAR || (isGEP(rhs) && cast<User>(rhs)->getNumOperands()==2)))
	{
		if(isa<ConstantPointerNull>(lhs))
			stream << '1';
		else
		{
			compilePointerBase(lhs);
			stream << ".length";
		}
		stream << compareString;
		if(isa<ConstantPointerNull>(rhs))
			stream << '1';
		else
		{
			compilePointerBase(rhs);
			stream << ".length";
		}
		stream << joinString;
		compilePointerBase(lhs);
		stream << compareString;
		compilePointerBase(rhs);
		stream << joinString;
		compilePointerOffset(lhs, COMPARISON);
		stream << compareString;
		compilePointerOffset(rhs, COMPARISON);
	}
	else if(lhsKind == BYTE_LAYOUT || rhsKind == BYTE_LAYOUT)
	{
		assert(PA.getPointerKind(lhs) != COMPLETE_OBJECT);
		assert(PA.getPointerKind(rhs) != COMPLETE_OBJECT);
		compilePointerBase(lhs);
		stream << compareString;
		compilePointerBase(rhs);
		stream << joinString;
		compilePointerOffset(lhs, COMPARISON);
		stream << compareString;
		compilePointerOffset(rhs, COMPARISON);
	}
	else
	{
		assert(PA.getPointerKind(lhs) != BYTE_LAYOUT);
		assert(PA.getPointerKind(rhs) != BYTE_LAYOUT);
		compilePointerAs(lhs, COMPLETE_OBJECT);
		stream << compareString;
		compilePointerAs(rhs, COMPLETE_OBJECT);
	}
}

void CheerpWriter::compileAccessToElement(Type* tp, ArrayRef< const Value* > indices, bool compileLastWrapperArray)
{
	for(uint32_t i=0;i<indices.size();i++)
	{
		// Stop when a byte layout type is found
		if (TypeSupport::hasByteLayout(tp))
			return;
		if(StructType* st = dyn_cast<StructType>(tp))
		{
			assert(isa<ConstantInt>(indices[i]));
			const APInt& index = cast<Constant>(indices[i])->getUniqueInteger();

			stream << '.' << types.getPrefixCharForMember(PA, st, index.getLimitedValue()) << index;
			if((i!=indices.size()-1 || compileLastWrapperArray) && types.useWrapperArrayForMember(PA, st, index.getLimitedValue()))
				stream << "[0]";

			tp = st->getElementType(index.getZExtValue());
		}
		else if(const ArrayType* at = dyn_cast<ArrayType>(tp))
		{
			stream << '[';
			compileOperand(indices[i], LOWEST);
			stream << ']';

			tp = at->getElementType();
		}
		else
		{
			llvm::errs() << "Unexpected type during GEP access " << *tp<< "\n";
			llvm::report_fatal_error("Unsupported code found, please report a bug", false);
		}
	}
}

void CheerpWriter::compileOffsetForGEP(Type* pointerOperandType, ArrayRef< const Value* > indices)
{
	// FIXME This will not compile cause getIndexedType is not const-correct
	/*
	 * Type * tp = GetElementPtrInst::getIndexedType( pointerOperandType, indices.slice(0, indices.size() - 1 ) );
	 */

	Type* tp = GetElementPtrInst::getIndexedType(pointerOperandType,
	                makeArrayRef(const_cast<Value* const*>(indices.begin()),
	                             const_cast<Value* const*>(indices.end() - 1)));

	if(tp->isStructTy())
	{
		// Literal index
		assert(isa<ConstantInt>(indices.back()));
		const ConstantInt* idx = cast<ConstantInt>(indices.back());

		assert(types.useWrapperArrayForMember(PA, cast<StructType>(tp), idx->getZExtValue()));

		stream << '0';
	}
	else
	{
		compileOperand(indices.back());
	}
}

void CheerpWriter::compileCompleteObject(const Value* p, const Value* offset)
{
	// Special handle for undefined pointers
	if(isa<UndefValue>(p))
	{
		compileOperand(p);
		return;
	}
	if(isa<ConstantPointerNull>(p))
	{
		stream << "null";
		if(offset)
		{
			// This will trap anyway, but make sure it's valid code
			stream << "[0]";
		}
		return;
	}

	bool isOffsetConstantZero = offset == nullptr || (isa<Constant>(offset) && cast<Constant>(offset)->isZeroValue());

	// Direct access path:
	/**
	 * If p comes from a gep, we can just compile that GEP as COMPLETE_OBJECT
	 * That is, instead of a0.a1["a2"] we got a0.a1.a2
	 */
	if(isOffsetConstantZero)
	{
		if(isGEP(p))
		{
			compileGEP(cast<User>(p), COMPLETE_OBJECT);
			return;
		}
	}

	POINTER_KIND kind = PA.getPointerKind(p);

	if(kind == REGULAR || kind == SPLIT_REGULAR)
	{
		compilePointerBase(p);
		stream << '[';

		const ConstantInt* c1 = dyn_cast_or_null<ConstantInt>(PA.getConstantOffsetForPointer(p));
		const ConstantInt* c2 = dyn_cast_or_null<ConstantInt>(offset);
		if(c1 && c2)
			stream << (c1->getSExtValue() + c2->getSExtValue());
		else if(c1 && c1->isZeroValue() && offset)
			compileOperand(offset, LOWEST);
		else
		{
			compilePointerOffset(p, isOffsetConstantZero ? LOWEST : ADD_SUB);

			if(!isOffsetConstantZero)
			{
				stream << "+";
				compileOperand(offset, ADD_SUB);
				stream << "|0";
			}
		}

		stream << ']';
	}
	else
	{
		compileOperand(p);

		if(!isOffsetConstantZero)
		{
			llvm::errs() << "Can not access a " << kind << " pointer with non zero offset:" << *offset << "\n";
			llvm::report_fatal_error("Unsupported code found, please report a bug", false);
		}
	}
}

void CheerpWriter::compileRawPointer(const Value*p)
{
	while ( isBitCast(p) || isGEP(p) )
	{
		const User * u = cast<User>(p);
		if (isGEP(p))
		{
			Type* curType = u->getOperand(0)->getType();
			SmallVector< const Value *, 8 > indices ( std::next(u->op_begin()), u->op_end() );
			for (uint32_t i=0; i<indices.size(); i++)
			{
				if (StructType* ST = dyn_cast<StructType>(curType))
				{
					uint32_t index = cast<ConstantInt>( indices[i] )->getZExtValue();
					const StructLayout* SL = targetData.getStructLayout( ST );
					curType = ST->getElementType(index);
					uint32_t offset =  SL->getElementOffset(index);
					stream << offset;
				}
				else
				{
					// NOTE: V8 requires imul to be coerced to int like normal functions
					stream << '(' << namegen.getBuiltinName(NameGenerator::Builtin::IMUL) << '(';
					compileOperand(indices[i] ,LOWEST);
					stream << ',' << targetData.getTypeAllocSize(curType->getSequentialElementType())<<')';
					curType = curType->getSequentialElementType();
					stream << "|0)";
				}
				stream << '+';
			}
		}
		p = u->getOperand(0);
		continue;
	}
	compileOperand(p, ADD_SUB);
}

int CheerpWriter::compileHeapForType(Type* et)
{
	uint32_t shift=0;
	if(et->isIntegerTy(8) || et->isIntegerTy(1))
	{
		stream << heapNames[HEAP8];
		shift = 0;
	}
	else if(et->isIntegerTy(16))
	{
		stream << heapNames[HEAP16];
		shift = 1;
	}
	else if(et->isIntegerTy(32) || et->isPointerTy())
	{
		stream << heapNames[HEAP32];
		shift = 2;
	}
	else if(et->isFloatTy())
	{
		stream << heapNames[HEAPF32];
		shift = 2;
	}
	else if(et->isDoubleTy())
	{
		stream << heapNames[HEAPF64];
		shift = 3;
	}
	else
	{
		llvm::errs() << "Unsupported heap access for  type " << *et << "\n";
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}
	return shift;
}
void CheerpWriter::compileHeapAccess(const Value* p, Type* t)
{
	if (!isa<PointerType>(p->getType()))
	{
		llvm::errs() << "not a pointer type:\n";
		p->dump();
		llvm::report_fatal_error("please report a bug");
		return;
	}
	PointerType* pt=cast<PointerType>(p->getType());
	Type* et = (t==nullptr) ? pt->getElementType() : t;
	uint32_t shift = compileHeapForType(et);
	stream << '[';
	compileRawPointer(p);
	stream << ">>" << shift;
	stream << ']';
}
void CheerpWriter::compilePointerBase(const Value* p, bool forEscapingPointer)
{
	// Collapse if p is a gepInst
	if(isGEP(p))
	{
		assert(!isa<Instruction>(p) || isInlineable(*cast<Instruction>(p), PA));
		const User* gepInst = cast<User>(p);
		assert(gepInst->getNumOperands() > 1);
		return compileGEPBase(gepInst, forEscapingPointer);
	}

	if(isBitCast(p) && (!isa<Instruction>(p) || isInlineable(*cast<Instruction>(p), PA) || forEscapingPointer))
	{
		compileBitCastBase(cast<User>(p), forEscapingPointer);
		return;
	}

	if(isa<ConstantPointerNull>(p))
	{
		stream << "nullArray";
		return;
	}

	if(PA.getPointerKind(p) == COMPLETE_OBJECT)
	{
		llvm::errs() << "compilePointerBase with COMPLETE_OBJECT pointer:" << *p << '\n' << "In function: " << *currentFun << '\n';
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}

	// Handle intrinsics
	if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(p))
	{
		switch(II->getIntrinsicID())
		{
			case Intrinsic::cheerp_upcast_collapsed:
			case Intrinsic::cheerp_cast_user:
				return compilePointerBase(II->getOperand(0));
			case Intrinsic::cheerp_make_regular:
				return compileCompleteObject(II->getOperand(0));
			default:
				break;
		}
	}

	if(isa<UndefValue>(p))
	{
		stream << "undefined";
		return;
	}

	if((isa<SelectInst> (p) && isInlineable(*cast<Instruction>(p), PA)) || (isa<ConstantExpr>(p) && cast<ConstantExpr>(p)->getOpcode() == Instruction::Select))
	{
		const User* u = cast<User>(p);
		stream << '(';
		compileOperand(u->getOperand(0), TERNARY, /*allowBooleanObjects*/ true);
		stream << '?';
		compilePointerBase(u->getOperand(1));
		stream << ':';
		compilePointerBase(u->getOperand(2));
		stream << ')';
		return;
	}

	if((!isa<Instruction>(p) || !isInlineable(*cast<Instruction>(p), PA)) && PA.getPointerKind(p) == SPLIT_REGULAR)
	{
		stream << namegen.getName(p);
		return;
	}

	// If value has not been generated from a GEP, just compile it and ask for .d
	compileOperand(p, HIGHEST);
	if(!PA.getConstantOffsetForPointer(p))
		stream << ".d";
}

const Value* CheerpWriter::compileByteLayoutOffset(const Value* p, BYTE_LAYOUT_OFFSET_MODE offsetMode)
{
	// If the value has byte layout skip GEPS and BitCasts until the base is found
	// We need to handle the first GEP having more than an index (so it actually changes types)
	// to support BYTE_LAYOUT_OFFSET_STOP_AT_ARRAY
	// If offsetMode is BYTE_LAYOUT_OFFSET_FULL we can treat every GEP in the same way
	bool findFirstTypeChangingGEP = (offsetMode != BYTE_LAYOUT_OFFSET_FULL);
	const Value* lastOffset = NULL;
	while ( isBitCast(p) || isGEP(p) )
	{
		const User * u = cast<User>(p);
		bool byteLayoutFromHere = PA.getPointerKind(u->getOperand(0)) != BYTE_LAYOUT;
		Type* curType = u->getOperand(0)->getType();
		if (isGEP(p))
		{
			bool skipUntilBytelayout = byteLayoutFromHere;
			SmallVector< const Value *, 8 > indices ( std::next(u->op_begin()), u->op_end() );
			for (uint32_t i=0;i<indices.size();i++)
			{
				if (StructType* ST = dyn_cast<StructType>(curType))
				{
					uint32_t index = cast<ConstantInt>( indices[i] )->getZExtValue();
					const StructLayout* SL = targetData.getStructLayout( ST );
					if (!skipUntilBytelayout && (offsetMode != BYTE_LAYOUT_OFFSET_NO_PRINT))
						stream << SL->getElementOffset(index) << '+';
					curType = ST->getElementType(index);
				}
				else
				{
					if (findFirstTypeChangingGEP && indices.size() > 1 && i == (indices.size() - 1))
					{
						assert (curType->isArrayTy());
						assert (offsetMode != BYTE_LAYOUT_OFFSET_FULL);
						// We have found an array just before the last type, the last offset will be returned instead of used directly.
						lastOffset = indices[i];
						break;
					}
					// This case also handles the first index
					if (!skipUntilBytelayout && (offsetMode != BYTE_LAYOUT_OFFSET_NO_PRINT))
					{
						compileOperand( indices[i], MUL_DIV );
						stream << '*' << targetData.getTypeAllocSize(curType->getSequentialElementType()) << '+';
					}
					curType = curType->getSequentialElementType();
				}
				if (skipUntilBytelayout && TypeSupport::hasByteLayout(curType))
					skipUntilBytelayout = false;
			}
			if (indices.size() > 1)
				findFirstTypeChangingGEP = false;
		}
		// In any case, close the summation here
		if(byteLayoutFromHere)
		{
			if(offsetMode != BYTE_LAYOUT_OFFSET_NO_PRINT)
				stream << '0';
			return lastOffset;
		}
		p = u->getOperand(0);
		continue;
	}
	assert (PA.getPointerKind(p) == BYTE_LAYOUT);
	if(offsetMode != BYTE_LAYOUT_OFFSET_NO_PRINT)
	{
		if(const ConstantInt* CI=PA.getConstantOffsetForPointer(p))
		{
			if(useMathImul)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::IMUL);
			stream << '(';
			compileConstant(CI);
			if(useMathImul)
				stream << ',';
			else
				stream << '*';
			stream << targetData.getTypeAllocSize(p->getType()->getPointerElementType()) << ')';
		}
		else
		{
			compileCompleteObject(p);
			stream << ".o";
		}
	}
	return lastOffset;
}

void CheerpWriter::compilePointerOffset(const Value* p, PARENT_PRIORITY parentPrio, bool forEscapingPointer)
{
	bool byteLayout = PA.getPointerKind(p) == BYTE_LAYOUT;
	if ( PA.getPointerKind(p) == COMPLETE_OBJECT && !isGEP(p) )
	{
		// This may still happen when doing ptrtoint of a function
		stream << '0';
	}
	// null must be handled first, even if it is bytelayout
	else if(isa<ConstantPointerNull>(p) || isa<UndefValue>(p))
	{
		stream << '0';
	}
	// byteLayout must be handled second, otherwise we may print a constant offset without the required byte multiplier
	else if ( byteLayout && !forEscapingPointer)
	{
		compileByteLayoutOffset(p, BYTE_LAYOUT_OFFSET_FULL);
	}
	else if(isGEP(p) && (!isa<Instruction>(p) || isInlineable(*cast<Instruction>(p), PA) || forEscapingPointer))
	{
		compileGEPOffset(cast<User>(p), parentPrio);
	}
	else if(isBitCast(p) && (!isa<Instruction>(p) || isInlineable(*cast<Instruction>(p), PA) || forEscapingPointer))
	{
		compileBitCastOffset(cast<User>(p), parentPrio);
	}
	else if (const ConstantInt* CI = PA.getConstantOffsetForPointer(p))
	{
		// Check if the offset has been constantized for this pointer
		compileConstant(CI);
	}
	else if(isa<Argument>(p))
	{
		stream << namegen.getSecondaryName(p);
	}
	else if((isa<SelectInst> (p) && isInlineable(*cast<Instruction>(p), PA)) || (isa<ConstantExpr>(p) && cast<ConstantExpr>(p)->getOpcode() == Instruction::Select))
	{
		const User* u = cast<User>(p);
		if(parentPrio >= TERNARY)
			stream << '(';
		compileOperand(u->getOperand(0), TERNARY, /*allowBooleanObjects*/ true);
		stream << '?';
		compilePointerOffset(u->getOperand(1), TERNARY);
		stream << ':';
		compilePointerOffset(u->getOperand(2), TERNARY);
		if(parentPrio >= TERNARY)
			stream << ')';
	}
	else if((!isa<Instruction>(p) || !isInlineable(*cast<Instruction>(p), PA)) && PA.getPointerKind(p) == SPLIT_REGULAR)
	{
		stream << namegen.getSecondaryName(p);
	}
	else if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(p))
	{
		// Handle intrinsics
		switch(II->getIntrinsicID())
		{
			case Intrinsic::cheerp_upcast_collapsed:
			case Intrinsic::cheerp_cast_user:
				compilePointerOffset(II->getOperand(0), parentPrio);
				return;
			case Intrinsic::cheerp_make_regular:
				compileOperand(II->getOperand(1), parentPrio);
				break;
			default:
				compileOperand(p,HIGHEST);
				stream << ".o";
		}
	} else {
		compileOperand(p, HIGHEST);
		stream << ".o";
	}
}

void CheerpWriter::compileConstantExpr(const ConstantExpr* ce)
{
	switch(ce->getOpcode())
	{
		case Instruction::GetElementPtr:
		{
			compileGEP(ce, PA.getPointerKind(ce));
			break;
		}
		case Instruction::BitCast:
		{
			POINTER_KIND k = PA.getPointerKind(ce);
			compileBitCast(ce, k);
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
			compileIntegerComparison(ce->getOperand(0), ce->getOperand(1), (CmpInst::Predicate)ce->getPredicate(), HIGHEST);
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
		default:
			stream << "undefined";
			llvm::errs() << "warning: Unsupported constant expr " << ce->getOpcodeName() << '\n';
	}
}

void CheerpWriter::compileConstantArrayMembers(const Constant* C)
{
	if(const ConstantArray* CA = dyn_cast<ConstantArray>(C))
	{
		Type* elementType = CA->getType()->getElementType();
		for(uint32_t i=0;i<CA->getNumOperands();i++)
		{
			if(i!=0)
				stream << ',';
			if(elementType->isPointerTy())
				compilePointerAs(CA->getOperand(i), PA.getPointerKindForStoredType(elementType));
			else
				compileOperand(CA->getOperand(i), LOWEST);
		}
	}
	else
	{
		const ConstantDataSequential* CD = dyn_cast<ConstantDataSequential>(C);
		assert(CD);
		for(uint32_t i=0;i<CD->getNumElements();i++)
		{
			if(i!=0)
				stream << ',';
			compileConstant(CD->getElementAsConstant(i));
		}
	}
}

bool CheerpWriter::doesConstantDependOnUndefined(const Constant* C) const
{
	if(isa<ConstantExpr>(C) && C->getOperand(0)->getType()->isPointerTy())
		return doesConstantDependOnUndefined(cast<Constant>(C->getOperand(0)));
	else if(isa<GlobalVariable>(C) && !compiledGVars.count(cast<GlobalVariable>(C)))
		return true;
	else
		return false;
}

void CheerpWriter::compileConstantAsBytes(const Constant* c, bool first, bool asmjs)
{
	if(const ConstantDataSequential* CD = dyn_cast<ConstantDataSequential>(c))
	{
		for(uint32_t i=0;i<CD->getNumElements();i++)
		{
			compileConstantAsBytes(CD->getElementAsConstant(i), first, asmjs);
			first = false;
		}
	}
	else if(const UndefValue* U = dyn_cast<UndefValue>(c))
	{
		uint32_t size = targetData.getTypeAllocSize(U->getType());
		for (uint32_t i = 0; i < size; i++)
		{
			if(i!=0 || !first)
				stream << ',';
			stream << '0';
		}
	}
	else if(isa<ConstantArray>(c) || isa<ConstantStruct>(c))
	{
		for(uint32_t i=0;i<c->getNumOperands();i++)
		{
			compileConstantAsBytes(cast<Constant>(c->getOperand(i)), first, asmjs);
			first = false;
		}
	}
	else if(const ConstantFP* f=dyn_cast<ConstantFP>(c))
	{
		const APFloat& flt = f->getValueAPF();
		const APInt& integerRepresentation = flt.bitcastToAPInt();
		uint64_t val = integerRepresentation.getLimitedValue();
		uint32_t bitWidth = integerRepresentation.getBitWidth();
		for(uint32_t i=0;i<bitWidth;i+=8)
		{
			if(i!=0 || !first)
				stream << ',';
			stream << ((val>>i)&255);
		}
	}
	else if(const ConstantInt* i=dyn_cast<ConstantInt>(c))
	{
		const APInt& integerRepresentation = i->getValue();
		uint64_t val = integerRepresentation.getLimitedValue();
		uint32_t bitWidth = integerRepresentation.getBitWidth();
		for(uint32_t i=0;i<bitWidth;i+=8)
		{
			if(i!=0 || !first)
				stream << ',';
			stream << ((val>>i)&255);
		}
	}
	else if (asmjs)
	{
		if(const ConstantAggregateZero* Z = dyn_cast<ConstantAggregateZero>(c))
		{
			uint32_t size = targetData.getTypeAllocSize(Z->getType());
			for (uint32_t i = 0; i < size; i++)
			{
				if(i!=0 || !first)
					stream << ',';
				stream << '0';
			}
		}
		else if(dyn_cast<ConstantPointerNull>(c))
		{
			if(!first)
				stream << ',';
			stream << "0,0,0,0";
		}
		else if(const Function* F = dyn_cast<Function>(c))
		{
			if (!globalDeps.functionAddresses().count(F))
			{
				llvm::errs() << "function not in table: "<<namegen.getName(F)<<"\n";
				llvm::report_fatal_error("please report a bug");
			}
			int32_t offset = globalDeps.functionAddresses().at(F) + functionAddrStart;
			for(uint32_t i=0;i<32;i+=8)
			{
				if(i!=0 || !first)
					stream << ',';
				stream << ((offset>>i)&255);
			}
		}
		else if(isa<ConstantExpr>(c))
		{
			const ConstantExpr* ce = cast<ConstantExpr>(c);
			switch(ce->getOpcode())
			{
				case Instruction::GetElementPtr:
				{
					assert(isa<GlobalVariable>(ce->getOperand(0)));
					const GlobalVariable* g = cast<GlobalVariable>(ce->getOperand(0));
					if (!gVarsAddr.count(g))
					{
						llvm::errs() << "global variable not found:" << namegen.getName(g) << "\n";
						llvm::report_fatal_error("please report a bug");
					}
					uint32_t addr = gVarsAddr[g];

					Type* curTy = g->getType();
					SmallVector< const Value *, 8 > indices ( std::next(ce->op_begin()), ce->op_end() );
					for (uint32_t i=0; i<indices.size(); i++)
					{
						uint32_t index = cast<ConstantInt>(indices[i])->getZExtValue();
						if (StructType* ST = dyn_cast<StructType>(curTy))
						{
							const StructLayout* SL = targetData.getStructLayout( ST );
							addr += SL->getElementOffset(index);
							curTy = ST->getElementType(index);
						}
						else
						{
							addr += index*targetData.getTypeAllocSize(curTy->getSequentialElementType());
							curTy = curTy->getSequentialElementType();
						}
					}
					for(uint32_t i=0;i<32;i+=8)
					{
						if(i!=0 || !first)
							stream << ',';
						stream << ((addr>>i)&255);
					}
					
					break;
				}
				case Instruction::IntToPtr:
				{
					const ConstantInt* ptr = cast<ConstantInt>(ce->getOperand(0));
					uint32_t val = ptr->getZExtValue();
					for(uint32_t i=0;i<32;i+=8)
					{
						if(i!=0 || !first)
							stream << ',';
						stream << ((val>>i)&255);
					}
					break;
				}
				case Instruction::BitCast:
				{
					compileConstantAsBytes(ce->getOperand(0),first, asmjs);
					break;
				}
				default:
					stream << "undefined";
					llvm::errs() << "warning: Unsupported constant expr in asm.js module :" << ce->getOpcodeName() << '\n';
			}
		}
		else if(isa<GlobalVariable>(c))
		{
			const GlobalVariable* g = cast<GlobalVariable>(c);
			if (gVarsAddr.count(g) != 1)
			{
				llvm::errs() << "global variable not found:" << namegen.getName(g) << "\n";
				llvm::report_fatal_error("please report a bug");
			}
			uint32_t val = gVarsAddr[g];
			for(uint32_t i=0;i<32;i+=8)
			{
				if(i!=0 || !first)
					stream << ',';
				stream << ((val>>i)&255);
			}
		}
		else
		{
			llvm::errs() << "Unsupported constant type for bytes in asm.js module :";
			c->getType()->dump();
			stream << "null";
		}
	}
	else
	{
		llvm::errs() << "Unsupported constant type for bytes ";
		c->dump();
		stream << "null";
	}
}

void CheerpWriter::compileConstant(const Constant* c, PARENT_PRIORITY parentPrio)
{
	//TODO: what to do when currentFun == nullptr? for now asmjs=false
	bool asmjs = false;
	if (currentFun)
	{
		asmjs = currentFun->getSection() == StringRef("asmjs");
	}
	if(!currentFun && doesConstantDependOnUndefined(c))
	{
		// If we are compiling a constant expr using a global variable which has
		// not been defined yet, just print undefined
		stream << "undefined";
	}
	else if(isa<ConstantExpr>(c))
	{
		compileConstantExpr(cast<ConstantExpr>(c));
	}
	else if(isa<ConstantDataSequential>(c))
	{
		const ConstantDataSequential* d=cast<ConstantDataSequential>(c);
		Type* t=d->getElementType();
		stream << "new ";
		compileTypedArrayType(t);
		stream << "([";
		compileConstantArrayMembers(d);
		stream << "])";
	}
	else if(isa<ConstantArray>(c))
	{
		const ConstantArray* d=cast<ConstantArray>(c);
		stream << '[';
		assert(d->getType()->getNumElements() == d->getNumOperands());
		compileConstantArrayMembers(d);
		stream << ']';
	}
	else if(isa<ConstantStruct>(c))
	{
		const ConstantStruct* d=cast<ConstantStruct>(c);
		if(cast<StructType>(c->getType())->hasByteLayout())
		{
			// Populate a DataView with a byte buffer
			stream << "new DataView(new Int8Array([";
			compileConstantAsBytes(c, true);
			stream << "]).buffer)";
			return;
		}
		stream << '{';
		assert(d->getType()->getNumElements() == d->getNumOperands());

		for(uint32_t i=0;i<d->getNumOperands();i++)
		{
			stream << types.getPrefixCharForMember(PA, d->getType(), i) << i << ':';
			bool useWrapperArray = types.useWrapperArrayForMember(PA, d->getType(), i);
			if (useWrapperArray)
				stream << '[';
			Type* elementType = d->getOperand(i)->getType();
			bool dependOnUndefined = !currentFun && doesConstantDependOnUndefined(d->getOperand(i));
			if(elementType->isPointerTy())
			{
				TypeAndIndex baseAndIndex(d->getType(), i, TypeAndIndex::STRUCT_MEMBER);
				POINTER_KIND k = PA.getPointerKindForMemberPointer(baseAndIndex);
				if((k==REGULAR || k==SPLIT_REGULAR) && PA.getConstantOffsetForMember(baseAndIndex))
				{
					if(dependOnUndefined)
						stream << "undefined";
					else
						compilePointerBase(d->getOperand(i));
				}
				else if(k == SPLIT_REGULAR)
				{
					if(dependOnUndefined)
						stream << "undefined";
					else
						compilePointerBase(d->getOperand(i));
					stream << ',';
					stream << types.getPrefixCharForMember(PA, d->getType(), i) << i << 'o';
					stream << ':';
					if(dependOnUndefined)
						stream << "undefined";
					else
						compilePointerOffset(d->getOperand(i), LOWEST);
				}
				else
				{
					if(dependOnUndefined)
						stream << "undefined";
					else
						compilePointerAs(d->getOperand(i), k);
				}
			}
			else if(dependOnUndefined)
				stream << "undefined";
			else
				compileOperand(d->getOperand(i), LOWEST);

			if (useWrapperArray)
				stream << ']';
			if((i+1)<d->getNumOperands())
				stream << ',';
		}

		stream << '}';
	}
	else if(isa<ConstantFP>(c))
	{
		const ConstantFP* f=cast<ConstantFP>(c);
		bool useFloat = false;
		
		if(parentPrio == HIGHEST && f->getValueAPF().isNegative())
			stream << '(';

		if(f->getValueAPF().isInfinity())
		{
			if(f->getValueAPF().isNegative())
				stream << '-';

			stream << "Infinity";
		}
		else if(f->getValueAPF().isNaN())
		{
			stream << "NaN";
		}
		else
		{
			APFloat apf = f->getValueAPF();
			// We want the most compact representation possible, so we first try
			// to represent the number with a maximum of nuumeric_limits::digits10.
			// We convert the string back to a double, and if it is not the same
			// as the original we try again with numeric_limits::max_digits10
			
			// Needed by APFloat::convert, not used here
			bool losesInfo = false;
			SmallString<32> buf;

			apf.convert(APFloat::IEEEdouble, APFloat::roundingMode::rmNearestTiesToEven, &losesInfo);
			assert(!losesInfo);
			double original = apf.convertToDouble();

			apf.toString(buf, std::numeric_limits<double>::digits10);
			double converted = 0;
			sscanf(buf.c_str(),"%lf",&converted);
			if(converted != original)
			{
				buf.clear();
				apf.toString(buf, std::numeric_limits<double>::max_digits10);
			}

			apf.convert(APFloat::IEEEsingle, APFloat::roundingMode::rmNearestTiesToEven, &losesInfo);
			// If we don't lose information or if the actual type is a float
			// (and thus we don't care that we are losing it), try to see if
			// it is shorter to use a float instead of a double (using fround)
			if(useMathFround && (!losesInfo || f->getType()->isFloatTy()))
			{
				float original = apf.convertToFloat();
				SmallString<32> tmpbuf;
				apf.toString(tmpbuf, std::numeric_limits<float>::digits10);
				float converted = 0;
				sscanf(tmpbuf.c_str(),"%f",&converted);
				if(converted != original)
				{
					tmpbuf.clear();
					apf.toString(tmpbuf, std::numeric_limits<float>::max_digits10);
				}
				// We actually use the float only if it is shorter to write,
				// including the call to fround
				size_t floatsize = tmpbuf.size() + namegen.getBuiltinName(NameGenerator::Builtin::FROUND).size()+2;  
				if(buf.size() > floatsize)
				{
					useFloat = true;
					// In asm.js double and float are distinct types, so
					// we cast back to double (we are never really using
					// floats for now)
					if(asmjs)
					{
						// if f is negative parenthesis are already added
						if (parentPrio == HIGHEST && !f->isNegative())
							stream << '(';
						stream << '+';
					}
					stream<< namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
					buf = tmpbuf;
				}
			}
			// asm.js require the floating point literals to have a dot
			if(asmjs && buf.find('.') == StringRef::npos)
			{
				auto it = buf.begin();
				// We must insert the dot before the exponent part
				// (or at the end if there is no exponent)
				for (;it != buf.end() && *it != 'E' && *it != 'e'; it++);
				buf.insert(it,'.');
			}
			// If the number is in the form `0.xyz...` we can remove the leading 0
			int start = 0;
			if (buf[0] == '0' && buf.size() > 2)
				start = 1;
			stream << buf.c_str()+start;
			if (useFloat)
				stream << ')';
		}
		if(parentPrio == HIGHEST && (f->getValueAPF().isNegative() | (asmjs && useFloat)))
			stream << ')';
	}
	else if(isa<ConstantInt>(c))
	{
		const ConstantInt* i=cast<ConstantInt>(c);
		if(parentPrio == HIGHEST && i->isNegative())
			stream << '(';
		if(i->getBitWidth()==1)
			stream << i->getZExtValue();
		else
			stream << i->getSExtValue();
		if(parentPrio == HIGHEST && i->isNegative())
			stream << ')';
	}
	else if(isa<ConstantPointerNull>(c))
	{
		if (asmjs)
			stream << '0';
		else if(PA.getPointerKind(c) == COMPLETE_OBJECT)
			stream << "null";
		else
			stream << "nullObj";
	}
	else if(isa<GlobalAlias>(c))
	{
		const GlobalAlias* a=cast<GlobalAlias>(c);
		compileConstant(a->getAliasee());
	}
	else if(isa<GlobalValue>(c))
	{
		assert(c->hasName());

		if(asmjs && isa<Function>(c))
		{
			if (globalDeps.functionAddresses().count(cast<Function>(c))) {
				int offset = globalDeps.functionAddresses().at(cast<Function>(c)) + functionAddrStart;
				stream << offset;
			} else {
				stream << '0';
			}
		}
		else if (asmjs && isa<GlobalVariable>(c) && gVarsAddr.count(cast<GlobalVariable>(c))==1 && !symbolicGlobalsAsmJS)
		{
			stream << gVarsAddr[cast<GlobalVariable>(c)];
		}
		else
			stream << namegen.getName(c);
	}
	else if(isa<ConstantAggregateZero>(c) || isa<UndefValue>(c))
	{
		if (asmjs && c->getType()->isPointerTy())
			stream << '0';
		else
			compileType(c->getType(), LITERAL_OBJ);
	}
	else
	{
		llvm::errs() << "Unsupported constant type ";
		c->dump();
		stream << "null";
	}
}

void CheerpWriter::compileOperand(const Value* v, PARENT_PRIORITY parentPrio, bool allowBooleanObjects)
{
	if(const Constant* c=dyn_cast<Constant>(v))
		compileConstant(c, parentPrio);
	else if(const Instruction* it=dyn_cast<Instruction>(v))
	{
		if(isInlineable(*it, PA))
		{
			bool isBooleanObject = false;
			if(it->getType()->isIntegerTy(1))
			{
				switch(it->getOpcode())
				{
					case Instruction::ICmp:
					case Instruction::FCmp:
					case Instruction::And:
					case Instruction::Or:
					case Instruction::Xor:
						isBooleanObject = true;
						break;
					default:
						break;
				}
			}
			PARENT_PRIORITY myPrio = parentPrio;
			if(isBooleanObject && !allowBooleanObjects)
			{
				myPrio = TERNARY;
				if (parentPrio >= TERNARY)
					stream << '(';
			}
			compileInlineableInstruction(*cast<Instruction>(v), myPrio);
			if(isBooleanObject && !allowBooleanObjects)
			{
				stream << "?1:0";
				if(parentPrio >= TERNARY)
					stream << ')';
			}
		}
		else
		{
			stream << namegen.getName(it);
		}
	}
	else if(const Argument* arg=dyn_cast<Argument>(v))
	{
		stream << namegen.getName(arg);
	}
	else if(const InlineAsm* a=dyn_cast<InlineAsm>(v))
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

bool CheerpWriter::needsPointerKindConversion(const Instruction* phi, const Value* incoming)
{
	Type* phiType=phi->getType();
	const Instruction* incomingInst=dyn_cast<Instruction>(incoming);
	if(!incomingInst)
		return true;
	POINTER_KIND incomingKind = UNKNOWN;
	POINTER_KIND phiKind = UNKNOWN;
	if(phiType->isPointerTy())
	{
		incomingKind = PA.getPointerKind(incoming);
		phiKind = PA.getPointerKind(phi);
	}
	return
		isInlineable(*incomingInst, PA) ||
		((incomingKind == SPLIT_REGULAR) != (phiKind == SPLIT_REGULAR)) ||
		registerize.getRegisterId(phi)!=registerize.getRegisterId(incomingInst) ||
		phiKind!=incomingKind ||
		PA.getConstantOffsetForPointer(phi)!=PA.getConstantOffsetForPointer(incoming);
}

bool CheerpWriter::needsPointerKindConversionForBlocks(const BasicBlock* to, const BasicBlock* from)
{
	class PHIHandler: public EndOfBlockPHIHandler
	{
	public:
		PHIHandler(CheerpWriter& w):EndOfBlockPHIHandler(w.PA),needsPointerKindConversion(false),writer(w)
		{
		}
		~PHIHandler()
		{
		}
		bool needsPointerKindConversion;
	private:
		CheerpWriter& writer;
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

void CheerpWriter::compilePHIOfBlockFromOtherBlock(const BasicBlock* to, const BasicBlock* from)
{
	class WriterPHIHandler: public EndOfBlockPHIHandler
	{
	public:
		WriterPHIHandler(CheerpWriter& w, const BasicBlock* f, const BasicBlock* t):EndOfBlockPHIHandler(w.PA),writer(w),fromBB(f),toBB(t)
		{
		}
		~WriterPHIHandler()
		{
		}
	private:
		CheerpWriter& writer;
		const BasicBlock* fromBB;
		const BasicBlock* toBB;
		void handleRecursivePHIDependency(const Instruction* incoming) override
		{
			assert(incoming);
			if(incoming->getType()->isPointerTy() && writer.PA.getPointerKind(incoming)==SPLIT_REGULAR && !writer.PA.getConstantOffsetForPointer(incoming))
			{
				writer.namegen.setEdgeContext(fromBB, toBB);
				writer.stream << writer.namegen.getSecondaryNameForEdge(incoming);
				writer.namegen.clearEdgeContext();
				writer.stream << '=';
				writer.compilePointerOffset(incoming, LOWEST);
				writer.stream << ';' << writer.NewLine;
			}
			writer.namegen.setEdgeContext(fromBB, toBB);
			writer.stream << writer.namegen.getNameForEdge(incoming);
			writer.namegen.clearEdgeContext();
			writer.stream << '=' << writer.namegen.getName(incoming) << ';' << writer.NewLine;
		}
		void handlePHI(const Instruction* phi, const Value* incoming) override
		{
			// We can avoid assignment from the same register if no pointer kind conversion is required
			if(!writer.needsPointerKindConversion(phi, incoming))
				return;
			Type* phiType=phi->getType();
			if(phiType->isPointerTy())
			{
				POINTER_KIND k=writer.PA.getPointerKind(phi);
				if((k==REGULAR || k==SPLIT_REGULAR) && writer.PA.getConstantOffsetForPointer(phi))
				{
					writer.stream << writer.namegen.getName(phi) << '=';
					writer.namegen.setEdgeContext(fromBB, toBB);
					writer.compilePointerBase(incoming);
				}
				else if(k==SPLIT_REGULAR)
				{
					writer.stream << writer.namegen.getSecondaryName(phi) << '=';
					writer.namegen.setEdgeContext(fromBB, toBB);
					writer.compilePointerOffset(incoming, LOWEST);
					writer.stream << ';' << writer.NewLine;
					writer.namegen.clearEdgeContext();
					writer.stream << writer.namegen.getName(phi) << '=';
					writer.namegen.setEdgeContext(fromBB, toBB);
					writer.compilePointerBase(incoming);
				}
				else
				{
					writer.stream << writer.namegen.getName(phi) << '=';
					writer.namegen.setEdgeContext(fromBB, toBB);
					if(k==REGULAR)
						writer.stream << "aSlot=";
					writer.compilePointerAs(incoming, k);
					if (phi->getParent()->getParent()->getSection() == StringRef("asmjs"))
						writer.stream << "|0";
				}
			}
			else
			{
				writer.stream << writer.namegen.getName(phi) << '=';
				writer.namegen.setEdgeContext(fromBB, toBB);
				writer.compileOperand(incoming, LOWEST);
			}
			writer.stream << ';' << writer.NewLine;
			writer.namegen.clearEdgeContext();
		}
	};
	WriterPHIHandler(*this, from, to).runOnEdge(registerize, from, to);
}

void CheerpWriter::compileMethodArgs(User::const_op_iterator it, User::const_op_iterator itE, ImmutableCallSite callV, bool forceBoolean)
{
	assert(callV.arg_begin() <= it && it <= callV.arg_end() && "compileMethodArgs, it out of range!");
	assert(callV.arg_begin() <= itE && itE <= callV.arg_end() && "compileMethodArgs, itE out of range!");
	assert(it <= itE);

	stream << '(';

	const Function* F = callV.getCalledFunction();
	bool asmjs = callV.getCaller()->getSection() == StringRef("asmjs");

	Function::const_arg_iterator arg_it;

	// Check if we have a direct call
	if(F && it != itE)
	{
		// Set arg_it to the argument relative to it.
		arg_it = F->arg_begin();
		unsigned argNo = callV.getArgumentNo(it);

		// Check if it is a variadic argument
		if(argNo < F->arg_size())
		{
			std::advance(arg_it, callV.getArgumentNo(it));
		}
		else
		{
			arg_it = F->arg_end();
		}
	}

	uint32_t opCount = 0;
	for(User::const_op_iterator cur=it; cur!=itE; ++cur, ++opCount)
	{
		if(cur!=it)
			stream << ',';

		Type* tp = (*cur)->getType();

		if (asmjs)
		{
			const PointerType* pTy = cast<PointerType>(callV.getCalledValue()->getType());
			const FunctionType* fTy = cast<FunctionType>(pTy->getElementType());
			PARENT_PRIORITY prio = LOWEST;
			if (tp->isPointerTy() || (tp->isIntegerTy() &&
				((F && globalDeps.asmJSImports().count(F)) ||
				(!F && !globalDeps.functionTables().count(fTy)))))
			{
				prio = BIT_OR;
			}
			compileOperand(*cur,prio);
			if (prio == BIT_OR)
				stream << "|0";
		}
		else if(tp->isPointerTy())
		{
			POINTER_KIND argKind = UNKNOWN;
			// Calling convention:
			// If this is a direct call and the argument is not a variadic one,
			// we pass the kind decided by getPointerKind(arg_it).
			// If it's variadic we use the base kind derived from the type
			// If it's indirect we use a kind good for any argument of a given type at a given position
			if (!F)
			{
				TypeAndIndex typeAndIndex(tp->getPointerElementType(), opCount, TypeAndIndex::ARGUMENT);
				argKind = PA.getPointerKindForArgumentTypeAndIndex(typeAndIndex);
			}
			else if (arg_it != F->arg_end())
				argKind = PA.getPointerKind(arg_it);
			else
			{
				if(isa<ConstantPointerNull>(*cur) && (cur+1)==itE && cur!=it)
				{
					// Special case for NULL which are the last variadic parameter, copy the previous type
					tp = (*(cur-1))->getType();
				}
				if(StructType* st = dyn_cast<StructType>(tp->getPointerElementType()))
				{
					while(st->getDirectBase())
						st = st->getDirectBase();
					tp = st->getPointerTo();
				}
				compilePointerAs(*cur, PA.getPointerKindForStoredType(tp));
			}

			assert(argKind != REGULAR);
			if(argKind == SPLIT_REGULAR)
			{
				compilePointerBase(*cur, true);
				stream << ',';
				compilePointerOffset(*cur, LOWEST, true);
			}
			else if(argKind != UNKNOWN)
				compilePointerAs(*cur, argKind);
		}
		else if(tp->isIntegerTy(1) && forceBoolean)
		{
			stream << "!!";
			compileOperand(*cur, HIGHEST);
		}
		else
			compileOperand(*cur,LOWEST);

		if(F && arg_it != F->arg_end())
		{
			++arg_it;
		}
	}
	stream << ')';
}

/*
 * This method is fragile, each opcode must handle the phis in the correct place
 */
CheerpWriter::COMPILE_INSTRUCTION_FEEDBACK CheerpWriter::compileTerminatorInstruction(const TerminatorInst& I)
{
	bool asmjs = currentFun->getSection() == StringRef("asmjs");
	switch(I.getOpcode())
	{
		case Instruction::Ret:
		{
			const ReturnInst& ri = cast<ReturnInst>(I);
			assert(I.getNumSuccessors()==0);
			Value* retVal = ri.getReturnValue();

			if (asmjs)
			{
				compileStackRet();
				// NOTE: This is needed because we are adding an extra return at the end of
				//       the function, and the asm.js validator does not like if it is never
				//       reachable. In this way we trick it.
				stream << "if (1) ";
			}
			if(retVal)
			{
				if(!asmjs && retVal->getType()->isPointerTy())
				{
					POINTER_KIND k=PA.getPointerKindForReturn(ri.getParent()->getParent());
					// For SPLIT_REGULAR we return the .d part and store the .o part into oSlot
					if(k==SPLIT_REGULAR)
					{
						stream << "oSlot=";
						compilePointerOffset(retVal, LOWEST);
						stream << ';' << NewLine;
					}
					stream << "return ";
					assert(k != REGULAR);
					if(k==SPLIT_REGULAR)
						compilePointerBase(retVal);
					else
						compilePointerAs(retVal, k);
				}
				else
				{
					stream << "return ";
					PARENT_PRIORITY prio = LOWEST;
					int width = needsIntCoercion(retVal, prio, &prio);
					compileOperand(retVal, prio);
					if(prio == BIT_OR)
						stream << "|0";
					else if(prio == BIT_AND)
						stream << "&" << getMaskForBitWidth(width);
				}
			}
			else
				stream << "return";

			stream << ';' << NewLine;
			return COMPILE_OK;
		}
		case Instruction::Invoke:
		{
			const InvokeInst& ci = cast<InvokeInst>(I);

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
					stream << namegen.getName(ci.getCalledFunction());
			}
			else
			{
				//Indirect call
				compileOperand(ci.getCalledValue());
			}

			compileMethodArgs(ci.op_begin(),ci.op_begin()+ci.getNumArgOperands(),&ci, /*forceBoolean*/ false);
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

CheerpWriter::COMPILE_INSTRUCTION_FEEDBACK CheerpWriter::compileNotInlineableInstruction(const Instruction& I, PARENT_PRIORITY parentPrio)
{
	bool asmjs = currentFun->getSection() == StringRef("asmjs");
	switch(I.getOpcode())
	{
		case Instruction::Alloca:
		{
			const AllocaInst* ai = cast<AllocaInst>(&I);
			if (asmjs)
			{
				Type* allocTy = ai->getAllocatedType();
				uint32_t size = targetData.getTypeAllocSize(allocTy);
				uint32_t alignment = TypeSupport::getAlignmentAsmJS(targetData, allocTy);
				const Value* n = nullptr;
				if (ai->isArrayAllocation())
					n = ai->getArraySize();
				assert((alignment & (alignment-1)) == 0 && "alignment must be power of 2");
				compileAllocaAsmJS(n,size,alignment);
				return COMPILE_OK;
			}
			//V8: If the variable is passed to a call make sure that V8 does not try to SROA it
			//This can be a problem if this function or one of the called ones is deoptimized,
			//as the SROA-ed object will then be materialied with a pessimized hidden type map
			//which will then be used for all the objects with the same structure
			stream << "aSlot=";
			POINTER_KIND k = PA.getPointerKind(ai);

			StringRef varName = namegen.getName(&I);
			if(k == REGULAR)
			{
				stream << "{d:[";
				compileType(ai->getAllocatedType(), LITERAL_OBJ, varName);
				stream << "],o:0}";
			}
			else if(k == SPLIT_REGULAR)
			{
				stream << '[';
				compileType(ai->getAllocatedType(), LITERAL_OBJ, varName);
				stream << ']';
				stream << ';' << NewLine;
				stream << namegen.getSecondaryName(ai) << "=0";
			}
			else if(k == BYTE_LAYOUT)
			{
				stream << "{d:";
				compileType(ai->getAllocatedType(), LITERAL_OBJ, varName);
				stream << ",o:0}";
			}
			else 
				compileType(ai->getAllocatedType(), LITERAL_OBJ, varName);

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
			const InsertValueInst& ivi = cast<InsertValueInst>(I);
			const Value* aggr=ivi.getAggregateOperand();
			StructType* t=dyn_cast<StructType>(aggr->getType());
			assert(ivi.getIndices().size()==1);
			if(!t)
			{
				llvm::errs() << "insertvalue: Expected struct, found " << *t << "\n";
				llvm::report_fatal_error("Unsupported code found, please report a bug", false);
				return COMPILE_UNSUPPORTED;
			}
			uint32_t offset=ivi.getIndices()[0];
			if(isa<UndefValue>(aggr))
			{
				// We have to assemble the type object from scratch
				stream << '{';
				for(unsigned int i=0;i<t->getNumElements();i++)
				{
					assert(!t->getElementType(i)->isStructTy());
					assert(!t->getElementType(i)->isArrayTy());
					char memberPrefix = types.getPrefixCharForMember(PA, t, i);
					bool useWrapperArray = types.useWrapperArrayForMember(PA, t, i);
					if(i!=0)
						stream << ',';
					stream << memberPrefix << i << ':';
					if(useWrapperArray)
						stream << '[';
					if(offset == i)
						compileOperand(ivi.getInsertedValueOperand(), LOWEST);
					else
						compileType(t->getElementType(i), LITERAL_OBJ);
					if(useWrapperArray)
						stream << ']';
				}
				stream << '}';
			}
			else
			{
				// We have to make a copy with a field of a different value
				stream << '{';
				for(unsigned int i=0;i<t->getNumElements();i++)
				{
					assert(!t->getElementType(i)->isStructTy());
					assert(!t->getElementType(i)->isArrayTy());
					char memberPrefix = types.getPrefixCharForMember(PA, t, i);
					bool useWrapperArray = types.useWrapperArrayForMember(PA, t, i);
					if(i!=0)
						stream << ',';
					stream << memberPrefix << i << ':';
					if(useWrapperArray)
						stream << '[';
					if(offset == i)
						compileOperand(ivi.getInsertedValueOperand(), LOWEST);
					else
					{
						stream << namegen.getName(aggr);
						stream << '.' << memberPrefix << i;
						if(useWrapperArray)
							stream << "[0]";
					}
					if(useWrapperArray)
						stream << ']';
				}
				stream << '}';
			}
			return COMPILE_OK;
		}
		case Instruction::Store:
		{
			const StoreInst& si = cast<StoreInst>(I);
			const Value* ptrOp=si.getPointerOperand();
			const Value* valOp=si.getValueOperand();
			POINTER_KIND kind = PA.getPointerKind(ptrOp);
			if (checkBounds && (kind == REGULAR || kind == SPLIT_REGULAR))
			{
				compileCheckBounds(ptrOp);
				stream<<";";
			}
			if (checkDefined && kind == COMPLETE_OBJECT && isGEP(ptrOp))
			{
				compileCheckDefined(ptrOp);
				stream<<";";
			}
			if (kind == BYTE_LAYOUT)
			{
				//Optimize stores of single values from unions
				compilePointerBase(ptrOp);
				Type* pointedType=ptrOp->getType()->getPointerElementType();
				if(pointedType->isIntegerTy(8))
					stream << ".setInt8(";
				else if(pointedType->isIntegerTy(16))
					stream << ".setInt16(";
				else if(pointedType->isIntegerTy(32))
					stream << ".setInt32(";
				else if(pointedType->isFloatTy())
					stream << ".setFloat32(";
				else if(pointedType->isDoubleTy())
					stream << ".setFloat64(";
				compilePointerOffset(ptrOp, LOWEST);
				stream << ',';

				//Special case compilation of operand, the default behavior use =
				compileOperand(valOp, LOWEST);
				if(!pointedType->isIntegerTy(8))
					stream << ",true";
				stream << ')';
				return COMPILE_OK;
			}
			else if (kind == RAW)
			{
				compileHeapAccess(ptrOp);
			}
			else
			{
				compileCompleteObject(ptrOp);
			}

			stream << '=';
			if(!asmjs && valOp->getType()->isPointerTy())
			{
				POINTER_KIND storedKind = PA.getPointerKind(&si);
				// If regular see if we can omit the offset part
				if((storedKind==SPLIT_REGULAR || storedKind==REGULAR || storedKind==BYTE_LAYOUT) && PA.getConstantOffsetForPointer(&si))
					compilePointerBase(valOp);
				else if(storedKind==SPLIT_REGULAR)
				{
					compilePointerBase(valOp);
					stream << ';' << NewLine;
					compileCompleteObject(ptrOp);
					stream << "o=";
					compilePointerOffset(valOp, LOWEST);
				}
				else
					compilePointerAs(valOp, storedKind);
			}
			else
				compileOperand(valOp, LOWEST);
			return COMPILE_OK;
		}
		default:
		{
			bool convertBoolean = false;
			if(!asmjs && I.getType()->isIntegerTy(1))
			{
				switch(I.getOpcode())
				{
					case Instruction::ICmp:
					case Instruction::FCmp:
					case Instruction::And:
					case Instruction::Or:
					case Instruction::Xor:
						convertBoolean = true;
						break;
					default:
						break;
				}
			}
			COMPILE_INSTRUCTION_FEEDBACK ret=compileInlineableInstruction(I, convertBoolean?TERNARY:parentPrio);
			if (ret == COMPILE_OK && convertBoolean)
				stream << "?1:0";
			return ret;
		}
	}
}

void CheerpWriter::compileGEPBase(const llvm::User* gep_inst, bool forEscapingPointer)
{
	SmallVector< const Value*, 8 > indices(std::next(gep_inst->op_begin()), gep_inst->op_end());
	Type* basePointerType = gep_inst->getOperand(0)->getType();
	Type* targetType = gep_inst->getType()->getPointerElementType();

	StructType* containerStructType = getGEPContainerStructType(gep_inst);
	bool useDownCastArray = false;
	if(containerStructType && indices.size() > 1)
	{
		assert(isa<ConstantInt>(indices.back()));
		const ConstantInt* idx = cast<ConstantInt>(indices.back());
		uint32_t lastOffsetConstant = idx->getZExtValue();
		useDownCastArray = !types.useWrapperArrayForMember(PA, containerStructType, lastOffsetConstant);
	}
	bool byteLayout = PA.getPointerKind(gep_inst) == BYTE_LAYOUT;
	if (byteLayout)
	{
		const Value* baseOperand = gep_inst->getOperand(0);
		bool byteLayoutFromHere = PA.getPointerKind(baseOperand) != BYTE_LAYOUT;
		if (byteLayoutFromHere)
			compileCompleteObject(gep_inst);
		else if (!TypeSupport::hasByteLayout(targetType) && forEscapingPointer)
		{
			assert(TypeSupport::isTypedArrayType(targetType, /* forceTypedArray*/ true));
			// Forge an appropiate typed array
			assert (!TypeSupport::hasByteLayout(targetType));
			stream << "new ";
			compileTypedArrayType(targetType);
			stream << '(';
			compilePointerBase( baseOperand );
			stream << ".buffer,";
			// If this GEP or a previous one passed through an array of immutables generate a regular from
			// the start of the array and not from the pointed element
			compileByteLayoutOffset( gep_inst, BYTE_LAYOUT_OFFSET_STOP_AT_ARRAY );
			stream << ')';
		}
		else
			compilePointerBase( baseOperand );
	}
	else if (indices.size() == 1)
	{
		// Just another pointer from this one
		compilePointerBase(gep_inst->getOperand(0));
	}
	else
	{
		// HACK: Accessing members of NULL is invalid, but it is used to compute an offset from the start of a structure
		// TODO: We need to detect and block this on the clang side. In the mean time, just compile an invalid null access
		if( isa<ConstantPointerNull>(gep_inst->getOperand(0)) )
		{
			stream << "null[0]";
			return;
		}
		compileCompleteObject(gep_inst->getOperand(0), indices.front());
		Type* basePointedType = basePointerType->getPointerElementType();
		if (useDownCastArray)
		{
			compileAccessToElement(basePointedType, makeArrayRef(std::next(indices.begin()),indices.end()), /*compileLastWrapperArray*/true);
			stream << ".a";
		}
		else if(containerStructType)
		{
			compileAccessToElement(basePointedType, makeArrayRef(std::next(indices.begin()),indices.end()), /*compileLastWrapperArray*/false);
		}
		else
		{
			compileAccessToElement(basePointedType, makeArrayRef(std::next(indices.begin()),std::prev(indices.end())), /*compileLastWrapperArray*/true);
		}
	}
}

void CheerpWriter::compileGEPOffset(const llvm::User* gep_inst, PARENT_PRIORITY parentPrio)
{
	SmallVector< const Value*, 8 > indices(std::next(gep_inst->op_begin()), gep_inst->op_end());
	Type* basePointerType = gep_inst->getOperand(0)->getType();
	Type* targetType = gep_inst->getType()->getPointerElementType();

	StructType* containerStructType = getGEPContainerStructType(gep_inst);
	bool useDownCastArray = false;
	if(containerStructType && indices.size() > 1)
	{
		assert(isa<ConstantInt>(indices.back()));
		const ConstantInt* idx = cast<ConstantInt>(indices.back());
		uint32_t lastOffsetConstant = idx->getZExtValue();
		useDownCastArray = !types.useWrapperArrayForMember(PA, containerStructType, lastOffsetConstant);
	}

	bool byteLayout = PA.getPointerKind(gep_inst) == BYTE_LAYOUT;
	if (byteLayout)
	{
		if (TypeSupport::hasByteLayout(targetType))
			compilePointerOffset( gep_inst, HIGHEST );
		else
		{
			assert(TypeSupport::isTypedArrayType(targetType, /* forceTypedArray*/ true));
			// If this GEP or a previous one passed through an array of immutables generate a regular from
			// the start of the array and not from the pointed element
			const Value* lastOffset = compileByteLayoutOffset( gep_inst, BYTE_LAYOUT_OFFSET_NO_PRINT );
			if (lastOffset)
				compileOperand(lastOffset);
			else
				stream << '0';
		}
	}
	else if (indices.size() == 1)
	{
		bool isOffsetConstantZero = isa<Constant>(indices.front()) && cast<Constant>(indices.front())->isNullValue();
		PARENT_PRIORITY prio = parentPrio;

		// Just another pointer from this one
		if (!isOffsetConstantZero)
			prio = ADD_SUB;
		compilePointerOffset(gep_inst->getOperand(0), prio);

		if(!isOffsetConstantZero)
		{
			stream << '+';
			compileOperand(indices.front(), prio);
		}
	}
	else
	{
		if (useDownCastArray)
		{
			Type* basePointedType = basePointerType->getPointerElementType();
			compileCompleteObject(gep_inst->getOperand(0), indices.front());
			compileAccessToElement(basePointedType, makeArrayRef(std::next(indices.begin()), indices.end()), /*compileLastWrapperArray*/true);
			stream << ".o";
		}
		else
			compileOffsetForGEP(gep_inst->getOperand(0)->getType(), indices);
	}
}

void CheerpWriter::compileGEP(const llvm::User* gep_inst, POINTER_KIND kind)
{
	SmallVector< const Value*, 8 > indices(std::next(gep_inst->op_begin()), gep_inst->op_end());
	Type* basePointerType = gep_inst->getOperand(0)->getType();

	StructType* containerStructType = dyn_cast<StructType>(GetElementPtrInst::getIndexedType(basePointerType,
			makeArrayRef(const_cast<Value* const*>(indices.begin()),
				     const_cast<Value* const*>(indices.end() - 1))));
	if(containerStructType && indices.size() > 1)
	{
		assert(isa<ConstantInt>(indices.back()));
	}


	if(COMPLETE_OBJECT == kind)
	{
		compileCompleteObject(gep_inst->getOperand(0), indices.front());
		compileAccessToElement(gep_inst->getOperand(0)->getType()->getPointerElementType(),
		                       makeArrayRef(std::next(indices.begin()), indices.end()), /*compileLastWrapperArray*/true);
	}
	else if (RAW == kind)
	{
		stream << '(';
		compileRawPointer(gep_inst);
		stream << "|0)";
	}
	else
	{
		if (PA.getConstantOffsetForPointer(gep_inst))
		{
			Type* basePointedType = basePointerType->getPointerElementType();
			compileCompleteObject(gep_inst->getOperand(0), indices.front());
			compileAccessToElement(basePointedType, makeArrayRef(std::next(indices.begin()),std::prev(indices.end())), /*compileLastWrapperArray*/false);
			return;
		}

		stream << "{d:";
		compilePointerBase( gep_inst, true);
		stream << ",o:";
		compilePointerOffset( gep_inst, LOWEST, true);
		stream << '}';
	}
}

void CheerpWriter::compileSignedInteger(const llvm::Value* v, bool forComparison, PARENT_PRIORITY parentPrio)
{
	//We anyway have to use 32 bits for sign extension to work
	uint32_t shiftAmount = 32-v->getType()->getIntegerBitWidth();
	if(const ConstantInt* C = dyn_cast<ConstantInt>(v))
	{
		if(forComparison)
			stream << (C->getSExtValue() << shiftAmount);
		else
			stream << C->getSExtValue();
		return;
	}
	PARENT_PRIORITY signedPrio = shiftAmount == 0 ? BIT_OR : SHIFT;
	if(parentPrio > signedPrio) stream << '(';
	if(shiftAmount==0)
	{
		//Use simpler code
		compileOperand(v, BIT_OR);
		stream << "|0";
	}
	else if(forComparison)
	{
		// When comparing two signed values we can avoid the right shift
		compileOperand(v, SHIFT);
		stream << "<<" << shiftAmount;
	}
	else
	{
		compileOperand(v, SHIFT);
		stream << "<<" << shiftAmount << ">>" << shiftAmount;
	}
	if(parentPrio > signedPrio) stream << ')';
}

void CheerpWriter::compileUnsignedInteger(const llvm::Value* v, PARENT_PRIORITY parentPrio)
{
	if(const ConstantInt* C = dyn_cast<ConstantInt>(v))
	{
		stream << C->getZExtValue();
		return;
	}
	//We anyway have to use 32 bits for sign extension to work
	uint32_t initialSize = v->getType()->getIntegerBitWidth();
	if(initialSize == 32)
	{
		if(parentPrio > SHIFT) stream << '(';
		//Use simpler code
		compileOperand(v, SHIFT);
		stream << ">>>0";
		if(parentPrio > SHIFT) stream << ')';
	}
	else
	{
		if(parentPrio > BIT_AND) stream << '(';
		compileOperand(v, BIT_AND);
		stream << '&' << getMaskForBitWidth(initialSize);
		if(parentPrio > BIT_AND) stream << ')';
	}
}

/*
 * This can be used for both named instructions and inlined ones
 * NOTE: Call, Ret, Invoke are NEVER inlined
 */
CheerpWriter::COMPILE_INSTRUCTION_FEEDBACK CheerpWriter::compileInlineableInstruction(const Instruction& I, PARENT_PRIORITY parentPrio)
{
	bool asmjs = currentFun->getSection() == StringRef("asmjs");
	switch(I.getOpcode())
	{
		case Instruction::BitCast:
		{
			POINTER_KIND k=PA.getPointerKind(&I);
			compileBitCast(&I, k);
			return COMPILE_OK;
		}
		case Instruction::FPToSI:
		{
			const CastInst& ci = cast<CastInst>(I);
			stream << "~~";
			compileOperand(ci.getOperand(0), HIGHEST);
			return COMPILE_OK;
		}
		case Instruction::FPToUI:
		{
			// TODO: When we will keep track of signedness to avoid useless casts we will need to fix this
			const CastInst& ci = cast<CastInst>(I);
			//Cast to signed anyway
			stream << "~~";
			compileOperand(ci.getOperand(0), HIGHEST);
			return COMPILE_OK;
		}
		case Instruction::SIToFP:
		{
			const CastInst& ci = cast<CastInst>(I);
			stream << "(+";
			compileSignedInteger(ci.getOperand(0), /*forComparison*/ false, HIGHEST);
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::UIToFP:
		{
			const CastInst& ci = cast<CastInst>(I);
			//We need to cast to unsigned before
			stream << "(+";
			compileUnsignedInteger(ci.getOperand(0), HIGHEST);
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::GetElementPtr:
		{
			const GetElementPtrInst& gep = cast<GetElementPtrInst>(I);
			Type* t=gep.getOperand(0)->getType();
			assert(t->isPointerTy());
			PointerType* ptrT=cast<PointerType>(t);

			if(TypeSupport::isClientType(ptrT->getElementType()))
			{
				//Client objects are just passed through
				compileOperand(gep.getOperand(0), parentPrio);
			}
			else
			{
				compileGEP(&gep, PA.getPointerKind(&gep));
			}
			return COMPILE_OK;
		}
		case Instruction::Add:
		{
			//Integer addition
			PARENT_PRIORITY addPrio = ADD_SUB;
			int width = needsIntCoercion(&I, parentPrio, &addPrio);
			if(parentPrio > addPrio) stream << '(';
			compileOperand(I.getOperand(0), ADD_SUB);
			stream << "+";
			compileOperand(I.getOperand(1), ADD_SUB);
			if(addPrio == BIT_OR)
				stream << "|0";
			else if (addPrio == BIT_AND)
				stream << '&' << getMaskForBitWidth(width);
			if(parentPrio > addPrio) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::FAdd:
		{
			//Double addition
			if(parentPrio > ADD_SUB) stream << '(';
			compileOperand(I.getOperand(0), ADD_SUB);
			stream << '+';
			//TODO: the HIGHEST for asmjs is needed in order to avoid `++`
			// when the operand is a call to fround but has type double
			PARENT_PRIORITY myPrio = asmjs?HIGHEST:nextPrio(ADD_SUB);
			compileOperand(I.getOperand(1), myPrio);
			if(parentPrio > ADD_SUB) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Sub:
		{
			compileSubtraction(I.getOperand(0), I.getOperand(1), parentPrio);
			return COMPILE_OK;
		}
		case Instruction::FSub:
		{
			//Double subtraction
			//TODO: optimize negation
			if(parentPrio > ADD_SUB) stream << '(';
			compileOperand(I.getOperand(0), ADD_SUB);
			stream << '-';
			// TODO: to avoid `--` for now we set HIGHEST priority, and
			// compileConstant adds parenthesis if the constant is negative
			compileOperand(I.getOperand(1), HIGHEST);
			if(parentPrio > ADD_SUB) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::ZExt:
		{
			const ZExtInst& bi = cast<ZExtInst>(I);
			Type* src=bi.getSrcTy();
#ifndef NDEBUG
			Type* dst=bi.getDestTy();
#endif
			assert(src->isIntegerTy() && dst->isIntegerTy());
			if(src->isIntegerTy(1))
			{
				//If the source type is i1, attempt casting from Boolean
				if(parentPrio >= TERNARY) stream << '(';
				compileOperand(bi.getOperand(0), TERNARY);
				stream << "?1:0";
				if(parentPrio >= TERNARY) stream << ')';
			}
			else
			{
				//Let's mask out upper bits, to make sure we get zero extension
				//The value might have been initialized with a negative value
				compileUnsignedInteger(I.getOperand(0), HIGHEST);
			}
			return COMPILE_OK;
		}
		case Instruction::SDiv:
		{
			//Integer signed division
			PARENT_PRIORITY sdivPrio = MUL_DIV;
			int width = needsIntCoercion(&I, parentPrio, &sdivPrio);
			if(parentPrio > sdivPrio) stream << '(';
			compileSignedInteger(I.getOperand(0), /*forComparison*/ false, MUL_DIV);
			stream << '/';
			compileSignedInteger(I.getOperand(1), /*forComparison*/ false, MUL_DIV);
			if(sdivPrio == BIT_OR)
				stream << "|0";
			else if(sdivPrio == BIT_AND)
				stream << "&" << getMaskForBitWidth(width);
			if(parentPrio > sdivPrio) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::UDiv:
		{
			//Integer unsigned division
			PARENT_PRIORITY udivPrio = MUL_DIV;
			needsIntCoercion(&I, parentPrio, &udivPrio);
			// The result is guaranteed to be represented with the correct
			// number of bits, so we avoid the mask
			if(udivPrio == BIT_AND)
				udivPrio = BIT_OR;
			if(parentPrio > udivPrio) stream << '(';
			compileUnsignedInteger(I.getOperand(0), MUL_DIV);
			stream << '/';
			compileUnsignedInteger(I.getOperand(1), MUL_DIV);
			if(udivPrio == BIT_OR)
				stream << "|0";
			if(parentPrio > udivPrio) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::SRem:
		{
			//Integer signed remainder
			PARENT_PRIORITY sremPrio = MUL_DIV;
			int width = needsIntCoercion(&I, parentPrio, &sremPrio);
			if(parentPrio > sremPrio) stream << '(';
			compileSignedInteger(I.getOperand(0), /*forComparison*/ false, MUL_DIV);
			stream << '%';
			compileSignedInteger(I.getOperand(1), /*forComparison*/ false, MUL_DIV);
			if(sremPrio == BIT_OR)
				stream << "|0";
			else if(sremPrio == BIT_AND)
				stream << "&" << getMaskForBitWidth(width);
			if(parentPrio > sremPrio) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::URem:
		{
			//Integer unsigned remainder
			PARENT_PRIORITY uremPrio = MUL_DIV;
			needsIntCoercion(&I, parentPrio, &uremPrio);
			// The result is guaranteed to be represented with the correct
			// number of bits, so we avoid the mask
			if(uremPrio == BIT_AND)
				uremPrio = BIT_OR;
			if(parentPrio > uremPrio) stream << '(';
			compileUnsignedInteger(I.getOperand(0), MUL_DIV);
			stream << '%';
			compileUnsignedInteger(I.getOperand(1), MUL_DIV);
			if(uremPrio == BIT_OR)
				stream << "|0";
			if(parentPrio > uremPrio) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::FDiv:
		{
			//Double division
			if(parentPrio > MUL_DIV) stream << '(';
			compileOperand(I.getOperand(0), MUL_DIV);
			stream << '/';
			compileOperand(I.getOperand(1), nextPrio(MUL_DIV));
			if(parentPrio > MUL_DIV) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::FRem:
		{
			//Double division
			if(parentPrio > MUL_DIV) stream << '(';
			compileOperand(I.getOperand(0), MUL_DIV);
			stream << '%';
			compileOperand(I.getOperand(1), nextPrio(MUL_DIV));
			if(parentPrio > MUL_DIV) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Mul:
		{
			//Integer signed multiplication
			PARENT_PRIORITY mulPrio = MUL_DIV;
			int width = needsIntCoercion(&I, parentPrio, &mulPrio);
			// NOTE: V8 requires imul to be coerced to int like normal functions
			if(asmjs)
			{
				mulPrio = BIT_OR;
			}
			if(parentPrio > mulPrio) stream << '(';
			if(useMathImul || asmjs)
			{
				stream << namegen.getBuiltinName(NameGenerator::Builtin::IMUL) << '(';
				compileOperand(I.getOperand(0), LOWEST);
				stream << ',';
				compileOperand(I.getOperand(1), LOWEST);
				stream << ')';
			}
			else
			{
				compileOperand(I.getOperand(0), MUL_DIV);
				stream << '*';
				compileOperand(I.getOperand(1), MUL_DIV);
			}
			if(mulPrio == BIT_OR)
				stream << "|0";
			else if(mulPrio == BIT_AND)
				stream << '&' << getMaskForBitWidth(width);
			if(parentPrio > mulPrio) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::FMul:
		{
			//Double multiplication
			if(parentPrio > MUL_DIV) stream << '(';
			compileOperand(I.getOperand(0), MUL_DIV);
			stream << '*';
			compileOperand(I.getOperand(1), nextPrio(MUL_DIV));
			if(parentPrio > MUL_DIV) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::ICmp:
		{
			//Integer comparison
			const CmpInst& ci = cast<CmpInst>(I);
			compileIntegerComparison(ci.getOperand(0), ci.getOperand(1), ci.getPredicate(), parentPrio);
			return COMPILE_OK;
		}
		case Instruction::FCmp:
		{
			//Integer comparison
			const CmpInst& ci = cast<CmpInst>(I);
			//Check that the operation is JS safe
			//Special case orderedness check
			if(ci.getPredicate()==CmpInst::FCMP_ORD)
			{
				if(parentPrio > LOGICAL_AND) stream << '(';
				stream << '!';
				if (asmjs)
					stream << '(';
				stream << "isNaN(";
				compileOperand(ci.getOperand(0),LOWEST);
				stream << ')';
				if (asmjs)
					stream << "|0)&!(";
				else
					stream << "&&!";
				stream << "isNaN(";
				compileOperand(ci.getOperand(1), LOWEST);
				stream << ')';
				if (asmjs)
					stream << "|0)";
				if(parentPrio > LOGICAL_AND) stream << ')';
			}
			else if(ci.getPredicate()==CmpInst::FCMP_UNO)
			{
				if(parentPrio > LOGICAL_OR) stream << '(';
				if (asmjs)
					stream << '(';
				stream << "isNaN(";
				compileOperand(ci.getOperand(0), LOWEST);
				stream << ')';
				if (asmjs)
					stream <<"|0)|(";
				else
					stream << "||";
				stream << "isNaN(";
				compileOperand(ci.getOperand(1), LOWEST);
				stream << ')';
				if (asmjs)
					stream << "|0)";
				if(parentPrio > LOGICAL_OR) stream << ')';
			}
			else
			{
				if(parentPrio > COMPARISON) stream << '(';
				compileOperand(ci.getOperand(0), COMPARISON);
				compilePredicate(ci.getPredicate());
				compileOperand(ci.getOperand(1), COMPARISON);
				if(parentPrio > COMPARISON) stream << ')';
			}
			return COMPILE_OK;
		}
		case Instruction::And:
		{
			//Integer logical and
			//No need to apply the >> operator. The result is an integer by spec
			PARENT_PRIORITY andPrio = (I.getType()->isIntegerTy(1)&&!asmjs) ? LOGICAL_AND : BIT_AND;
			if(parentPrio > andPrio) stream << '(';
			compileOperand(I.getOperand(0), andPrio, /*allowBooleanObjects*/ true);
			if(!asmjs && I.getType()->isIntegerTy(1))
				stream << "&&";
			else
				stream << '&';
			compileOperand(I.getOperand(1), andPrio, /*allowBooleanObjects*/ true);
			if(parentPrio > andPrio) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::LShr:
		{
			//Integer logical shift right
			//No need to apply the >> operator. The result is an integer by spec
			if(parentPrio > SHIFT) stream << '(';
			compileOperand(I.getOperand(0), SHIFT);
			stream << ">>>";
			compileOperand(I.getOperand(1), nextPrio(SHIFT));
			if(parentPrio > SHIFT) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::AShr:
		{
			//Integer arithmetic shift right
			//No need to apply the >> operator. The result is an integer by spec
			if(parentPrio > SHIFT) stream << '(';
			if(types.isI32Type(I.getOperand(0)->getType()))
				compileOperand(I.getOperand(0), SHIFT);
			else
				compileSignedInteger(I.getOperand(0), /*forComparison*/ false, SHIFT);
			stream << ">>";
			compileOperand(I.getOperand(1), nextPrio(SHIFT));
			if(parentPrio > SHIFT) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Shl:
		{
			//Integer shift left
			//No need to apply the >> operator. The result is an integer by spec
			if(parentPrio > SHIFT) stream << '(';
			compileOperand(I.getOperand(0), SHIFT);
			stream << "<<";
			compileOperand(I.getOperand(1), nextPrio(SHIFT));
			if(parentPrio > SHIFT) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Or:
		{
			//Integer logical or
			//No need to apply the >> operator. The result is an integer by spec
			PARENT_PRIORITY orPrio = (!asmjs && I.getType()->isIntegerTy(1)) ? LOGICAL_OR : BIT_OR;
			if(parentPrio > orPrio) stream << '(';
			compileOperand(I.getOperand(0), orPrio, /*allowBooleanObjects*/ true);
			//If the type is i1 we can use the boolean operator to take advantage of logic short-circuit
			//This is possible because we know that instruction with side effects, like calls, are never inlined
			if(!asmjs && I.getType()->isIntegerTy(1))
				stream << "||";
			else
				stream << '|';
			compileOperand(I.getOperand(1), orPrio, /*allowBooleanObjects*/ true);
			if(parentPrio > orPrio) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Xor:
		{
			//Integer logical xor
			//Xor with 1s is used to implement bitwise and logical negation
			//TODO: Optimize the operation with 1s
			//No need to apply the >> operator. The result is an integer by spec
			if(parentPrio > BIT_XOR) stream << '(';
			compileOperand(I.getOperand(0), BIT_XOR);
			stream << '^';
			compileOperand(I.getOperand(1), BIT_XOR);
			if(parentPrio > BIT_XOR) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Trunc:
		{
			//Well, ideally this should not be used since, since it's a waste of bit to
			//use integers less than 32 bit wide. Still we can support it
			PARENT_PRIORITY truncPrio = LOWEST;
			uint32_t width = needsIntCoercion(&I, parentPrio, &truncPrio);
			if(parentPrio > truncPrio) stream << '(';
			compileOperand(I.getOperand(0), truncPrio);
			if (truncPrio == BIT_AND)
				stream << '&' << getMaskForBitWidth(width);
			if(parentPrio > truncPrio) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::SExt:
		{
			//We can use a couple of shift to make this work
			compileSignedInteger(I.getOperand(0), /*forComparison*/ false, parentPrio);
			return COMPILE_OK;
		}
		case Instruction::Select:
		{
			const SelectInst& si = cast<SelectInst>(I);
			if(si.getType()->isPointerTy() && PA.getPointerKind(&si) == SPLIT_REGULAR)
			{
				compileOperand(si.getOperand(0), TERNARY, /*allowBooleanObjects*/ true);
				stream << '?';
				compilePointerBase(si.getOperand(1));
				stream << ':';
				compilePointerBase(si.getOperand(2));
				stream << ';' << NewLine;
				stream << namegen.getSecondaryName(&si) << '=';
				compileOperand(si.getOperand(0), TERNARY, /*allowBooleanObjects*/ true);
				stream << '?';
				compilePointerOffset(si.getOperand(1), TERNARY);
				stream << ':';
				compilePointerOffset(si.getOperand(2), TERNARY);
			}
			else
				compileSelect(&si, si.getCondition(), si.getTrueValue(), si.getFalseValue(), parentPrio);
			return COMPILE_OK;
		}
		case Instruction::ExtractValue:
		{
			const ExtractValueInst& evi = cast<ExtractValueInst>(I);
			const Value* aggr=evi.getAggregateOperand();
			Type* t=aggr->getType();
			if(!t->isStructTy())
			{
				llvm::errs() << "extractvalue: Expected struct, found " << *t << "\n";
				llvm::report_fatal_error("Unsupported code found, please report a bug", false);
				return COMPILE_OK;
			}
			assert(!isa<UndefValue>(aggr));

			compileOperand(aggr, HIGHEST);

			uint32_t offset=evi.getIndices()[0];
			stream << '.' << types.getPrefixCharForMember(PA, cast<StructType>(t), offset) << offset;
			if(types.useWrapperArrayForMember(PA, cast<StructType>(t), offset))
				stream << "[0]";
			return COMPILE_OK;
		}
		case Instruction::FPExt:
		{
			const Value* src=I.getOperand(0);
			compileOperand(src, parentPrio);
			return COMPILE_OK;
		}
		case Instruction::FPTrunc:
		{
			const Value* src=I.getOperand(0);
			compileOperand(src, parentPrio);
			return COMPILE_OK;
		}
		case Instruction::PtrToInt:
		{
			const PtrToIntInst& pi=cast<PtrToIntInst>(I);
			compilePtrToInt(pi.getOperand(0));
			return COMPILE_OK;
		}
		case Instruction::VAArg:
		{
			const VAArgInst& vi=cast<VAArgInst>(I);
			if (asmjs)
			{
				// floats are promoted to double as per standard
				if (vi.getType()->isFloatingPointTy())
					stream<< '+' << heapNames[HEAPF64];
				// int8 and int16 are promoted to int32 as per standard
				else if (vi.getType()->isIntegerTy() || vi.getType()->isPointerTy())
					stream << heapNames[HEAP32];
				stream << '[';
				compileHeapAccess(vi.getPointerOperand());
				if (vi.getType()->isIntegerTy() || vi.getType()->isPointerTy() || vi.getType()->isFloatTy())
					stream << ">>2]|0";
				else
					stream << ">>3]";
				stream << ';' << NewLine;

				compileHeapAccess(vi.getPointerOperand());
				stream << "=((";
				compileHeapAccess(vi.getPointerOperand());
				stream << "|0)+8)|0";
			}
			else
			{
				stream << "handleVAArg(";
				compileCompleteObject(vi.getPointerOperand());
				stream << ')';

				assert( globalDeps.needHandleVAArg() );
			}
			return COMPILE_OK;
		}
		case Instruction::Call:
		{
			const CallInst& ci = cast<CallInst>(I);
			const Function * calledFunc = ci.getCalledFunction();
			const Value * calledValue = ci.getCalledValue();
			const PointerType* pTy = cast<PointerType>(calledValue->getType());
			const FunctionType* fTy = cast<FunctionType>(pTy->getElementType());
			const Type* retTy = fTy->getReturnType();
			// Calling convention for variadic arguments in asm.js mode:
			// arguments are pushed into the stack in the reverse order
			// in which they appear.
			// We use the 'comma trick' to make all the operations a
			// single expression
			if (fTy->isVarArg() && asmjs)
			{
				size_t n = ci.getNumArgOperands();
				size_t arg_size = fTy->getNumParams();
				size_t i = 0;
				stream << '(';
				for (auto op = ci.op_begin() + n - 1; op != ci.op_begin() + arg_size - 1; op--)
				{
					i++;
					uint32_t shift = compileHeapForType(op->get()->getType());
					stream << "[(__stackPtr-"<< i*8 << ')' << ">>" << shift << "]=";
					compileOperand(op->get());
					stream << ',' << NewLine;
				}
				stream << "__stackPtr=(__stackPtr-" << i*8 << ")|0," << NewLine;
			}
			PARENT_PRIORITY callPrio = HIGHEST;
			needsIntCoercion(&ci, parentPrio==BIT_OR?parentPrio:callPrio, &callPrio);
			// The cast to the appropriate int width is already done by the callee
			if (callPrio == BIT_AND)
				callPrio = BIT_OR;
			// Avoid `++`
			else if (retTy->isFloatingPointTy() && parentPrio == ADD_SUB)
				callPrio = LOWEST;
			if (parentPrio > callPrio)
				stream<<'(';
			if (retTy->isFloatingPointTy())
				stream << '+';
			if(calledFunc)
			{
				COMPILE_INSTRUCTION_FEEDBACK cf=handleBuiltinCall(&ci, calledFunc);
				if(cf!=COMPILE_UNSUPPORTED)
				{
					if (parentPrio > callPrio)
						stream << ')';
					return cf;
				}
				// handle calls to asm.js functions
				if (!asmjs && globalDeps.asmJSExports().count(calledFunc))
					stream << "__asm.";
				stream << namegen.getName(calledFunc);
			}
			else if (asmjs)
			{
				//Indirect call, asm.js mode
				if (!globalDeps.functionTables().count(fTy))
				{
					stream << "__dummy";
				}
				else
				{
					const auto& table = globalDeps.functionTables().at(fTy);
					stream << "__FUNCTION_TABLE_" << table.name << '[';
					// NOTE: V8 does not like constants here, so we add a useless
					// ternary operator to make it happy.
					if (const Constant* c = dyn_cast<Constant>(calledValue))
					{
						stream << "(0!=0?0:";
						compileConstant(c);
						stream << ')';
					}
					else
					{
						compileRawPointer(calledValue);
					}
					stream << '&' << table.mask << ']';
				}

			}
			else
			{
				//Indirect call, normal mode
				compilePointerAs(calledValue, COMPLETE_OBJECT);
			}

			//If we are dealing with inline asm we are done
			if(!ci.isInlineAsm())
			{
				// On asm.js mode the varargs are passed on the stack
				size_t n = asmjs?fTy->getNumParams():ci.getNumArgOperands();
				compileMethodArgs(ci.op_begin(),ci.op_begin()+n, &ci, /*forceBoolean*/ false);
			}
			if (callPrio == BIT_OR)
				stream << "|0";
			if (parentPrio > callPrio)
				stream << ')';
			if(ci.getType()->isPointerTy() && PA.getPointerKind(&ci) == SPLIT_REGULAR && !ci.use_empty())
			{
				assert(!isInlineable(ci, PA));
				stream << ';' << NewLine;
				stream << namegen.getSecondaryName(&ci) << "=oSlot";
			}
			// If this was a vararg function, pop the arguments from the stack
			if (asmjs && fTy->isVarArg())
			{
				uint32_t n = ci.getNumArgOperands() - fTy->getNumParams();
				if (ci.getType()->isVoidTy())
					stream << ",0";
				stream << ");" << NewLine << "__stackPtr=(__stackPtr+" << n*8 << ")|0";

			}
			return COMPILE_OK;
		}
		case Instruction::Load:
		{
			const LoadInst& li = cast<LoadInst>(I);
			const Value* ptrOp=li.getPointerOperand();
			POINTER_KIND kind = PA.getPointerKind(ptrOp);
			if (checkBounds && (kind == REGULAR || kind == SPLIT_REGULAR))
			{
				stream<<"(";
				compileCheckBounds(ptrOp);
				stream<<",";
			}
			if (checkDefined && kind == COMPLETE_OBJECT && isGEP(ptrOp))
			{
				stream<<"(";
				compileCheckDefined(ptrOp);
				stream<<",";
			}
			PARENT_PRIORITY loadPrio = HIGHEST;
			if (li.getType()->isFloatingPointTy() && parentPrio == ADD_SUB)
				loadPrio = LOWEST;
			int width = needsIntCoercion(&li, parentPrio, &loadPrio);
			if (parentPrio > loadPrio)
				stream << '(';
			if (li.getType()->isFloatingPointTy())
			{
				stream << '+';
			}
			if (kind == BYTE_LAYOUT)
			{
				//Optimize loads of single values from unions
				compilePointerBase(ptrOp);
				Type* pointedType=ptrOp->getType()->getPointerElementType();
				if(pointedType->isIntegerTy(8))
					stream << ".getInt8(";
				else if(pointedType->isIntegerTy(16))
					stream << ".getInt16(";
				else if(pointedType->isIntegerTy(32))
					stream << ".getInt32(";
				else if(pointedType->isFloatTy())
					stream << ".getFloat32(";
				else if(pointedType->isDoubleTy())
					stream << ".getFloat64(";
				compilePointerOffset(ptrOp, LOWEST);
				if(!pointedType->isIntegerTy(8))
					stream << ",true";
				stream << ')';
			}
			else if (kind == RAW)
				compileHeapAccess(ptrOp);
			else
				compileCompleteObject(ptrOp);
			// 32-bit integers are all loaded as signed, other integers as unsigned
			if(loadPrio == BIT_OR)
				stream << "|0";
			else if(loadPrio == BIT_AND)
				stream << '&' << getMaskForBitWidth(width);
			if(parentPrio > loadPrio)
				stream << ')';
			if (checkBounds && (kind == REGULAR || kind == SPLIT_REGULAR))
				stream<<')';
			if (checkDefined && kind == COMPLETE_OBJECT && isGEP(ptrOp))
				stream<<')';
			if(li.getType()->isPointerTy() && !li.use_empty() && PA.getPointerKind(&li) == SPLIT_REGULAR && !PA.getConstantOffsetForPointer(&li))
			{
				assert(!isInlineable(li, PA));
				stream << ';' << NewLine;
				stream << namegen.getSecondaryName(&li) << '=';
				compileCompleteObject(ptrOp);
				stream <<'o';
			}
			return COMPILE_OK;
		}
		default:
			stream << "alert('Unsupported code')";
			llvm::errs() << "\tImplement inst " << I.getOpcodeName() << '\n';
			return COMPILE_UNSUPPORTED;
	}
}

void CheerpWriter::compileBB(const BasicBlock& BB)
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
		const DebugLoc& debugLoc = I->getDebugLoc();
		if(sourceMapGenerator && !debugLoc.isUnknown())
			sourceMapGenerator->setDebugLoc(I->getDebugLoc());
		if(!I->getType()->isVoidTy() && !I->use_empty())
		{
			stream << namegen.getName(I) << '=';
		}
		if(I->isTerminator())
		{
			compileTerminatorInstruction(*dyn_cast<TerminatorInst>(I));
		}
		else if(!I->use_empty() || I->mayHaveSideEffects())
		{
			COMPILE_INSTRUCTION_FEEDBACK ret=compileNotInlineableInstruction(*I, LOWEST);

			if(ret==COMPILE_OK)
			{
				stream << ';' << NewLine;
			}
			else if(ret==COMPILE_UNSUPPORTED)
			{
				//Stop basic block compilation
				return;
			}
		}
	}
}

void CheerpRenderInterface::renderBlock(const void* privateBlock)
{
	const BasicBlock* bb=(const BasicBlock*)privateBlock;
	writer->compileBB(*bb);
}

void CheerpRenderInterface::renderCondition(const BasicBlock* bb, int branchId, CheerpWriter::PARENT_PRIORITY parentPrio)
{
	const TerminatorInst* term=bb->getTerminator();

	bool asmjs = bb->getParent()->getSection() == StringRef("asmjs");
	if(isa<BranchInst>(term))
	{
		const BranchInst* bi=cast<BranchInst>(term);
		assert(bi->isConditional());
		//The second branch is the default
		assert(branchId==0);
		writer->compileOperand(bi->getCondition(), parentPrio, /*allowBooleanObjects*/ true);
	}
	else if(isa<SwitchInst>(term))
	{
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
	}
	else
	{
		term->dump();
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}
}

void CheerpRenderInterface::renderLabelForSwitch(int labelId)
{
	writer->stream << 'L' << labelId << ':';
}
void CheerpRenderInterface::renderSwitchOnLabel()
{
	writer->stream << "switch(label";
	if (asmjs)
		writer->stream << "|0";
	writer->stream << "){" << NewLine;
}
void CheerpRenderInterface::renderCaseOnLabel(int labelId)
{
	writer->stream << "case ";
	writer->stream << labelId << ":{" << NewLine;
}
void CheerpRenderInterface::renderSwitchBlockBegin(const void* privateBranchVar)
{
	const Value* cond = (const Value*)privateBranchVar;
	writer->stream << "switch(";
	CheerpWriter::PARENT_PRIORITY myPrio = asmjs?CheerpWriter::BIT_OR:CheerpWriter::LOWEST;
	writer->compileOperand(cond,myPrio);
	if (asmjs)
		writer->stream << "|0";
	writer->stream << "){" << NewLine;
}
void CheerpRenderInterface::renderCaseBlockBegin(const void* privateBlock, int branchId)
{
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
}
void CheerpRenderInterface::renderDefaultBlockBegin()
{
	writer->stream << "default:{" << NewLine;
}
void CheerpRenderInterface::renderIfBlockBegin(const void* privateBlock, int branchId, bool first)
{
	const BasicBlock* bb=(const BasicBlock*)privateBlock;
	if(!first)
		writer->stream << "}else ";
	writer->stream << "if(";
	renderCondition(bb, branchId, CheerpWriter::LOWEST);
	writer->stream << "){" << NewLine;
}

void CheerpRenderInterface::renderIfBlockBegin(const void* privateBlock, const std::vector<int>& skipBranchIds, bool first)
{
	const BasicBlock* bb=(const BasicBlock*)privateBlock;
	if(!first)
		writer->stream << "}else ";
	writer->stream << "if(!(";
	for(uint32_t i=0;i<skipBranchIds.size();i++)
	{
		if(i!=0)
			writer->stream << "||";
		renderCondition(bb, skipBranchIds[i], skipBranchIds.size() == 1 ? CheerpWriter::LOWEST : CheerpWriter::LOGICAL_OR);
	}
	writer->stream << ")){" << NewLine;
}

void CheerpRenderInterface::renderElseBlockBegin()
{
	writer->stream << "}else{" << NewLine;
}

void CheerpRenderInterface::renderBlockEnd()
{
	writer->stream << '}' << NewLine;
}

void CheerpRenderInterface::renderBlockPrologue(const void* privateBlockTo, const void* privateBlockFrom)
{
	const BasicBlock* bbTo=(const BasicBlock*)privateBlockTo;
	const BasicBlock* bbFrom=(const BasicBlock*)privateBlockFrom;
	writer->compilePHIOfBlockFromOtherBlock(bbTo, bbFrom);
}

bool CheerpRenderInterface::hasBlockPrologue(const void* privateBlockTo, const void* privateBlockFrom) const
{
	const BasicBlock* to=(const BasicBlock*)privateBlockTo;
	const BasicBlock* from=(const BasicBlock*)privateBlockFrom;

	if (to->getFirstNonPHI()==&to->front())
		return false;

	// We can avoid assignment from the same register if no pointer kind
	// conversion is required
	return writer->needsPointerKindConversionForBlocks(to, from);
}

void CheerpRenderInterface::renderWhileBlockBegin()
{
	writer->stream << "while(1){" << NewLine;
}

void CheerpRenderInterface::renderWhileBlockBegin(int blockLabel)
{
	writer->stream << 'L' << blockLabel << ':';
	renderWhileBlockBegin();
}

void CheerpRenderInterface::renderDoBlockBegin()
{
	writer->stream << "do{" << NewLine;
}

void CheerpRenderInterface::renderDoBlockBegin(int blockLabel)
{
	writer->stream << 'L' << blockLabel << ':';
	renderDoBlockBegin();
}

void CheerpRenderInterface::renderDoBlockEnd()
{
	writer->stream << "}while(0);" << NewLine;
}

void CheerpRenderInterface::renderBreak()
{
	writer->stream << "break;" << NewLine;
}

void CheerpRenderInterface::renderBreak(int labelId)
{
	writer->stream << "break L" << labelId << ';' << NewLine;
}

void CheerpRenderInterface::renderContinue()
{
	writer->stream << "continue;" << NewLine;
}

void CheerpRenderInterface::renderContinue(int labelId)
{
	writer->stream << "continue L" << labelId << ';' << NewLine;
}

void CheerpRenderInterface::renderLabel(int labelId)
{
	writer->stream << "label=" << labelId << "|0;" << NewLine;
}

void CheerpRenderInterface::renderIfOnLabel(int labelId, bool first)
{
	if(first==false)
		writer->stream << "else ";
	if (asmjs)
		writer->stream << "if(label>>>0==" << labelId << ">>>0){" << NewLine;
	else
		writer->stream << "if(label===" << labelId << "){" << NewLine;
}

void CheerpWriter::compileMethodLocal(StringRef name, Registerize::REGISTER_KIND kind)
{
	bool asmjs = currentFun->getSection() == StringRef("asmjs");
	stream << name << '=';
	if(kind == Registerize::INTEGER)
		stream << '0';
	else if(!asmjs && kind == Registerize::DOUBLE)
		stream << "-0";
	// NOTE: V8 requires the `.` to identify it as a double in asm.js
	else if(asmjs && kind == Registerize::DOUBLE)
		stream << "0.";
	else if(asmjs)
		stream << '0';
	else
		stream << "null";
}

void CheerpWriter::compileMethodLocals(const Function& F, bool needsLabel)
{
	// Declare are all used locals in the beginning
	enum LOCAL_STATE { NOT_DONE, NAME_DONE, SECONDARY_NAME_DONE };
	std::vector<LOCAL_STATE> localsFound;
	bool firstVar = true;
	if(needsLabel)
	{
		stream << "var label=0";
		firstVar = false;
	}
	std::set<StringRef> compiledTmpPHIs;
	for(const BasicBlock& BB: F)
	{
		for(const Instruction& I: BB)
		{
			if (!namegen.needsName(I, PA))
				continue;
			// Get the register
			uint32_t regId = registerize.getRegisterId(&I);
			if(localsFound.size() <= regId)
				localsFound.resize(regId+1, NOT_DONE);
			if(localsFound[regId]==SECONDARY_NAME_DONE)
				continue;
			bool needsSecondaryName = namegen.needsSecondaryName(&I, PA);
			if(localsFound[regId]<NAME_DONE)
			{
				if(firstVar)
					stream << "var ";
				else
					stream << ',';
				compileMethodLocal(namegen.getName(&I),Registerize::getRegKindFromType(I.getType()));
				firstVar = false;
				localsFound[regId]=NAME_DONE;
			}
			if(localsFound[regId]<SECONDARY_NAME_DONE && needsSecondaryName)
			{
				stream << ',';
				compileMethodLocal(namegen.getSecondaryName(&I),Registerize::INTEGER);
				localsFound[regId]=SECONDARY_NAME_DONE;
			}
		}
		// Handle the special names required for the edges between blocks
		class LocalsPHIHandler: public EndOfBlockPHIHandler
		{
		public:
			LocalsPHIHandler(CheerpWriter& w, const BasicBlock* f, const BasicBlock* t, std::set<StringRef>& c):EndOfBlockPHIHandler(w.PA),compiledLocals(c),writer(w),fromBB(f),toBB(t)
			{
			}
			~LocalsPHIHandler()
			{
			}
			std::set<StringRef>& compiledLocals;
		private:
			CheerpWriter& writer;
			const BasicBlock* fromBB;
			const BasicBlock* toBB;
			void handleRecursivePHIDependency(const Instruction* incoming) override
			{
				writer.namegen.setEdgeContext(fromBB, toBB);
				if(incoming->getType()->isPointerTy() && writer.PA.getPointerKind(incoming)==SPLIT_REGULAR && !writer.PA.getConstantOffsetForPointer(incoming))
				{
					StringRef secondaryName = writer.namegen.getSecondaryNameForEdge(incoming);
					if(compiledLocals.insert(secondaryName).second)
					{
						writer.stream << ',';
						writer.compileMethodLocal(secondaryName, Registerize::INTEGER);
					}
				}
				StringRef primaryName = writer.namegen.getNameForEdge(incoming);
				if(compiledLocals.insert(primaryName).second)
				{
					writer.stream << ',';
					writer.compileMethodLocal(primaryName, Registerize::getRegKindFromType(incoming->getType()));
				}
				writer.namegen.clearEdgeContext();
			}
			void handlePHI(const Instruction* phi, const Value* incoming) override
			{
			}
		};
		const TerminatorInst* term=BB.getTerminator();
		for(uint32_t i=0;i<term->getNumSuccessors();i++)
		{
			const BasicBlock* succBB=term->getSuccessor(i);
			LocalsPHIHandler(*this, &BB, succBB, compiledTmpPHIs).runOnEdge(registerize, &BB, succBB);
		}
	}
	if(!firstVar)
		stream << ';' << NewLine;
}

void CheerpWriter::compileMethod(const Function& F)
{
	bool asmjs = F.getSection() == StringRef("asmjs");
	if (sourceMapGenerator) {
#ifdef CHEERP_DEBUG_SOURCE_MAP
		llvm::errs() << "compileMethod: " << F.getName() << "\n";
#endif

		auto search = functionToDebugInfoMap.find(F.getName());
		if (search != functionToDebugInfoMap.end()) {
#ifdef CHEERP_DEBUG_SOURCE_MAP
			llvm::errs() << "Found on " << search->second.getFilename()
				<< ":"  << search->second.getLineNumber() << '\n';
#endif
			sourceMapGenerator->setFunctionName(search->second);
		}
	}
	currentFun = &F;
	stream << "function " << namegen.getName(&F) << '(';
	const Function::const_arg_iterator A=F.arg_begin();
	const Function::const_arg_iterator AE=F.arg_end();
	for(Function::const_arg_iterator curArg=A;curArg!=AE;++curArg)
	{
		if(curArg!=A)
			stream << ',';
		if(curArg->getType()->isPointerTy() && PA.getPointerKind(curArg) == SPLIT_REGULAR)
			stream << namegen.getName(curArg) << ',' << namegen.getSecondaryName(curArg);
		else
			stream << namegen.getName(curArg);
	}
	stream << "){" << NewLine;
	if (measureTimeToMain && F.getName() == "main")
	{
		stream << "__cheerp_main_time=__cheerp_now();" << NewLine;
	}
	if (asmjs)
	{
		compileParamTypeAnnotationsAsmJS(&F);
	}
	if(F.size()==1)
	{
		compileMethodLocals(F, false);
		if (asmjs)
			compileStackFrame();
		compileBB(*F.begin());
	}
	else
	{
		Relooper* rl = runRelooperOnFunction(F);
		CheerpRenderInterface ri(this, NewLine, asmjs);
		compileMethodLocals(F, rl->needsLabel());
		if (asmjs)
			compileStackFrame();
		rl->Render(&ri);
	}
	if (asmjs)
	{
		// TODO: asm.js needs a final return statement.
		// for now we are putting one at the end of every method, even
		// if there is already one
		compileStackRet();
		stream << "return";
		Type* ret = F.getReturnType();
		if (ret->isIntegerTy() || ret->isPointerTy())
			stream << " 0";
		else if (ret->isFloatingPointTy())
			stream << " 0.";
		stream << ';' << NewLine;
	}
	stream << '}' << NewLine;
	currentFun = NULL;
}

CheerpWriter::GlobalSubExprInfo CheerpWriter::compileGlobalSubExpr(const GlobalDepsAnalyzer::SubExprVec& subExpr)
{
	for ( auto it = std::next(subExpr.begin()); it != subExpr.end(); ++it )
	{
		const Use * u = *it;

		if ( isa<ConstantArray>( u->getUser() ) )
		{
			stream << '[' << u->getOperandNo() << ']';
			if (it == (subExpr.end()-1) && (*it)->get()->getType()->isPointerTy())
			{
				POINTER_KIND elementPointerKind = PA.getPointerKindForStoredType((*it)->get()->getType());
				return GlobalSubExprInfo(elementPointerKind, false);
			}
		}
		else if ( ConstantStruct* cs=dyn_cast<ConstantStruct>( u->getUser() ) )
		{
			stream << ".a" << u->getOperandNo();
			bool useWrapperArray = types.useWrapperArrayForMember(PA, cs->getType(), u->getOperandNo());
			if (useWrapperArray)
				stream << "[0]";
			if (it == (subExpr.end()-1) && (*it)->get()->getType()->isPointerTy())
			{
				// We don't expect anything which is not a pointer here, as we are fixing dependencies between globals
				assert(cs->getType()->getElementType(u->getOperandNo())->isPointerTy());
				TypeAndIndex b(cs->getType(), u->getOperandNo(), TypeAndIndex::STRUCT_MEMBER);
				POINTER_KIND elementPointerKind = PA.getPointerKindForMemberPointer(b);
				bool hasConstantOffset = PA.getConstantOffsetForMember(b) != NULL;
				return GlobalSubExprInfo(elementPointerKind, hasConstantOffset);
			}
		}
		else
			assert(false);
	}
	assert(false);
}

void CheerpWriter::compileGlobal(const GlobalVariable& G)
{
	assert(G.hasName());
	if(TypeSupport::isClientGlobal(&G) && !G.hasInitializer())
	{
		// Extern globals in the client namespace are only placeholders for JS globals
		return;
	}
	stream  << "var " << namegen.getName(&G);

	if(G.hasInitializer())
	{
		stream << '=';
		const Constant* C = G.getInitializer();
		POINTER_KIND k = PA.getPointerKind(&G);

		if(k == REGULAR)
		{
			stream << "{d:[";
			if(C->getType()->isPointerTy())
				compilePointerAs(C, PA.getPointerKindForStoredType(C->getType()));
			else
				compileOperand(C, LOWEST);
			stream << "],o:0}";
		}
		else if(k == BYTE_LAYOUT)
		{
			stream << "{d:";
			if(C->getType()->isPointerTy())
				compilePointerAs(C, PA.getPointerKindForStoredType(C->getType()));
			else
				compileOperand(C, LOWEST);
			stream << ",o:0}";
		}
		else if(k == SPLIT_REGULAR)
		{
			stream << '[';
			if(C->getType()->isPointerTy())
				compilePointerAs(C, PA.getPointerKindForStoredType(C->getType()));
			else
				compileOperand(C, LOWEST);
			stream << ']';
			stream << ';' << NewLine;
			stream << "var " << namegen.getSecondaryName(&G);
			stream << "=0";
		}
		else
		{
			if(C->getType()->isPointerTy())
			{
				POINTER_KIND storedKind = PA.getPointerKindForStoredType(C->getType());
				if(storedKind == REGULAR && PA.getConstantOffsetForPointer(&G))
					compilePointerBase(C);
				else
					compilePointerAs(C, storedKind);
			}
			else
				compileOperand(C);
		}
	}
	stream << ';' << NewLine;

	compiledGVars.insert(&G);
	if(G.hasInitializer())
	{
		if(StructType* st=dyn_cast<StructType>(G.getType()->getPointerElementType()))
		{
			//TODO: Verify that it makes sense to assume struct with no name has no bases
			if(st->hasName() && module.getNamedMetadata(Twine(st->getName(),"_bases")) &&
				globalDeps.classesWithBaseInfo().count(st))
			{
				stream << namegen.getClassName(st) << '(';
				compilePointerAs(&G, COMPLETE_OBJECT);
				stream << ");" << NewLine;
			}
		}
	}

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

		compileCompleteObject(otherGV);

		Value* valOp = subExpr.back()->get();
		GlobalSubExprInfo subExprInfo = compileGlobalSubExpr(subExpr);

		stream << '=';
		if (valOp->getType()->isPointerTy())
		{
			assert(subExprInfo.kind != UNKNOWN);
			if((subExprInfo.kind == REGULAR || subExprInfo.kind == SPLIT_REGULAR) && subExprInfo.hasConstantOffset)
				compilePointerBase(valOp);
			else if(subExprInfo.kind == SPLIT_REGULAR)
			{
				compilePointerBase(valOp);
				stream << ";" << NewLine;
				compileCompleteObject(otherGV);
				compileGlobalSubExpr(subExpr);
				stream << 'o';
				stream << '=';
				compilePointerOffset(valOp, LOWEST);
			}
			else
				compilePointerAs(valOp, subExprInfo.kind);
		}
		else
			compileOperand(valOp, LOWEST);
		stream << ';' << NewLine;
	}
}

void CheerpWriter::compileGlobalAsmJS(const GlobalVariable& G)
{
	assert(G.hasName());
	if (TypeSupport::isClientGlobal(&G) && !G.hasInitializer())
	{
		// Extern globals in the client namespace are only placeholders for JS globals
		llvm::errs() << "client namespace functions not supported in asmjs mode yet:\n";
		G.dump();
		return;
	}
	if (symbolicGlobalsAsmJS)
		stream << "var " << namegen.getName(&G) << '=';
	Type* ty = G.getType();
	uint32_t size = targetData.getTypeAllocSize(ty->getPointerElementType());
	// Ensure the right alignment for the type
	uint32_t alignment = TypeSupport::getAlignmentAsmJS(targetData, ty->getPointerElementType());
	// The following is correct if alignment is a power of 2 (which it should be)
	heapStartAsmJS = (heapStartAsmJS + alignment - 1) & ~(alignment - 1);
	if (symbolicGlobalsAsmJS)
		stream << heapStartAsmJS << ';' << NewLine;
	gVarsAddr.emplace(&G,heapStartAsmJS);
	heapStartAsmJS += size;
}

void CheerpWriter::compileGlobalsInitAsmJS()
{
	for (const auto& g : gVarsAddr)
	{
		if (g.first->hasInitializer())
		{
			Type* ty = g.first->getInitializer()->getType();
			// If the initializer is a function, skip it
			if (ty->isPointerTy() && ty->getPointerElementType()->isFunctionTy())
				continue;
			const Constant* init = g.first->getInitializer();
			stream  << heapNames[HEAP8] << ".set([";
			compileConstantAsBytes(init,true,/* asmjs */ true);
			stream << "]," << g.second << ");" << NewLine;
		}
	}
}

void CheerpWriter::compileParamTypeAnnotationsAsmJS(const Function* F)
{
	const Function::const_arg_iterator A=F->arg_begin();
	const Function::const_arg_iterator AE=F->arg_end();
	for(Function::const_arg_iterator curArg=A;curArg!=AE;++curArg)
	{
		stream << namegen.getName(curArg) << '=';
		if (curArg->getType()->isFloatingPointTy())
			stream << "+";
		stream<< namegen.getName(curArg);
		if (curArg->getType()->isIntegerTy() || curArg->getType()->isPointerTy())
			stream << "|0";
		stream << ';' << NewLine;
	}
}

void CheerpWriter::compileNullPtrs()
{
	stream << "var aSlot=null;var oSlot=0;var nullArray=[null];var nullObj={d:nullArray,o:0};" << NewLine;
}

void CheerpWriter::compileCreateClosure()
{
	stream << "function cheerpCreateClosure(func, obj){return function(e){func(obj,e);};}" << NewLine;
}

void CheerpWriter::compileHandleVAArg()
{
	stream << "function handleVAArg(ptr){var ret=ptr.d[ptr.o];ptr.o++;return ret;}" << NewLine;
}

void CheerpWriter::compileBuiltins(bool asmjs)
{
	StringRef math = asmjs?"stdlib.Math.":"Math.";
	if(useMathImul || asmjs)
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::IMUL) << '=' << math << "imul;" << NewLine;
	if(useMathFround || asmjs)
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '=' << math << "fround;" << NewLine;
}

void CheerpWriter::compileCheckBoundsHelper()
{
	stream << "function checkBounds(arr,offs){if(offs>=arr.length || offs<0) throw new Error('OutOfBounds');}" << NewLine;
}

void CheerpWriter::compileCheckBounds(const Value* p)
{
	stream<<"checkBounds(";
	compilePointerBase(p);
	stream<<",";
	compilePointerOffset(p,LOWEST);
	stream<<")";
}

void CheerpWriter::compileCheckDefinedHelper()
{
	stream << "function checkDefined(m){if(m===undefined) throw new Error('UndefinedMemberAccess');}" << NewLine;
}

void CheerpWriter::compileCheckDefined(const Value* p)
{
	stream<<"checkDefined(";
	compileGEP(cast<User>(p),COMPLETE_OBJECT);
	stream<<")";
}

void CheerpWriter::compileStackFrame()
{
	// NOTE: the stack frame always starts with 8 byte alignment
	stream<< "var __savedStack=0;__savedStack=__stackPtr;__stackPtr=__stackPtr&0xfffffff8;"<<NewLine;
}
void CheerpWriter::compileStackRet()
{
	stream<< "__stackPtr=__savedStack;"<<NewLine;
}
void CheerpWriter::compileAllocaAsmJS(const Value* n, uint32_t elem_size, uint32_t alignment)
{
	StringRef num = "1";
	if (n != nullptr)
		num = namegen.getName(n);
	// NOTE: the `and` operation ensures the proper alignment
	stream << "(__stackPtr-(";
	stream << namegen.getBuiltinName(NameGenerator::Builtin::IMUL) << '(';
	stream << elem_size << ',' << num << ")|0))&" << uint32_t(0-alignment) << ';' <<NewLine;
	stream << "__stackPtr=(__stackPtr-(";
	stream << namegen.getBuiltinName(NameGenerator::Builtin::IMUL) << '(';
	stream << elem_size << ',' << num << ")|0))&" << uint32_t(0-alignment);
}

void CheerpWriter::compileMemmoveHelperAsmJS()
{
	stream << "function __asmjs_memmove(src,dst,size){" << NewLine;
	stream << "src=src|0;dst=dst|0;size=size|0;" << NewLine;
	stream << "var i=0;var end=0;var inc=1;" << NewLine;
	stream << "if(src>>>0<dst>>>0){i=size-1|0;end=-1;inc=-1;}else end=size;"<<NewLine;
	stream << "while(1){if((i|0)==(end|0))break;HEAP8[dst+i|0]=HEAP8[src+i|0];i=i+inc|0;}"<<NewLine;
	stream << "return dst|0;"<<NewLine;
	stream << "}"<<NewLine;
}

void CheerpWriter::compilePrintStringHelperAsmJS()
{
	stream << "function printString(p,n) {" << NewLine;
	stream << "var s='';" << NewLine;
	stream << "for(var i=0;i<n;i++){" << NewLine;
	stream << "s+=String.fromCharCode(" << heapNames[HEAP8] << "[p+i]);" << NewLine;
	stream << '}' << NewLine;
	stream << "console.log(s);" << NewLine;
	stream << "};" << NewLine;
}

void CheerpWriter::compileFunctionTablesAsmJS()
{
	for (const auto& table : globalDeps.functionTables())
	{
		stream << "var " << "__FUNCTION_TABLE_" << table.second.name << "=[";
		bool first = true;
		uint32_t num = 0;
		for (const auto F : table.second.functions)
		{
			if (!first)
				stream << ',';
			first = false;
			stream << namegen.getName(F);
			num++;
		}
		for (; num <= table.second.mask; num++)
		{
			stream << ',' << namegen.getName(table.second.functions[0]);
		}
		stream << "];" << NewLine;
	}
}

void CheerpWriter::compileMathDeclAsmJS()
{
	stream << "var Infinity=stdlib.Infinity;" << NewLine;
	stream << "var NaN=stdlib.NaN;" << NewLine;
	stream << "var abs=stdlib.Math.abs;" << NewLine;
	stream << "var acos=stdlib.Math.acos;" << NewLine;
	stream << "var asin=stdlib.Math.asin;" << NewLine;
	stream << "var atan=stdlib.Math.atan;" << NewLine;
	stream << "var atan2=stdlib.Math.atan2;" << NewLine;
	stream << "var ceil=stdlib.Math.ceil;" << NewLine;
	stream << "var cos=stdlib.Math.cos;" << NewLine;
	stream << "var exp=stdlib.Math.exp;" << NewLine;
	stream << "var floor=stdlib.Math.floor;" << NewLine;
	stream << "var log=stdlib.Math.log;" << NewLine;
	stream << "var pow=stdlib.Math.pow;" << NewLine;
	stream << "var sin=stdlib.Math.sin;" << NewLine;
	stream << "var sqrt=stdlib.Math.sqrt;" << NewLine;
	stream << "var tan=stdlib.Math.tan;" << NewLine;
}

void CheerpWriter::makeJS()
{
	if (sourceMapGenerator) {
		sourceMapGenerator->beginFile();

		NamedMDNode *cu = module.getNamedMetadata("llvm.dbg.cu");
		if (!cu || cu->getNumOperands() == 0) {
			llvm::errs() << "warning: no debug symbols found but source map is requested\n";
		}

		DebugInfoFinder finder;
		finder.processModule(module);

		for (const DISubprogram &method : finder.subprograms()) {
#ifdef CHEERP_DEBUG_SOURCE_MAP
			llvm::errs() << "Name: " << method.getName()
				<< " LinkageName: " << method.getLinkageName()
				<< " Line: " << method.getLineNumber()
				<< " ScopeLine: " << method.getScopeLineNumber()
				<< " Type: " << method.getType()
				<< " IsLocalToUnit: " << method.isLocalToUnit()
				<< " IsDefinition: " << method.isDefinition()
				<< "\n";
#endif

			StringRef linkName = method.getLinkageName();
			if (linkName.empty())
				linkName = method.getName();
			functionToDebugInfoMap.insert(std::make_pair(linkName, method));
		}
	}

	if (makeModule)
		stream << "(function(){" << NewLine;

	// Enable strict mode first
	stream << "\"use strict\";" << NewLine;

	if(addCredits)
		stream << "/*Compiled using Cheerp (R) by Leaning Technologies Ltd*/" << NewLine;

	if (measureTimeToMain)
	{
		stream << "var __cheerp_now = typeof dateNow!==\"undefined\"?dateNow:(typeof performance!==\"undefined\"?performance.now:function(){return new Date().getTime()});" << NewLine;
		stream << "var __cheerp_main_time = -0;" << NewLine;
		stream << "var __cheerp_start_time = __cheerp_now();" << NewLine;
	}

	//Compile the bound-checking function
	if ( checkBounds )
	{
		compileCheckBoundsHelper();
	}
	//Compile the defined-checking function
	if ( checkDefined )
	{
		compileCheckDefinedHelper();
	}

	compileBuiltins(false);

	std::vector<StringRef> exportedClassNames = compileClassesExportedToJs();
	compileNullPtrs();

	if (globalDeps.needAsmJS())
	{
		// compile boilerplate
		stream << "function asmJS(stdlib, ffi, heap){" << NewLine;
		stream << "\"use asm\";" << NewLine;
		stream << "var __stackPtr=ffi.heapSize|0;" << NewLine;
		for (int i = HEAP8; i<=HEAPF64; i++)
		{
			stream << "var "<<heapNames[i]<<"=new stdlib."<<typedArrayNames[i]<<"(heap);" << NewLine;
		}
		compileMathDeclAsmJS();
		compileBuiltins(true);
		stream << "var isNaN=ffi.isNaN;" << NewLine;
		stream << "var printString=ffi.printString;" << NewLine;
		stream << "var __dummy=ffi.__dummy;" << NewLine;
		if (checkBounds)
		{
			stream << "var checkBoundsAsmJS=ffi.checkBoundsAsmJS;" << NewLine;
			stream << "var checkFunctionPtrAsmJS=ffi.checkFunctionPtrAsmJS;" << NewLine;
		}
		for (const Function* imported: globalDeps.asmJSImports())
		{
			stream << "var " << namegen.getName(imported) << "=ffi." << namegen.getName(imported) << ';' << NewLine;
		}

		// Declare globals
		for ( const GlobalVariable & GV : module.getGlobalList() )
		{
			if (GV.getSection() == StringRef("asmjs"))
				compileGlobalAsmJS(GV);
		}
		for ( const Function & F : module.getFunctionList() )
		{
			if (!F.empty() && F.getSection() == StringRef("asmjs"))
			{
				compileMethod(F);
			}
		}
		compileMemmoveHelperAsmJS();
		stream << "function __init(){" << NewLine;
		//Call constructors
		for (const Function * F : globalDeps.constructors() )
		{
			if (F->getSection() == StringRef("asmjs"))
				stream << namegen.getName(F) << "();" << NewLine;
		}
		stream << '}' << NewLine;
		
		compileFunctionTablesAsmJS();

		stream << "return {" << NewLine;
		stream << "init:__init," << NewLine;
		// if entry point is in asm.js, explicitly export it
		if ( const Function * entryPoint = globalDeps.getEntryPoint())
		{
			if (entryPoint->getSection() == StringRef("asmjs"))
				stream << namegen.getName(entryPoint) << ':' << namegen.getName(entryPoint) << ',' << NewLine;
		}
		for (const Function* exported: globalDeps.asmJSExports())
		{
			StringRef name = namegen.getName(exported);
			stream << name << ':' << name << ',' << NewLine;
		}
		stream << "};" << NewLine;
		stream << "};" << NewLine;
		stream << "var heap = new ArrayBuffer("<<heapSize*1024*1024<<");" << NewLine;
		stream << "var " << heapNames[HEAP8] << "= new " << typedArrayNames[HEAP8] << "(heap);" << NewLine;
		compilePrintStringHelperAsmJS();
		stream << "function __dummy() { throw new Error('this should be unreachable'); };" << NewLine;
		stream << "var ffi = {" << NewLine;
		stream << "heapSize:heap.byteLength," << NewLine;
		stream << "isNaN:isNaN," << NewLine;
		stream << "printString:printString," << NewLine;
		stream << "__dummy:__dummy," << NewLine;
		if (checkBounds)
		{
			stream << "checkBoundsAsmJS:checkBoundsAsmJS," << NewLine;
			stream << "checkFunctionPtrAsmJS:checkFunctionPtrAsmJS," << NewLine;
		}
		for (const Function* imported: globalDeps.asmJSImports())
		{
			StringRef name = namegen.getName(imported);
			if (imported->empty() && !TypeSupport::isClientGlobal(imported))
				name = "__dummy";
			stream << namegen.getName(imported) << ':' << name << ',' << NewLine;
		}
		stream << "};" << NewLine;
		stream << "var stdlib = {"<<NewLine;
		stream << "Math:Math,"<<NewLine;
		stream << "Infinity:Infinity,"<<NewLine;
		stream << "NaN:NaN,"<<NewLine;
		for (int i = HEAP8; i<=HEAPF64; i++)
		{
			stream << typedArrayNames[i] << ':' << typedArrayNames[i] << ',' << NewLine;
		}
		stream << "};" << NewLine;
		compileGlobalsInitAsmJS();
		stream << "var __asm = asmJS(stdlib, ffi, heap);" << NewLine;
		stream << "__asm.init();" << NewLine;
	}

	for ( const Function & F : module.getFunctionList() )
		if (!F.empty() && F.getSection() != StringRef("asmjs"))
		{
#ifdef CHEERP_DEBUG_POINTERS
			dumpAllPointers(F, PA);
#endif //CHEERP_DEBUG_POINTERS
			compileMethod(F);
		}
	for ( const GlobalVariable & GV : module.getGlobalList() )
	{
		if (GV.getSection() != StringRef("asmjs"))
			compileGlobal(GV);
	}

	for ( StructType * st : globalDeps.classesUsed() )
	{
		if ( st->getNumElements() > V8MaxLiteralProperties )
			compileClassConstructor(st);
	}

	for ( StructType * st : globalDeps.classesWithBaseInfo() )
		compileClassType(st);

	for ( Type * st : globalDeps.dynAllocArrays() )
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
		if (F->getSection() != StringRef("asmjs"))
			stream << namegen.getName(F) << "();" << NewLine;
	}

	//Invoke the entry point
	if ( const Function * entryPoint = globalDeps.getEntryPoint() )
	{
		if (entryPoint->getSection() == StringRef("asmjs"))
			stream << "__asm.";
		stream << namegen.getName(entryPoint) << "();" << NewLine;
	}

	if (makeModule) {
		if (!exportedClassNames.empty()) {
			// The following JavaScript code originates from:
			// https://github.com/jashkenas/underscore/blob/master/underscore.js
			// Establish the root object, `window` (`self`) in the browser, `global`
			// on the server, or `this` in some virtual machines. We use `self`
			// instead of `window` for `WebWorker` support.
			stream << "var __root =" << NewLine;
			stream << "\ttypeof self === 'object' && self.self === self && self ||" << NewLine;
			stream << "\ttypeof global === 'object' && global.global === global && global ||" << NewLine;
			stream << "\tthis;" << NewLine;
		}

		for (StringRef &className : exportedClassNames)
		{
			stream << "__root." << className << " = " << className << ";" << NewLine;
		}

		stream << "})();" << NewLine;
	}

	if (measureTimeToMain)
	{
		stream << "console.log(\"main() called after\", __cheerp_main_time-__cheerp_start_time, \"ms\");" << NewLine;
	}


	// Link the source map if necessary
	if(sourceMapGenerator)
	{
		sourceMapGenerator->endFile();
		stream << "//# sourceMappingURL=" << sourceMapGenerator->getSourceMapName();
	}
}

Relooper* CheerpWriter::runRelooperOnFunction(const llvm::Function& F)
{
	//TODO: Support exceptions
	Function::const_iterator B=F.begin();
	Function::const_iterator BE=F.end();
	//First run, create the corresponding relooper blocks
	std::map<const BasicBlock*, /*relooper::*/Block*> relooperMap;
	int BlockId = 0;
	for(;B!=BE;++B)
	{
		if(B->isLandingPad())
			continue;
		//Decide if this block should be duplicated instead
		//of actually directing the control flow to reach it
		//Currently we just check if the block ends with a return
		//and its small enough. This should simplify some control flows.
		bool isSplittable = B->size()<3 && isa<ReturnInst>(B->getTerminator());
		//Decide if we can use a switch instead of an if/else chain for 
		//the control flow
		const TerminatorInst* term = B->getTerminator();
		// If this is not null, we can use a switch
		Value*  branchVar = nullptr;
		if(const SwitchInst* si = dyn_cast<SwitchInst>(term))
		{
			//In asm.js cases values must be in the range [-2^31,2^31),
			//and the difference between the biggest and the smaller must be < 2^31
			bool useSwitch = true;
			int64_t max = std::numeric_limits<int32_t>::min();
			int64_t min = std::numeric_limits<int32_t>::max();
			for (auto& c: si->cases())
			{
				max = std::max(max,c.getCaseValue()->getSExtValue());
				min = std::max(min,c.getCaseValue()->getSExtValue());
			}
			if ((max-min)>=(1<<31))
				useSwitch = false;
			if(useSwitch)
				branchVar = si->getCondition();
		}
		Block* rlBlock = new Block(&(*B), isSplittable, BlockId++, branchVar);
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
		if(isa<BranchInst>(term))
		{
			const BranchInst* bi=cast<BranchInst>(term);
			if(bi->isUnconditional())
				defaultBranchId = 0;
			else
				defaultBranchId = 1;
		}
		else if(isa<SwitchInst>(term))
		{
#ifndef NDEBUG
			const SwitchInst* si=cast<SwitchInst>(term);
#endif
			assert(si->getDefaultDest()==si->getSuccessor(0));
			defaultBranchId = 0;
		}
		else if(isa<InvokeInst>(term))
		{
#ifndef NDEBUG
			const InvokeInst* ii=cast<InvokeInst>(term);
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
			{
				assert(isa<SwitchInst>(term));
			}
		}
	}

	B=F.begin();
	BE=F.end();
	//Third run, add the block to the relooper and run it
	Relooper* rl=new Relooper(BlockId);
	for(;B!=BE;++B)
	{
		if(B->isLandingPad())
			continue;
		rl->AddBlock(relooperMap[&(*B)]);
	}
	rl->Calculate(relooperMap[&F.getEntryBlock()]);
	return rl;
}

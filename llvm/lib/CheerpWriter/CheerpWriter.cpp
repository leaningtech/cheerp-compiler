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
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/Support/ErrorHandling.h"

using namespace llvm;
using namespace std;
using namespace cheerp;

//De-comment this to debug the pointer kind of every function
//#define CHEERP_DEBUG_POINTERS

class CheerpRenderInterface: public RenderInterface
{
private:
	CheerpWriter* writer;
	const NewLineHandler& NewLine;
	void renderCondition(const BasicBlock* B, int branchId);
public:
	CheerpRenderInterface(CheerpWriter* w, const NewLineHandler& n):writer(w),NewLine(n)
	{
	}
	void renderBlock(const void* privateBlock);
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

	if(callV->getType()->isDoubleTy() || callV->getType()->isFloatTy())
		stream << '+';

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

		compileOperand(callV.getArgument(0));
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

		compileOperand(callV.getArgument(0));
		if(funcNameLen == 4)
		{
			// Generic setter
			assert(callV.arg_size()==3);
			stream << '[';
			compileOperand(callV.getArgument(1));
			stream << "]=";
			compileOperand(callV.getArgument(2));
		}
		else
		{
			assert(callV.arg_size()==2);
			stream << '.' << StringRef( funcName + 4, funcNameLen - 4 ) <<  '=';
			compileOperand(callV.getArgument(1));
		}
	}
	else if(className == NULL && strncmp(funcName,"Objectix",8)==0)
	{
		// operator[]
		assert(callV.arg_size()==2);
		compileOperand(callV.getArgument(0));
		stream << '[';
		compileOperand(callV.getArgument(1));
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
				compileOperand(*it);
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
		case Type::StructTyID:
		{
			if(TypeSupport::hasByteLayout(currentType))
			{
				stream << "var __tmp__=new Int8Array(";
				compileCompleteObject(baseDest, nullptr);
				stream << ".buffer);" << NewLine;
				stream << "__tmp__.set(";
				stream << "new Int8Array(";
				compileCompleteObject(baseSrc, nullptr);
				stream << ".buffer));" << NewLine;
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
		compilePointerAs(src, result_kind);
	}
	else
	{
		//Do a runtime downcast
		if(REGULAR == result_kind)
		{
			stream << "{d:";
			compileCompleteObject(src);
			stream << ".a,o:";
			compileCompleteObject(src);

			stream << ".o-(";
			compileOperand(offset);
			stream << ")}";
		}
		else
		{
			compileCompleteObject(src);
			stream << ".a[";
			compileCompleteObject(src);
			stream << ".o-(";
			compileOperand(offset);
			stream << ")]";
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

	uint64_t typeSize = targetData.getTypeAllocSize(pointedType);

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
		compileOperand(size);
		stream << '/' << typeSize;
		//Make sure to close this if below
		stream << ';' << NewLine;
	}


	// Handle the case for multiple elements, it assumes that we can use TypedArray.set
	if(!constantNumElements)
		stream << "if(__numElem__>1)" << NewLine << '{';
	if(!constantNumElements || numElem>1)
	{
		// The semantics of TypedArray.set is memmove-like, no need to care about direction
		compilePointerBase(dest);
		stream << ".set(";
		compilePointerBase(src);

		//We need to get a subview of the source
		stream << ".subarray(";
		compilePointerOffset(src);
		stream << ',';
		compilePointerOffset(src);
		stream << '+';

		// Use the size
		if(constantNumElements)
			stream << numElem;
		else
			stream << "__numElem__";

		stream << "),";
		compilePointerOffset(dest);
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

void CheerpWriter::compileAllocation(const DynamicAllocInfo & info)
{
	assert (info.isValidAlloc());

	Type * t = info.getCastedType()->getElementType();

	uint32_t typeSize = targetData.getTypeAllocSize(t);

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
		if (info.useTypedArray())
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
		assert( globalDeps.dynAllocArrays().count(t) );
		
		stream << "createArray" << namegen.getTypeName(t) << '(';
		if (info.getAllocType() == DynamicAllocInfo::cheerp_reallocate)
		{
			compilePointerBase(info.getMemoryArg());
			stream << ',';
			compilePointerBase(info.getMemoryArg());
			stream << ".length,";
			if( !info.sizeIsRuntime() )
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
		else
		{
			stream << "[],0,";
			if( info.getNumberOfElementsArg() )
				compileOperand( info.getNumberOfElementsArg() );
			else
			{
				compileOperand( info.getByteSizeArg() );
				stream << '/' << typeSize;
			}
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
			if( !info.sizeIsRuntime() )
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
		}
		else
		{
			stream << "[],0,";
			if( info.getNumberOfElementsArg() )
				compileOperand( info.getNumberOfElementsArg() );
			else
			{
				compileOperand( info.getByteSizeArg() );
				stream << '/' << typeSize;
			}
		}
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
		const Value * numberOfElems = info.getNumberOfElementsArg();
		
		uint32_t numElem;
		
		if (numberOfElems)
			numElem = getIntFromValue( numberOfElems );
		else
		{
			assert( isa<ConstantInt>( info.getByteSizeArg() ) );
			uint32_t allocatedSize = getIntFromValue( info.getByteSizeArg() );

			numElem = (allocatedSize+typeSize-1)/typeSize;
		}

		assert((REGULAR == result || SPLIT_REGULAR == result) || numElem == 1);

		if((REGULAR == result || SPLIT_REGULAR == result) && !needsDowncastArray)
			stream << '[';

		for(uint32_t i = 0; i < numElem;i++)
		{
			compileType(t, LITERAL_OBJ, !isInlineable(*info.getInstruction(), PA) ? namegen.getName(info.getInstruction()) : StringRef());
			if((i+1) < numElem)
				stream << ',';
		}

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
		if (info.useTypedArray())
		{
			stream << ';' << NewLine;
			//__ret__ now contains the new array, we need to copy over the data
			//The amount of data to copy is limited by the shortest between the old and new array
			stream << "__ret__.set(__old__.subarray(0, Math.min(__ret__.length,__old__.length)));" << NewLine;
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
	//First handle high priority builtins, they will be used even
	//if an implementation is available from the user
	if(intrinsicId==Intrinsic::memmove ||
		intrinsicId==Intrinsic::memcpy)
	{
		compileMemFunc(*(it), *(it+1), *(it+2));
		return COMPILE_EMPTY;
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
		compileCompleteObject(*it);
		stream << "={d:arguments,o:" << namegen.getName(currentFun) << ".length}";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::vaend)
	{
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
		compileCompleteObject(*it);
		stream << ".o";
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
		compilePointerOffset(*it, true);
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
		compileOperand(*(it+1));
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
		compileOperand(*it);
		stream << ')';
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::expect)
	{
		compileOperand(*it);
		return COMPILE_OK;
	}
	else if(ident=="free" || ident=="_ZdlPv" || ident=="_ZdaPv" || intrinsicId==Intrinsic::cheerp_deallocate)
	{
		compileFree(*it);
		return COMPILE_OK;
	}
	else if(ident=="fmod")
	{
		// Handle this internally, C++ does not have float mod operation
		stream << '(';
		compileOperand(*(it));
		stream << '%';
		compileOperand(*(it+1));
		stream << ')';
		return COMPILE_OK;
	}
	else if(useNativeJavaScriptMath)
	{
		if(ident=="fabs" || ident=="fabsf")
		{
			stream << "Math.abs(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="acos" || ident=="acosf")
		{
			stream << "Math.acos(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="asin" || ident=="asinf")
		{
			stream << "Math.asin(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="atan" || ident=="atanf")
		{
			stream << "Math.atan(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="atan2" || ident=="atan2f")
		{
			stream << "Math.atan2(";
			compileOperand(*(it));
			stream << ',';
			compileOperand(*(it+1));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="ceil" || ident=="ceilf")
		{
			stream << "Math.ceil(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="cos" || ident=="cosf")
		{
			stream << "Math.cos(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="exp" || ident=="expf")
		{
			stream << "Math.exp(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="floor" || ident=="floorf")
		{
			stream << "Math.floor(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="log" || ident=="logf")
		{
			stream << "Math.log(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="pow" || ident=="powf")
		{
			stream << "Math.pow(";
			compileOperand(*(it));
			stream << ',';
			compileOperand(*(it+1));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="round" || ident=="roundf")
		{
			stream << "Math.round(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="sin" || ident=="sinf")
		{
			stream << "Math.sin(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="sqrt" || ident=="sqrtf")
		{
			stream << "Math.sqrt(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="tan" || ident=="tanf")
		{
			stream << "Math.tan(";
			compileOperand(*(it));
			stream << ')';
			return COMPILE_OK;
		}
	}

	DynamicAllocInfo da(callV, &targetData);
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
			stream << '"' << str << '"';
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
	switch(p)
	{
		case CmpInst::FCMP_UEQ: //TODO: fix this, if an operand is NaN LLVM expects false,
		case CmpInst::FCMP_OEQ:
		case CmpInst::ICMP_EQ:
			stream << "===";
			break;
		case CmpInst::FCMP_UNE: //The undordered case correspond to the usual JS operator
					//See ECMA-262, Section 11.9.6
		case CmpInst::FCMP_ONE:
		case CmpInst::ICMP_NE:
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

void CheerpWriter::compileOperandForIntegerPredicate(const Value* v, CmpInst::Predicate p)
{
	assert(v->getType()->isIntegerTy());
	if(CmpInst::isSigned(p))
		compileSignedInteger(v, /*forComparison*/ true);
	else if(CmpInst::isUnsigned(p) || !v->getType()->isIntegerTy(32))
		compileUnsignedInteger(v);
	else
		compileSignedInteger(v, /*forComparison*/ true);
}

void CheerpWriter::compileEqualPointersComparison(const llvm::Value* lhs, const llvm::Value* rhs, CmpInst::Predicate p)
{
	StringRef compareString = (p == CmpInst::ICMP_NE) ? "!==" : "===";
	StringRef joinString = (p == CmpInst::ICMP_NE) ? "||" : "&&";

	POINTER_KIND lhsKind = PA.getPointerKind(lhs);
	POINTER_KIND rhsKind = PA.getPointerKind(rhs);

	if((lhsKind == REGULAR || lhsKind == SPLIT_REGULAR) &&
		(rhsKind == REGULAR || rhsKind == SPLIT_REGULAR))
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
		compilePointerOffset(lhs);
		stream << compareString;
		compilePointerOffset(rhs);
	}
	else
	{
		compilePointerAs(lhs, COMPLETE_OBJECT);
		stream << compareString;
		compilePointerAs(rhs, COMPLETE_OBJECT);
	}
}

void CheerpWriter::compileAccessToElement(Type* tp, ArrayRef< const Value* > indices, bool compileLastWrapperArray)
{
	for(uint32_t i=0;i<indices.size();i++)
	{
		if(StructType* st = dyn_cast<StructType>(tp))
		{
			// Stop when a byte layout type is found
			if (TypeSupport::hasByteLayout(st))
				return;
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
			compileOperand(indices[i]);
			stream << ">>0]";

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

		if(!isOffsetConstantZero)
			stream << '(';
		compilePointerOffset(p);
		if(!isOffsetConstantZero)
			stream << ">>0)";

		if(!isOffsetConstantZero)
		{
			stream << "+(";
			compileOperand(offset);
			stream << ">>0)";
		}

		stream << ">>0]";
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

	if(isa<Argument>(p))
	{
		stream << namegen.getName(p);
		return;
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
		compileOperand(u->getOperand(0), /*allowBooleanObjects*/ true);
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
	compileOperand(p);
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
						compileOperand( indices[i] );
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
		compileCompleteObject(p);
		stream << ".o";
	}
	return NULL;
}

void CheerpWriter::compilePointerOffset(const Value* p, bool forEscapingPointer)
{
	if ( PA.getPointerKind(p) == COMPLETE_OBJECT )
	{
		// This may still happen when doing ptrtoint of a function
		stream << '0';
		return;
	}
	bool byteLayout = PA.getPointerKind(p) == BYTE_LAYOUT;
	if ( byteLayout && !forEscapingPointer)
	{
		compileByteLayoutOffset(p, BYTE_LAYOUT_OFFSET_FULL);
		return;
	}
	else if(isGEP(p) && (!isa<Instruction>(p) || isInlineable(*cast<Instruction>(p), PA) || forEscapingPointer))
	{
		return compileGEPOffset(cast<User>(p));
	}
	else if(isBitCast(p) && (!isa<Instruction>(p) || isInlineable(*cast<Instruction>(p), PA) || forEscapingPointer))
	{
		return compileBitCastOffset(cast<User>(p));
	}
	else if (const ConstantInt* CI = PA.getConstantOffsetForPointer(p))
	{
		// Check if the offset has been constantized for this pointer
		return compileConstant(CI);
	}
	else if(isa<ConstantPointerNull>(p))
	{
		stream << '0';
		return;
	}
	else if(isa<Argument>(p))
	{
		stream << namegen.getSecondaryName(p);
		return;
	}
	else if(isa<UndefValue>(p))
	{
		stream << "undefined";
		return;
	}
	else if((isa<SelectInst> (p) && isInlineable(*cast<Instruction>(p), PA)) || (isa<ConstantExpr>(p) && cast<ConstantExpr>(p)->getOpcode() == Instruction::Select))
	{
		const User* u = cast<User>(p);
		stream << '(';
		compileOperand(u->getOperand(0), /*allowBooleanObjects*/ true);
		stream << '?';
		compilePointerOffset(u->getOperand(1));
		stream << ':';
		compilePointerOffset(u->getOperand(2));
		stream << ')';
		return;
	}
	else if((!isa<Instruction>(p) || !isInlineable(*cast<Instruction>(p), PA)) && PA.getPointerKind(p) == SPLIT_REGULAR)
	{
		stream << namegen.getSecondaryName(p);
		return;
	}
	else if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(p))
	{
		// Handle intrinsics
		switch(II->getIntrinsicID())
		{
			case Intrinsic::cheerp_upcast_collapsed:
			case Intrinsic::cheerp_cast_user:
				return compilePointerOffset(II->getOperand(0));
			case Intrinsic::cheerp_make_regular:
				return compileOperand(II->getOperand(1));
			default:
				break;
		}
	}

	compileOperand(p);
	stream << ".o";
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
			compileIntegerComparison(ce->getOperand(0), ce->getOperand(1), (CmpInst::Predicate)ce->getPredicate());
			break;
		}
		case Instruction::Select:
		{
			compileSelect(ce, ce->getOperand(0), ce->getOperand(1), ce->getOperand(2));
			break;
		}
		case Instruction::Sub:
		{
			compileSubtraction(ce->getOperand(0), ce->getOperand(1));
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
				compileOperand(CA->getOperand(i));
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

void CheerpWriter::compileConstant(const Constant* c)
{
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
						compilePointerOffset(d->getOperand(i));
				}
				else
					compilePointerAs(d->getOperand(i), k);
			}
			else if(dependOnUndefined)
				stream << "undefined";
			else
				compileOperand(d->getOperand(i));

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

		//Must compare pointers for semantics, it seems
		if(f->getValueAPF().isInfinity())
		{
			if(f->getValueAPF().isNegative())
				stream << '-';

			stream << "Infinity";
		}
		else
		{
			SmallString<32> buf;
			f->getValueAPF().toString(buf);
			stream << buf;
		}
	}
	else if(isa<ConstantInt>(c))
	{
		const ConstantInt* i=cast<ConstantInt>(c);
		if(i->getBitWidth()==1)
			stream << i->getZExtValue();
		else
			stream << i->getSExtValue();
	}
	else if(isa<ConstantPointerNull>(c))
	{
		if(PA.getPointerKind(c) == COMPLETE_OBJECT)
			stream << "null";
		else
			stream << "nullObj";
	}
	else if(isa<UndefValue>(c))
	{
		stream << "undefined";
	}
	else if(isa<GlobalAlias>(c))
	{
		const GlobalAlias* a=cast<GlobalAlias>(c);
		compileConstant(a->getAliasee());
	}
	else if(isa<GlobalValue>(c))
	{
		assert(c->hasName());
		stream << namegen.getName(c);
	}
	else if(isa<ConstantAggregateZero>(c))
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

void CheerpWriter::compileOperand(const Value* v, bool allowBooleanObjects)
{
	if(const Constant* c=dyn_cast<Constant>(v))
		compileConstant(c);
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
			if(isBooleanObject && !allowBooleanObjects)
				stream << '(';
			compileInlineableInstruction(*cast<Instruction>(v));
			if(isBooleanObject && !allowBooleanObjects)
				stream << "?1:0)";
		}
		else
		{
			if(it->getType()->isIntegerTy(1))
				stream << '(';
			stream << namegen.getName(it);
			if(it->getType()->isIntegerTy(1))
				stream << ">>0)";
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
		registerize.getRegisterId(phi)!=registerize.getRegisterId(incomingInst) ||
		phiKind!=incomingKind ||
		PA.getConstantOffsetForPointer(phi)!=PA.getConstantOffsetForPointer(incoming);
}

bool CheerpWriter::needsPointerKindConversionForBlocks(const BasicBlock* to, const BasicBlock* from)
{
	class PHIHandler: public EndOfBlockPHIHandler
	{
	public:
		PHIHandler(CheerpWriter& w):EndOfBlockPHIHandler(w.PA),writer(w)
		{
		}
		~PHIHandler()
		{
		}
		bool needsPointerKindConversion = false;
	private:
		CheerpWriter& writer;
		void handleRecursivePHIDependency(const Instruction* phi) override
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
		void handleRecursivePHIDependency(const Instruction* phi) override
		{
			if(phi->getType()->isPointerTy() && writer.PA.getPointerKind(phi)==SPLIT_REGULAR && !writer.PA.getConstantOffsetForPointer(phi))
			{
				writer.namegen.setEdgeContext(fromBB, toBB);
				writer.stream << "var " << writer.namegen.getSecondaryNameForEdge(phi);
				writer.namegen.clearEdgeContext();
				writer.stream << '=' << writer.namegen.getSecondaryName(phi) << ';' << writer.NewLine;
			}
			writer.namegen.setEdgeContext(fromBB, toBB);
			writer.stream << "var " << writer.namegen.getNameForEdge(phi);
			writer.namegen.clearEdgeContext();
			writer.stream << '=' << writer.namegen.getName(phi) << ';' << writer.NewLine;
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
					writer.stream << "var " << writer.namegen.getName(phi) << '=';
					writer.namegen.setEdgeContext(fromBB, toBB);
					writer.compilePointerBase(incoming);
				}
				else if(k==SPLIT_REGULAR)
				{
					writer.stream << "var " << writer.namegen.getSecondaryName(phi) << '=';
					writer.namegen.setEdgeContext(fromBB, toBB);
					writer.compilePointerOffset(incoming);
					writer.stream << ';' << writer.NewLine;
					writer.namegen.clearEdgeContext();
					writer.stream << "var " << writer.namegen.getName(phi) << '=';
					writer.namegen.setEdgeContext(fromBB, toBB);
					writer.compilePointerBase(incoming);
				}
				else
				{
					writer.stream << "var " << writer.namegen.getName(phi) << '=';
					writer.namegen.setEdgeContext(fromBB, toBB);
					if(k==REGULAR)
						writer.stream << "aSlot=";
					writer.compilePointerAs(incoming, k);
				}
			}
			else
			{
				writer.stream << "var " << writer.namegen.getName(phi) << '=';
				writer.namegen.setEdgeContext(fromBB, toBB);
				writer.compileOperand(incoming);
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

		if(tp->isPointerTy())
		{
			// Calling convention:
			// If this is a direct call and the argument is not a variadic one,
			// we pass the kind decided by getPointerKind(arg_it).
			// If it's variadic we use the base kind derived from the type
			// If it's indirect we use a kind good for any argument of a given type at a given position
			POINTER_KIND argKind = UNKNOWN;
			if (!F)
			{
				TypeAndIndex typeAndIndex(tp->getPointerElementType(), opCount, TypeAndIndex::ARGUMENT);
				argKind = PA.getPointerKindForArgumentTypeAndIndex(typeAndIndex);
			}
			else if (arg_it != F->arg_end())
				argKind = PA.getPointerKind(arg_it);
			else
				compilePointerAs(*cur, PA.getPointerKindForStoredType(tp));

			assert(argKind != REGULAR);
			if(argKind == SPLIT_REGULAR)
			{
				compilePointerBase(*cur, true);
				stream << ',';
				compilePointerOffset(*cur, true);
			}
			else if(argKind != UNKNOWN)
				compilePointerAs(*cur, argKind);
		}
		else if(tp->isIntegerTy(1) && forceBoolean)
		{
			stream << "!!";
			compileOperand(*cur);
		}
		else
		{
			compileOperand(*cur);
			if(tp->isIntegerTy())
				stream << ">>0";
		}

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
	switch(I.getOpcode())
	{
		case Instruction::Ret:
		{
			const ReturnInst& ri = cast<ReturnInst>(I);
			assert(I.getNumSuccessors()==0);
			Value* retVal = ri.getReturnValue();
			stream << "return ";

			if(retVal)
			{
				if(retVal->getType()->isPointerTy())
				{
					POINTER_KIND k=PA.getPointerKindForReturn(ri.getParent()->getParent());
					if(k==REGULAR)
						stream << "aSlot=";
					compilePointerAs(retVal, k);
				}
				else
				{
					compileOperand(retVal);
					if(retVal->getType()->isIntegerTy())
						stream << ">>0";
				}
			}

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

CheerpWriter::COMPILE_INSTRUCTION_FEEDBACK CheerpWriter::compileNotInlineableInstruction(const Instruction& I)
{
	switch(I.getOpcode())
	{
		case Instruction::Alloca:
		{
			const AllocaInst* ai = cast<AllocaInst>(&I);
			//V8: If the variable is passed to a call make sure that V8 does not try to SROA it
			//This can be a problem if this function or one of the called ones is deoptimized,
			//as the SROA-ed object will then be materialied with a pessimized hidden type map
			//which will then be used for all the objects with the same structure
			stream << "aSlot=";

			StringRef varName = namegen.getName(&I);
			if(PA.getPointerKind(ai) == REGULAR)
			{
				stream << "{d:[";
				compileType(ai->getAllocatedType(), LITERAL_OBJ, varName);
				stream << "],o:0}";
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
			Type* t=aggr->getType();
			if(!t->isStructTy())
			{
				llvm::errs() << "insertvalue: Expected struct, found " << *t << "\n";
				llvm::report_fatal_error("Unsupported code found, please report a bug", false);
				return COMPILE_UNSUPPORTED;
			}
			if(isa<UndefValue>(aggr))
			{
				//We have to assemble the type object from scratch
				compileType(t, LITERAL_OBJ);
				stream << ';' << NewLine;
				//Also assign the element
				assert(ivi.getNumIndices()==1);
				//Find the offset to the pointed element
				assert(ivi.hasName());
				stream << namegen.getName(&ivi);
			}
			else
			{
				//Optimize for the assembly of the aggregate values
				assert(aggr->hasOneUse());
				assert(aggr->hasName());
				stream << namegen.getName(aggr);
			}
			uint32_t offset=ivi.getIndices()[0];
			stream << '.' << types.getPrefixCharForMember(PA, cast<StructType>(t), offset) << offset;
			if(types.useWrapperArrayForMember(PA, cast<StructType>(t), offset))
				stream << "[0]";
			stream << '=';
			compileOperand(ivi.getInsertedValueOperand());
			return COMPILE_OK;
		}
		case Instruction::Store:
		{
			const StoreInst& si = cast<StoreInst>(I);
			const Value* ptrOp=si.getPointerOperand();
			const Value* valOp=si.getValueOperand();

			if (PA.getPointerKind(ptrOp) == BYTE_LAYOUT)
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
				compilePointerOffset(ptrOp);
				stream << ',';

				//Special case compilation of operand, the default behavior use =
				compileOperand(valOp);
				if(!pointedType->isIntegerTy(8))
					stream << ",true";
				stream << ')';
				return COMPILE_OK;
			}
			else
			{
				compileCompleteObject(ptrOp);
			}

			stream << '=';
			if(valOp->getType()->isIntegerTy(32))
				compileSignedInteger(valOp, /*forComparison*/ false);
			else if(valOp->getType()->isIntegerTy())
				compileUnsignedInteger(valOp);
			else if(valOp->getType()->isPointerTy())
			{
				POINTER_KIND storedKind = PA.getPointerKind(&si);
				// If regular see if we can omit the offset part
				if(storedKind==REGULAR && PA.getConstantOffsetForPointer(&si))
					compilePointerBase(valOp);
				else
					compilePointerAs(valOp, storedKind);
			}
			else
				compileOperand(valOp);
			return COMPILE_OK;
		}
		default:
		{
			COMPILE_INSTRUCTION_FEEDBACK ret=compileInlineableInstruction(I);
			if(ret == COMPILE_OK && I.getType()->isIntegerTy(1))
			{
				switch(I.getOpcode())
				{
					case Instruction::ICmp:
					case Instruction::FCmp:
					case Instruction::And:
					case Instruction::Or:
					case Instruction::Xor:
						stream << "?1:0";
						break;
					default:
						break;
				}
			}
			return ret;
		}
	}
}

void CheerpWriter::compileGEPBase(const llvm::User* gep_inst, bool forEscapingPointer)
{
	SmallVector< const Value*, 8 > indices(std::next(gep_inst->op_begin()), gep_inst->op_end());
	Type* basePointerType = gep_inst->getOperand(0)->getType();
	Type* targetType = gep_inst->getType()->getPointerElementType();

	StructType* containerStructType = dyn_cast<StructType>(GetElementPtrInst::getIndexedType(basePointerType,
			makeArrayRef(const_cast<Value* const*>(indices.begin()),
				     const_cast<Value* const*>(indices.end() - 1))));
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
		// TODO: We need to detect and block this on the clang side. In the mean time, just compile null.
		if( isa<ConstantPointerNull>(gep_inst->getOperand(0)) )
		{
			stream << "null";
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

void CheerpWriter::compileGEPOffset(const llvm::User* gep_inst)
{
	SmallVector< const Value*, 8 > indices(std::next(gep_inst->op_begin()), gep_inst->op_end());
	Type* basePointerType = gep_inst->getOperand(0)->getType();
	Type* targetType = gep_inst->getType()->getPointerElementType();

	StructType* containerStructType = dyn_cast<StructType>(GetElementPtrInst::getIndexedType(basePointerType,
			makeArrayRef(const_cast<Value* const*>(indices.begin()),
				     const_cast<Value* const*>(indices.end() - 1))));
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
			compilePointerOffset( gep_inst );
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

		// Just another pointer from this one
		compilePointerOffset(gep_inst->getOperand(0));

		if(!isOffsetConstantZero)
		{
			stream << '+';
			compileOperand(indices.front());
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
	bool useDownCastArray = false;
	bool useWrapperArray = false;
	if(containerStructType && indices.size() > 1)
	{
		assert(isa<ConstantInt>(indices.back()));
		const ConstantInt* idx = cast<ConstantInt>(indices.back());
		uint32_t lastOffsetConstant = idx->getZExtValue();
		useWrapperArray = types.useWrapperArrayForMember(PA, containerStructType, lastOffsetConstant);
		useDownCastArray = !useWrapperArray;
	}


	if(COMPLETE_OBJECT == kind)
	{
		compileCompleteObject(gep_inst->getOperand(0), indices.front());
		compileAccessToElement(gep_inst->getOperand(0)->getType()->getPointerElementType(),
		                       makeArrayRef(std::next(indices.begin()), indices.end()), /*compileLastWrapperArray*/true);
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
		compilePointerOffset( gep_inst, true);
		stream << ">>0";
		stream << '}';
	}
}

void CheerpWriter::compileSignedInteger(const llvm::Value* v, bool forComparison)
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
	if(shiftAmount==0)
	{
		//Use simpler code
		stream << '(';
		compileOperand(v);
		stream << ">>0)";
	}
	else if(forComparison)
	{
		// When comparing two signed values we can avoid the right shift
		stream << '(';
		compileOperand(v);
		stream << "<<" << shiftAmount << ')';
	}
	else
	{
		stream << "((";
		compileOperand(v);
		stream << "<<" << shiftAmount << ")>>" << shiftAmount << ')';
	}
}

void CheerpWriter::compileUnsignedInteger(const llvm::Value* v)
{
	if(const ConstantInt* C = dyn_cast<ConstantInt>(v))
	{
		stream << C->getZExtValue();
		return;
	}
	//We anyway have to use 32 bits for sign extension to work
	uint32_t initialSize = v->getType()->getIntegerBitWidth();
	stream << '(';
	if(initialSize == 32)
	{
		//Use simpler code
		compileOperand(v);
		stream << ">>>0)";
	}
	else
	{
		compileOperand(v);
		stream << '&' << getMaskForBitWidth(initialSize) << ')';
	}
}

/*
 * This can be used for both named instructions and inlined ones
 * NOTE: Call, Ret, Invoke are NEVER inlined
 */
CheerpWriter::COMPILE_INSTRUCTION_FEEDBACK CheerpWriter::compileInlineableInstruction(const Instruction& I)
{
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
			stream << '(';
			compileOperand(ci.getOperand(0));
			//Seems to be the fastest way
			//http://jsperf.com/math-floor-vs-math-round-vs-parseint/33
			stream << ">>0)";
			return COMPILE_OK;
		}
		case Instruction::FPToUI:
		{
			const CastInst& ci = cast<CastInst>(I);
			stream << '(';
			compileOperand(ci.getOperand(0));
			//Cast to signed anyway
			//ECMA-262 guarantees that (a >> 0) >>> 0
			//is the same as (a >>> 0)
			stream << ">>0)";
			return COMPILE_OK;
		}
		case Instruction::SIToFP:
		{
			const CastInst& ci = cast<CastInst>(I);
			stream << "(+";
			compileSignedInteger(ci.getOperand(0), /*forComparison*/ false);
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::UIToFP:
		{
			const CastInst& ci = cast<CastInst>(I);
			//We need to cast to unsigned before
			stream << "(+";
			compileUnsignedInteger(ci.getOperand(0));
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
				compileOperand(gep.getOperand(0));
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
			stream << "((";
			compileOperand(I.getOperand(0));
			stream << ">>0)+(";
			compileOperand(I.getOperand(1));
			stream << ">>0)";
			if(types.isI32Type(I.getType()))
				stream << ">>0";
			else
				stream << '&' << getMaskForBitWidth(I.getType()->getIntegerBitWidth());
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::FAdd:
		{
			//Double addition
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << '+';
			compileOperand(I.getOperand(1));
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Sub:
		{
			compileSubtraction(I.getOperand(0), I.getOperand(1));
			return COMPILE_OK;
		}
		case Instruction::FSub:
		{
			//Double subtraction
			//TODO: optimize negation
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << '-';
			compileOperand(I.getOperand(1));
			stream << ')';
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
				stream << '(';
				compileOperand(bi.getOperand(0));
				stream << "?1:0)";
			}
			else
			{
				//Let's mask out upper bits, to make sure we get zero extension
				//The value might have been initialized with a negative value
				compileUnsignedInteger(I.getOperand(0));
			}
			return COMPILE_OK;
		}
		case Instruction::SDiv:
		{
			//Integer signed division
			stream << "((";
			compileSignedInteger(I.getOperand(0), /*forComparison*/ false);
			stream << '/';
			compileSignedInteger(I.getOperand(1), /*forComparison*/ false);
			stream << ")>>0)";
			return COMPILE_OK;
		}
		case Instruction::UDiv:
		{
			//Integer unsigned division
			stream << "((";
			compileUnsignedInteger(I.getOperand(0));
			stream << '/';
			compileUnsignedInteger(I.getOperand(1));
			//Result is already unsigned
			stream << ")>>0)";
			return COMPILE_OK;
		}
		case Instruction::SRem:
		{
			//Integer signed remainder
			stream << "((";
			compileSignedInteger(I.getOperand(0), /*forComparison*/ false);
			stream << '%';
			compileSignedInteger(I.getOperand(1), /*forComparison*/ false);
			stream << ")>>0)";
			return COMPILE_OK;
		}
		case Instruction::URem:
		{
			//Integer unsigned remainder
			stream << "((";
			compileUnsignedInteger(I.getOperand(0));
			stream << '%';
			compileUnsignedInteger(I.getOperand(1));
			//The result is necessarily unsigned
			stream << ")>>0)";
			return COMPILE_OK;
		}
		case Instruction::FDiv:
		{
			//Double division
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << '/';
			compileOperand(I.getOperand(1));
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::FRem:
		{
			//Double division
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << '%';
			compileOperand(I.getOperand(1));
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Mul:
		{
			stream << '(';
			//Integer signed multiplication
			if(useMathImul)
			{
				stream << "Math.imul(";
				compileOperand(I.getOperand(0));
				stream << ',';
				compileOperand(I.getOperand(1));
				stream << ')';
			}
			else
			{
				stream << '(';
				compileOperand(I.getOperand(0));
				stream << '*';
				compileOperand(I.getOperand(1));
				stream << ')';
			}
			if(types.isI32Type(I.getType()))
				stream << ">>0";
			else
				stream << '&' << getMaskForBitWidth(I.getType()->getIntegerBitWidth());
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::FMul:
		{
			//Double multiplication
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << '*';
			compileOperand(I.getOperand(1));
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::ICmp:
		{
			//Integer comparison
			const CmpInst& ci = cast<CmpInst>(I);
			stream << '(';
			compileIntegerComparison(ci.getOperand(0), ci.getOperand(1), ci.getPredicate());
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::FCmp:
		{
			//Integer comparison
			const CmpInst& ci = cast<CmpInst>(I);
			//Check that the operation is JS safe
			stream << '(';
			//Special case orderedness check
			if(ci.getPredicate()==CmpInst::FCMP_ORD)
			{
				stream << "!isNaN(";
				compileOperand(ci.getOperand(0));
				stream << ")&&!isNaN(";
				compileOperand(ci.getOperand(1));
				stream << ')';
			}
			else if(ci.getPredicate()==CmpInst::FCMP_UNO)
			{
				stream << "isNaN(";
				compileOperand(ci.getOperand(0));
				stream << ")||isNaN(";
				compileOperand(ci.getOperand(1));
				stream << ')';
			}
			else
			{
				compileOperand(ci.getOperand(0));
				compilePredicate(ci.getPredicate());
				compileOperand(ci.getOperand(1));
			}
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::And:
		{
			//Integer logical and
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			compileOperand(I.getOperand(0), /*allowBooleanObjects*/ true);
			if(I.getType()->isIntegerTy(1))
				stream << "&&";
			else
				stream << '&';
			compileOperand(I.getOperand(1), /*allowBooleanObjects*/ true);
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::LShr:
		{
			//Integer logical shift right
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << ">>>";
			compileOperand(I.getOperand(1));
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::AShr:
		{
			//Integer arithmetic shift right
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			if(types.isI32Type(I.getOperand(0)->getType()))
				compileOperand(I.getOperand(0));
			else
				compileSignedInteger(I.getOperand(0), /*forComparison*/ false);
			stream << ">>";
			compileOperand(I.getOperand(1));
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Shl:
		{
			//Integer shift left
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << "<<";
			compileOperand(I.getOperand(1));
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Or:
		{
			//Integer logical or
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			compileOperand(I.getOperand(0), /*allowBooleanObjects*/ true);
			//If the type is i1 we can use the boolean operator to take advantage of logic short-circuit
			//This is possible because we know that instruction with side effects, like calls, are never inlined
			if(I.getType()->isIntegerTy(1))
				stream << "||";
			else
				stream << '|';
			compileOperand(I.getOperand(1), /*allowBooleanObjects*/ true);
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Xor:
		{
			//Integer logical xor
			//Xor with 1s is used to implement bitwise and logical negation
			//TODO: Optimize the operation with 1s
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << '^';
			compileOperand(I.getOperand(1));
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Trunc:
		{
			//Well, ideally this should not be used since, since it's a waste of bit to
			//use integers less than 32 bit wide. Still we can support it
			uint32_t finalSize = I.getType()->getIntegerBitWidth();
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << '&' << getMaskForBitWidth(finalSize) << ')';
			return COMPILE_OK;
		}
		case Instruction::SExt:
		{
			//We can use a couple of shift to make this work
			compileSignedInteger(I.getOperand(0), /*forComparison*/ false);
			return COMPILE_OK;
		}
		case Instruction::Select:
		{
			const SelectInst& si = cast<SelectInst>(I);
			compileSelect(&si, si.getCondition(), si.getTrueValue(), si.getFalseValue());
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

			compileOperand(aggr);

			uint32_t offset=evi.getIndices()[0];
			stream << '.' << types.getPrefixCharForMember(PA, cast<StructType>(t), offset) << offset;
			if(types.useWrapperArrayForMember(PA, cast<StructType>(t), offset))
				stream << "[0]";
			return COMPILE_OK;
		}
		case Instruction::FPExt:
		{
			const Value* src=I.getOperand(0);
			compileOperand(src);
			return COMPILE_OK;
		}
		case Instruction::FPTrunc:
		{
			const Value* src=I.getOperand(0);
			compileOperand(src);
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
			stream << "handleVAArg(";
			compileCompleteObject(vi.getPointerOperand());
			stream << ')';
			
			assert( globalDeps.needHandleVAArg() );
			return COMPILE_OK;
		}
		case Instruction::Call:
		{
			const CallInst& ci = cast<CallInst>(I);
			const Function * calledFunc = ci.getCalledFunction();
			if(calledFunc)
			{
				//Direct call
				COMPILE_INSTRUCTION_FEEDBACK cf=handleBuiltinCall(&ci, calledFunc);
				if(cf!=COMPILE_UNSUPPORTED)
					return cf;
				stream << namegen.getName(calledFunc);
			}
			else
			{
				//Indirect call
				compilePointerAs(ci.getCalledValue(), COMPLETE_OBJECT);
			}
			//If we are dealing with inline asm we are done
			if(!ci.isInlineAsm())
			{
				compileMethodArgs(ci.op_begin(),ci.op_begin()+ci.getNumArgOperands(), &ci, /*forceBoolean*/ false);
			}
			return COMPILE_OK;
		}
		case Instruction::Load:
		{
			const LoadInst& li = cast<LoadInst>(I);
			const Value* ptrOp=li.getPointerOperand();
			stream << '(';

			if (PA.getPointerKind(ptrOp) == BYTE_LAYOUT)
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
				compilePointerOffset(ptrOp);
				if(!pointedType->isIntegerTy(8))
					stream << ",true";
				stream << ')';
			}
			else
			{
				compileCompleteObject(ptrOp);
			}
			if(li.getType()->isIntegerTy())
			{
				uint32_t width = li.getType()->getIntegerBitWidth();
				// 32-bit integers are all loaded as signed, other integers as unsigned
				if(width==32)
					stream << ">>0";
				else
					stream << '&' << getMaskForBitWidth(width);
			}

			stream << ')';
			return COMPILE_OK;
		}
		default:
			stream << "alert('Unsupported code')";
			llvm::errs() << "\tImplement inst " << I.getOpcodeName() << '\n';
			return COMPILE_UNSUPPORTED;
	}
}

void CheerpWriter::compileBB(const BasicBlock& BB, const std::map<const BasicBlock*, uint32_t>& blocksMap)
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
			stream << "var " << namegen.getName(I) << '=';
		}
		if(I->isTerminator())
		{
			compileTerminatorInstruction(*dyn_cast<TerminatorInst>(I));
		}
		else if(!I->use_empty() || I->mayHaveSideEffects())
		{
			COMPILE_INSTRUCTION_FEEDBACK ret=compileNotInlineableInstruction(*I);

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
	std::map<const BasicBlock*, uint32_t> blocksMap;
	writer->compileBB(*bb, blocksMap);
}

void CheerpRenderInterface::renderCondition(const BasicBlock* bb, int branchId)
{
	const TerminatorInst* term=bb->getTerminator();

	if(isa<BranchInst>(term))
	{
		const BranchInst* bi=cast<BranchInst>(term);
		assert(bi->isConditional());
		//The second branch is the default
		assert(branchId==0);
		writer->compileOperand(bi->getCondition(), /*allowBooleanObjects*/ true);
	}
	else if(isa<SwitchInst>(term))
	{
		const SwitchInst* si=cast<SwitchInst>(term);
		assert(branchId > 0);
		SwitchInst::ConstCaseIt it=si->case_begin();
		for(int i=1;i<branchId;i++)
			++it;
		const BasicBlock* dest=it.getCaseSuccessor();
		writer->compileOperandForIntegerPredicate(si->getCondition(), CmpInst::ICMP_EQ);
		writer->stream << "===";
		writer->compileOperandForIntegerPredicate(it.getCaseValue(), CmpInst::ICMP_EQ);
		//We found the destination, there may be more cases for the same
		//destination though
		for(++it;it!=si->case_end();++it)
		{
			if(it.getCaseSuccessor()==dest)
			{
				//Also add this condition
				writer->stream << "||(";
				writer->compileOperandForIntegerPredicate(si->getCondition(), CmpInst::ICMP_EQ);
				writer->stream << "===";
				writer->compileOperandForIntegerPredicate(it.getCaseValue(), CmpInst::ICMP_EQ);
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

void CheerpRenderInterface::renderIfBlockBegin(const void* privateBlock, int branchId, bool first)
{
	const BasicBlock* bb=(const BasicBlock*)privateBlock;
	if(!first)
		writer->stream << "}else ";
	writer->stream << "if(";
	renderCondition(bb, branchId);
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
		renderCondition(bb, skipBranchIds[i]);
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
	writer->stream << "label=" << labelId << ';' << NewLine;
}

void CheerpRenderInterface::renderIfOnLabel(int labelId, bool first)
{
	if(first==false)
		writer->stream << "else ";
	writer->stream << "if(label===" << labelId << "){" << NewLine;
}

void CheerpWriter::compileMethod(const Function& F)
{
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
	std::map<const BasicBlock*, uint32_t> blocksMap;
	if(F.size()==1)
		compileBB(*F.begin(), blocksMap);
	else
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
			Block* rlBlock = new Block(&(*B), isSplittable, BlockId++);
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
		if(rl->needsLabel())
			stream << "var label=0;" << NewLine;
		
		CheerpRenderInterface ri(this, NewLine);
		rl->Render(&ri);
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
				POINTER_KIND elementPointerKind = PA.getPointerKindForStoredType((*it)->get()->getType()->getPointerElementType());
				return GlobalSubExprInfo{elementPointerKind, false};
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
				return GlobalSubExprInfo{elementPointerKind, hasConstantOffset};
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
	if(TypeSupport::isClientGlobal(&G) )
	{
		//Global objects in the client namespace are only
		//placeholders for JS calls
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
				compileOperand(C);
			stream << "],o:0}";
		}
		else if(k == SPLIT_REGULAR)
		{
			stream << '[';
			if(C->getType()->isPointerTy())
				compilePointerAs(C, PA.getPointerKindForStoredType(C->getType()));
			else
				compileOperand(C);
			stream << ']';
			stream << ';' << NewLine;
			stream  << "var " << namegen.getSecondaryName(&G);
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
				stream << "create" << namegen.getTypeName(st) << '(';
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
				compilePointerOffset(valOp);
			}
			else
				compilePointerAs(valOp, subExprInfo.kind);
		}
		else
			compileOperand(valOp);
		stream << ';' << NewLine;
	}
}

void CheerpWriter::compileNullPtrs()
{
	stream << "var aSlot=null;var nullArray=[null];var nullObj={d:nullArray,o:0};" << NewLine;
}

void CheerpWriter::compileCreateClosure()
{
	stream << "function cheerpCreateClosure(func, obj){return function(e){func(obj,e);};}" << NewLine;
}

void CheerpWriter::compileHandleVAArg()
{
	stream << "function handleVAArg(ptr){var ret=ptr.d[ptr.o];ptr.o++;return ret;}" << NewLine;
}

void CheerpWriter::makeJS()
{
	if (sourceMapGenerator)
		sourceMapGenerator->beginFile();

	if (makeModule)
		stream << "(function(){" << NewLine;

	// Enable strict mode first
	stream << "\"use strict\";" << NewLine;

	if(addCredits)
		stream << "/*Compiled using Cheerp (R) by Leaning Technologies Ltd*/" << NewLine;

	std::vector<StringRef> exportedClassNames = compileClassesExportedToJs();
	compileNullPtrs();
	
	for ( const Function & F : module.getFunctionList() )
		if (!F.empty())
		{
#ifdef CHEERP_DEBUG_POINTERS
			dumpAllPointers(F, PA);
#endif //CHEERP_DEBUG_POINTERS
			compileMethod(F);
		}
	
	for ( const GlobalVariable & GV : module.getGlobalList() )
		compileGlobal(GV);

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
		stream << namegen.getName(F) << "();" << NewLine;
	}

	//Invoke the entry point
	if ( const Function * entryPoint = globalDeps.getEntryPoint() )
		stream << namegen.getName(entryPoint) << "();" << NewLine;

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

	// Link the source map if necessary
	if(sourceMapGenerator)
	{
		sourceMapGenerator->endFile();
		stream << "//# sourceMappingURL=" << sourceMapGenerator->getSourceMapName();
	}
}

//===-- CheerpWriter.cpp - The Cheerp JavaScript generator -------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2014 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "Relooper.h"
#include "llvm/ADT/StringExtras.h"
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
	if(strncmp(funcName,"get_",4)==0 && callV.arg_size()==1)
	{
		//Getter
		if(className == NULL)
		{
			llvm::report_fatal_error(Twine("Unexpected getter without class: ", StringRef(identifier)), false);
			return;
		}

		compileOperand(callV.getArgument(0));
		stream << '.' << StringRef( funcName + 4, funcNameLen - 4 );
	}
	else if(strncmp(funcName,"set_",4)==0 && callV.arg_size()==2)
	{
		//Setter
		if(className == NULL)
		{
			llvm::report_fatal_error(Twine("Unexpected setter without class: ", StringRef(identifier)), false);
			return;
		}

		compileOperand(callV.getArgument(0));
		stream << '.' << StringRef( funcName + 4, funcNameLen - 4 ) <<  '=';
		compileOperand(callV.getArgument(1));
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
		compileMethodArgs(it,callV.arg_end(), callV);
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
	const APInt& baseOffset = cast<Constant>(callV.getArgument(1))->getUniqueInteger();

	Type* t=src->getType()->getPointerElementType();

	if(TypeSupport::isClientType(t) || baseOffset == 0)
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
			stream << ".o-" << baseOffset << '}';
		}
		else
		{
			compileCompleteObject(src);
			stream << ".a[";
			compileCompleteObject(src);
			stream << ".o-" << baseOffset;
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
	if(!(types.isTypedArrayType(pointedType) || TypeSupport::hasByteLayout(pointedType)))
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

	if(result == REGULAR)
	{
		stream << "{d:";
	}

	// To implement realloc we need to strategies:
	// 1) Immutable types are stored in typed array which cannot be resized, we need to make a new one
	//    and copy the old data over
	// 2) Objects and pointers are stored in a regular array and we can just resize them
	if (info.getAllocType() == DynamicAllocInfo::realloc)
	{
		stream << "(function(){";
		stream << "var __old__=";
		compilePointerBase(info.getMemoryArg());
		stream << ';' << NewLine;
		if (!info.useTypedArray())
		{
			stream << "var __len__=__old__.length; " << NewLine;
			stream << "__old__.length = ";
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
			stream << ';' << NewLine;
		}
		else
		{
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
		assert( t->isStructTy() );
		StructType* st = cast<StructType>(t);
		
		assert( globalDeps.dynAllocArrays().count(st) );
		
		stream << "createArray" << namegen.filterLLVMName(st->getName(), true);
		stream << '(';
		if (info.getAllocType() == DynamicAllocInfo::realloc)
			stream << "__old__,__len__)";
		else
		{
			stream << "new Array(";
			if( info.getNumberOfElementsArg() )
				compileOperand( info.getNumberOfElementsArg() );
			else
			{
				compileOperand( info.getByteSizeArg() );
				stream << '/' << typeSize;
			}
			stream << "),0)";
		}
	}
	else if (info.useCreatePointerArrayFunc() )
	{
		stream << "createPointerArray(";
		if (info.getAllocType() == DynamicAllocInfo::realloc)
			stream << "__old__,__len__)";
		else
		{
			stream << "new Array(";
			if( info.getNumberOfElementsArg() )
				compileOperand( info.getNumberOfElementsArg() );
			else
			{
				compileOperand( info.getByteSizeArg() );
				stream << '/' << typeSize;
			}
			stream << "),0)";
		}
	
		assert( globalDeps.needCreatePointerArray() );
	}
	else if (!info.sizeIsRuntime() )
	{
		assert( info.getAllocType() != DynamicAllocInfo::realloc );
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

		assert(REGULAR == result || numElem == 1);

		if(REGULAR == result)
			stream << '[';

		for(uint32_t i = 0; i < numElem;i++)
		{
			compileType(t, LITERAL_OBJ);
			if((i+1) < numElem)
				stream << ',';
		}

		if(REGULAR == result)
			stream << ']';
	}
	else
	{
		llvm::errs() << "Allocating type " << *t << "\n";
		llvm::report_fatal_error("Unsupported type in allocation", false);
	}

	if (info.getAllocType() == DynamicAllocInfo::realloc)
	{
		stream << ';' << NewLine;
		if (info.useTypedArray())
		{
			//__ret__ now contains the new array, we need to copy over the data
			//The amount of data to copy is limited by the shortest between the old and new array
			stream << "__ret__.set(__old__.subarray(0, Math.min(__ret__.length,__old__.length)));" << NewLine;
			stream << "return __ret__;})()";
		}
		else
			stream << "return __old__;})()";
	}

	if(result == REGULAR)
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
	
	const char* ident = func->getName().data();
	unsigned instrinsicId = func->getIntrinsicID();
	//First handle high priority builtins, they will be used even
	//if an implementation is available from the user
	if(instrinsicId==Intrinsic::memmove ||
		instrinsicId==Intrinsic::memcpy)
	{
		compileMemFunc(*(it), *(it+1), *(it+2));
		return COMPILE_EMPTY;
	}
	else if(instrinsicId==Intrinsic::memset)
	{
		llvm::report_fatal_error("Unsupported memory intrinsic, please rebuild the code using an updated version of Cheerp", false);
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
		compileCompleteObject(*it);
		stream << "={d:arguments,o:" << namegen.getName(currentFun) << ".length}";
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::vaend)
	{
		compileCompleteObject(*it);
		stream << "=null";
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::cheerp_downcast)
	{
		compileDowncast( callV );
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::cheerp_upcast_collapsed)
	{
		compilePointerAs(*it, PA.getPointerKind(callV.getInstruction()));
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::cheerp_cast_user)
	{
		//HACK to make client array works, we produce a regular even if we have a client object
		// This is to emulate the old COMPLETE_ARRAY, we should actually make this cast return a pointer to array
		if(TypeSupport::isClientArrayType(callV.getArgument(0)->getType()->getPointerElementType()) &&
		                PA.getPointerKind(callV.getInstruction()) == REGULAR)
		{
			stream << "{d:";
			compileCompleteObject(callV.getArgument(0));
			stream << ",o:0}";
		}
		else
		{
			compilePointerAs(*it, PA.getPointerKind(callV.getInstruction()));
		}

		return COMPILE_OK;

		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::cheerp_pointer_base)
	{
		compilePointerBase(*it);
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::cheerp_pointer_offset)
	{
		compilePointerOffset(*it);
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::cheerp_create_closure)
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
	else if(instrinsicId==Intrinsic::cheerp_make_complete_object)
	{
		compileCompleteObject(*it);
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::cheerp_element_distance)
	{
		// TODO: Eliminate this intrinsic in a pre-processing step
		stream << '1';
		return COMPILE_OK;
	}
	else if(instrinsicId==Intrinsic::flt_rounds)
	{
		// Rounding mode 1: nearest
		stream << '1';
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
		handleBuiltinNamespace(ident+10,callV);
		return COMPILE_OK;
	}
	else if(strncmp(ident,"_ZNK6client",11)==0)
	{
		handleBuiltinNamespace(ident+11,callV);
		return COMPILE_OK;
	}
	else if(strncmp(ident,"cheerpCreate_ZN6client",22)==0)
	{
		//Default handling of builtin constructors
		char* typeName;
		int typeLen=strtol(ident+22,&typeName,10);
		//For builtin String, do not use new
		if(strncmp(typeName, "String", 6)!=0)
			stream << "new ";
		stream << StringRef(typeName, typeLen);
		compileMethodArgs(it, itE, callV);
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
	if(CmpInst::isUnsigned(p))
		compileUnsignedInteger(v);
	else
		compileSignedInteger(v);
}

void CheerpWriter::compileEqualPointersComparison(const llvm::Value* lhs, const llvm::Value* rhs, CmpInst::Predicate p)
{
	StringRef compareString = (p == CmpInst::ICMP_NE) ? "!==" : "===";
	StringRef joinString = (p == CmpInst::ICMP_NE) ? " || " : " && ";

	if(PA.getPointerKind(lhs) == REGULAR &&
	                PA.getPointerKind(rhs) == REGULAR)
	{
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

void CheerpWriter::compileAccessToElement(Type* tp, ArrayRef< const Value* > indices)
{
	for(const Value * idx : indices)
	{
		if(StructType* st = dyn_cast<StructType>(tp))
		{
			// Stop when a byte layout type is found
			if (TypeSupport::hasByteLayout(st))
				return;
			assert(isa<ConstantInt>(idx));
			const APInt& index = cast<Constant>(idx)->getUniqueInteger();

			stream << ".a" << index;

			tp = st->getElementType(index.getZExtValue());
		}
		else if(const ArrayType* at = dyn_cast<ArrayType>(tp))
		{
			stream << '[';
			compileOperand(idx);
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
		stream << "\"a" << idx->getZExtValue() << '\"';
	}
	else
	{
		compileOperand(indices.back());
	}
}

/**
 * Return the GEP this value comes from (propagating across bitcasts/nopcasts).
 * If the value is not a GEP, return null
 */
static const User* propagate_till_gep(const Value* p)
{
	const Value* q = p;

	while((isNopCast(q) || isBitCast(q)) && (!isa<Instruction>(q) || isInlineable(*cast<Instruction>(q))))
	{
		q = cast<User>(q)->getOperand(0);
	}

	return isGEP(q) && (!isa<Instruction>(q) || isInlineable(*cast<Instruction>(q))) ? cast<User>(q) : nullptr;
}

void CheerpWriter::compileCompleteObject(const Value* p, const Value* offset)
{
	// Special handle for undefined pointers
	if(isa<UndefValue>(p))
	{
		compileOperand(p);
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
		if(const User* gepInst = propagate_till_gep(p))
		{
			compileGEP(gepInst, COMPLETE_OBJECT);
			return;
		}
	}

	if(PA.getPointerKind(p) == REGULAR)
	{
		// Determine if we need to add a runtime check to avoid adding 0
		// This is an hack to avoid adding a zero offset to a literal offset,
		// i.e. to avoid things like "a0" + 0 ( "a00" ).
		bool isChecked = offset && (!isa<Constant>(offset));

		compilePointerBase(p);
		stream << '[';

		if(isChecked)
		{
			compileOperand(offset);
			stream << "===0?";
			compilePointerOffset(p);
			stream <<':';
		}

		compilePointerOffset(p);

		if(!isOffsetConstantZero)
		{
			stream << '+';
			compileOperand(offset);
		}

		stream << ']';
	}
	else
	{
		compileOperand(p);

		if(!isOffsetConstantZero)
		{
			llvm::errs() << "Can not access a COMPLETE_OBJECT pointer with non zero offset:" << *offset << "\n";
			llvm::report_fatal_error("Unsupported code found, please report a bug", false);
		}
	}
}

void CheerpWriter::compilePointerBase(const Value* p)
{
	bool byteLayout = PA.getPointerKind(p) == BYTE_LAYOUT;
	// If the value has byte layout skip GEPS and BitCasts until the base is found
	if ( byteLayout )
	{
		bool byteLayoutFromHere = false;
		while ( isBitCast(p) || isGEP(p) )
		{
			const Value* operand = cast<User>(p)->getOperand(0);
			if ( PA.getPointerKind(operand) != BYTE_LAYOUT)
			{
				byteLayoutFromHere = true;
				break;
			}
			p = operand;
		}
		if ( !byteLayoutFromHere )
			return compilePointerBase(p);
		else if ( isBitCast(p))
			compileCompleteObject(cast<User>(p)->getOperand(0));
		else
			compileCompleteObject(p);
		return;
	}
	// Collapse if p is a gepInst
	while(const User* gepInst = propagate_till_gep(p))
	{
		assert(gepInst->getNumOperands() > 1);

		// Load the indices
		SmallVector< const Value*, 8 > indices(std::next(gepInst->op_begin()), gepInst->op_end());

		if(indices.size() > 1)
		{
			// If we have gep(val, idx, ..., iN ) the base is always gep(val, idx, ... )
			// we want to write val. access_expression
			compileCompleteObject(gepInst->getOperand(0), indices.front());
			compileAccessToElement(gepInst->getOperand(0)->getType()->getPointerElementType(),
			                       makeArrayRef(std::next(indices.begin()), std::prev(indices.end())));

			// We are done
			return;
		}
		else
		{
			// If we have gep(val, idx) the base is val regardless of the value of idx
			p = gepInst->getOperand(0);
		}
	}

	if(PA.getPointerKind(p) != REGULAR)
	{
		llvm::errs() << "compilePointerBase with COMPLETE_OBJECT pointer:" << *p << '\n' << "In function: " << *currentFun << '\n';
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}

	// If value has not been generated from a GEP, just compile it and ask for .d
	compileOperand(p);
	stream << ".d";
}

const Value* CheerpWriter::compileByteLayoutOffset(const Value* p, BYTE_LAYOUT_OFFSET_MODE offsetMode)
{
	// If the value has byte layout skip GEPS and BitCasts until the base is found
	// We need to handle the first GEP having more than an index (so it actually changes types)
	// to support BYTE_LAYOUT_OFFSET_STOP_AT_ARRAY
	// If offsetMode is BYTE_LAYOUT_OFFSET_FULL we can treat every GEP in the same way
	bool findFirstTypeChangingGEP = (offsetMode == BYTE_LAYOUT_OFFSET_STOP_AT_ARRAY);
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
					if (!skipUntilBytelayout)
						stream << SL->getElementOffset(index) << '+';
					curType = ST->getElementType(index);
				}
				else
				{
					if (findFirstTypeChangingGEP && indices.size() > 1 && i == (indices.size() - 1))
					{
						assert (curType->isArrayTy());
						assert (offsetMode == BYTE_LAYOUT_OFFSET_STOP_AT_ARRAY);
						// We have found an array just before the last type, the last offset will be returned instead of used directly.
						lastOffset = indices[i];
						break;
					}
					// This case also handles the first index
					if (!skipUntilBytelayout)
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
			stream << '0';
			return lastOffset;
		}
		p = u->getOperand(0);
		continue;
	}
	assert (PA.getPointerKind(p) == BYTE_LAYOUT);
	stream << ".o";
	return NULL;
}

void CheerpWriter::compilePointerOffset(const Value* p)
{
	if ( PA.getPointerKind(p) == COMPLETE_OBJECT )
	{
		// This may still happen when doing ptrtoint of a function
		stream << '0';
		return;
	}

	bool byteLayout = PA.getPointerKind(p) == BYTE_LAYOUT;
	if ( byteLayout )
	{
		compileByteLayoutOffset(p, BYTE_LAYOUT_OFFSET_FULL);
		return;
	}
	const User* gep_inst = propagate_till_gep(p);

	if(gep_inst)
	{
		SmallVector< const Value*, 8 > indices(std::next(gep_inst->op_begin()), gep_inst->op_end());

		if(indices.size() == 1)
		{
			compilePointerOffset(gep_inst->getOperand(0));

			bool isOffsetConstantZero = isa<Constant>(indices.front()) && cast<Constant>(indices.front())->isNullValue();

			if(!isOffsetConstantZero)
			{
				stream << '+';
				compileOperand(indices.front());
			}
		}
		else
		{
			compileOffsetForGEP(gep_inst->getOperand(0)->getType(), indices);
		}
	}
	else
	{
		compileOperand(p);
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
			Value* val=ce->getOperand(0);
			Type* dst=ce->getType();
			Type* src=val->getType();

			if(!TypeSupport::isValidTypeCast(val, dst))
			{
				llvm::errs() << "Between:\n\t" << *src << "\n\t" << *dst << "\n";
				llvm::errs() << "warning: Type conversion is not safe, expect issues. And report a bug.\n";
			}

			compilePointerAs(val, PA.getPointerKind(ce));
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
			stream << "undefined";
			llvm::errs() << "warning: Unsupported constant expr " << ce->getOpcodeName() << '\n';
	}
}

void CheerpWriter::compileConstant(const Constant* c)
{
	if(isa<ConstantExpr>(c))
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

		for(uint32_t i=0;i<d->getNumElements();i++)
		{
			compileConstant(d->getElementAsConstant(i));

			if((i+1)<d->getNumElements())
				stream << ',';
		}

		stream << "])";
	}
	else if(isa<ConstantArray>(c))
	{
		const ConstantArray* d=cast<ConstantArray>(c);
		stream << '[';
		assert(d->getType()->getNumElements() == d->getNumOperands());

		for(uint32_t i=0;i<d->getNumOperands();i++)
		{
			compileOperand(d->getOperand(i));

			if((i+1)<d->getNumOperands())
				stream << ',';
		}

		stream << ']';
	}
	else if(isa<ConstantStruct>(c))
	{
		const ConstantStruct* d=cast<ConstantStruct>(c);
		stream << '{';
		assert(d->getType()->getNumElements() == d->getNumOperands());

		for(uint32_t i=0;i<d->getNumOperands();i++)
		{
			stream << 'a' << i << ':';
			compileOperand(d->getOperand(i));

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
			stream << (i->isZero()?"false":"true");
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
		//Check if this is a client global value, if so skip mangling
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

void CheerpWriter::compileOperand(const Value* v)
{
	if(const Constant* c=dyn_cast<Constant>(v))
	{
		if(!currentFun && isa<GlobalVariable>(v) && !compiledGVars.count(cast<GlobalVariable>(v)))
		{
			// If we are compiling a constant expr for a GVar, and v has not been defined yet
			// just print undefined
			stream << "undefined";
			return;
		}

		compileConstant(c);
	}
	else if(const Instruction* it=dyn_cast<Instruction>(v))
	{
		if(isInlineable(*it))
			compileInlineableInstruction(*cast<Instruction>(v));
		else
			stream << namegen.getName(it);
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

void CheerpWriter::compilePHIOfBlockFromOtherBlock(const BasicBlock* to, const BasicBlock* from)
{
	BasicBlock::const_iterator I=to->begin();
	BasicBlock::const_iterator IE=to->end();
	SmallVector<uint32_t, 4> tmps;
	//Phase 1, use temporaries to store the results of PHIs
	for(;I!=IE;++I)
	{
		const PHINode* phi=dyn_cast<PHINode>(I);
		//TODO: I think that after the first non-phi node we can stop
		if(phi==NULL)
			continue;
		const Value* val=phi->getIncomingValueForBlock(from);
		// We only need temporaries for PHIs from this same block as they may be mutually dependent
		if (!isa<PHINode>(val) || cast<PHINode>(val)->getParent()!=to)
			continue;
		uint32_t tmpIndex = namegen.getUniqueIndexForPHI( currentFun );
		stream << "var tmpphi" << tmpIndex << '=';
		tmps.push_back(tmpIndex);

		if(phi->getType()->isPointerTy())
		{
			compilePointerAs(val, PA.getPointerKind(phi));
		}
		else
		{
			compileOperand(val);
		}

		stream << ';' << NewLine;
	}
	//Phase 2, actually assign the values
	I=to->begin();
	for(uint32_t tmpI=0;I!=IE;++I)
	{
		const PHINode* phi=dyn_cast<PHINode>(I);
		if(phi==NULL)
			continue;
		stream << "var " << namegen.getName(phi);
		const Value* val=phi->getIncomingValueForBlock(from);
		stream << '=';
		if (!isa<PHINode>(val) || cast<PHINode>(val)->getParent()!=to)
		{
			if(phi->getType()->isPointerTy())
				compilePointerAs(val, PA.getPointerKind(phi));
			else
				compileOperand(val);
		}
		else
		{
			stream << "tmpphi" << tmps[tmpI];
			tmpI++;
		}
		stream << ';' << NewLine;
	}
}

void CheerpWriter::compileMethodArgs(User::const_op_iterator it, User::const_op_iterator itE, ImmutableCallSite callV)
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

	for(User::const_op_iterator cur=it; cur!=itE; ++cur)
	{
		if(cur!=it)
			stream << ',';

		Type* tp = (*cur)->getType();

		if(tp->isPointerTy())
		{
			// Calling convention:
			// If this is a direct call and the argument is not a variadic one,
			// we pass the kind decided by getPointerKind(arg_it).
			// Otherwise we pass the generic kind for the pointer type
			// (that is: CO if client or function pointer, REGULAR otherwise )
			POINTER_KIND k = (F && arg_it != F->arg_end()) ?
			                 PA.getPointerKind(arg_it) :
			                 PA.getPointerKindForType(tp->getPointerElementType());
			compilePointerAs(*cur, k);
		}
		else
		{
			compileOperand(*cur);
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
					compilePointerAs(retVal, PA.getPointerKindForReturn(ri.getParent()->getParent()));
				}
				else
				{
					compileOperand(retVal);
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

			compileMethodArgs(ci.op_begin(),ci.op_begin()+ci.getNumArgOperands(),&ci);
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

			if(PA.getPointerKind(ai) == REGULAR)
			{
				stream << "{d:[";
				compileType(ai->getAllocatedType(), LITERAL_OBJ);
				stream << "],o:0}";
			}
			else 
				compileType( ai->getAllocatedType(), LITERAL_OBJ);

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
				compileMethodArgs(ci.op_begin(),ci.op_begin()+ci.getNumArgOperands(), &ci);
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
			stream << ".a" << offset << '=';
			compileOperand(ivi.getInsertedValueOperand());
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
					stream << ",true)";
			}
			else
			{
				compileCompleteObject(ptrOp);
			}

			stream << ')';
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
			compileOperand(valOp);
			return COMPILE_OK;
		}
		default:
			return compileInlineableInstruction(I)?COMPILE_OK:COMPILE_UNSUPPORTED;
	}
}

void CheerpWriter::compileGEP(const llvm::User* gep_inst, POINTER_KIND kind)
{
	SmallVector< const Value*, 8 > indices(std::next(gep_inst->op_begin()), gep_inst->op_end());
	Type * targetType = gep_inst->getType()->getPointerElementType();

	if(COMPLETE_OBJECT == kind)
	{
		compileCompleteObject(gep_inst->getOperand(0), indices.front());
		compileAccessToElement(gep_inst->getOperand(0)->getType()->getPointerElementType(),
		                       makeArrayRef(std::next(indices.begin()), indices.end()));
	}
	else
	{
		bool byteLayout = PA.getPointerKind(gep_inst) == BYTE_LAYOUT;
		if (byteLayout)
		{
			if (TypeSupport::hasByteLayout(targetType))
			{
				stream << "{d:";
				compilePointerBase( gep_inst );
				stream << ",o:";
				compilePointerOffset( gep_inst );
				stream << '}';
			}
			else
			{
				assert(TypeSupport::isTypedArrayType(targetType));
				stream << "{d:";
				// Forge an appropiate typed array
				assert (!TypeSupport::hasByteLayout(targetType));
				stream << "new ";
				compileTypedArrayType(targetType);
				stream << '(';
				compilePointerBase( gep_inst );
				stream << ".buffer,";
				// If this GEP or a previous one passed through an array of immutables generate a regular from
				// the start of the array and not from the pointed element
				const Value* lastOffset = compileByteLayoutOffset( gep_inst, BYTE_LAYOUT_OFFSET_STOP_AT_ARRAY );
				stream << "),o:";
				if (lastOffset)
					compileOperand(lastOffset);
				else
					stream << '0';
				stream << '}';
			}
		}
		else if (indices.size() == 1)
		{
			bool isOffsetConstantZero = isa<Constant>(indices.front()) && cast<Constant>(indices.front())->isNullValue();

			// Just another pointer from this one
			stream << "{d:";
			compilePointerBase(gep_inst->getOperand(0));
			stream << ",o:";
			compilePointerOffset(gep_inst->getOperand(0));

			if(!isOffsetConstantZero)
			{
				stream << '+';
				compileOperand(indices.front());
			}

			stream << '}';
		}
		else
		{
			bool hasBasesInfo = isa<StructType>(targetType) && types.hasBasesInfo(cast<StructType>(targetType));
			stream << "{d:";
			compileCompleteObject(gep_inst->getOperand(0), indices.front());
			if (hasBasesInfo)
			{
				compileAccessToElement(gep_inst->getOperand(0)->getType()->getPointerElementType(),
						makeArrayRef(std::next(indices.begin()),indices.end()));
				stream << ".a";
			}
			else
			{
				compileAccessToElement(gep_inst->getOperand(0)->getType()->getPointerElementType(),
						makeArrayRef(std::next(indices.begin()),std::prev(indices.end())));
			}
			stream << ",o:";
			if (hasBasesInfo)
			{
				compileCompleteObject(gep_inst->getOperand(0), indices.front());
				compileAccessToElement(gep_inst->getOperand(0)->getType()->getPointerElementType(),
						makeArrayRef(std::next(indices.begin()), indices.end()));
				stream << ".o";
			}
			else
				compileOffsetForGEP(gep_inst->getOperand(0)->getType(), indices);
			stream << '}';
		}
	}
}

void CheerpWriter::compileSignedInteger(const llvm::Value* v)
{
	//We anyway have to use 32 bits for sign extension to work
	uint32_t shiftAmount = 32-v->getType()->getIntegerBitWidth();
	if(shiftAmount==0)
	{
		//Use simpler code
		stream << '(';
		compileOperand(v);
		stream << ">>0)";
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
bool CheerpWriter::compileInlineableInstruction(const Instruction& I)
{
	switch(I.getOpcode())
	{
		case Instruction::BitCast:
		{
			const BitCastInst& bi = cast<BitCastInst>(I);
			Type* src=bi.getSrcTy();
			Type* dst=bi.getDestTy();
			if(!TypeSupport::isValidTypeCast(bi.getOperand(0), dst))
			{
				llvm::errs() << "Between:\n\t" << *src << "\n\t" << *dst << "\n";
				llvm::errs() << "warning: Type conversion is not safe, expect issues. And report a bug.\n";
			}
			//Special case unions
			if(src->isPointerTy() && TypeSupport::hasByteLayout(src->getPointerElementType()))
			{
				stream << "{d:";
				//Find the type
				llvm::Type* elementType = dst->getPointerElementType();
				bool isArray=isa<ArrayType>(elementType);
				stream << "new ";
				compileTypedArrayType((isArray)?elementType->getSequentialElementType():elementType);
				stream << '(';
				compileCompleteObject(bi.getOperand(0));
				stream << ".buffer), o:0}";
				return true;
			}

			compilePointerAs(bi.getOperand(0), PA.getPointerKind(&I));
			return true;
		}
		case Instruction::FPToSI:
		{
			const CastInst& ci = cast<CastInst>(I);
			stream << '(';
			compileOperand(ci.getOperand(0));
			//Seems to be the fastest way
			//http://jsperf.com/math-floor-vs-math-round-vs-parseint/33
			stream << ">>0)";
			return true;
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
			return true;
		}
		case Instruction::SIToFP:
		{
			const CastInst& ci = cast<CastInst>(I);
			stream << "(+";
			compileOperand(ci.getOperand(0));
			stream << ')';
			return true;
		}
		case Instruction::UIToFP:
		{
			const CastInst& ci = cast<CastInst>(I);
			//We need to cast to unsigned before
			stream << "(+(";
			compileOperand(ci.getOperand(0));
			stream << ">>>0))";
			return true;
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
			return true;
		}
		case Instruction::Add:
		{
			//Integer addition
			stream << "((";
			compileOperand(I.getOperand(0));
			stream << '+';
			compileOperand(I.getOperand(1));
			stream << ')';
			if(types.isI32Type(I.getType()))
				stream << ">>0";
			else
				stream << '&' << getMaskForBitWidth(I.getType()->getIntegerBitWidth());
			stream << ')';
			return true;
		}
		case Instruction::FAdd:
		{
			//Double addition
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << '+';
			compileOperand(I.getOperand(1));
			stream << ')';
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
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << '-';
			compileOperand(I.getOperand(1));
			stream << ')';
			return true;
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
			stream << '/';
			compileSignedInteger(I.getOperand(1));
			stream << ")>>0)";
			return true;
		}
		case Instruction::UDiv:
		{
			//Integer unsigned division
			stream << "((";
			compileUnsignedInteger(I.getOperand(0));
			stream << '/';
			compileUnsignedInteger(I.getOperand(1));
			stream << ")>>>0)";
			return true;
		}
		case Instruction::SRem:
		{
			//Integer signed remainder
			stream << "((";
			compileSignedInteger(I.getOperand(0));
			stream << '%';
			compileSignedInteger(I.getOperand(1));
			stream << ")>>0)";
			return true;
		}
		case Instruction::URem:
		{
			//Integer unsigned remainder
			stream << "((";
			compileUnsignedInteger(I.getOperand(0));
			stream << '%';
			compileUnsignedInteger(I.getOperand(1));
			stream << ")>>>0)";
			return true;
		}
		case Instruction::FDiv:
		{
			//Double division
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << '/';
			compileOperand(I.getOperand(1));
			stream << ')';
			return true;
		}
		case Instruction::Mul:
		{
			//Integer signed multiplication
			stream << "((";
			compileOperand(I.getOperand(0));
			stream << '*';
			compileOperand(I.getOperand(1));
			stream << ')';
			if(types.isI32Type(I.getType()))
				stream << ">>0";
			else
				stream << '&' << getMaskForBitWidth(I.getType()->getIntegerBitWidth());
			stream << ')';
			return true;
		}
		case Instruction::FMul:
		{
			//Double multiplication
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << '*';
			compileOperand(I.getOperand(1));
			stream << ')';
			return true;
		}
		case Instruction::ICmp:
		{
			//Integer comparison
			const CmpInst& ci = cast<CmpInst>(I);
			stream << '(';
			compileIntegerComparison(ci.getOperand(0), ci.getOperand(1), ci.getPredicate());
			stream << ')';
			return true;
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
			return true;
		}
		case Instruction::And:
		{
			//Integer logical and
			//No need to apply the >> operator. The result is an integer by spec
			stream << '(';
			compileOperand(I.getOperand(0));
			stream << '&';
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
			stream << ">>>";
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
			stream << ">>";
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
			stream << "<<";
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
			stream << '|';
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
			stream << '^';
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
			stream << '&' << getMaskForBitWidth(finalSize) << ')';
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
			const SelectInst& si = cast<SelectInst>(I);
			stream << '(';
			compileOperand(si.getCondition());
			stream << '?';

			if(si.getType()->isPointerTy())
			{
				POINTER_KIND k = PA.getPointerKind(&si);
				compilePointerAs(si.getTrueValue(), k);
				stream << ':';
				compilePointerAs(si.getFalseValue(), k);
			}
			else
			{
				compileOperand(si.getTrueValue());
				stream << ':';
				compileOperand(si.getFalseValue());
			}

			stream << ')';
			return true;
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
				return true;
			}
			assert(!isa<UndefValue>(aggr));

			compileOperand(aggr);

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
			const PtrToIntInst& pi=cast<PtrToIntInst>(I);
			compilePtrToInt(pi.getOperand(0));
			return true;
		}
		case Instruction::VAArg:
		{
			const VAArgInst& vi=cast<VAArgInst>(I);
			stream << "handleVAArg(";
			compileCompleteObject(vi.getPointerOperand());
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

void CheerpWriter::compileBB(const BasicBlock& BB, const std::map<const BasicBlock*, uint32_t>& blocksMap)
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
			stream << "var " << namegen.getName(I) << '=';
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
	//At the end of the block
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
		writer->stream << "===";
		writer->compileConstant(it.getCaseValue());
		//We found the destination, there may be more cases for the same
		//destination though
		for(++it;it!=si->case_end();++it)
		{
			if(it.getCaseSuccessor()==dest)
			{
				//Also add this condition
				writer->stream << "||(";
				writer->compileOperand(si->getCondition());
				writer->stream << "===";
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

bool CheerpRenderInterface::hasBlockPrologue(const void* privateBlockTo) const
{
	const BasicBlock* bbTo=(const BasicBlock*)privateBlockTo;
	return bbTo->getFirstNonPHI()!=&bbTo->front();
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
		stream << namegen.getName(curArg);
	}
	stream << "){" << NewLine;
	std::map<const BasicBlock*, uint32_t> blocksMap;
	if(F.size()==1)
		compileBB(*F.begin(), blocksMap);
	else
	{
		stream << "var label=0;" << NewLine;
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
			bool isSplittable = B->size()<3 && isa<ReturnInst>(B->getTerminator());
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
		Relooper* rl=new Relooper();
		for(;B!=BE;++B)
		{
			if(B->isLandingPad())
				continue;
			rl->AddBlock(relooperMap[&(*B)]);
		}
		rl->Calculate(relooperMap[&F.getEntryBlock()]);
		
		CheerpRenderInterface ri(this, NewLine);
		rl->Render(&ri);
	}

	stream << '}' << NewLine;
	currentFun = NULL;
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

		if(PA.getPointerKind(&G) == REGULAR)
		{
			stream << "{d:[";
			compileOperand(C);
			stream << "],o:0}";
		}
		else
		{
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
				stream << "create" << namegen.filterLLVMName(st->getName(), true) << '(';
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

		for ( auto it = std::next(subExpr.begin()); it != subExpr.end(); ++it )
		{
			const Use * u = *it;

			if ( isa<ConstantArray>( u->getUser() ) )
				stream << '[' << u->getOperandNo() << ']';
			else if ( isa<ConstantStruct>( u->getUser() ) )
				stream << ".a" << u->getOperandNo();
		}

		stream << '=';
		compileOperand(subExpr.back()->get());
		stream << ';' << NewLine;
	}
}

void CheerpWriter::compileNullPtrs()
{
	stream << "var nullArray=[null];var nullObj={d:nullArray,o:0};" << NewLine;
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
	sourceMapGenerator.beginFile();
	// Enable strict mode first
	stream << "\"use strict\"" << NewLine;

	compileClassesExportedToJs();
	compileNullPtrs();
	
	PA.prefetch(module);

	for ( const Function * F : globalDeps.functionOrderedList() )
		if (!F->empty())
		{
#ifdef CHEERP_DEBUG_POINTERS
			dumpAllPointers(*F, PA);
#endif //CHEERP_DEBUG_POINTERS
			compileMethod(*F);
		}
	
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
		stream << namegen.getName(F) << "();" << NewLine;
	}

	//Invoke the webMain function
	if ( const Function * webMain = module.getFunction("_Z7webMainv") )
		stream << namegen.getName(webMain) << "()" << NewLine;

	sourceMapGenerator.endFile();
	// Link the source map if necessary
	if (!sourceMapName.empty())
		stream << "//# sourceMappingURL=" << sourceMapName;
}

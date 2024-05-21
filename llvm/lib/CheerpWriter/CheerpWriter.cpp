//===-- CheerpWriter.cpp - The Cheerp JavaScript generator -------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2011-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "CFGStackifier.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Cheerp/Demangler.h"
#include "llvm/Cheerp/JsExport.h"
#include "llvm/Cheerp/NameGenerator.h"
#include "llvm/Cheerp/PHIHandler.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/Cheerp/InvokeWrapping.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Dominators.h"
#include "llvm/CodeGen/Analysis.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/IR/Verifier.h"

#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Cheerp/NameGenerator.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/InvokeWrapping.h"
#include "llvm/Cheerp/TokenList.h"
#include "llvm/Cheerp/DeterministicUnorderedSet.h"
#include "llvm/Cheerp/WasmOpcodes.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/Support/FormattedStream.h"
using namespace llvm;
using namespace std;
using namespace cheerp;

extern llvm::cl::opt<std::string> EnvironName;
extern llvm::cl::opt<std::string> ArgvName;

//De-comment this to debug the pointer kind of every function
//#define CHEERP_DEBUG_POINTERS

//De-comment this to debug the source map generation
//#define CHEERP_DEBUG_SOURCE_MAP

CheerpWriter::COMPILE_INSTRUCTION_FEEDBACK CheerpWriter::handleBuiltinNamespace(const char* identifier, const CallBase& callV)
{
	assert(callV.getCalledFunction());

	TypeSupport::ClientFunctionDemangled clientHelper(identifier);
	StringRef namespacedName(clientHelper.namespacedName);
	StringRef funcName(clientHelper.funcName);

	bool isClientStatic = callV.getCalledFunction()->hasFnAttribute(Attribute::Static);
	bool asmjs = callV.getCaller()->getSection() == StringRef("asmjs");

	auto compileAsPointerOrAsOperand = [&](unsigned index) -> void
	{
		const Value* v = callV.getOperand(index);

		if (v->getType()->isPointerTy())
		{
			if (Type* ty = callV.getCalledFunction()->getParamAttribute(index, Attribute::JsExportType).getValueAsType())
			{
				const auto& name = jsExportedTypes.find(ty)->getSecond();
				stream << "Object.create(" << name << ".prototype,{this:{value:";
				POINTER_KIND kind = PA.getPointerKindForJSExportedType(ty);
				compilePointerAs(v, kind, LOWEST);
				stream << "}})";
			}
			else
			{
				compilePointerAs(v, COMPLETE_OBJECT, LOWEST);
			}
		}
		else
		{
			compileOperand(v, LOWEST);
		}
	};

	//The first arg should be the object
	if(funcName.startswith("get_"))
	{
		if(funcName.size() == 4)
		{
			// Generic getter
			if (isClientStatic)
			{
				assert(callV.arg_size()==1);
				//namespacedName is either empty (-> error!) or ends with '.' that we have to skip
				if (namespacedName.empty())
					llvm_unreachable("Not supported top level getter");
				else
					stream << namespacedName.substr(0, namespacedName.size() -1);
			}
			else
			{
				assert(callV.arg_size()==2);
				compilePointerAs(callV.getOperand(0), COMPLETE_OBJECT, HIGHEST);
			}
			stream << '[';
			compileAsPointerOrAsOperand(callV.arg_size() - 1);
			stream << "]";
		}
		else
		{
			//Getter
			if (isClientStatic)
			{
				assert(callV.arg_size()==0);
				stream << namespacedName;	//namespacedName is either empty or ends with '.'
			}
			else
			{
				assert(callV.arg_size()==1);
				compileOperand(callV.getOperand(0), HIGHEST);
				stream << ".";
			}
			stream << funcName.drop_front(4);
		}
	}
	else if(funcName.startswith("set_"))
	{
		if(funcName.size() == 4)
		{
			// Generic setter
			if (isClientStatic)
			{
				assert(callV.arg_size()==2);
				//namespacedName is either empty (-> error!) or ends with '.' that we have to skip
				if (namespacedName.empty())
					llvm_unreachable("Not supported top level setter");
				else
					stream << namespacedName.substr(0, namespacedName.size() -1);
			}
			else
			{
				assert(callV.arg_size()==3);
				compilePointerAs(callV.getOperand(0), COMPLETE_OBJECT, HIGHEST);
			}
			stream << '[';
			compileAsPointerOrAsOperand(callV.arg_size() - 2);
			stream << "]=";
			compileAsPointerOrAsOperand(callV.arg_size() - 1);
		}
		else
		{
			//Setter
			if (isClientStatic)
			{
				assert(callV.arg_size()==1);
				stream << namespacedName;	//namespacedName is either empty or ends with '.'
			}
			else
			{
				assert(callV.arg_size()==2);
				compilePointerAs(callV.getOperand(0), COMPLETE_OBJECT, HIGHEST);
				stream << ".";
			}

			stream << funcName.drop_front(4) <<  '=';

			compileAsPointerOrAsOperand(callV.arg_size() - 1);
		}
	}
	else if(funcName == StringRef("operator[]"))
	{
		// operator[]
		assert(callV.arg_size()==2);
		compilePointerAs(callV.getOperand(0), COMPLETE_OBJECT, HIGHEST);
		stream << '[';
		compileAsPointerOrAsOperand(1);
		stream << ']';
	}
	else if(funcName == StringRef("operator()"))
	{
		// operator()
		User::const_op_iterator it = callV.arg_begin();
		compilePointerAs(*it, COMPLETE_OBJECT, HIGHEST);
		it++;
		compileMethodArgs(it,callV.arg_end(), callV, /*forceBoolean*/ true);
	}
	else
	{
		if (auto* X = callV.getCalledFunction()->getMetadata("cheerp.interfacename"))
			funcName = dyn_cast<MDString>(X->getOperand(0))->getString();
		User::const_op_iterator it = callV.arg_begin();

		//Normal function
		if (isClientStatic)
		{
			// In asmjs we import static client function with their mangled name
			if (asmjs)
				return COMPILE_UNSUPPORTED;
			stream << namespacedName;	//namespacedName is either empty or ends with '.'
		}
		else
		{
			assert(callV.arg_size()>=1);
			compilePointerAs(*it, COMPLETE_OBJECT, HIGHEST);
			++it;
			stream << ".";
		}

		stream << funcName;
		compileMethodArgs(it,callV.arg_end(), callV, /*forceBoolean*/ true);
	}
	Type* retTy = callV.getType();
	if (!retTy->isPointerTy())
		return COMPILE_OK;
	Type* ty = callV.getCalledFunction()->getAttributes().getRetAttrs().getAttribute(Attribute::JsExportType).getValueAsType();
	if(!jsExportedTypes.count(ty))
		return COMPILE_OK;
	POINTER_KIND retKind = PA.getPointerKindAssert(&callV);
	POINTER_KIND wrapperKind = PA.getPointerKindForJSExportedType(ty);
	if (wrapperKind == COMPLETE_OBJECT || wrapperKind == RAW)
	{
		assert(retKind != SPLIT_REGULAR);
		stream << ".this";
	}
	else
	{
		assert(wrapperKind == REGULAR);
		if (retKind ==  SPLIT_REGULAR)
		{
			stream
				<< ';' << NewLine
				<< getName(&callV, 1)
				<< '='
				<< getName(&callV, 0)
				<< ".this.o;"
				<< NewLine
				<< getName(&callV, 0)
				<< '='
				<< getName(&callV, 0)
				<< ".this.d";
		}
		else
		{
			assert(retKind == COMPLETE_OBJECT);
			stream
				<< ';' << NewLine
				<< getName(&callV, 0)
				<< '='
				<< getName(&callV, 0)
				<< ".this.d["
				<< getName(&callV, 0)
				<< ".this.o]";
		}
	}
	return COMPILE_OK;
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
			[[clang::fallthrough]];
		}
		default:
			llvm::errs() << "Support type in copy " << *currentType << "\n";
	}
}

void CheerpWriter::compileDowncast( const CallBase& callV )
{
	assert( callV.arg_size() == 2 );
	assert( callV.getCalledFunction() && callV.getCalledFunction()->getIntrinsicID() == Intrinsic::cheerp_downcast);

	POINTER_KIND result_kind = PA.getPointerKindAssert(&callV);
	const Value * src = callV.getOperand(0);
	const Value * offset = callV.getOperand(1);

	Type* t=callV.getParamElementType(0);

	if(TypeSupport::isClientType(t) || (isa<ConstantInt>(offset) && cast<ConstantInt>(offset)->isNullValue()))
	{
		if(result_kind == SPLIT_REGULAR)
		{
			compilePointerBaseTyped(src, t);
			stream << ';' << NewLine;
			stream << getName(&callV, 1) << '=';
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
			stream << getName(&callV, 1) << '=';
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
			stream  << '+';
			compileOperand(offset, ADD_SUB);
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

void CheerpWriter::compileVirtualcast( const CallBase& callV )
{
	assert( callV.arg_size() == 2 );
	assert( callV.getCalledFunction() && callV.getCalledFunction()->getIntrinsicID() == Intrinsic::cheerp_virtualcast);

	POINTER_KIND result_kind = PA.getPointerKindAssert(&callV);
	const Value * src = callV.getOperand(0);
	const Value * offset = callV.getOperand(1);

      if(result_kind == SPLIT_REGULAR)
      {
            compileCompleteObject(src);
            stream << ".a;" << NewLine;
            stream << getName(&callV, 1) << '=';
            compileOperand(offset, HIGHEST);
      }
      else if(result_kind == REGULAR)
      {
            stream << "{d:";
            compileCompleteObject(src);
            stream << ".a,o:";
            compileOperand(offset, HIGHEST);
            stream << '}';
      }
      else if(result_kind == RAW)
      {
            stream << '(';
            compileOperand(src, ADD_SUB);
            stream  << '+';
            compileOperand(offset, ADD_SUB);
            stream << "|0)";
      }
      else
      {
            compileCompleteObject(src);
            stream << ".a[";
            compileOperand(offset, HIGHEST);
            stream << ']';
      }
}

/* Method that handles memcpy and memmove.
 * Since only immutable types are handled in the backend and we use TypedArray.set to make the copy
 * there is not need to handle memmove in a special way
*/
void CheerpWriter::compileMemFunc(const Value* dest, Type* destElementType, const Value* src, const Value* size)
{
	Type* pointedType = destElementType;
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
		bool byteLayout = PA.getPointerKindAssert(dest) == BYTE_LAYOUT;
		// The semantics of TypedArray.set is memmove-like, no need to care about direction
		if(byteLayout)
			stream << "(new Int8Array(";
		compilePointerBaseTyped(dest, destElementType);
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
	Type * t = info.getCastedPointedType();
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

	Type * t = info.getCastedPointedType();

	POINTER_KIND result = PA.getPointerKindAssert(info.getInstruction());
	const ConstantInt* constantOffset = PA.getConstantOffsetForPointer(info.getInstruction());
	bool needsDowncastArray = isa<StructType>(t) && globalDeps.needsDowncastArray(cast<StructType>(t));
	bool needsRegular = result==REGULAR && !constantOffset;
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
			compilePointerBaseTyped(info.getMemoryArg(), info.getCastedPointedType());
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
	else if (info.useCreatePointerArrayFunc() )
	{
		stream << namegen.getBuiltinName(NameGenerator::Builtin::CREATE_POINTER_ARRAY) << "(";
		if (info.getAllocType() == DynamicAllocInfo::cheerp_reallocate)
		{
			compilePointerBaseTyped(info.getMemoryArg(), info.getCastedPointedType());
			stream << ',';
			compilePointerBaseTyped(info.getMemoryArg(), info.getCastedPointedType());
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
	else if (info.useCreateArrayFunc() )
	{
		if (info.getAllocType() == DynamicAllocInfo::cheerp_reallocate)
		{
			assert( globalDeps.dynResizeArrays().count(t) );

			stream << namegen.getArrayResizeName(t) << '(';
			compilePointerBaseTyped(info.getMemoryArg(), info.getCastedPointedType());
			stream << ',';
			compilePointerBaseTyped(info.getMemoryArg(), info.getCastedPointedType());
			stream << ".length,";
			compileArraySize(info, /* shouldPrint */true);
			stream << ')';
		}
		else
		{
			assert( globalDeps.dynAllocArrays().count(t) );
			stream << namegen.getArrayName(t) << '(';
			compileArraySize(info, /* shouldPrint */true);
			stream << ')';
		}
		if(result == COMPLETE_OBJECT)
			stream << "[0]";
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
			compileType(t, LITERAL_OBJ, isInlineable(*info.getInstruction(), PA) || info.getInstruction()->use_empty() ? StringRef() : getName(info.getInstruction(), 0));
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

CheerpWriter::COMPILE_INSTRUCTION_FEEDBACK CheerpWriter::compileFree(const Value* obj)
{
	// TODO: Only arrays of primitives can be backed by the linear heap
	bool needsLinearCheck = isWasmTarget;
	if(const ConstantInt* CI = PA.getConstantOffsetForPointer(obj))
	{
		// 0 is clearly not a good address in the linear address space
		if(CI->getZExtValue() == 0)
			needsLinearCheck = false;
	}
	else if(isa<ConstantPointerNull>(obj))
		needsLinearCheck = false;

	if(!needsLinearCheck)
		return COMPILE_EMPTY;

	Function* Free = module.getFunction("free");
	if (Free)
		stream << getName(Free, 0) << '(';
	else
		stream << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY);
	compilePointerAs(obj, RAW, PARENT_PRIORITY::LOWEST);
	stream << ')';

	return COMPILE_OK;
}

void CheerpWriter::compileEscapedString(raw_ostream& stream, StringRef str, bool forJSON)
{
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
		else if(c=='"')
			stream << "\\\"";
		else if(c=='\\')
			stream << "\\\\";
		else if(c>=' ' && c<='~')
		{
			// Printable ASCII after we exscluded the previous one
			stream << c;
		}
		else if(forJSON)
		{
			char buf[7];
			snprintf(buf, 7, "\\u%04x", c);
			stream << buf;
		}
		else
		{
			char buf[5];
			snprintf(buf, 5, "\\x%02x", c);
			stream << buf;
		}
	}
}

CheerpWriter::COMPILE_INSTRUCTION_FEEDBACK CheerpWriter::handleBuiltinCall(const CallBase& callV, const Function * func)
{
	assert( func );
	assert( (func == callV.getCalledFunction() ) || !(callV.getCalledFunction()) );
	
	bool userImplemented = !func->empty();
	
	auto it = callV.arg_begin(), itE = callV.arg_end();
	
	StringRef ident = func->getName();
	unsigned intrinsicId = func->getIntrinsicID();

	StringRef section  = currentFun->getSection();
	bool asmjs = section == StringRef("asmjs");
	const char* Math = "Math.";

	//First handle high priority builtins, they will be used even
	//if an implementation is available from the user
	if(intrinsicId==Intrinsic::memmove ||
		intrinsicId==Intrinsic::memcpy)
	{
		if (!asmjs)
		{
			compileMemFunc(*(it), callV.getParamElementType(0), *(it+1), *(it+2));
			return COMPILE_EMPTY;
		}
		else if (intrinsicId == Intrinsic::memcpy)
		{
			Function* memcpy = module.getFunction("memcpy");
			assert(memcpy);
			stream << getName(memcpy, 0) << '(';
			compileOperand(*(it),LOWEST);
			stream << "|0,";
			compileOperand(*(it+1),LOWEST);
			stream << "|0,";
			compileOperand(*(it+2),LOWEST);
			stream << "|0)|0";
			return COMPILE_OK;
		}
		else
		{
			assert(intrinsicId == Intrinsic::memmove);
			Function* memmove = module.getFunction("memmove");
			assert(memmove);
			stream << getName(memmove, 0) << '(';
			compileOperand(*(it),LOWEST);
			stream << "|0,";
			compileOperand(*(it+1),LOWEST);
			stream << "|0,";
			compileOperand(*(it+2),LOWEST);
			stream << "|0)|0";
			return COMPILE_OK;
		}
	}
	else if(intrinsicId==Intrinsic::memset)
	{
		if (asmjs) {
			Function* memset = module.getFunction("memset");
			assert(memset);
			stream << getName(memset, 0) << '(';
			compileOperand(*(it),LOWEST);
			stream << "|0,";
			compileOperand(*(it+1),LOWEST);
			stream << "|0,";
			compileOperand(*(it+2),LOWEST);
			stream << "|0)|0";
			return COMPILE_OK;
		}
		llvm::report_fatal_error("Unsupported memory intrinsic, please rebuild the code using an updated version of Cheerp", false);
		return COMPILE_EMPTY;
	}
	else if(intrinsicId==Intrinsic::vastart)
	{
		assert(!asmjs && "vastart instructions in asmjs functions are removed in the AllocaLowering pass");
		compileCompleteObject(*it);
		stream << "={d:arguments,o:" << getName(currentFun, 0) << ".length}";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::vaend)
	{
		if (asmjs) return COMPILE_EMPTY;

		compileCompleteObject(*it);
		stream << "=null";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::vacopy)
	{
		if (asmjs)
		{
			stream << getHeapName(HEAP32) << '[';
			compileRawPointer(*it, PARENT_PRIORITY::SHIFT);
			stream << pointerShiftOperator() << "2]=";
			stream << getHeapName(HEAP32) << '[';
			compileRawPointer(*(it+1), PARENT_PRIORITY::SHIFT);
			stream << pointerShiftOperator() << "2]|0";
			return COMPILE_OK;
		}
		else
		{
			compileCompleteObject(*it);
			stream << "={d:";
			compileCompleteObject(*(it+1));
			stream << ".d,o:";
			compileCompleteObject(*(it+1));
			stream << ".o}";
			return COMPILE_OK;
		}
	}
	else if(intrinsicId==Intrinsic::cheerp_get_array_len)
	{
		compilePointerBase(*it);
		stream << ".length";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_virtualcast)
	{
		if(callV.use_empty())
			return COMPILE_EMPTY;
		compileVirtualcast( callV );
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_downcast)
	{
		if(callV.use_empty())
			return COMPILE_EMPTY;
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
		compilePointerAs(*it, PA.getPointerKindAssert(&callV));
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_cast_user)
	{
		if(callV.use_empty())
			return COMPILE_EMPTY;

		compileBitCast(&callV, PA.getPointerKindAssert(&callV), HIGHEST);
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_typed_ptrcast)
	{
		if(callV.use_empty())
			return COMPILE_EMPTY;

		compileBitCast(&callV, PA.getPointerKindAssert(&callV), HIGHEST);
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_pointer_base)
	{
		compilePointerBase(*it, true);
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_pointer_offset)
	{
		compilePointerOffset(*it, HIGHEST, true);
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_is_linear_heap)
	{
		stream << '(';
		compilePointerBase(*it);
		stream<<".buffer===__heap)";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_pointer_elem_size)
	{
		stream << '(';
		compilePointerBase(*it);
		stream<<".BYTES_PER_ELEMENT)";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_pointer_kind)
	{
		stream << (int)PA.getPointerKindAssert(*it);
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_create_closure)
	{
		assert( globalDeps.needCreateClosure() );

		//We use an helper method to create closures without
		//keeping all local variable around. The helper
		//method is printed on demand depending on a flag
		assert( isa<Function>( callV.getOperand(0) ) );
		POINTER_KIND argKind = PA.getPointerKindAssert( &*cast<Function>(callV.getOperand(0))->arg_begin() );
		if(argKind == SPLIT_REGULAR)
			stream << namegen.getBuiltinName(NameGenerator::Builtin::CREATE_CLOSURE_SPLIT) << "(";
		else
			stream << namegen.getBuiltinName(NameGenerator::Builtin::CREATE_CLOSURE) << "(";
		compileCompleteObject( callV.getOperand(0) );
		stream << ',';
		if(argKind == SPLIT_REGULAR)
		{
			compilePointerBase( callV.getOperand(1) );
			stream << ',';
			compilePointerOffset( callV.getOperand(1), LOWEST );
		}
		else
			compilePointerAs( callV.getOperand(1), argKind );
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
	else if(intrinsicId==Intrinsic::cheerp_grow_memory)
	{
		stream << namegen.getBuiltinName(NameGenerator::Builtin::GROW_MEM) << '(';
		compileOperand(*it, BIT_OR);
		stream << "|0)|0";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_environ)
	{
		if (makeModule == MODULE_TYPE::ES6)
			stream << EnvironName;
		else
			stream  << "typeof " << EnvironName << " == 'undefined' ? null : " << EnvironName;
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_argv)
	{
		if (makeModule == MODULE_TYPE::ES6)
			stream << ArgvName;
		else
			stream  << "typeof " << ArgvName << " == 'undefined' ? null : " << ArgvName;
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_get_threading_object)
	{
		stream << namegen.getBuiltinName(NameGenerator::Builtin::THREADINGOBJECT);
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_get_threading_blob)
	{
		StringRef threadingObject = namegen.getBuiltinName(NameGenerator::Builtin::THREADINGOBJECT);
		stream << "new Blob([" << '"';
		stream << "onmessage=(e)=>{";
		stream << threadingObject << "=e.data;";
		stream << threadingObject << ".inWorker=true;";
		if (makeModule == MODULE_TYPE::ES6)
			stream << "import(" << threadingObject << ".script).then(m=>{m.default();});";
		else
			stream << "importScripts(" << threadingObject << ".script);";
		stream << "}" << '"' << "])";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::abs)
	{
		//Implementing ( X >= 0 ) ? X : -X
		//Note that abs takes 2 arguments, the actual value (X) + a flag that dictates
		//what the output should be for X == INT_MIN (=-2^31).
		//Since in one case it's -2^31 and the other in Undefined, we consider both
		//at the same time rendering -2^31 (note that it's surprisingly a negative value)

		const int bitWidth = (*it)->getType()->getIntegerBitWidth();

		stream << "((";
		compileSignedInteger(*(it), /*forComparison*/ true, COMPARISON);
		stream << ">=0";
		if (bitWidth > 32)
		{
			assert(bitWidth == 64);
			stream << "n";
		}
		stream << ")?";
		compileOperand(*(it), TERNARY);
		stream << ":";
		if (bitWidth > 32)
		{
			assert(bitWidth == 64);
			stream << "BigInt.asIntN(64,0n-";
			compileSignedInteger(*(it), /*forComparison*/ false, ADD_SUB);
			stream << ")";
		}
		else
		{
			stream << "0-";
			compileSignedInteger(*(it), /*forComparison*/ false, ADD_SUB);
			stream << "|0";
		}
		stream << ')';
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
		if(asmjs)
			stream << namegen.getBuiltinName(NameGenerator::Builtin::CLZ32);
		else
			stream << Math << "clz32";
		stream << "(";
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
		stream << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY) << "()";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::stacksave)
	{
		stream << namegen.getBuiltinName(NameGenerator::Builtin::STACKPTR);
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::stackrestore)
	{
		stream << namegen.getBuiltinName(NameGenerator::Builtin::STACKPTR) << "=";
		compileOperand(*it, LOWEST);
		return COMPILE_OK;
	}
	else if (intrinsicId==Intrinsic::get_dynamic_area_offset)
	{
		// The stack pointer is the same as the address of the most recent alloca, so 0 offset
		stream << "0|0";
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::eh_typeid_for)
	{
		stream << getTypeIdFor(*it);
		return COMPILE_OK;
	}
	else if(intrinsicId==Intrinsic::cheerp_throw)
	{
		stream << "throw ";
		compileOperand(*it);
		return COMPILE_OK;
	}
	else if(cheerp::isFreeFunctionName(ident) || intrinsicId==Intrinsic::cheerp_deallocate)
	{
		if (asmjs || TypeSupport::isAsmJSPointer((*it)->getType()))
		{
			Function* ffree = module.getFunction("free");
			if (!ffree)
				llvm::report_fatal_error("missing free definition");
			if (ffree->empty() && asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY);
			else
				stream << getName(ffree, 0);
			stream <<'(';
			compileOperand(*it, PARENT_PRIORITY::BIT_OR);
			stream << "|0)";
			return COMPILE_OK;
		}
		else
		{
			return compileFree(*it);
		}
	}
	else if(ident=="fmod")
	{
		// Handle this internally, C++ does not have float mod operation
		stream << '(';
		compileOperand(*(it), MUL_DIV);
		stream << '%';
		compileOperand(*(it+1), nextPrio(MUL_DIV));
		stream << ')';
		return COMPILE_OK;
	}
	else if(ident=="fmodf")
	{
		stream << "(+";
		compileOperand(*(it), HIGHEST);
		stream << "%+";
		compileOperand(*(it+1), HIGHEST);
		stream << ')';
		return COMPILE_OK;
	}
	else if(useNativeJavaScriptMath || intrinsicId)
	{
		// NOTE: V8 has very strict rules about mixing the double builtins with
		// floats in asm.js, so we need an extra `+` for those
		PARENT_PRIORITY mathPrio = LOWEST;
		bool asmjsFloats = asmjs && useMathFround;
		if(intrinsicId==Intrinsic::copysign)
		{
			bool isFloat = (*it)->getType()->isFloatTy();
			stream << "(";
			Value* mag = *it;
			Value* sgn = *(it+1);
			PARENT_PRIORITY prio = TERNARY;
			if (isFloat)
			{
				stream << '+';
				prio = HIGHEST;
			}
			compileOperand(sgn, prio);
			stream << "<0.?-";
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::ABS);
			else
				stream << Math << "abs";
			stream << "(";
			prio = LOWEST;
			if (isFloat)
			{
				stream << '+';
				prio = HIGHEST;
			}
			compileOperand(mag, prio);
			stream << "):";
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::ABS);
			else
				stream << Math << "abs";
			stream << "(";
			prio = LOWEST;
			if (isFloat)
			{
				stream << '+';
				prio = HIGHEST;
			}
			compileOperand(mag, prio);
			stream << "))";
			return COMPILE_OK;
		}
		if(ident=="fabs" || ident=="fabsf" || intrinsicId==Intrinsic::fabs)
		{
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::ABS);
			else
				stream << Math << "abs";
			stream << "(";
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="acos" || ident=="acosf")
		{
			if(asmjsFloats && (*it)->getType()->isFloatTy())
				stream << '+';
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::ACOS);
			else
				stream << Math << "acos";
			stream << "(";
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				mathPrio = HIGHEST;
				stream << '+';
			}
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="asin" || ident=="asinf")
		{
			if(asmjsFloats && (*it)->getType()->isFloatTy())
				stream << '+';
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::ASIN);
			else
				stream << Math << "asin";
			stream << "(";
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				mathPrio = HIGHEST;
				stream << '+';
			}
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="atan" || ident=="atanf")
		{
			if(asmjsFloats && (*it)->getType()->isFloatTy())
				stream << '+';
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::ATAN);
			else
				stream << Math << "atan";
			stream << "(";
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				mathPrio = HIGHEST;
				stream << '+';
			}
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="atan2" || ident=="atan2f")
		{
			if(asmjsFloats && (*it)->getType()->isFloatTy())
				stream << '+';
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::ATAN2);
			else
				stream << Math << "atan2";
			stream << "(";
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				mathPrio = HIGHEST;
				stream << '+';
			}
			compileOperand(*(it), mathPrio);
			stream << ',';
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				stream << '+';
			}
			compileOperand(*(it+1), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="ceil" || ident=="ceilf" || intrinsicId==Intrinsic::ceil)
		{
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::CEIL);
			else
				stream << Math << "ceil";
			stream << "(";
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="cos" || ident=="cosf" || intrinsicId==Intrinsic::cos)
		{
			if(asmjsFloats && (*it)->getType()->isFloatTy())
				stream << '+';
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::COS);
			else
				stream << Math << "cos";
			stream << "(";
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				mathPrio = HIGHEST;
				stream << '+';
			}
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="exp" || ident=="expf" || intrinsicId==Intrinsic::exp)
		{
			if(asmjsFloats && (*it)->getType()->isFloatTy())
				stream << '+';
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::EXP);
			else
				stream << Math << "exp";
			stream << "(";
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				mathPrio = HIGHEST;
				stream << '+';
			}
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="floor" || ident=="floorf" || intrinsicId==Intrinsic::floor)
		{
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::FLOOR);
			else
				stream << Math << "floor";
			stream << "(";
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="log" || ident=="logf" || intrinsicId==Intrinsic::log)
		{
			if(asmjsFloats && (*it)->getType()->isFloatTy())
				stream << '+';
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::LOG);
			else
				stream << Math << "log";
			stream << "(";
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				mathPrio = HIGHEST;
				stream << '+';
			}
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="pow" || ident=="powf" || intrinsicId==Intrinsic::pow)
		{
			if(asmjsFloats && (*it)->getType()->isFloatTy())
				stream << '+';
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::POW);
			else
				stream << Math << "pow";
			stream << "(";
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				mathPrio = HIGHEST;
				stream << '+';
			}
			compileOperand(*(it), mathPrio);
			stream << ',';
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				stream << '+';
			}
			compileOperand(*(it+1), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(!asmjs && (ident=="round" || ident=="roundf"))
		{
			stream << Math << "round(";
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				mathPrio = HIGHEST;
				stream << '+';
			}
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="sin" || ident=="sinf" || intrinsicId==Intrinsic::sin)
		{
			if(asmjsFloats && (*it)->getType()->isFloatTy())
				stream << '+';
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::SIN);
			else
				stream << Math << "sin";
			stream << "(";
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				mathPrio = HIGHEST;
				stream << '+';
			}
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="sqrt" || ident=="sqrtf" || intrinsicId==Intrinsic::sqrt)
		{
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::SQRT);
			else
				stream << Math << "sqrt";
			stream << "(";
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
		else if(ident=="tan" || ident=="tanf")
		{
			if(asmjsFloats && (*it)->getType()->isFloatTy())
				stream << '+';
			if(asmjs)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::TAN);
			else
				stream << Math << "tan";
			stream << "(";
			if(asmjsFloats && (*it)->getType()->isFloatTy())
			{
				mathPrio = HIGHEST;
				stream << '+';
			}
			compileOperand(*(it), mathPrio);
			stream << ')';
			return COMPILE_OK;
		}
	}

	DynamicAllocInfo da(&callV, &targetData, forceTypedArrays);
	if (da.isValidAlloc())
	{
		// Dead allocations won't be removed by LLVM, skip them here
		if(callV.use_empty())
			return COMPILE_EMPTY;
		compileAllocation(da);
		return COMPILE_OK;
	}
	if ((func->getIntrinsicID()==Intrinsic::cheerp_allocate || func->getIntrinsicID()==Intrinsic::cheerp_allocate_array) &&
	    (asmjs || TypeSupport::isAsmJSPointed(callV.getParamElementType(0))))
	{
		Function* fmalloc = module.getFunction("malloc");
		if (!fmalloc)
			llvm::report_fatal_error("missing malloc definition");
		stream << getName(fmalloc, 0) << "(";
		compileOperand(*(it+1), PARENT_PRIORITY::LOWEST);
		stream << ")|0";
		return COMPILE_OK;
	}
	else if (asmjs && func->getIntrinsicID()==Intrinsic::cheerp_reallocate && (asmjs || TypeSupport::isAsmJSPointed(callV.getParamElementType(0))))
	{
		Function* frealloc = module.getFunction("realloc");
		if (!frealloc)
			llvm::report_fatal_error("missing realloc definition");
		stream << getName(frealloc, 0) <<'(';
		compileOperand(*it);
		stream << ',';
		compileOperand(*(it+1));
		stream << ")|0";
		return COMPILE_OK;
	}
	else if(ident=="cheerpCreate_ZN6client6StringC2EPKc")
	{
		// NativeRewriter has encoded the value as metadata if this optimization is possible
		if(llvm::MDNode* md = callV.getMetadata("jsliteral"))
		{
			StringRef str = cast<MDString>(md->getOperand(0))->getString();
			stream << '"';
			auto& rawStream = stream.getRawStream();
			uint64_t beginVal = rawStream.tell();
			compileEscapedString(stream.getRawStream(), str, /*forJSON*/false);
			stream.syncRawStream(beginVal);
			stream << '"';
			return COMPILE_OK;
		}
	}

	//If the method is implemented by the user, stop here
	if(userImplemented)
		return COMPILE_UNSUPPORTED;

	if(TypeSupport::isClientFuncName(ident))
	{
		return handleBuiltinNamespace(ident.data(),callV);
	}
	else if(TypeSupport::isClientConstructorName(ident))
	{
		//12 since it will skip cheerpCreate that was appended in front
		cheerp::Demangler demangler(ident.data() + 12);

		std::string mangledJS = demangler.getJSMangling(/*doCleanup*/true);

		if (mangledJS != "String")
			stream << "new ";
		stream << mangledJS;

		compileMethodArgs(it, itE, callV, /*forceBoolean*/ true);
		return COMPILE_OK;
	}
	else if(TypeSupport::isCheerpGetTypeHelperName(ident))
	{
		// Replace calls to `cheerp::GetTypeHelper<T>::getType()` with the
		// JavaScript type name of `T`.
		//
		// cheerp.GetTypeHelper<client.String>.getType
		// cheerp.GetTypeHelper<client.Map<client._Any*, client._Any*>>.getType
		// cheerp.GetTypeHelper<Foo>.getType
		//                      ^ 21

		cheerp::Demangler demangler(ident.data());
		std::string mangledJS = demangler.getJSMangling(false);
		llvm::StringRef name = mangledJS;
		name = name.substr(21);
		name = name.substr(0, name.find_first_of("<>"));

		if (name.startswith("client."))
			name = name.substr(7);

		if (name == "TArray")
			stream << "Array";
		else
			stream << name;

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
	else if(CmpInst::isUnsigned(p) || v->getType()->getIntegerBitWidth() < 32)
	{
		bool asmjs = currentFun->getSection() == StringRef("asmjs");
		compileUnsignedInteger(v, /*forAsmJSComparison*/ asmjs, parentPrio);
	}
	else
		compileSignedInteger(v, /*forComparison*/ true, parentPrio);
}

void CheerpWriter::compileEqualPointersComparison(const llvm::Value* lhs, const llvm::Value* rhs, CmpInst::Predicate p)
{
	StringRef compareString;
	StringRef joinString;
	joinString = (p == CmpInst::ICMP_NE) ? "||" : "&&";

	POINTER_KIND lhsKind = PA.getPointerKind(lhs);
	POINTER_KIND rhsKind = PA.getPointerKind(rhs);

	if(lhsKind == RAW)
		assert(rhsKind != COMPLETE_OBJECT && rhsKind != BYTE_LAYOUT);
	if(rhsKind == RAW)
		assert(lhsKind != COMPLETE_OBJECT && lhsKind != BYTE_LAYOUT);

	bool asmjs = currentFun && currentFun->getSection() == "asmjs";
	bool compareRaw = ((lhsKind == RAW || lhsKind == CONSTANT) && (rhsKind == RAW || rhsKind == CONSTANT))
		&& !(lhsKind == CONSTANT && rhsKind == CONSTANT && !asmjs);

	if (compareRaw)
		compareString = (p == CmpInst::ICMP_NE) ? "!=" : "==";
	else
		compareString = (p == CmpInst::ICMP_NE) ? "!==" : "===";

	if(compareRaw)
	{
		stream << "(";
		compileRawPointer(lhs, pointerCoercionPrio());
		stream << pointerCoercionSuffix() << ")";
		stream << compareString;
		stream << "(";
		compileRawPointer(rhs, pointerCoercionPrio());
		stream << pointerCoercionSuffix() << ")";
	}
	// NOTE: For any pointer-to-immutable, converting to CO is actually a dereference. (base[offset] in both cases)
	//       PA enforces that comparisons between pointers-to-immutable (which include pointers-to-pointers)
	//       need a SPLIT_REGULAR kind. Make sure to also use SPLIT_REGULAR if one kind is CONSTANT (e.g. null)
	else if((lhsKind == REGULAR || lhsKind == SPLIT_REGULAR || lhsKind == RAW ||lhsKind == CONSTANT || (isGEP(lhs) && cast<User>(lhs)->getNumOperands()==2)) &&
		(rhsKind == REGULAR || rhsKind == SPLIT_REGULAR || rhsKind == RAW ||rhsKind == CONSTANT || (isGEP(rhs) && cast<User>(rhs)->getNumOperands()==2)))
	{
		assert(lhsKind != COMPLETE_OBJECT || !isa<Instruction>(lhs) ||
				isInlineable(*cast<Instruction>(lhs), PA));
		assert(rhsKind != COMPLETE_OBJECT || !isa<Instruction>(rhs) ||
				isInlineable(*cast<Instruction>(rhs), PA));
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
		assert(lhsKind != COMPLETE_OBJECT);
		assert(rhsKind != COMPLETE_OBJECT);
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
		assert(lhsKind != BYTE_LAYOUT);
		assert(rhsKind != BYTE_LAYOUT);
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

void CheerpWriter::compileOffsetForGEP(Type* pointedOperandType, ArrayRef< const Value* > indices)
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

	const llvm::Instruction* I = dyn_cast<Instruction>(p);
	if (I && !isInlineable(*I, PA) &&
		(isGEP(I) || isBitCast(I)) && PA.getPointerKindAssert(I) == COMPLETE_OBJECT)
	{
		stream << getName(I, 0);
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
			compileGEP(cast<User>(p), COMPLETE_OBJECT, HIGHEST);
			return;
		}
	}

	POINTER_KIND kind = PA.getPointerKindAssert(p);

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
				stream << pointerCoercionSuffix();
			}
			else if (needsUnsignedPointers())
				stream << pointerCoercionSuffix();
		}

		stream << ']';
	}
	else
	{
		compileOperand(p);

		if(!isOffsetConstantZero)
		{
			llvm::errs() << "Can not access a " << int(kind) << " pointer with non zero offset:" << *offset << "\n";
			llvm::report_fatal_error("Unsupported code found, please report a bug", false);
		}
	}
}

void CheerpWriter::compileRawPointer(const Value* p, PARENT_PRIORITY parentPrio, bool forceGEP)
{
	const llvm::Instruction* I = dyn_cast<Instruction>(p);
	if (I && !isInlineable(*I, PA) && !forceGEP) {
		stream << getName(I, 0);
		return;
	}

	bool asmjs = currentFun && currentFun->getSection() == StringRef("asmjs");
	bool use_imul = asmjs || useMathImul;
	bool needsCoercion = needsIntCoercion(parentPrio);
	PARENT_PRIORITY basePrio = ADD_SUB;
	if(needsCoercion)
		basePrio = pointerCoercionPrio();
	if(parentPrio > basePrio)
		stream << "(";
	AsmJSGepWriter gepWriter(*this, use_imul);
	p = linearHelper.compileGEP(p, &gepWriter, &PA);
	PARENT_PRIORITY gepPrio = gepWriter.offset?ADD_SUB:basePrio;
	if (isa<ConstantPointerNull>(p))
		stream << '0';
	else
		compileOperand(p, gepPrio);
	if(needsCoercion)
		stream << pointerCoercionSuffix();
	if(parentPrio > basePrio)
		stream << ")";
}

int CheerpWriter::getTypeIdFor(Value* V)
{
	auto& local = landingPadTable.getLocalTypeIdMap(currentFun);
	return local.getTypeIdFor(V, linearHelper);
}

int CheerpWriter::getHeapShiftForType(Type* et)
{
	uint32_t shift=0;
	if(et->isIntegerTy(8) || et->isIntegerTy(1))
	{
		shift = 0;
	}
	else if(et->isIntegerTy(16))
	{
		shift = 1;
	}
	else if(et->isIntegerTy(32) || et->isPointerTy() || et->isArrayTy())
	{
		shift = 2;
	}
	else if(et->isFloatTy())
	{
		shift = 2;
	}
	else if(et->isDoubleTy() || et->isIntegerTy(64))
	{
		shift = 3;
	}
	else
	{
		llvm::errs() << "Unsupported heap access for  type " << *et << "\n";
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}
	return shift;
}
int CheerpWriter::compileHeapForType(Type* et)
{
	uint32_t shift=0;
	if(et->isIntegerTy(8) || et->isIntegerTy(1))
	{
		stream << getHeapName(HEAP8);
		shift = 0;
	}
	else if(et->isIntegerTy(16))
	{
		stream << getHeapName(HEAP16);
		shift = 1;
	}
	else if(et->isIntegerTy(32) || et->isPointerTy() || et->isArrayTy())
	{
		stream << getHeapName(HEAP32);
		shift = 2;
	}
	else if(et->isIntegerTy(64))
	{
		stream << getHeapName(HEAP64);
		shift = 3;
	}
	else if(et->isFloatTy())
	{
		stream << getHeapName(HEAPF32);
		shift = 2;
	}
	else if(et->isDoubleTy())
	{
		stream << getHeapName(HEAPF64);
		shift = 3;
	}
	else
	{
		llvm::errs() << "Unsupported heap access for  type " << *et << "\n";
		llvm::report_fatal_error("Unsupported code found, please report a bug", false);
	}
	return shift;
}
void CheerpWriter::compileHeapAccess(const Value* p, Type* t, uint32_t offset)
{
	if (!isa<PointerType>(p->getType()))
	{
#ifndef NDEBUG
		llvm::errs() << "not a pointer type:\n";
		p->dump();
#endif
		llvm::report_fatal_error("please report a bug");
		return;
	}
	PointerType* pt=cast<PointerType>(p->getType());
	Type* et = (t==nullptr) ? pt->getPointerElementType() : t;
	uint32_t shift = compileHeapForType(et);
	stream << '[';
	PARENT_PRIORITY prio = PARENT_PRIORITY::SHIFT;
	if(checkBounds)
	{
		stream << '(';
		compileCheckBoundsAsmJS(targetData.getTypeAllocSize(et)-1);
		prio = pointerCoercionPrio();
	}
	bool needsShift = true;
	if(!symbolicGlobalsAsmJS && isa<GlobalVariable>(p))
	{
		uint32_t addr = linearHelper.getGlobalVariableAddress(cast<GlobalVariable>(p)) + offset;
		if(checkBounds)
		{
			stream << addr;
		}
		else
		{
			stream << (addr >> shift);
			needsShift = false;
		}
	}
	else
	{
		if(offset != 0)
		{
			stream << '(';
			prio = PARENT_PRIORITY::ADD_SUB;
		}
		compileRawPointer(p, prio);
		if(offset != 0)
		{
			stream << '+';
			stream << offset;
			stream << pointerCoercionSuffix() << ")";
		}
	}
	if(checkBounds)
	{
		stream << pointerCoercionSuffix() << ")" << pointerCoercionSuffix() << ")";
	}
	if(needsShift)
		stream << pointerShiftOperator() << shift;
	stream << ']';
}
void CheerpWriter::compilePointerBase(const Value* p, bool forEscapingPointer)
{
	compilePointerBaseTyped(p, p->getType()->getPointerElementType(), forEscapingPointer);
}

void CheerpWriter::compilePointerBaseTyped(const Value* p, Type* elementType, bool forEscapingPointer)
{
	POINTER_KIND kind = PA.getPointerKind(p);
	if(kind == RAW)
	{
		assert(isa<PointerType>(p->getType()));
		if (isWasmTarget)
			compileHeapForType(elementType);
		else
			stream << "nullArray";
		return;
	}
	// Collapse if p is a gepInst
	if(isGEP(p))
	{
		const User* gepInst = cast<User>(p);
		assert(gepInst->getNumOperands() > 1);
		return compileGEPBase(gepInst, forEscapingPointer);
	}

	if(isBitCast(p) && (!isa<Instruction>(p) || isInlineable(*cast<Instruction>(p), PA) || forEscapingPointer))
	{
		compileBitCastBase(cast<User>(p), forEscapingPointer);
		return;
	}

	if(kind == CONSTANT)
	{
		Type* ty = llvm::cast<PointerType>(p->getType())->getPointerElementType();
		if (isa<ConstantPointerNull>(p))
			stream << "nullArray";
		else if (isWasmTarget && !ty->isStructTy())
		{
			compileHeapForType(ty);
		}
		else
			stream << "nullArray";
		return;
	}

	if(kind == COMPLETE_OBJECT)
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
			case Intrinsic::cheerp_typed_ptrcast:
			case Intrinsic::cheerp_cast_user:
				return compilePointerBaseTyped(II->getOperand(0), II->getParamElementType(0));
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
		compilePointerBaseTyped(u->getOperand(1), elementType);
		stream << ':';
		compilePointerBaseTyped(u->getOperand(2), elementType);
		stream << ')';
		return;
	}

	if((!isa<Instruction>(p) || !isInlineable(*cast<Instruction>(p), PA)) && kind == SPLIT_REGULAR)
	{
		stream << getName(p, 0);
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
		bool byteLayoutFromHere = PA.getPointerKindAssert(u->getOperand(0)) != BYTE_LAYOUT;
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
						bool isOffsetConstantZero = isa<Constant>(indices[i])
							&& cast<Constant>(indices[i])->isNullValue();
						if (!isOffsetConstantZero)
						{
							compileOperand( indices[i], MUL_DIV );
							stream << '*' << targetData.getTypeAllocSize(getElementType(curType, cast<const GEPOperator>(p)->getSourceElementType())) << '+';
						}
					}
					curType = getElementType(curType, cast<const GEPOperator>(p)->getSourceElementType());
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
	assert (PA.getPointerKindAssert(p) == BYTE_LAYOUT);
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
	POINTER_KIND kind = PA.getPointerKind(p);
	bool byteLayout = kind == BYTE_LAYOUT;
	if ( kind == RAW)
	{
		assert(isa<PointerType>(p->getType()));
		Type* ty = llvm::cast<PointerType>(p->getType())->getPointerElementType();
		if (parentPrio > SHIFT)
			stream << '(';
		compileRawPointer(p, SHIFT);
		stream << pointerShiftOperator() << getHeapShiftForType(ty);
		if (parentPrio > SHIFT)
			stream << ')';
		return;
	}
	if ( kind == COMPLETE_OBJECT && !isGEP(p) )
	{
		// This may still happen when doing ptrtoint of a function
		stream << '0';
	}
	// null must be handled first, even if it is bytelayout
	else if(kind == CONSTANT || isa<UndefValue>(p))
	{
		if (const IntToPtrInst* ITP = dyn_cast<IntToPtrInst>(p))
		{
			ConstantInt* CI = cast<ConstantInt>(ITP->getOperand(0));
			stream << CI->getSExtValue();
		}
		else
		{
			stream << '0';
		}
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
	else if((!isa<Instruction>(p) || !isInlineable(*cast<Instruction>(p), PA)) && kind == SPLIT_REGULAR)
	{
		stream << getName(p, 1);
	}
	else if(const IntrinsicInst* II=dyn_cast<IntrinsicInst>(p))
	{
		// Handle intrinsics
		switch(II->getIntrinsicID())
		{
			case Intrinsic::cheerp_upcast_collapsed:
			case Intrinsic::cheerp_typed_ptrcast:
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

void CheerpWriter::compileConstantExpr(const ConstantExpr* ce, PARENT_PRIORITY parentPrio, bool asmjs)
{
	switch(ce->getOpcode())
	{
		case Instruction::GetElementPtr:
		{
			compileGEP(ce, PA.getPointerKindAssert(ce), parentPrio);
			break;
		}
		case Instruction::BitCast:
		{
			POINTER_KIND k = PA.getPointerKindAssert(ce);
			compileBitCast(ce, k, parentPrio);
			break;
		}
		case Instruction::AddrSpaceCast:
		{
			compileOperand(ce->getOperand(0), parentPrio);
			break;
		}
		case Instruction::IntToPtr:
		{
			compileOperand(ce->getOperand(0), parentPrio);
			break;
		}
		case Instruction::PtrToInt:
		{
			if(asmjs)
				compileRawPointer(ce->getOperand(0), parentPrio);
			else
				compilePtrToInt(ce->getOperand(0), ce->getType()->isIntegerTy(64));
			break;
		}
		case Instruction::ICmp:
		{
			compileIntegerComparison(ce->getOperand(0), ce->getOperand(1), (CmpInst::Predicate)ce->getPredicate(), parentPrio);
			break;
		}
		case Instruction::Select:
		{
			compileSelect(ce, ce->getOperand(0), ce->getOperand(1), ce->getOperand(2), parentPrio);
			break;
		}
		case Instruction::Sub:
		{
			compileSubtraction(ce->getOperand(0), ce->getOperand(1), parentPrio, asmjs);
			break;
		}
		case Instruction::Add:
		{
			stream << '(';
			compileOperand(ce->getOperand(0), ADD_SUB);
			stream << "+";
			compileOperand(ce->getOperand(1), ADD_SUB);
			stream << "|0)";
			break;
		}
		case Instruction::And:
		{
			stream << '(';
			compileOperand(ce->getOperand(0), BIT_AND);
			stream << "&";
			compileOperand(ce->getOperand(1), BIT_AND);
			stream << "|0)";
			break;
		}
		case Instruction::Or:
		{
			stream << '(';
			compileOperand(ce->getOperand(0), BIT_OR);
			stream << "|";
			compileOperand(ce->getOperand(1), BIT_OR);
			stream << "|0)";
			break;
		}
		case Instruction::LShr:
		{
			stream << '(';
			compileOperand(ce->getOperand(0), SHIFT);
			stream << ">>>";
			compileOperand(ce->getOperand(1), SHIFT);
			stream << "|0)";
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
	else if(isa<GlobalVariable>(C) && cast<GlobalVariable>(C)->getSection() != "asmjs" && !compiledGVars.count(cast<GlobalVariable>(C)))
		return true;
	else
		return false;
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
		compileConstantExpr(cast<ConstantExpr>(c), parentPrio, asmjs);
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
			JSBytesWriter bytesWriter(stream);
			linearHelper.compileConstantAsBytes(c, /*asmjs*/false, &bytesWriter);
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
						compilePointerBaseTyped(d->getOperand(i), elementType);
				}
				else if(k == SPLIT_REGULAR)
				{
					if(dependOnUndefined)
						stream << "undefined";
					else
						compilePointerBaseTyped(d->getOperand(i), elementType);
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
		bool isFloat = c->getType()->isFloatTy();
		const ConstantFP* f=cast<ConstantFP>(c);
		bool useFloat = false;
		
		if(f->getValueAPF().isInfinity())
		{
			if (isFloat && needsFloatCoercion(parentPrio))
				stream<< namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
			if(f->getValueAPF().isNegative())
			{
				if(parentPrio > LOWEST)
					stream << ' ';
				stream << '-';
			}

			stream << "Infinity";
			if (isFloat && needsFloatCoercion(parentPrio))
				stream << ')';
		}
		else if(f->getValueAPF().isNaN())
		{
			if (isFloat && needsFloatCoercion(parentPrio))
				stream<< namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
			stream << "NaN";
			if (isFloat && needsFloatCoercion(parentPrio))
				stream << ')';
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

			apf.convert(APFloat::IEEEdouble(), APFloat::roundingMode::NearestTiesToEven, &losesInfo);
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

			apf.convert(APFloat::IEEEsingle(), APFloat::roundingMode::NearestTiesToEven, &losesInfo);
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
				if(buf.size() > floatsize || isFloat)
				{
					useFloat = true;
					// In asm.js double and float are distinct types, so
					// we cast back to double if needed
					if(asmjs && f->getType()->isDoubleTy())
					{
						if (parentPrio > LOWEST)
							stream << ' ';
						stream << '+';
					}
					if(parentPrio != FROUND)
						stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
					buf = tmpbuf;
				}
				else if(parentPrio > LOWEST && f->getValueAPF().isNegative())
					stream << ' ';
			}
			else
			{
				if(parentPrio > LOWEST && f->getValueAPF().isNegative())
					stream << ' ';
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
			if (useFloat && parentPrio != FROUND)
				stream << ')';
		}
	}
	else if(isa<ConstantInt>(c))
	{
		const ConstantInt* i=cast<ConstantInt>(c);
		if(i->getBitWidth()>=32)
		{
			if(parentPrio > LOWEST && i->isNegative())
				stream << ' ';
			stream << i->getSExtValue();
		}
		else
			stream << i->getZExtValue();
		if(i->getBitWidth()==64)
		{
			stream << 'n';
		}
	}
	else if(isa<ConstantPointerNull>(c))
	{
		if (asmjs)
			stream << '0';
		else
		{
			POINTER_KIND kind = PA.getPointerKind(c);
			if(kind == COMPLETE_OBJECT || kind == CONSTANT)
				stream << "null";
			else
				stream << "nullObj";
		}
	}
	else if(isa<GlobalAlias>(c))
	{
		const GlobalAlias* a=cast<GlobalAlias>(c);
		compileConstant(a->getAliasee());
	}
	else if(isa<GlobalValue>(c))
	{
		bool funcIsAsmJS = asmjs;
		if (!asmjs)
		{
			asmjs = cast<GlobalValue>(c)->getSection() == StringRef("asmjs");
		}
		assert(c->hasName());

		if(asmjs && isa<Function>(c))
		{
			const Function* f = cast<Function>(c);
			if (linearHelper.functionHasAddress(f)) {
				int addr = linearHelper.getFunctionAddress(f);
				stream << addr;
			} else {
				assert(f->empty());
				stream << '0';
			}
		}
		else if (isa<GlobalVariable>(c) && (!symbolicGlobalsAsmJS || !funcIsAsmJS) && asmjs)
		{
			stream << linearHelper.getGlobalVariableAddress(cast<GlobalVariable>(c));
		}
		else
			stream << getName(c, 0);
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
		llvm::errs() << "Unsupported constant type " << *c << "\n";
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
			const llvm::DebugLoc* oldLoc = nullptr;
			if(sourceMapGenerator)
			{
				const DebugLoc& debugLoc = it->getDebugLoc();
				if(debugLoc)
				{
					oldLoc = sourceMapGenerator->getDebugLoc();
					sourceMapGenerator->setDebugLoc(&debugLoc);
				}
			}
			compileInlineableInstruction(*cast<Instruction>(v), myPrio);
			if(sourceMapGenerator && oldLoc)
				sourceMapGenerator->setDebugLoc(oldLoc);
			if(isBooleanObject && !allowBooleanObjects)
			{
				stream << "?1:0";
				if(parentPrio >= TERNARY)
					stream << ')';
			}
		}
		else
		{
			stream << getName(it, 0);
		}
	}
	else if(const Argument* arg=dyn_cast<Argument>(v))
	{
		stream << getName(arg, 0);
	}
	else
	{
		llvm::errs() << "No name for value " << *v << "\n";
	}
}

void CheerpWriter::compileAggregateElem(const llvm::Value* v, uint32_t structIdx, uint32_t totalIdx,  PARENT_PRIORITY parentPrio)
{
	assert(v->getType()->isAggregateType());
	assert(structIdx == totalIdx && "aggregate with SPIT_REGULAR member not supported yet");
	if(auto* I = dyn_cast<Instruction>(v))
	{
		stream <<  getName(v, totalIdx);
	}
	else if(auto* C = dyn_cast<Constant>(v))
	{
		compileConstant(C->getAggregateElement(structIdx), parentPrio);
	}
	else
	{
#ifndef NDEBUG
		v->dump();
#endif
		report_fatal_error("unsupported aggregate");
	}
}

bool CheerpWriter::needsPointerKindConversion(const PHINode* phi, const Value* incoming, uint32_t elemIdx,
                                              const PointerAnalyzer& PA, const Registerize& registerize, const EdgeContext& edgeContext)
{
	if(canDelayPHI(phi, PA, registerize))
		return false;
	if(isa<UndefValue>(incoming))
		return false;
	Type* phiType=phi->getType();
	const Instruction* incomingInst=getUniqueIncomingInst(incoming, PA);
	if(!incomingInst)
		return true;
	assert(!isInlineable(*incomingInst, PA));
	if(phiType->isPointerTy())
	{
		POINTER_KIND incomingKind = PA.getPointerKindAssert(incomingInst);
		POINTER_KIND phiKind = PA.getPointerKindAssert(phi);
		if(incomingKind != phiKind)
			return true;
		const llvm::ConstantInt* incomingOffset = nullptr;
		const llvm::ConstantInt* phiOffset = nullptr;
		if(incomingKind == SPLIT_REGULAR || incomingKind == REGULAR || incomingKind == BYTE_LAYOUT)
			incomingOffset = PA.getConstantOffsetForPointer(incomingInst);
		if(phiKind == SPLIT_REGULAR || phiKind == REGULAR || phiKind == BYTE_LAYOUT)
			phiOffset = PA.getConstantOffsetForPointer(phi);
		if(incomingOffset != phiOffset)
			return true;
	}
	uint32_t phiReg = registerize.getRegisterId(phi, elemIdx, EdgeContext::emptyContext());
	uint32_t incomingReg = registerize.getRegisterId(incomingInst, elemIdx, edgeContext);
	return phiReg != incomingReg;
}

bool CheerpWriter::needsPointerKindConversionForBlocks(const BasicBlock* to, const BasicBlock* from,
                                                       const PointerAnalyzer& PA, const Registerize& registerize)
{
	class PHIHandler: public PHIHandlerUsingTemp
	{
	public:
		PHIHandler(const PointerAnalyzer& PA, EdgeContext& edgeContext, const Registerize& registerize):
		           PHIHandlerUsingTemp(PA, edgeContext),needsPointerKindConversion(false),PA(PA),registerize(registerize)
		{
		}
		~PHIHandler()
		{
		}
		bool needsPointerKindConversion;
	private:
		const PointerAnalyzer& PA;
		const Registerize& registerize;
		void handleRecursivePHIDependency(const InstElem& incomingEl) override
		{
			//Whenever we need to move to a temporary, we will also need to materialize the block
			needsPointerKindConversion = true;
		}
		void handlePHI(const InstElem& phiEl, const Value* incoming) override
		{
			auto* phi = cast<PHINode>(phiEl.instruction);
			needsPointerKindConversion |= CheerpWriter::needsPointerKindConversion(phi, incoming, phiEl.totalIdx, PA, registerize, edgeContext);
		}
	};

	EdgeContext localEdgeContext;
	auto handler = PHIHandler(PA, localEdgeContext, registerize);
	handler.runOnEdge(registerize, from, to);
	return handler.needsPointerKindConversion;
}

void CheerpWriter::compilePHIOfBlockFromOtherBlock(const BasicBlock* to, const BasicBlock* from)
{
	class WriterPHIHandler: public PHIHandlerUsingTemp
	{
	public:
		WriterPHIHandler(CheerpWriter& w, const Function* F):
			PHIHandlerUsingTemp(w.PA, w.edgeContext),asmjs(F->getSection() == StringRef("asmjs")),writer(w)
		{
		}
		~WriterPHIHandler()
		{
		}
	private:
		const bool asmjs;
		CheerpWriter& writer;
		void handleRecursivePHIDependency(const InstElem& incomingEl) override
		{
			writer.stream << writer.getName(incomingEl.instruction, incomingEl.totalIdx);

			//We walk back a step in "time"
			edgeContext.undoAssigment();
			//Find what name the instruction had previously
			writer.stream
				<< '='
				<< writer.getName(incomingEl.instruction, incomingEl.totalIdx)
				<< ';'
				<< writer.NewLine;
			//And undo the undo, back to the original situatuion
			edgeContext.processAssigment();
		}
		void handlePHI(const InstElem& phiEl, const Value* incoming) override
		{
			const auto* phi = cast<PHINode>(phiEl.instruction);
			// We can avoid assignment from the same register if no pointer kind conversion is required
			if(!needsPointerKindConversion(phi, incoming, phiEl.totalIdx, writer.PA, writer.registerize, edgeContext))
				return;
			// We can leave undefined values undefined
			if (isa<UndefValue>(incoming))
				return;
			Type* phiType=phi->getType();

			writer.stream << writer.getName(phi, phiEl.totalIdx, /*doNotConsiderEdgeContext*/true);
			if(phiType->isStructTy())
			{
				writer.stream << "=";
				writer.compileAggregateElem(incoming, phiEl.structIdx, phiEl.totalIdx, LOWEST);
			}
			else if(phiType->isPointerTy())
			{
				writer.stream << "=";
				POINTER_KIND k=writer.PA.getPointerKind(phi);
				assert(k!=CONSTANT);
				if((k==REGULAR || k==SPLIT_REGULAR || k==BYTE_LAYOUT) && writer.PA.getConstantOffsetForPointer(phi))
				{
					assert(phiEl.totalIdx == 0);
					writer.compilePointerBase(incoming);
				}
				else if(k==SPLIT_REGULAR)
				{
					if(phiEl.ptrIdx == 0)
					{
						writer.compilePointerBase(incoming);
					}
					else
					{
						POINTER_KIND incomingKind=writer.PA.getPointerKind(incoming);
						if (incomingKind == RAW)
						{
							writer.compileRawPointer(incoming, SHIFT);
							writer.stream << writer.pointerShiftOperator() << writer.getHeapShiftForType(cast<PointerType>(phiType)->getPointerElementType());
						}
						else
						{
							writer.compilePointerOffset(incoming, LOWEST);
						}
					}
				}
				else if(k==RAW)
				{
					assert(phiEl.totalIdx == 0);
					writer.compileRawPointer(incoming, LOWEST);
				}
				else
				{
					assert(phiEl.totalIdx == 0);
					writer.compilePointerAs(incoming, k);
				}
			}
			else
			{
				assert(phiEl.totalIdx == 0);
				const llvm::Instruction* instIncoming = dyn_cast<const Instruction>(incoming);
				if (!asmjs &&
						(writer.registerize.getRegisterId(phi, 0, EdgeContext::emptyContext())
							== writer.registerize.getRegisterId(phi, 0, edgeContext)) &&
						instIncoming &&
						isInlineable(*instIncoming, PA) &&
						writer.compileCompoundStatement(instIncoming, writer.registerize.getRegisterId(phi, 0, edgeContext)))
				{
					//compileCompoundStattement whenever returns true has also written into the stream the appropriate operation
				}
				else
				{
					writer.stream << "=";
					writer.compileOperand(incoming, LOWEST);
				}
			}
			writer.stream << ';' << writer.NewLine;
		}
	};
	WriterPHIHandler(*this, from->getParent()).runOnEdge(registerize, from, to);
}

void CheerpWriter::compileMethodArgs(User::const_op_iterator it, User::const_op_iterator itE, const CallBase& callV, bool forceBoolean)
{
	assert(callV.arg_begin() <= it && it <= callV.arg_end() && "compileMethodArgs, it out of range!");
	assert(callV.arg_begin() <= itE && itE <= callV.arg_end() && "compileMethodArgs, itE out of range!");
	assert(it <= itE);

	stream << '(';

	const Function* F = callV.getCalledFunction();
	bool asmjs = callV.getCaller()->getSection() == StringRef("asmjs");
	bool asmjsCallee = (F && F->getSection() == StringRef("asmjs")) || (!F && asmjs);

	// If the function is only declared and not in client namespace, skip arguments altogether
	if (F && F->empty() && !TypeSupport::isClientFunc(F) && !TypeSupport::isClientConstructorName(F->getName()))
	{
		stream << ')';
		return;
	}

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

	bool isImport = F && asmjs && globalDeps.asmJSImports().count(F);
	bool isClientF = F && TypeSupport::isClientFunc(F);
	uint32_t opCount = 0;
	for(User::const_op_iterator cur=it; cur!=itE; ++cur, ++opCount)
	{
		if(cur!=it)
			stream << ',';

		Type* tp = (*cur)->getType();

		bool asmjsCalleeArg = asmjsCallee || (F && arg_it != F->arg_end() && F->getAttributes().hasParamAttr(arg_it->getArgNo(), "force-raw"));
		if(isImport && tp->isFloatTy())
		{
			stream << "+";
			compileOperand(*cur,LOWEST);
		}
		else if(isClientF && F->hasParamAttribute(callV.getArgOperandNo(cur), Attribute::JsExportType))
		{
			Type* etp = F->getParamAttribute(callV.getArgOperandNo(cur), Attribute::JsExportType).getValueAsType();
			const auto& name = jsExportedTypes.find(etp)->getSecond();
			stream << "Object.create(" << name << ".prototype,{this:{value:";
			POINTER_KIND kind = PA.getPointerKindForJSExportedType(etp);
			compilePointerAs(*cur, kind);
			stream << "}})";
		}
		else if(tp->isPointerTy() && !TypeSupport::isRawPointer(tp, asmjsCalleeArg))
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
				argKind = PA.getPointerKindForArgument(&*arg_it);
			else
			{
				if(isa<ConstantPointerNull>(*cur) && (cur+1)==itE && cur!=it)
				{
					// Special case for NULL which are the last variadic parameter, copy the previous type
					Type* prevType = (*(cur-1))->getType();
					if(prevType->isPointerTy())
						tp = prevType;
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
			POINTER_KIND curKind = PA.getPointerKind(cur->get());
			// The second condition is for when the function is only declared
			// And the passed pointer is BYTE_LAYOUT. We decide to compile it as
			// SPLIT_REGULAR, since the code will crash here anyway
			if(argKind == SPLIT_REGULAR ||
				(argKind == COMPLETE_OBJECT && curKind == BYTE_LAYOUT))
			{
				if(F && PA.getConstantOffsetForPointer(&*arg_it))
					compilePointerBase(*cur, true);
				else
				{
					compilePointerBase(*cur, true);
					stream << ',';
					compilePointerOffset(*cur, LOWEST, true);
				}
			}
			else if(argKind != UNKNOWN)
				compilePointerAs(*cur, argKind, LOWEST);
		}
		else if(tp->isIntegerTy(1) && forceBoolean && !asmjs)
		{
			stream << "!!";
			compileOperand(*cur, HIGHEST);
		}
		else
		{
			PARENT_PRIORITY prio = LOWEST;
			const FunctionType* fTy = callV.getFunctionType();
			if (asmjs && (TypeSupport::isRawPointer(tp, true) || (tp->isIntegerTy() &&
				(isImport ||
				(!F && !linearHelper.getFunctionTables().count(fTy))))))
			{
				prio = BIT_OR;
			}
			if (tp->isPointerTy())
				compilePointerAs(*cur, RAW, prio);
			else
				compileOperand(*cur,prio);
			if (prio == BIT_OR)
				stream << "|0";
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
CheerpWriter::COMPILE_INSTRUCTION_FEEDBACK CheerpWriter::compileTerminatorInstruction(const Instruction& I)
{
	bool asmjs = currentFun->getSection() == StringRef("asmjs");
	switch(I.getOpcode())
	{
		case Instruction::Ret:
		{
			const ReturnInst& ri = cast<ReturnInst>(I);
			assert(I.getNumSuccessors()==0);
			Value* retVal = ri.getReturnValue();

			if(retVal)
			{
				if(auto* STy = dyn_cast<StructType>(retVal->getType()))
				{
					assert(STy->getNumElements() == 2 && STy->getElementType(0) == STy->getElementType(1) && STy->getElementType(0)->isIntegerTy(32));
					stream << "oSlot=";
					compileAggregateElem(retVal, 1, 1, LOWEST);
					stream << ";" << NewLine;
					stream << "return ";
					compileAggregateElem(retVal, 0, 0, LOWEST);
				}
				else
				{
					Registerize::REGISTER_KIND kind = registerize.getRegKindFromType(retVal->getType(), asmjs);
					switch(kind)
					{
						case Registerize::INTEGER:
							stream << "return ";
							if (retVal->getType()->isPointerTy())
								compilePointerAs(retVal, RAW, BIT_OR);
							else
							{
								compileOperand(retVal, BIT_OR);
							}
							stream << "|0";
							break;
						case Registerize::INTEGER64:
							stream << "return ";
							compileOperand(retVal, LOWEST);
							break;
						case Registerize::DOUBLE:
							stream << "return ";
							compileOperand(retVal, LOWEST);
							break;
						case Registerize::FLOAT:
							stream << "return ";
							stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
							compileOperand(retVal, FROUND);
							stream << ')';
							break;
						case Registerize::VECTOR:
							llvm::report_fatal_error("VECTOR register kind should not appear outside of WASM");
							break;
						case Registerize::OBJECT:
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
								compilePointerBase(retVal, /*forEscapingPointer*/true);
							else
								compilePointerAs(retVal, k);
							break;
					}
				}
			}
			else if(blockDepth == 0)
			{
				// A return statement at depth 0 means the function is finished
				// This is a void return at the end of the function, just skip it
				return COMPILE_EMPTY;
			}
			else
				stream << "return";

			stream << ';' << NewLine;
			return COMPILE_OK;
		}
		case Instruction::Resume:
		{
			stream << "throw ";
			compileOperand(I.getOperand(0));
			stream << ".a0[";
			compileOperand(I.getOperand(0));
			stream << ".a0o]";
			stream << ".a0;" << NewLine;
			return COMPILE_OK;
		}
		case Instruction::Invoke:
		{
			compileCallInstruction(cast<CallBase>(I), LOWEST);
			stream << ";" << NewLine;
			return COMPILE_OK;
		}
		case Instruction::Br:
		case Instruction::Switch:
			return COMPILE_EMPTY;
		case Instruction::Unreachable:
			stream << ";" << NewLine;
			return COMPILE_EMPTY;
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
			const auto* allocaStores = allocaStoresExtractor.getValuesForAlloca(ai);
			POINTER_KIND k = PA.getPointerKindAssert(ai);
			assert(k != RAW && "Allocas to RAW pointers are removed in the AllocaLowering pass");

			StringRef varName = getName(&I, 0);
			if(k == REGULAR)
			{
				stream << "{d:[";
				compileType(ai->getAllocatedType(), LITERAL_OBJ, varName, allocaStores);
				stream << "],o:0}";
			}
			else if(k == SPLIT_REGULAR)
			{
				stream << '[';
				compileType(ai->getAllocatedType(), LITERAL_OBJ, varName, allocaStores);
				stream << ']';
			}
			else if(k == BYTE_LAYOUT)
			{
				assert(!allocaStores);
				stream << "{d:";
				compileType(ai->getAllocatedType(), LITERAL_OBJ, varName);
				stream << ",o:0}";
			}
			else 
				compileType(ai->getAllocatedType(), LITERAL_OBJ, varName, allocaStores);

			return COMPILE_OK;
		}
		case Instruction::LandingPad:
		{
			Constant* C = currentFun->getPersonalityFn();
			assert(C);
			Value* PersonalityF = C->getOperand(0);
			compileOperand(PersonalityF);
			stream <<'(';
			if(I.use_empty())
				stream << "null";
			else
				stream << namegen.getName(&I, 0);
			stream << ",";
			const LandingPadInst& LP = cast<LandingPadInst>(I);
			auto entry = landingPadTable.getEntry(&LP);
			compileOperand(entry.start);
			stream << ",";
			compileOperand(entry.n);
			stream << ");" << NewLine;
			stream << getName(&I, 1) << "=oSlot";
			return COMPILE_OK;
		}
		case Instruction::InsertValue:
		{
			const auto& IV = cast<InsertValueInst>(I);
			assert(IV.getNumIndices() == 1);
			uint32_t newIdx = IV.getIndices()[0];
			for(const auto& ie: getInstElems(&IV, PA))
			{
				assert(ie.ptrIdx == 0);
				if(ie.totalIdx != 0)
				{
					stream << ";" << NewLine;
					stream << getName(&IV, ie.totalIdx);
					stream << '=';
				}
				if(ie.structIdx == newIdx)
				{
					compileOperand(IV.getOperand(1));
				}
				else
				{
					compileAggregateElem(IV.getAggregateOperand(), ie.structIdx, ie.totalIdx, LOWEST);
				}
			}
			return COMPILE_OK;
		}
		case Instruction::Store:
		{
			compileStore(cast<StoreInst>(I));
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
	Type* targetType = cast<GEPOperator>(gep_inst)->getResultElementType();

	StructType* containerStructType = dyn_cast<StructType>(getGEPContainerType(gep_inst));
	bool useDownCastArray = false;
	if(containerStructType && indices.size() > 1)
	{
		assert(isa<ConstantInt>(indices.back()));
		const ConstantInt* idx = cast<ConstantInt>(indices.back());
		uint32_t lastOffsetConstant = idx->getZExtValue();
		useDownCastArray = !types.useWrapperArrayForMember(PA, containerStructType, lastOffsetConstant);
	}
	const Value* baseOperand = gep_inst->getOperand(0);
	Type* elementTypeBaseOperand = cast<GEPOperator>(gep_inst)->getSourceElementType();
	bool byteLayout = PA.getPointerKindAssert(gep_inst) == BYTE_LAYOUT || TypeSupport::hasByteLayout(targetType);
	if (byteLayout)
	{
		bool byteLayoutFromHere = PA.getPointerKindAssert(baseOperand) != BYTE_LAYOUT;
		if (byteLayoutFromHere)
			compileCompleteObject(gep_inst);
		else if (!TypeSupport::hasByteLayout(targetType) && forEscapingPointer)
		{
			assert(TypeSupport::isTypedArrayType(targetType, /* forceTypedArray*/ true));
			// Recycle or create an appropiate typed array
			assert (!TypeSupport::hasByteLayout(targetType));
			stream << "(";
			compilePointerBaseTyped( baseOperand, elementTypeBaseOperand );
			stream << ".";
			compileTypedArrayType(targetType);
			stream << "||(";
			compilePointerBaseTyped( baseOperand, elementTypeBaseOperand );
			stream << ".";
			compileTypedArrayType(targetType);
			stream << "=new ";
			compileTypedArrayType(targetType);
			stream << '(';
			compilePointerBaseTyped( baseOperand, elementTypeBaseOperand );
			stream << ".buffer)))";
		}
		else
			compilePointerBaseTyped( baseOperand, elementTypeBaseOperand );
	}
	else if (indices.size() == 1)
	{
		// Just another pointer from this one
		compilePointerBaseTyped(gep_inst->getOperand(0), elementTypeBaseOperand );
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
		Type* basePointedType = cast<GEPOperator>(gep_inst)->getSourceElementType();
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

void CheerpWriter::compilePointerAs(const llvm::Value* p, POINTER_KIND kind, PARENT_PRIORITY prio)
{
	assert(p->getType()->isPointerTy());
	assert(kind != SPLIT_REGULAR);
	assert(kind != CONSTANT);
	POINTER_KIND valueKind = PA.getPointerKind(p);

	switch(kind)
	{
		case COMPLETE_OBJECT:
		{
			if (valueKind == BYTE_LAYOUT || valueKind == RAW)
				stream << "null";
			else
				compileCompleteObject(p);
			break;
		}
		case RAW:
		{
			assert(valueKind != BYTE_LAYOUT);
			assert(valueKind != COMPLETE_OBJECT);
			if (valueKind == RAW)
				compileRawPointer(p, prio);
			else
				compilePointerOffset(p, prio);
			break;
		}
		case REGULAR:
		{
			if (valueKind == CONSTANT)
			{
				stream << "nullObj";
			}
			else if (PA.getConstantOffsetForPointer(p) ||
					valueKind == SPLIT_REGULAR ||
					valueKind == RAW)
			{
				stream << "{d:";
				compilePointerBase(p, true);
				stream << ",o:";
				compilePointerOffset(p, LOWEST, true);
				stream << "}";
			}
			else
			{
				compileOperand(p);
			}
			break;
		}
		case BYTE_LAYOUT:
		{
			assert(valueKind==BYTE_LAYOUT || valueKind == CONSTANT);
			if (valueKind == CONSTANT)
			{
				stream << "nullObj";
			}
			else if (PA.getConstantOffsetForPointer(p))
			{
				stream << "{d:";
				compilePointerBase(p, true);
				stream << ",o:";
				compilePointerOffset(p, LOWEST);
				stream << "}";
			}
			else
			{
				compileOperand(p);
			}
			break;
		}
		default:
		{
			llvm::report_fatal_error("Unexpected pointer kind. This is a bug");
		}
	}
}

void CheerpWriter::compileGEPOffset(const llvm::User* gep_inst, PARENT_PRIORITY parentPrio)
{
	SmallVector< const Value*, 8 > indices(std::next(gep_inst->op_begin()), gep_inst->op_end());
	Type* basePointerType = gep_inst->getOperand(0)->getType();
	Type* targetType = cast<GEPOperator>(gep_inst)->getResultElementType();

	StructType* containerStructType = dyn_cast<StructType>(getGEPContainerType(gep_inst));
	bool useDownCastArray = false;
	if(containerStructType && indices.size() > 1)
	{
		assert(isa<ConstantInt>(indices.back()));
		const ConstantInt* idx = cast<ConstantInt>(indices.back());
		uint32_t lastOffsetConstant = idx->getZExtValue();
		useDownCastArray = !types.useWrapperArrayForMember(PA, containerStructType, lastOffsetConstant);
	}

	bool byteLayout = PA.getPointerKindAssert(gep_inst) == BYTE_LAYOUT;
	if (byteLayout)
	{
		if (TypeSupport::hasByteLayout(targetType))
			compilePointerOffset( gep_inst, HIGHEST );
		else
		{
			assert(TypeSupport::isTypedArrayType(targetType, /* forceTypedArray*/ true));
			compileByteLayoutOffset( gep_inst, BYTE_LAYOUT_OFFSET_FULL );
			uint32_t size = targetData.getTypeAllocSize(targetType);
			if(size != 1)
				stream << pointerShiftOperator() << Log2_32(size);
		}
	}
	else if (indices.size() == 1)
	{
		bool isOffsetConstantZero = isa<Constant>(indices.front()) && cast<Constant>(indices.front())->isNullValue();
		PARENT_PRIORITY prio = parentPrio;

		// Just another pointer from this one
		if (!isOffsetConstantZero)
		{
			if(parentPrio > pointerCoercionPrio()) stream << '(';
			prio = ADD_SUB;
		}
		compilePointerOffset(gep_inst->getOperand(0), prio);

		if(!isOffsetConstantZero)
		{
			stream << '+';
			compileOperand(indices.front(), prio);
			stream << pointerCoercionSuffix();
			if(parentPrio > pointerCoercionPrio()) stream << ')';
		}
	}
	else
	{
		if (useDownCastArray)
		{
			Type* basePointedType = cast<GEPOperator>(gep_inst)->getSourceElementType();
			compileCompleteObject(gep_inst->getOperand(0), indices.front());
			compileAccessToElement(basePointedType, makeArrayRef(std::next(indices.begin()), indices.end()), /*compileLastWrapperArray*/true);
			stream << ".o";
		}
		else
			compileOffsetForGEP(cast<GEPOperator>(gep_inst)->getSourceElementType(), indices);
	}
}

void CheerpWriter::compileGEP(const llvm::User* gep_inst, POINTER_KIND kind, PARENT_PRIORITY parentPrio)
{
	SmallVector< const Value*, 8 > indices(std::next(gep_inst->op_begin()), gep_inst->op_end());
	Type* basePointedType = cast<GEPOperator>(gep_inst)->getSourceElementType();

	StructType* containerStructType = dyn_cast<StructType>(GetElementPtrInst::getIndexedType(basePointedType,
			makeArrayRef(const_cast<Value* const*>(indices.begin()),
				     const_cast<Value* const*>(indices.end() - 1))));
	if(containerStructType && indices.size() > 1)
	{
		assert(isa<ConstantInt>(indices.back()));
	}


	// TODO: we need this hack because PointerAnalyzer cannot correctly assign
	// the RAW kind to null pointers
	bool asmjs = currentFun && currentFun->getSection()==StringRef("asmjs");
	bool asmjs_nullptr = asmjs && isa<ConstantPointerNull>(gep_inst->getOperand(0));
	if (RAW == kind || asmjs_nullptr)
	{
		compileRawPointer(gep_inst, parentPrio);
	}
	else if(COMPLETE_OBJECT == kind)
	{
		const llvm::Instruction* I = dyn_cast<Instruction>(gep_inst->getOperand(0));
		if (I && !isInlineable(*I, PA) &&
			(isGEP(I) || isBitCast(I)) && PA.getPointerKindAssert(I) == COMPLETE_OBJECT)
		{
			stream << getName(I, 0);
		} else {
			compileCompleteObject(gep_inst->getOperand(0), indices.front());
		}
		compileAccessToElement(cast<GEPOperator>(gep_inst)->getSourceElementType(),
		                       makeArrayRef(std::next(indices.begin()), indices.end()), /*compileLastWrapperArray*/true);
	}
	else
	{
		if (PA.getConstantOffsetForPointer(gep_inst))
		{
			compileCompleteObject(gep_inst->getOperand(0), indices.front());
			compileAccessToElement(basePointedType, makeArrayRef(std::next(indices.begin()),std::prev(indices.end())), /*compileLastWrapperArray*/false);
			return;
		}

		stream << "{d:";
		compilePointerBaseTyped( gep_inst, cast<GEPOperator>(gep_inst)->getResultElementType(), true);
		stream << ",o:";
		compilePointerOffset( gep_inst, LOWEST, true);
		stream << '}';
	}
}

void CheerpWriter::compileSignedInteger(const llvm::Value* v, bool forComparison, PARENT_PRIORITY parentPrio)
{
	//We anyway have to use 32 bits for sign extension to work
	uint32_t width = v->getType()->getIntegerBitWidth();
	uint32_t shiftAmount = width < 32 ? 32-width : 0;
	if(const ConstantInt* C = dyn_cast<ConstantInt>(v))
	{
		if (width == 64)
			stream << C->getSExtValue() << 'n';
		else if(forComparison)
			stream << (C->getSExtValue() << shiftAmount);
		else
			stream << C->getSExtValue();
		return;
	}
	if(width == 64)
	{
		compileOperand(v, parentPrio);
		return;
	}

	PARENT_PRIORITY signedPrio = shiftAmount == 0 ? BIT_OR : SHIFT;
	if(parentPrio > signedPrio) stream << '(';
	if(width == 32)
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

void CheerpWriter::compileUnsignedInteger(const llvm::Value* v, bool forAsmJSComparison, PARENT_PRIORITY parentPrio, bool forceTruncation)
{
	//We anyway have to use 32 bits for sign extension to work
	uint32_t width = v->getType()->getIntegerBitWidth();
	if(const ConstantInt* C = dyn_cast<ConstantInt>(v))
	{
		stream << C->getZExtValue();
		if (width == 64)
			stream << 'n';
		return;
	}
	if (width == 64)
	{
		stream << "BigInt.asUintN(64,";
		compileOperand(v, INTN);
		stream << ')';
	}
	else if(width == 32)
	{
		if(parentPrio > SHIFT) stream << '(';
		//Use simpler code
		compileOperand(v, SHIFT);
		stream << ">>>0";
		if(parentPrio > SHIFT) stream << ')';
	}
	else if(!forceTruncation && !needsUnsignedTruncation(v, /*asmjs, not fully accurate*/forAsmJSComparison))
	{
		if(forAsmJSComparison)
		{
			if(parentPrio > BIT_OR) stream << '(';
			compileOperand(v, BIT_OR);
			stream << "|0";
			if(parentPrio > BIT_OR) stream << ')';
		}
		else
			compileOperand(v, parentPrio);
	}
	else
	{
		if(parentPrio > BIT_AND) stream << '(';
		compileOperand(v, BIT_AND);
		stream << '&' << getMaskForBitWidth(width);
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
	bool isFloat = I.getType()->isFloatTy();
	bool isInt64 = I.getType()->isIntegerTy(64);
	switch(I.getOpcode())
	{
		case Instruction::BitCast:
		{
			POINTER_KIND k=PA.getPointerKindAssert(&I);
			compileBitCast(&I, k, parentPrio);
			return COMPILE_OK;
		}
		case Instruction::AddrSpaceCast:
		{
			compileOperand(I.getOperand(0), parentPrio);
			return COMPILE_OK;
		}
		case Instruction::FPToSI:
		case Instruction::FPToUI:
		{
			const CastInst& ci = cast<CastInst>(I);
			PARENT_PRIORITY prio = HIGHEST;
			bool isSigned = I.getOpcode() == Instruction::FPToSI;
			if (isInt64)
			{
				if (parentPrio != INTN)
					stream << "BigInt.as" << (isSigned?"Int":"Uint") << "N(64,";
				stream << "BigInt(Math.trunc(";
				prio = LOWEST;
			}
			else
			{
				stream << "~~";
			}
			compileOperand(ci.getOperand(0), prio);
			if (isInt64)
			{
				stream << "))";
				if (parentPrio != INTN)
					stream << ')';
			}
			return COMPILE_OK;
		}
		case Instruction::SIToFP:
		{
			const CastInst& ci = cast<CastInst>(I);
			bool opIsI64 = ci.getOperand(0)->getType()->isIntegerTy(64);
			if (isFloat && needsFloatCoercion(parentPrio))
				stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
			else
				stream << "(+";
			if (opIsI64)
			{
				stream << "Number(";
				compileOperand(ci.getOperand(0));
				stream << ')';
			}
			else
				compileSignedInteger(ci.getOperand(0), /*forComparison*/ false, HIGHEST);
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::UIToFP:
		{
			const CastInst& ci = cast<CastInst>(I);
			bool opIsI64 = ci.getOperand(0)->getType()->isIntegerTy(64);
			if (isFloat && needsFloatCoercion(parentPrio))
				stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
			else
				stream << "(+";
			//We need to cast to unsigned before
			if (opIsI64)
			{
				stream << "Number(";
			}
			compileUnsignedInteger(ci.getOperand(0), /*forAsmJSComparison*/ false, HIGHEST, /*forceTruncation*/ true);
			if (opIsI64)
			{
				stream << ')';
			}
			stream << ')';
			return COMPILE_OK;
		}
		case Instruction::GetElementPtr:
		{
			const GetElementPtrInst& gep = cast<GetElementPtrInst>(I);
			Type* t=gep.getOperand(0)->getType();
			assert(t->isPointerTy());

			if(TypeSupport::isClientType(gep.getSourceElementType()))
			{
				//Client objects are just passed through
				compileOperand(gep.getOperand(0), parentPrio);
			}
			else if (!isInlineable(gep, PA) && PA.getPointerKindAssert(&gep) == RAW)
			{
				compileRawPointer(&gep, parentPrio, /*forceGEP*/true);
			}
			else
			{
				compileGEP(&gep, PA.getPointerKindAssert(&gep), parentPrio);
			}
			return COMPILE_OK;
		}
		case Instruction::Add:
		{
			//Integer addition
			PARENT_PRIORITY addPrio = ADD_SUB;
			Value* lhs = I.getOperand(0);
			Value* rhs = I.getOperand(1);
			if(isInt64 && parentPrio!=INTN)
			{
				addPrio = INTN;
				parentPrio = LOWEST;
				stream << "BigInt.asIntN(64,";
			}
			else if(!isInt64 && needsIntCoercion(parentPrio))
				addPrio = BIT_OR;
			// TODO: Move negative constants on RHS
			if(parentPrio > addPrio) stream << '(';
			compileOperand(lhs, ADD_SUB);
			if(I.getType()->isIntegerTy(32) && isa<ConstantInt>(rhs) && cast<ConstantInt>(rhs)->isNegative())
			{
				// Special case negative constants, print them directly without adding an operator
				// NOTE: i8/i16 constants are always zero extended
				compileConstant(cast<ConstantInt>(rhs), LOWEST);
			}
			else
			{
				stream << "+";
				compileOperand(rhs, ADD_SUB);
			}
			if(addPrio == INTN)
				stream << ')';
			else if(addPrio == BIT_OR)
				stream << "|0";
			if(parentPrio > addPrio) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::FAdd:
		{
			//Floating point addition
			bool needsFround = isFloat && needsFloatCoercion(parentPrio);
			if(needsFround)
			{
				stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
				parentPrio = FROUND;
			}
			if(parentPrio > ADD_SUB) stream << '(';
			compileOperand(I.getOperand(0), ADD_SUB);
			stream << '+';
			compileOperand(I.getOperand(1), nextPrio(ADD_SUB));
			if(parentPrio > ADD_SUB) stream << ')';
			if(needsFround)
				stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Sub:
		{
			compileSubtraction(I.getOperand(0), I.getOperand(1), parentPrio, asmjs);
			return COMPILE_OK;
		}
		case Instruction::FNeg:
		{
			//Floating point subtraction
			bool needsFround = isFloat && needsFloatCoercion(parentPrio);
			if(needsFround)
			{
				stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
				parentPrio = FROUND;
			}
			if(parentPrio > ADD_SUB) stream << '(';
			stream << '-';
			compileOperand(I.getOperand(0), nextPrio(ADD_SUB));
			if(parentPrio > ADD_SUB) stream << ')';
			if(needsFround)
				stream << ')';
			return COMPILE_OK;
		}
		case Instruction::FSub:
		{
			//Floating point subtraction
			bool needsFround = isFloat && needsFloatCoercion(parentPrio);
			if(needsFround)
			{
				stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
				parentPrio = FROUND;
			}
			if(parentPrio > ADD_SUB) stream << '(';
			// Optimize negation
			if(I.getOperand(0) != ConstantFP::getZeroValueForNegation(I.getOperand(0)->getType()))
				compileOperand(I.getOperand(0), ADD_SUB);
			stream << '-';
			compileOperand(I.getOperand(1), nextPrio(ADD_SUB));
			if(parentPrio > ADD_SUB) stream << ')';
			if(needsFround)
				stream << ')';
			return COMPILE_OK;
		}
		case Instruction::ZExt:
		{
			const ZExtInst& bi = cast<ZExtInst>(I);
			Type* src=bi.getSrcTy();
			Type* dst=bi.getDestTy();
			PARENT_PRIORITY prio = parentPrio;
			assert(src->isIntegerTy() && dst->isIntegerTy());

			if (dst->isIntegerTy(64))
			{
				assert(!asmjs);
				stream << "BigInt(";
				prio = LOWEST;
			}
			if(src->isIntegerTy(1) && !asmjs)
			{
				//If the source type is i1, attempt casting from Boolean
				if(prio >= TERNARY) stream << '(';
				compileOperand(bi.getOperand(0), TERNARY, /*allowBooleanObjects*/true);
				stream << "?1:0";
				if(prio >= TERNARY) stream << ')';
			}
			else
			{
				//Let's mask out upper bits, to make sure we get zero extension
				//The value might have been initialized with a negative value
				compileUnsignedInteger(I.getOperand(0), /*forAsmJSComparison*/ false, prio);
			}
			if (dst->isIntegerTy(64))
			{
				stream << ")";
			}
			return COMPILE_OK;
		}
		case Instruction::SDiv:
		{

			compileDivRem(I.getOperand(0), I.getOperand(1), parentPrio, '/', /*isSigned*/true);
			return COMPILE_OK;
		}
		case Instruction::UDiv:
		{
			compileDivRem(I.getOperand(0), I.getOperand(1), parentPrio, '/', /*isSigned*/false);
			return COMPILE_OK;
		}
		case Instruction::SRem:
		{
			compileDivRem(I.getOperand(0), I.getOperand(1), parentPrio, '%', /*isSigned*/true);
			return COMPILE_OK;
		}
		case Instruction::URem:
		{
			compileDivRem(I.getOperand(0), I.getOperand(1), parentPrio, '%', /*isSigned*/false);
			return COMPILE_OK;
		}
		case Instruction::FDiv:
		{
			//Floating point division
			bool needsFround = isFloat && needsFloatCoercion(parentPrio);
			if(needsFround)
			{
				stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
				parentPrio = FROUND;
			}
			if(parentPrio > MUL_DIV) stream << '(';
			compileOperand(I.getOperand(0), MUL_DIV);
			stream << '/';
			compileOperand(I.getOperand(1), nextPrio(MUL_DIV));
			if(parentPrio > MUL_DIV) stream << ')';
			if(needsFround)
				stream << ')';
			return COMPILE_OK;
		}
		case Instruction::FRem:
		{
			//Floating point division remainder
			bool needsFround = isFloat && needsFloatCoercion(parentPrio);
			if(needsFround)
			{
				stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
				parentPrio = FROUND;
			}
			if(parentPrio > MUL_DIV) stream << '(';
			// NOTE: Modulo on float is not properly supported by Asm.js
			if(isFloat)
			{
				stream << "+";
				compileOperand(I.getOperand(0), HIGHEST);
			}
			else
				compileOperand(I.getOperand(0), MUL_DIV);
			stream << '%';
			if(isFloat)
			{
				stream << "+";
				compileOperand(I.getOperand(1), HIGHEST);
			}
			else
				compileOperand(I.getOperand(1), nextPrio(MUL_DIV));
			if(parentPrio > MUL_DIV) stream << ')';
			if(needsFround)
				stream << ')';
			return COMPILE_OK;
		}
		case Instruction::Mul:
		{
			//Integer signed multiplication
			PARENT_PRIORITY mulPrio = MUL_DIV;
			if(isInt64 && parentPrio!=INTN)
			{
				mulPrio = INTN;
				parentPrio = LOWEST;
				stream << "BigInt.asIntN(64,";
			}
			// NOTE: V8 requires imul to be coerced with `|0` no matter what in asm.js
			else if(!isInt64 && (needsIntCoercion(parentPrio) || asmjs))
				mulPrio = BIT_OR;
			if(parentPrio > mulPrio) stream << '(';
			if(!isInt64 && (useMathImul || asmjs))
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
			if(mulPrio == INTN)
				stream << ')';
			else if(mulPrio == BIT_OR)
				stream << "|0";
			if(parentPrio > mulPrio) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::FMul:
		{
			//Floating point multiplication
			bool needsFround = isFloat && needsFloatCoercion(parentPrio);
			if(needsFround)
			{
				stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
				parentPrio = FROUND;
			}
			if(parentPrio > MUL_DIV) stream << '(';
			compileOperand(I.getOperand(0), MUL_DIV);
			stream << '*';
			compileOperand(I.getOperand(1), nextPrio(MUL_DIV));
			if(parentPrio > MUL_DIV) stream << ')';
			if(needsFround)
				stream << ')';
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
			//Float comparison
			const CmpInst& ci = cast<CmpInst>(I);
			compileFloatComparison(ci.getOperand(0), ci.getOperand(1), ci.getPredicate(), parentPrio, asmjs);
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
			PARENT_PRIORITY shiftPrio = SHIFT;
			if(isInt64 && parentPrio!=INTN)
			{
				shiftPrio = INTN;
				parentPrio = LOWEST;
				stream << "BigInt.asIntN(64,";
			}
			int width = I.getOperand(0)->getType()->getIntegerBitWidth();
			if(parentPrio > SHIFT) stream << '(';
			bool needsTruncation = width < 32 && needsUnsignedTruncation(I.getOperand(0), asmjs);
			if(needsTruncation)
			{
				shiftPrio = BIT_AND;
				stream << '(';
			}
			if(width == 64)
				compileUnsignedInteger(I.getOperand(0), false, shiftPrio);
			else
				compileOperand(I.getOperand(0), shiftPrio);
			if(needsTruncation)
				stream << '&' << getMaskForBitWidth(width) << ')';
			if(width == 64)
				stream << ">>";
			else
				stream << ">>>";
			compileOperand(I.getOperand(1), nextPrio(SHIFT));
			if (shiftPrio == INTN)
				stream << ')';
			if(parentPrio > SHIFT) stream << ')';
			return COMPILE_OK;
		}
		case Instruction::AShr:
		{
			//Integer arithmetic shift right
			//No need to apply the >> operator. The result is an integer by spec
			int width = I.getOperand(0)->getType()->getIntegerBitWidth();
			if(parentPrio > SHIFT) stream << '(';
			if(width >= 32)
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
			PARENT_PRIORITY shiftPrio = SHIFT;
			if(isInt64 && parentPrio!=INTN)
			{
				shiftPrio = INTN;
				parentPrio = LOWEST;
				stream << "BigInt.asIntN(64,";
			}

			if(parentPrio > SHIFT) stream << '(';

			compileOperand(I.getOperand(0), SHIFT);
			stream << "<<";
			compileOperand(I.getOperand(1), nextPrio(SHIFT));

			if (shiftPrio == INTN)
				stream << ')';

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
			const TruncInst& ti = cast<TruncInst>(I);
			Type* src=ti.getSrcTy();
			PARENT_PRIORITY prio = parentPrio;
			if (src->isIntegerTy(64))
			{
				if (parentPrio > BIT_OR)
					stream << '(';
				stream << "Number(BigInt.asIntN(32,";
				prio = INTN;
			}
			compileOperand(I.getOperand(0), prio);
			if (src->isIntegerTy(64))
			{
				stream << "))|0";
				if (parentPrio > BIT_OR)
					stream << ')';
			}
			return COMPILE_OK;
		}
		case Instruction::SExt:
		{
			const SExtInst& si = cast<SExtInst>(I);
			Type* dst=si.getDestTy();
			PARENT_PRIORITY prio = parentPrio;
			if (dst->isIntegerTy(64))
			{
				assert(!asmjs);
				stream << "BigInt(";
				prio = LOWEST;
			}
			//We can use a couple of shift to make this work
			compileSignedInteger(I.getOperand(0), /*forComparison*/ false, prio);
			if (dst->isIntegerTy(64))
			{
				stream << ")";
			}
			return COMPILE_OK;
		}
		case Instruction::Select:
		{
			const SelectInst& si = cast<SelectInst>(I);
			Type* Ty = si.getType();
			StructType* STy = dyn_cast<StructType>(Ty);
			for(const auto& ie: getInstElems(&si, PA))
			{
				POINTER_KIND elemPtrKind = COMPLETE_OBJECT;
				if(STy)
				{
					Ty = STy->getElementType(ie.structIdx);
					if(Ty->isPointerTy())
					{
						TypeAndIndex b(STy, ie.structIdx, TypeAndIndex::STRUCT_MEMBER);
						elemPtrKind = PA.getPointerKindForMemberPointer(b);
					}
				}
				else if(Ty->isPointerTy())
				{
					elemPtrKind = PA.getPointerKind(&si);
				}
				if(ie.totalIdx != 0)
				{
					stream << ';' << NewLine << getName(&si, ie.totalIdx) << '=';
					parentPrio = LOWEST;
				}
				// We need to protect the outside RHS from being absorbed by the rightmost part of the select
				if(parentPrio != LOWEST)
					stream << "(";
				if(elemPtrKind == SPLIT_REGULAR)
				{
					compileOperand(si.getOperand(0), TERNARY, /*allowBooleanObjects*/ true);
					stream << '?';
					if(ie.ptrIdx == 0)
					{
						compilePointerBase(si.getOperand(1));
						stream << ':';
						compilePointerBase(si.getOperand(2));
					}
					else
					{
						compilePointerOffset(si.getOperand(1), TERNARY);
						stream << ':';
						compilePointerOffset(si.getOperand(2), TERNARY);
					}
				}
				else if(STy)
				{
					assert(ie.ptrIdx == 0 && "TODO handle aggregates with nested split regulars");
					compileOperand(si.getOperand(0), TERNARY, /*allowBooleanObjects*/ true);
					stream << '?';
					compileAggregateElem(si.getOperand(1), ie.structIdx, ie.totalIdx, TERNARY);
					stream << ':';
					compileAggregateElem(si.getOperand(2), ie.structIdx, ie.totalIdx, TERNARY);
				}
				else
				{
					compileSelect(&si, si.getCondition(), si.getTrueValue(), si.getFalseValue(), LOWEST);
				}
				if(parentPrio != LOWEST)
					stream << ")";
			}
			return COMPILE_OK;
		}
		case Instruction::ExtractValue:
		{
			const auto& EV = cast<ExtractValueInst>(I);
			assert(EV.getNumIndices() == 1);
			const Value* Op = EV.getAggregateOperand();
			StructType* OpTy = cast<StructType>(Op->getType());
			uint32_t structIdx = EV.getIndices()[0];
			if(EV.getType()->isPointerTy() && PA.getPointerKind(&EV) == SPLIT_REGULAR)
			{
				// TODO be more general (like compileLoad) and handle more cases
				// For now, such cases don't really exist, so we just assert asmjs
				assert(OpTy->hasAsmJS());
				compileHeapForType(EV.getType());
				stream << ';' << NewLine;
				stream << getName(&EV, 1) << '=';
				compileAggregateElem(EV.getAggregateOperand(), structIdx, structIdx, parentPrio);
			}
			else
			{
				compileAggregateElem(EV.getAggregateOperand(), structIdx, structIdx, parentPrio);
			}
			return COMPILE_OK;
		}
		case Instruction::FPExt:
		{
			const Value* src=I.getOperand(0);
			if(asmjs)
			{
				parentPrio = LOWEST;
				stream << "(+(";
			}
			compileOperand(src, parentPrio);
			if(asmjs)
				stream << "))";
			return COMPILE_OK;
		}
		case Instruction::FPTrunc:
		{
			const Value* src=I.getOperand(0);
			if(useMathFround)
			{
				stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
				parentPrio = FROUND;
			}
			compileOperand(src, parentPrio);
			if(useMathFround)
				stream << ')';
			return COMPILE_OK;
		}
		case Instruction::PtrToInt:
		{
			const PtrToIntInst& pi=cast<PtrToIntInst>(I);
			if(asmjs)
				compileRawPointer(pi.getOperand(0), parentPrio);
			else
				compilePtrToInt(pi.getOperand(0), pi.getType()->isIntegerTy(64));
			return COMPILE_OK;
		}
		case Instruction::VAArg:
		{
			const VAArgInst& vi=cast<VAArgInst>(I);
			if (asmjs)
			{
				// floats are promoted to double as per standard
				if (vi.getType()->isFloatingPointTy())
					stream<< '+' << getHeapName(HEAPF64);
				// int8 and int16 are promoted to int32 as per standard
				else if (vi.getType()->isIntegerTy() || vi.getType()->isPointerTy())
					stream << getHeapName(HEAP32);
				stream << '[';
				compileHeapAccess(vi.getPointerOperand());
				if (vi.getType()->isIntegerTy() || vi.getType()->isPointerTy() || vi.getType()->isFloatTy())
					stream << pointerShiftOperator() << "2]|0";
				else
					stream << pointerShiftOperator() << "3]";
				stream << ';' << NewLine;

				compileHeapAccess(vi.getPointerOperand());
				stream << "=((";
				compileHeapAccess(vi.getPointerOperand());
				stream << "|0)+8)|0";
			}
			else
			{
				stream << namegen.getBuiltinName(NameGenerator::Builtin::HANDLE_VAARG) << "(";
				compileCompleteObject(vi.getPointerOperand());
				stream << ')';
				if (vi.getType()->isPointerTy() && vi.getNumUses() > 0)
				{
					StringRef name = namegen.getName(&vi, 0);
					stream << ';' << NewLine;
					stream << name << "=";
					stream << name << "===0?";
					POINTER_KIND k = PA.getPointerKindForStoredType(vi.getType());
					if (k == COMPLETE_OBJECT)
						stream << "null";
					else
					{
						assert(k == REGULAR);
						stream << "nullObj";
					}
					stream << ':' << name;
				}

				assert( globalDeps.needHandleVAArg() );
			}
			return COMPILE_OK;
		}
		case Instruction::Call:
		{
			return compileCallInstruction(cast<CallBase>(I), parentPrio);
		}
		case Instruction::Load:
		{
			const LoadInst& li = cast<LoadInst>(I);
			if(li.use_empty())
				return COMPILE_EMPTY;
			compileLoad(li, parentPrio);
			return COMPILE_OK;
		}
		case Instruction::IntToPtr:
		{
			bool opIsI64 = I.getOperand(0)->getType()->isIntegerTy(64);
			PARENT_PRIORITY prio = parentPrio;
			if (opIsI64)
			{
				stream << "Number(BigInt.asIntN(32,";
				prio = INTN;
			}
			compileOperand(I.getOperand(0), prio);
			if (opIsI64)
			{
				stream << "))|0";
			}
			return COMPILE_OK;
		}
		case Instruction::AtomicRMW:
		{
			const AtomicRMWInst& ai = cast<AtomicRMWInst>(I);
			compileAtomicRMW(ai, parentPrio);
			return COMPILE_OK;
		}
		case Instruction::AtomicCmpXchg:
		{
			const AtomicCmpXchgInst& ai = cast<AtomicCmpXchgInst>(I);
			compileAtomicCmpXchg(ai, parentPrio);
			return COMPILE_OK;
		}
		default:
			stream << "alert('Unsupported code')";
			llvm::errs() << "\tImplement inst " << I.getOpcodeName() << '\n';
			return COMPILE_UNSUPPORTED;
	}
}

void CheerpWriter::compileLoad(const LoadInst& li, PARENT_PRIORITY parentPrio)
{
	auto* Ty = li.getType();
	const Value* ptrOp = li.getPointerOperand();
	bool asmjs = currentFun->getSection()==StringRef("asmjs");
	POINTER_KIND ptrKind = PA.getPointerKind(ptrOp);
	bool needsCheckBounds = false;
	if (checkBounds)
	{
			if(ptrKind == REGULAR || ptrKind == SPLIT_REGULAR)
			{
					needsCheckBounds = true;
					stream<<"(";
					compileCheckBounds(ptrOp);
					stream<<",";
			}
			else if(ptrKind == COMPLETE_OBJECT && isGEP(ptrOp))
			{
					needsCheckBounds = true;
					bool needsOffset = !li.use_empty() && Ty->isPointerTy() && PA.getPointerKindAssert(&li) == SPLIT_REGULAR && !PA.getConstantOffsetForPointer(&li);
					stream<<"(";
					compileCheckDefined(ptrOp, needsOffset);
					stream<<",";
			}
	}
	for(const auto& ie: getInstElems(&li, PA))
	{
		if(ie.totalIdx != 0)
		{
			stream << ';' << NewLine;
			stream << getName(&li, ie.totalIdx) << '=';
			parentPrio = LOWEST;
		}
		Type* Ty = li.getType();
		StructType* STy = nullptr;
		Registerize::REGISTER_KIND elemRegKind = registerize.getRegKindFromInstElem(ie, asmjs, &PA);
		POINTER_KIND elemPtrKind = COMPLETE_OBJECT;
		if(STy = dyn_cast<StructType>(Ty); STy)
		{
			Ty = STy->getElementType(ie.structIdx);
			if(Ty->isPointerTy())
			{
				TypeAndIndex b(STy, ie.structIdx, TypeAndIndex::STRUCT_MEMBER);
				elemPtrKind = PA.getPointerKindForMemberPointer(b);
			}
		}
		else if(Ty->isPointerTy())
		{
			elemPtrKind = PA.getPointerKind(&li);
		}
		bool isOffset = ie.ptrIdx == 1;
		compileLoadElem(li, Ty, STy, ptrKind, elemPtrKind, isOffset, elemRegKind, ie.structIdx, asmjs, parentPrio);
		if(needsCheckBounds)
		{
			needsCheckBounds = false;
			stream << ')';
		}
	}
}

void CheerpWriter::compileLoadElem(const LoadInst& li, Type* Ty, StructType* STy, POINTER_KIND ptrKind, POINTER_KIND loadKind, bool isOffset, Registerize::REGISTER_KIND regKind, uint32_t structElemIdx, bool asmjs, PARENT_PRIORITY parentPrio)
{
	const Value* ptrOp = li.getPointerOperand();
	if (li.isAtomic())
	{
		assert(!STy);
		assert(!isOffset);
		PARENT_PRIORITY shiftPrio = SHIFT;
		uint32_t shift = getHeapShiftForType(Ty);
		if (shift == 0)
			shiftPrio = LOWEST;
		if (parentPrio > BIT_OR)
			stream << "(";
		stream << namegen.getBuiltinName(NameGenerator::Builtin::ATOMICLOAD) << "(";
		if (Ty->isIntegerTy(1) || Ty->isIntegerTy(8))
			stream << "8,";
		else if (Ty->isIntegerTy(16))
			stream << "16,";
		else if (Ty->isIntegerTy(32))
			stream << "32,";
		else if (Ty->isIntegerTy(64) && UseBigInts && LinearOutput!=AsmJs)
			stream << "64,";
		else
			llvm::report_fatal_error("Unsupported bitwidth for atomic load");
		compileRawPointer(ptrOp, shiftPrio);
		if (shift != 0)
			stream << pointerShiftOperator() << shift;
		stream << ")";

		if (li.getType()->isIntegerTy() && parentPrio != BIT_OR)
			stream << "|0";
		if (parentPrio > BIT_OR)
			stream << ")";
		return ;
	}
	else if(regKind==Registerize::INTEGER && needsIntCoercion(parentPrio))
	{
		if (parentPrio > (Ty->isPointerTy() ? pointerCoercionPrio() : BIT_OR))
			stream << '(';
	}
	else if(regKind==Registerize::DOUBLE)
	{
		if (parentPrio > LOWEST)
			stream << ' ';
		stream << '+';
	}
	else if(regKind==Registerize::FLOAT && needsFloatCoercion(parentPrio))
	{
		stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
	}

	auto* PTy = dyn_cast<PointerType>(Ty);
	if(asmjs || ptrKind == RAW)
	{
		uint32_t offset = 0;
		if(STy)
		{
			const StructLayout* SL = targetData.getStructLayout(STy);
			offset =  SL->getElementOffset(structElemIdx);
		}
		if(PTy && (loadKind == REGULAR || loadKind == SPLIT_REGULAR))
		{
			switch(loadKind)
			{
			case REGULAR:
			{
				stream << "{d:";
				compileHeapForType(PTy->getPointerElementType());
				stream << ",o:";
				compileHeapAccess(ptrOp, Ty, offset);
				stream << '}';
				break;
			}
			case SPLIT_REGULAR:
			{
				if(isOffset)
				{
					compileHeapAccess(ptrOp, Ty, offset);
					int shift =  getHeapShiftForType(PTy->getPointerElementType());
					if (shift != 0)
						stream << pointerShiftOperator() << shift;
				}
				else
				{
					compileHeapForType(PTy->getPointerElementType());
				}
				break;
			}
			default:
			{
				assert(false);
			}
			}
		}
		else
		{
			compileHeapAccess(ptrOp, Ty, offset);
		}
	}
	else if (ptrKind == BYTE_LAYOUT)
	{
		assert(!STy);
		//Optimize loads of single values from unions
		compilePointerBase(ptrOp);
		assert(ptrOp->getType()== Ty->getPointerTo(ptrOp->getType()->getPointerAddressSpace()));
		if(Ty->isIntegerTy(8))
			stream << ".getUint8(";
		else if(Ty->isIntegerTy(16))
			stream << ".getUint16(";
		else if(Ty->isIntegerTy(32))
			stream << ".getInt32(";
		else if(Ty->isFloatTy())
			stream << ".getFloat32(";
		else if(Ty->isDoubleTy())
			stream << ".getFloat64(";
		else if (Ty->isIntegerTy(64))
		{
			if (!UseBigInts)
				report_fatal_error("unsupported INTEGER64 register");
			stream << ".getBigInt64(";
		}
		else
			report_fatal_error("Unsupported byte layout field");
		compilePointerOffset(ptrOp, LOWEST);
		if(!Ty->isIntegerTy(8))
			stream << ",true";
		stream << ')';
	}
	else if (ptrKind == CONSTANT)
	{
		// An invalid access to null/undefined which has not been removed by optizations.
		// Generate code that will trap at runtime.
		stream << "null[0]";
	}
	else
	{
		compileCompleteObject(ptrOp);
		if(STy)
		{
			compileAccessToElement(STy, {ConstantInt::get(IntegerType::get(Ty->getContext(), 32), structElemIdx)}, true);
		}
		if(isOffset)
			stream << 'o';
	}
	if(regKind==Registerize::INTEGER && needsIntCoercion(parentPrio))
	{
		stream << (Ty->isPointerTy() ? pointerCoercionSuffix() : "|0");
		if (parentPrio > (Ty->isPointerTy() ? pointerCoercionPrio() : BIT_OR))
			stream << ')';
	}
	else if(regKind==Registerize::FLOAT && needsFloatCoercion(parentPrio))
	{
		stream << ')';
	}
}

void CheerpWriter::compileStore(const StoreInst& si)
{
	const Value* ptrOp=si.getPointerOperand();
	const Value* valOp=si.getValueOperand();
	POINTER_KIND ptrKind = PA.getPointerKind(ptrOp);
	bool asmjs = currentFun && currentFun->getSection()==StringRef("asmjs");
	assert(ptrKind != CONSTANT);

	auto* Ty = valOp->getType();
	if (checkBounds)
	{
		if(ptrKind == REGULAR || ptrKind == SPLIT_REGULAR)
		{
			compileCheckBounds(ptrOp);
			stream<<",";
		}
		else if(ptrKind == COMPLETE_OBJECT && isGEP(ptrOp))
		{
			bool needsOffset = Ty->isPointerTy() && PA.getPointerKindAssert(&si) == SPLIT_REGULAR && !PA.getConstantOffsetForPointer(&si);
			compileCheckDefined(ptrOp, needsOffset);
			stream<<",";
		}
	}
	StructType* STy = dyn_cast<StructType>(Ty);
	for(const auto& ie: getInstElems(&si, PA))
	{
		if(ie.totalIdx != 0)
		{
			stream << ';' << NewLine;
		}
		Registerize::REGISTER_KIND elemRegKind = registerize.getRegKindFromInstElem(ie, asmjs, &PA);
		POINTER_KIND elemPtrKind = COMPLETE_OBJECT;
		if(STy)
		{
			Ty = STy->getElementType(ie.structIdx);
			if(Ty->isPointerTy())
			{
				TypeAndIndex b(STy, ie.structIdx, TypeAndIndex::STRUCT_MEMBER);
				elemPtrKind = PA.getPointerKindForMemberPointer(b);
			}
		}
		else if(Ty->isPointerTy())
		{
			elemPtrKind = PA.getPointerKind(&si);
		}
		bool isOffset = ie.ptrIdx == 1;
		compileStoreElem(si, Ty, STy, ptrKind, elemPtrKind, isOffset, elemRegKind, ie.totalIdx, ie.structIdx, asmjs);
	}
}

void CheerpWriter::compileStoreElem(const StoreInst& si, Type* Ty, StructType* STy, POINTER_KIND ptrKind, POINTER_KIND storedKind, bool isOffset, Registerize::REGISTER_KIND regKind, uint32_t structElemIdx, uint32_t elemIdx, bool asmjs)
{
	const Value* ptrOp=si.getPointerOperand();
	const Value* valOp=si.getValueOperand();
	assert(ptrKind != CONSTANT);
	if (si.isAtomic())
	{
		assert(!STy);
		assert(!isOffset);
		stream << namegen.getBuiltinName(NameGenerator::Builtin::ATOMICSTORE) << "(";
		Type* t = valOp->getType();
		PARENT_PRIORITY shiftPrio = SHIFT;
		uint32_t shift = getHeapShiftForType(t);
		if (shift == 0)
			shiftPrio = LOWEST;
		if (t->isIntegerTy(1) || t->isIntegerTy(8))
			stream << "8,";
		else if (t->isIntegerTy(16))
			stream << "16,";
		else if (t->isIntegerTy(32))
			stream << "32,";
		else if (t->isIntegerTy(64) && UseBigInts && LinearOutput!=AsmJs)
			stream << "64,";
		else
			llvm::report_fatal_error("Unsupported bitwidth for atomic store");
		compileRawPointer(ptrOp, shiftPrio);
		if (shift != 0)
			stream << pointerShiftOperator() << shift;
		stream << ",";
		compileOperand(valOp, BIT_OR);
		stream << "|0)";
		return ;
	}
	if (RAW == ptrKind || (asmjs && ptrKind == CONSTANT))
	{
		assert(!isOffset);
		uint32_t offset = 0;
		if(STy)
		{
			const StructLayout* SL = targetData.getStructLayout(STy);
			offset =  SL->getElementOffset(structElemIdx);
		}
		compileHeapAccess(ptrOp, Ty, offset);
	}
	else if (ptrKind == BYTE_LAYOUT)
	{
		assert(!STy);
		assert(!isOffset);
		//Optimize stores of single values from unions
		compilePointerBaseTyped(ptrOp, Ty);
		if(Ty->isIntegerTy(8))
			stream << ".setInt8(";
		else if(Ty->isIntegerTy(16))
			stream << ".setInt16(";
		else if(Ty->isIntegerTy(32))
			stream << ".setInt32(";
		else if(Ty->isFloatTy())
			stream << ".setFloat32(";
		else if(Ty->isDoubleTy())
			stream << ".setFloat64(";
		else if (Ty->isIntegerTy(64))
		{
			if (!UseBigInts)
				report_fatal_error("unsupported INTEGER64 register");
			stream << ".setBigInt64(";
		}
		else
			report_fatal_error("Unsupported byte layout field");
		compilePointerOffset(ptrOp, LOWEST);
		stream << ',';

		//Special case compilation of operand, the default behavior use =
		compileOperand(valOp, LOWEST);
		if(!Ty->isIntegerTy(8))
			stream << ",true";
		stream << ')';
		return;
	}
	else
	{
		compileCompleteObject(ptrOp);
		if(STy)
		{
			compileAccessToElement(STy, {ConstantInt::get(IntegerType::get(Ty->getContext(), 32), structElemIdx)}, true);
		}
		if(isOffset)
			stream << 'o';
	}

	stream << '=';
	if(STy)
	{
		compileAggregateElem(valOp, structElemIdx, elemIdx, LOWEST);
	}
	else
	{
		if(Ty->isPointerTy())
		{
			assert(storedKind != CONSTANT);
			bool hasConstantOffset = PA.getConstantOffsetForPointer(&si);
			if(storedKind==SPLIT_REGULAR || ((storedKind == REGULAR || storedKind == BYTE_LAYOUT) && hasConstantOffset))
			{
				if(isOffset)
				{
					assert(storedKind == SPLIT_REGULAR);
					compilePointerOffset(valOp, LOWEST, /*forEscapingPointer*/true);
				}
				else
				{
					compilePointerBase(valOp, /*forEscapingPointer*/true);
				}
			}
			else
			{
				compilePointerAs(valOp, storedKind);
			}
		}
		else
		{
			PARENT_PRIORITY storePrio = LOWEST;
			if(asmjs)
			{
				// On asm.js we can pretend the store will add a |0
				// This is not necessarily true in genericjs
				// As we might be storing in an object member or a plain array
				if(regKind == Registerize::INTEGER)
					storePrio = BIT_OR;
				// The same applies for fround
				else if(regKind == Registerize::FLOAT)
					storePrio = FROUND;
			}
			compileOperand(valOp, storePrio);
		}
	}
}

void CheerpWriter::compileAtomicRMW(const AtomicRMWInst& ai, PARENT_PRIORITY parentPrio)
{
	if (parentPrio > BIT_OR)
		stream << "(";
	switch(ai.getOperation())
	{
		case AtomicRMWInst::BinOp::Xchg:
			stream << namegen.getBuiltinName(NameGenerator::Builtin::ATOMICXCHG) << "(";
			break;
		case AtomicRMWInst::BinOp::Add:
			stream << namegen.getBuiltinName(NameGenerator::Builtin::ATOMICADD) << "(";
			break;
		case AtomicRMWInst::BinOp::Sub:
			stream << namegen.getBuiltinName(NameGenerator::Builtin::ATOMICSUB) << "(";
			break;
		case AtomicRMWInst::BinOp::And:
			stream << namegen.getBuiltinName(NameGenerator::Builtin::ATOMICAND) << "(";
			break;
		case AtomicRMWInst::BinOp::Or:
			stream << namegen.getBuiltinName(NameGenerator::Builtin::ATOMICOR) << "(";
			break;
		case AtomicRMWInst::BinOp::Xor:
			stream << namegen.getBuiltinName(NameGenerator::Builtin::ATOMICXOR) << "(";
			break;
		default:
			llvm::report_fatal_error("Unsupported atomicrmw opcode");
	}
	const Value* ptrOp=ai.getPointerOperand();
	const Value* valOp=ai.getValOperand();
	Type* t = valOp->getType();
	PARENT_PRIORITY shiftPrio = SHIFT;
	uint32_t shift = getHeapShiftForType(t);
	if (shift == 0)
		shiftPrio = LOWEST;
	if (t->isIntegerTy(1) || t->isIntegerTy(8))
		stream << "8,";
	else if (t->isIntegerTy(16))
		stream << "16,";
	else if (t->isIntegerTy(32))
		stream << "32,";
	else if (t->isIntegerTy(64) && UseBigInts && LinearOutput!=AsmJs)
		stream << "64,";
	else
		llvm::report_fatal_error("Unsupported bitwidth for atomicrmw");
	compileRawPointer(ptrOp, shiftPrio);
	if (shift != 0)
		stream << pointerShiftOperator() << shift;
	stream << ",";
	compileOperand(valOp, BIT_OR);
	stream << "|0)";

	if (ai.getType()->isIntegerTy() && parentPrio != BIT_OR)
		stream << "|0";
	if (parentPrio > BIT_OR)
		stream << ")";
}

void CheerpWriter::compileAtomicCmpXchg(const AtomicCmpXchgInst& ai, PARENT_PRIORITY parentPrio)
{
	const Value* ptrOp=ai.getPointerOperand();
	const Value* cmpOp=ai.getCompareOperand();
	const Value* newValOp=ai.getNewValOperand();
	Type* t = newValOp->getType();
	PARENT_PRIORITY shiftPrio = SHIFT;
	uint32_t shift = getHeapShiftForType(t);
	if (shift == 0)
		shiftPrio = LOWEST;

	stream << namegen.getBuiltinName(NameGenerator::Builtin::ATOMICCMPXCHG) << "(";
	if (t->isIntegerTy(1) || t->isIntegerTy(8))
		stream << "8,";
	else if (t->isIntegerTy(16))
		stream << "16,";
	else if (t->isIntegerTy(32))
		stream << "32,";
	else if (t->isIntegerTy(64) && UseBigInts && LinearOutput!=AsmJs)
		stream << "64,";
	else
		llvm::report_fatal_error("Unsupported bitwidth for atomicmpxchg");
	compileRawPointer(ptrOp, shiftPrio);
	if (shift != 0)
		stream << pointerShiftOperator() << shift;
	stream << ",";
	compileOperand(cmpOp, BIT_OR);
	stream << "|0,";
	compileOperand(newValOp, BIT_OR);
	stream << "|0)|0";

	// Compile the second part of this instruction, the comparison between the loaded value
	// and the compare operand. A compare operand to a cmpxchg instruction cannot be inlined, so
	// calling compileOperand twice is safe.
	// We only compile this part if this instruction has uses.
	if (!ai.use_empty())
	{
		stream << ";" << NewLine;
		stream << namegen.getName(&ai, 1) << "=(" << namegen.getName(&ai, 0) << "|0)==(";
		compileOperand(cmpOp, BIT_OR);
		stream << "|0)";
	}
}

CheerpWriter::COMPILE_INSTRUCTION_FEEDBACK CheerpWriter::compileCallInstruction(const CallBase& ci, PARENT_PRIORITY parentPrio)
{
	bool asmjs = currentFun->getSection() == StringRef("asmjs");
	const Function * calledFunc = ci.getCalledFunction();
	const Value * calledValue = ci.getCalledOperand();
	const FunctionType* fTy = ci.getFunctionType();
	// Skip over bitcasts of function
	if(isBitCast(calledValue))
	{
		calledValue = cast<User>(calledValue)->getOperand(0);
		calledFunc = dyn_cast<Function>(calledValue);
	}
	const Type* retTy = fTy->getReturnType();
	// NOTE: if the type is void, OBJECT is returned, but we explicitly
	// check the void case later
	Registerize::REGISTER_KIND kind = registerize.getRegKindFromType(retTy, asmjs);

	// If the caller is genericjs, the callee is asmjs, and a SPLIT_REGULAR is returned,
	// the function is returning the offset, not the object. So assign the main name now
	// to the correct heap type, and the return value to oSlot, correctly shifted,
	// afterwards
	bool asmjsCallee = calledFunc && calledFunc->getSection() == StringRef("asmjs");
	uint32_t addrShift = 0;
	if (!asmjs && asmjsCallee && kind == Registerize::OBJECT && retTy->isPointerTy() && PA.getPointerKindAssert(&ci) == SPLIT_REGULAR && !ci.use_empty())
	{
		addrShift = compileHeapForType(cast<PointerType>(ci.getType())->getPointerElementType());
		stream << ';' << NewLine;
		stream << getName(&ci, 1) << '=';
	}

	if(!retTy->isVoidTy())
	{
		if(kind == Registerize::DOUBLE)
		{
			if(parentPrio > LOWEST)
				stream << ' ';
			stream << '+';
		}
		else if(kind == Registerize::FLOAT)
		{
			stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
			if (asmjs && !asmjsCallee && !(calledFunc && calledFunc->isIntrinsic()))
				stream << '+';
		}
		else if(kind == Registerize::INTEGER && parentPrio >= BIT_OR)
		{
			stream << '(';
		}
	}
	if(calledFunc)
	{
		COMPILE_INSTRUCTION_FEEDBACK cf=handleBuiltinCall(ci, calledFunc);
		if(cf!=COMPILE_UNSUPPORTED)
		{
			if (!retTy->isVectorTy() && (
				(kind == Registerize::INTEGER && parentPrio >= BIT_OR) ||
				kind == Registerize::FLOAT))
			{
				stream << ')';
			}
			return cf;
		}
		stream << getName(calledFunc, 0);
	}
	else if (ci.isInlineAsm())
	{
		compileInlineAsm(cast<CallInst>(ci));
		//If we are dealing with inline asm we are done, close coercions
		switch(kind)
		{
			case Registerize::INTEGER:
				stream << "|0";
				if(parentPrio >= BIT_OR)
					stream << ')';
				break;
			case Registerize::INTEGER64:
				break;
			case Registerize::DOUBLE:
				break;
			case Registerize::FLOAT:
				stream << ')';
				break;
			case Registerize::OBJECT:
				break;
			case Registerize::VECTOR:
				llvm::report_fatal_error("VECTOR register kind should not appear outside of WASM");
				break;
		}
		return COMPILE_OK;
	}
	else if (asmjs)
	{
		//Indirect call, asm.js mode
		if (!linearHelper.getFunctionTables().count(fTy))
		{
			stream << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY);
		}
		else
		{
			const auto& table = linearHelper.getFunctionTables().at(fTy);
			stream << table.name << '[';
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
				compileRawPointer(calledValue, PARENT_PRIORITY::BIT_AND);
			}
			stream << '&' << linearHelper.getFunctionAddressMask(fTy) << ']';
		}

	}
	else
	{
		//Indirect call, normal mode
		compilePointerAs(calledValue, COMPLETE_OBJECT);
	}

	{
		// In calling asmjs functions the varargs are passed on the stack
		bool asmJSCallingConvention = asmjs || (calledFunc && calledFunc->getSection() == StringRef("asmjs"));
		size_t n = asmJSCallingConvention ? fTy->getNumParams() : ci.arg_size();
		compileMethodArgs(ci.op_begin(),ci.op_begin()+n, ci, /*forceBoolean*/ false);
	}
	if(!retTy->isVoidTy())
	{
		switch(kind)
		{
			case Registerize::INTEGER:
				stream << "|0";
				if(parentPrio >= BIT_OR)
					stream << ')';
				break;
			case Registerize::INTEGER64:
				break;
			case Registerize::DOUBLE:
				break;
			case Registerize::FLOAT:
				stream << ')';
				break;
			case Registerize::VECTOR:
				llvm::report_fatal_error("VECTOR register kind should not appear outside of WASM");
				break;
			case Registerize::OBJECT:
				if(PA.getPointerKindAssert(&ci) == SPLIT_REGULAR && !ci.use_empty())
				{
					assert(!isInlineable(ci, PA));
					if (asmjsCallee)
					{
						if (addrShift != 0)
							stream << pointerShiftOperator() << addrShift;
						else
							stream << pointerCoercionSuffix();
					}
					else
					{
						stream << ';' << NewLine;
						stream << getName(&ci, 1) << "=oSlot";
					}
				}
				break;
		}
	}
	return COMPILE_OK;
}

bool CheerpWriter::compileCompoundStatement(const Instruction* I, uint32_t regId)
{
	StringRef oper;
	bool checkOp0 = false;
	bool checkOp1 = false;
	// We don't use a compound statement if float 32 bit precision is enabled,
	// because we need to wrap every operation in fround()
	// We also don't use it with 64 bit integers, because we need to wrap
	// every operation with BigInt.asIntN()
	if (!I->getType()->isIntegerTy(64) && (!I->getType()->isFloatTy() || NoJavaScriptMathFround))
	{
		switch(I->getOpcode())
		{
			//Commutative operations
			case Instruction::FAdd:
				checkOp0 = true;
				checkOp1 = true;
				oper = "+=";
				break;
			case Instruction::FMul:
				checkOp0 = true;
				checkOp1 = true;
				oper = "*=";
				break;
			case Instruction::Or:
				checkOp0 = true;
				checkOp1 = true;
				oper = "|=";
				break;
			case Instruction::And:
				checkOp0 = true;
				checkOp1 = true;
				oper = "&=";
				break;
			case Instruction::Xor:
				checkOp0 = true;
				checkOp1 = true;
				oper = "^=";
				break;
			//Non-commutative operations
			case Instruction::FSub:
				checkOp0 = true;
				oper = "-=";
				break;
			case Instruction::FDiv:
				checkOp0 = true;
				oper = "/=";
				break;
			case Instruction::FRem:
				checkOp0 = true;
				oper = "%=";
				break;
			case Instruction::LShr:
				checkOp0 = true;
				oper = ">>>=";
				break;
			case Instruction::AShr:
				checkOp0 = true;
				oper = ">>=";
				break;
			case Instruction::Shl:
				checkOp0 = true;
				oper = "<<=";
				break;
		}
	}
	auto DoCompount = [this](Value* op, uint32_t regId) -> bool
	{
		Instruction* opI = dyn_cast<Instruction>(op);
		if(!opI)
			return false;
		if(isInlineable(*opI, PA))
			return false;
		if(regId != registerize.getRegisterId(opI, 0, edgeContext))
			return false;
		return true;
	};
	if(checkOp0 && DoCompount(I->getOperand(0), regId))
	{
		stream << oper;
		compileOperand(I->getOperand(1));
		return true;
	}
	else if(checkOp1 && DoCompount(I->getOperand(1), regId))
	{
		stream << oper;
		compileOperand(I->getOperand(0));
		return true;
	}
	else
		return false;
}

void CheerpWriter::compileBB(const BasicBlock& BB)
{
	bool asmjs = BB.getParent()->getSection() == StringRef("asmjs");
	bool emptyBlock = true;
	class PHIHandler: public PHIHandlerUsingTemp
	{
	public:
		PHIHandler(CheerpWriter& writer):
		           PHIHandlerUsingTemp(writer.PA, writer.edgeContext), hasDelayedPHIs(false), writer(writer)
		{
		}
		bool hasDelayedPHIs;
	private:
		CheerpWriter writer;
		void handleRecursivePHIDependency(const InstElem& incomingEl) override
		{
		}
		void handlePHI(const InstElem& phiEl, const Value* incoming) override
		{
			const auto* phi = cast<PHINode>(phiEl.instruction);
			if(!canDelayPHI(phi, PA, writer.registerize))
				return;
			hasDelayedPHIs = true;
			assert(!phi->getType()->isStructTy());
			POINTER_KIND k = PA.getPointerKindAssert(phi);
			writer.stream << writer.getName(phi, phiEl.totalIdx) << '=';
			if((k == REGULAR || k == SPLIT_REGULAR || k == BYTE_LAYOUT) && PA.getConstantOffsetForPointer(phi))
				writer.compilePointerBase(incoming);
			else if(k == SPLIT_REGULAR)
			{
				if(phiEl.ptrIdx == 0)
					writer.compilePointerBase(incoming);
				else
					writer.compilePointerOffset(incoming, LOWEST);
			}
			else
				writer.compilePointerAs(incoming, k);
			writer.stream << ';' << writer.NewLine;
		}
	};
	if(!BB.phis().empty())
	{
		PHIHandler PH(*this);
		PH.runOnEdge(registerize, *pred_begin(&BB), &BB);
		emptyBlock = !PH.hasDelayedPHIs;
	}
	for(const auto& I: BB)
	{
		if(const PHINode* phi = dyn_cast<PHINode>(&I))
		{
			continue;
		}
		if(isInlineable(I, PA))
			continue;
		const DebugLoc& debugLoc = I.getDebugLoc();
		if(sourceMapGenerator)
		{
			if(debugLoc)
				sourceMapGenerator->setDebugLoc(&debugLoc);
			else
				sourceMapGenerator->setDebugLoc(nullptr);
		}
		bool isDowncast = false;
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
				continue;
			}
			else if(II->getIntrinsicID()==Intrinsic::cheerp_downcast)
				isDowncast = true;
		}
		if(!I.use_empty() && !I.getType()->isVoidTy())
		{
			uint32_t regId = registerize.getRegisterId(&I, 0, edgeContext);
			assert(namegen.getRegName(BB.getParent(), regId) == getName(&I, 0));
			stream << namegen.getRegName(BB.getParent(), regId);
			if(!asmjs && compileCompoundStatement(&I, regId))
			{
				stream << ';' << NewLine;
				emptyBlock = false;
				continue;
			}
			else
				stream << "=";
		}
		if(I.isTerminator())
		{
			auto ret = compileTerminatorInstruction(I);
			if (ret == COMPILE_OK)
				emptyBlock = false;
		}
		else if(!I.use_empty() || I.mayHaveSideEffects())
		{
			COMPILE_INSTRUCTION_FEEDBACK ret=compileNotInlineableInstruction(I, LOWEST);

			if(ret==COMPILE_OK)
			{
				stream << ';' << NewLine;
				emptyBlock = false;
			}
			else if(ret==COMPILE_UNSUPPORTED)
			{
				//Stop basic block compilation
				return;
			}
		}
	}
	if (emptyBlock && !isNumStatementsLessThan<1>(&BB, PA, registerize) && lastDepth0Block != &BB)
	{
		stream << ';' << NewLine;
	}
}

bool CheerpWriter::isInlineableInstruction(const Value* v) const
{
	if(const Instruction* I = dyn_cast<Instruction>(v))
		return isInlineable(*I, PA);
	else
		return false;
}

void CheerpWriter::compileMethodLocal(StringRef name, Registerize::REGISTER_KIND kind)
{
	stream << name << '=';
	if(kind == Registerize::INTEGER)
		stream << '0';
	else if (kind == Registerize::INTEGER64)
	{
		if (!UseBigInts)
			report_fatal_error("unsupported INTEGER64 register");
		stream << "0n";
	}
	else if(kind == Registerize::DOUBLE)
	{
		// NOTE: V8 requires the `.` to identify it as a double in asm.js
		stream << "-0.";
	}
	else if(kind == Registerize::FLOAT)
		stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << "(0.)";
	else
		stream << "null";
}

void CheerpWriter::compileMethodLocals(const Function& F)
{
	// Declare are all used locals in the beginning
	bool firstVar = true;
	const std::vector<Registerize::RegisterInfo>& regsInfo = registerize.getRegistersForFunction(&F);
	for(unsigned int regId = 0; regId < regsInfo.size(); regId++)
	{
		if(firstVar)
			stream << "var ";
		else
			stream << ',';
		compileMethodLocal(namegen.getRegName(&F, regId), regsInfo[regId].regKind);
		firstVar = false;
	}
	if(!firstVar)
		stream << ';' << NewLine;
}

void CheerpWriter::compileCondition(const BasicBlock* BB, bool booleanInvert)
{
	const Instruction* term=BB->getTerminator();
	bool asmjs = BB->getParent()->getSection() == StringRef("asmjs");
	assert(isa<BranchInst>(term));
	const BranchInst* bi=cast<BranchInst>(term);
	assert(bi->isConditional());
	const Value* cond = bi->getCondition();
	bool canInvertCond = isInlineableInstruction(cond);
	if(canInvertCond && isa<ICmpInst>(cond))
	{
		const CmpInst* ci = cast<CmpInst>(cond);
		CmpInst::Predicate p = ci->getPredicate();
		if(booleanInvert)
			p = CmpInst::getInversePredicate(p);
		compileIntegerComparison(ci->getOperand(0), ci->getOperand(1), p, PARENT_PRIORITY::LOWEST);
	}
	else if(canInvertCond && isa<FCmpInst>(cond))
	{
		const CmpInst* ci = cast<CmpInst>(cond);
		CmpInst::Predicate p = ci->getPredicate();
		if(booleanInvert)
			p = CmpInst::getInversePredicate(p);
		compileFloatComparison(ci->getOperand(0), ci->getOperand(1), p, PARENT_PRIORITY::LOWEST, asmjs);
	}
	else
	{
		if(booleanInvert)
			stream << "!(";
		compileOperand(cond, PARENT_PRIORITY::LOWEST, /*allowBooleanObjects*/ true);
		if(booleanInvert)
			stream << ")";
	}
}

DenseSet<const Token*> CheerpWriter::getLabeledTokens(const TokenList& Tokens)
{
	std::vector<const Token*> ScopeStack;
	// First, compute which Tokens need a label
	DenseSet<const Token*> LabeledTokens;
	for (const Token& T: Tokens)
	{
		switch (T.getKind())
		{
			case Token::TK_BasicBlock:
			case Token::TK_Block:
			case Token::TK_Case:
			case Token::TK_If:
			case Token::TK_IfNot:
			case Token::TK_Else:
			case Token::TK_Prologue:
			case Token::TK_Condition:
			case Token::TK_BrIf:
			case Token::TK_BrIfNot:
			case Token::TK_Try:
			case Token::TK_Catch:
				break;
			case Token::TK_Loop:
			case Token::TK_Switch:
				ScopeStack.push_back(&T);
				break;
			case Token::TK_Branch:
			{
				const Token* Target = T.getMatch();
				if (Target->getKind() == Token::TK_End)
					Target = Target->getMatch();
				if ((Target->getKind() & (Token::TK_Block|Token::TK_If|Token::TK_IfNot))
					|| Target != ScopeStack.back())
					LabeledTokens.insert(Target);
				break;
			}
			case Token::TK_End:
				if (T.getMatch()->getKind() == Token::TK_Loop
					|| T.getMatch()->getKind() == Token::TK_Switch)
				{
					ScopeStack.pop_back();
				}
				break;
			case Token::TK_Invalid:
				report_fatal_error("Invalid token found");
				break;
		}
	}
	assert(ScopeStack.empty());
	return LabeledTokens;
}

bool CheerpWriter::omitBraces(const Token& T, const PointerAnalyzer& PA, const Registerize& registerize)
{
	assert(T.getKind()&(Token::TK_If|Token::TK_IfNot|Token::TK_Else));
	const Token* Inner = T.getNextNode();
	const Token* End = T.getMatch();
	// Empty if. Ideally it should have been removed by now...
	if (End == Inner)
		return true;
	switch (Inner->getKind())
	{
		case Token::TK_Prologue:
		{
			return false;
		}
		case Token::TK_BasicBlock:
		{
			if (Inner->getNextNode() != End)
				return false;
			return isNumStatementsLessThan<2>(Inner->getBB(), PA, registerize);
		}
		case Token::TK_Block:
		case Token::TK_Loop:
		case Token::TK_Switch:
		{
			return Inner->getMatch()->getNextNode() == End;
		}
		case Token::TK_If:
		case Token::TK_IfNot:
		{
			if (End->getKind() == Token::TK_Else)
				return false;
			else if (Inner->getMatch()->getKind() == Token::TK_Else)
				return Inner->getMatch()->getMatch()->getNextNode() == End;
			else
				return Inner->getMatch()->getNextNode() == End;
		}
		case Token::TK_Try:
		{
			return Inner->getMatch()->getMatch()->getNextNode() == End;
		}
		case Token::TK_Branch:
		{
			return Inner->getNextNode() == End;
		}
		default:
		{
			llvm_unreachable("Unexpected Token");
		}
	} 
}

class LabelNameGenerator
{
	DenseMap<const Token*, uint32_t> Labels;
	std::vector<SmallString<2>> LabelList;
	uint32_t NextLabel{0};
	std::vector<std::string> External;
	name_iterator<JSSymbols, 2> NameIt{"", JSSymbols(External)};
public:
	using iterator = std::vector<SmallString<2>>::iterator;
	iterator end()
	{
		return LabelList.end();
	}
	const SmallString<2>& allocate(const Token* T)
	{
		if (NextLabel == LabelList.size())
		{
			LabelList.push_back(*NameIt);
			++NameIt;
		}
		Labels.insert(std::make_pair(T, NextLabel));
		return LabelList[NextLabel++];
	}
	void deallocate()
	{
		NextLabel--;
	}
	iterator get(const Token* T)
	{
		auto it = Labels.find(T);
		if (it == Labels.end())
			return end();
		return LabelList.begin()+it->second;
	}
};

void CheerpWriter::compileTokens(const TokenList& Tokens)
{
	auto LabeledTokens = getLabeledTokens(Tokens);
	LabelNameGenerator LabelGen;
	for (auto it = Tokens.begin(), ie = Tokens.end(); it != ie; ++it)
	{
		const Token& T = *it;
		bool Labeled = LabeledTokens.count(&T);
		if (Labeled)
		{
			auto Label = LabelGen.allocate(&T);
			stream << Label << ':';
		}
		switch (T.getKind())
		{
			case Token::TK_BasicBlock:
			{
				if(blockDepth == 0)
					lastDepth0Block = T.getBB();
				else
					lastDepth0Block = nullptr;
				compileBB(*T.getBB());
				break;
			}
			case Token::TK_Loop:
			{
				stream << "while(1){" << NewLine;
				blockDepth++;
				break;
			}
			case Token::TK_Block:
			{
				stream << '{' << NewLine;
				blockDepth++;
				break;
			}
			case Token::TK_Condition:
			{
				compileCondition(T.getBB(), false);
				stream << ';' << NewLine;
				break;
			}
			case Token::TK_If:
			case Token::TK_IfNot:
			{
				bool IfNot = T.getKind() == Token::TK_IfNot;
				stream << "if(";
				compileCondition(T.getBB(), IfNot);
				stream << ")";
				if (!omitBraces(T, PA, registerize))
				{
					stream << '{' << NewLine;
				}
				else if (T.getNextNode() == T.getMatch())
				{
					// Empty if. Ideally it should have been removed by now...
					stream << ';' << NewLine;
				}
				blockDepth++;
				break;
			}
			case Token::TK_Else:
			{
				if (!omitBraces(*T.getMatch()->getMatch(), PA, registerize))
					stream << '}';
				stream << "else";
				if (!omitBraces(T, PA, registerize))
				{
					stream << '{' << NewLine;
				}
				else if (T.getNextNode() == T.getMatch())
				{
					// Empty else. Ideally it should have been removed by now...
					stream << ';' << NewLine;
				}
				else 
				{
					stream << ' ';
				}
				break;
			}
			case Token::TK_Branch:
			{
				if (T.getMatch()->getKind() == Token::TK_Loop)
					stream << "continue";
				else
					stream << "break";
				const Token* Scope = T.getMatch()->getKind() == Token::TK_Loop
					? T.getMatch()
					: T.getMatch()->getMatch();
				auto LabelIt = LabelGen.get(Scope);
				if (LabelIt != LabelGen.end())
				{
					stream << ' ' << *LabelIt;
				}
				stream << ';' << NewLine;
				break;
			}
			case Token::TK_End:
			{
				if (LabelGen.get(T.getMatch()) != LabelGen.end())
					LabelGen.deallocate();
				if (T.getMatch()->getKind() == Token::TK_Loop
					&& T.getPrevNode()->getKind() != Token::TK_Branch
					&& !(T.getPrevNode()->getKind() == Token::TK_BasicBlock
						&& isa<ReturnInst>(T.getPrevNode()->getBB()->getTerminator())))
				{
					stream << "break;" << NewLine;
				}
				if (T.getMatch()->getKind() & (Token::TK_If|Token::TK_IfNot))
				{
					const Token* Prev = T.getMatch();
					if (Prev->getMatch() != &T)
						Prev = Prev->getMatch();
					if (!omitBraces(*Prev, PA, registerize))
						stream << '}' << NewLine;
				}
				else
					stream << '}' << NewLine;
				blockDepth--;
				break;
			}
			case Token::TK_Prologue:
			{
				const BasicBlock* To = T.getBB()->getTerminator()->getSuccessor(T.getId());
				compilePHIOfBlockFromOtherBlock(To, T.getBB());
				break;
			}
			case Token::TK_Switch:
			{
				const SwitchInst* si = cast<SwitchInst>(T.getBB()->getTerminator());
				stream << "switch(";
				compileOperandForIntegerPredicate(si->getCondition(), CmpInst::ICMP_EQ, CheerpWriter::LOWEST);
				stream << "){" << NewLine;
				blockDepth++;
				break;
			}
			case Token::TK_Case:
			{
				const SwitchInst* si = cast<SwitchInst>(T.getBB()->getTerminator());
				int id = T.getId();
				if (id == 0)
					stream << "default";
				else
				{
					// The value to match for case `i` has index `2*i`
					auto cv = cast<ConstantInt>(si->getOperand(2*id));
					stream << "case ";
					compileOperandForIntegerPredicate(cv, CmpInst::ICMP_EQ, CheerpWriter::LOWEST);
				}
				stream << ':' << NewLine;
				break;
			}
			case Token::TK_Try:
			{
				stream << "try{" << NewLine;
				blockDepth++;
				break;
			}
			case Token::TK_Catch:
			{
				stream << "}catch(" << namegen.getBuiltinName(NameGenerator::EXCEPTION) << "){" << NewLine;
				assert(T.getBB()->isLandingPad());
				if(!T.getBB()->getLandingPadInst()->use_empty())
					stream << namegen.getName(T.getBB()->getLandingPadInst(), 0) << '=' << namegen.getBuiltinName(NameGenerator::EXCEPTION) << ";" << NewLine;
				break;
			}
			case Token::TK_BrIf:
			case Token::TK_BrIfNot:
			{
				report_fatal_error("Unexpected BR_IF token found");
			}
			case Token::TK_Invalid:
			{
				report_fatal_error("Invalid token found");
			}
		}
	}
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
	stream << "function " << getName(&F, 0) << '(';
	const Function::const_arg_iterator A=F.arg_begin();
	const Function::const_arg_iterator AE=F.arg_end();
	for(Function::const_arg_iterator curArg=A;curArg!=AE;++curArg)
	{
		if(curArg!=A)
			stream << ',';
		if(curArg->getType()->isPointerTy() && PA.getPointerKindForArgument(&*curArg) == SPLIT_REGULAR && !PA.getConstantOffsetForPointer(&*curArg))
			stream << getName(&*curArg, 0) << ',' << getName(&*curArg, 1);
		else
			stream << getName(&*curArg, 0);
	}
	stream << "){" << NewLine;
	if (measureTimeToMain && (&F == (globalDeps.getEntryPoint())))
	{
		stream << "__cheerp_main_time=__cheerp_now();" << NewLine;
	}
	if (asmjs)
	{
		compileParamTypeAnnotationsAsmJS(&F);
	}
	else
	{
		for(Function::const_arg_iterator curArg=A;curArg!=AE;++curArg)
		{
			if(curArg->use_empty())
				continue;
			if(!curArg->getType()->isPointerTy())
				continue;
			POINTER_KIND callKind = PA.getPointerKindForArgument(&*curArg);
			POINTER_KIND argKind = PA.getPointerKindAssert(&*curArg);
			if(callKind == argKind)
				continue;
			if(callKind != SPLIT_REGULAR)
				continue;
			if(argKind == REGULAR && PA.getConstantOffsetForPointer(&*curArg))
				continue;
			stream << getName(&*curArg, 0);
			stream << "=";
			if(argKind == REGULAR)
				stream << "{d:" << getName(&*curArg, 0) << ",o:" << getName(&*curArg, 1) << "}";
			else if(argKind == COMPLETE_OBJECT)
				stream << getName(&*curArg, 0) << "[" << getName(&*curArg, 1) << "]";
			else
			{
				assert(false);
			}
			stream << ";" << NewLine;
		}
	}
	lastDepth0Block = nullptr;
	if(F.size()==1)
	{
		compileMethodLocals(F);
		lastDepth0Block = &*F.begin();
		compileBB(*F.begin());
	}
	else
	{
		{
			compileMethodLocals(F);

			DominatorTree &DT = FAM.getResult<DominatorTreeAnalysis>(const_cast<Function&>(F));
			LoopInfo &LI = FAM.getResult<LoopAnalysis>(const_cast<Function&>(F));
			CFGStackifier::Mode Mode = asmjs ? CFGStackifier::AsmJS : CFGStackifier::GenericJS;
			CFGStackifier CN(F, LI, DT, registerize, PA, Mode);
			compileTokens(CN.Tokens);
		}
	}
	assert(blockDepth == 0);
	if (asmjs && (!lastDepth0Block || !isa<ReturnInst>(lastDepth0Block->getTerminator())))
	{
		if(!F.getReturnType()->isVoidTy())
		{
			// asm.js needs a final return statement for not-void functions
			stream << "return";
			Registerize::REGISTER_KIND kind = registerize.getRegKindFromType(F.getReturnType(), true);
			switch(kind)
			{
				case Registerize::INTEGER:
					stream << " 0";
					break;
				case Registerize::INTEGER64:
					if (!UseBigInts)
						report_fatal_error("unsupported INTEGER64 register");
					stream << " 0n";
					break;
				case Registerize::FLOAT:
					stream << " " << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << "(0.)";
					break;
				case Registerize::DOUBLE:
					stream << " 0.";
					break;
				case Registerize::OBJECT:
					llvm::errs() << "OBJECT register kind should not appear in asm.js functions\n";
					llvm::report_fatal_error("please report a bug");
					break;
				case Registerize::VECTOR:
					llvm::report_fatal_error("VECTOR register kind should not appear outside of WASM");
					break;
			}
			stream << ';' << NewLine;
		}
	}
	stream << '}' << NewLine;
	currentFun = NULL;
	typeIdMap.clear();
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
			llvm_unreachable("Should be addressed by one of the ifs");
	}
	llvm_unreachable("Should be addressed in the for cycle");
}

void CheerpWriter::compileGlobal(const GlobalVariable& G)
{
	assert(G.hasName());
	if(TypeSupport::isClientGlobal(&G) && !G.hasInitializer())
	{
		// Extern globals in the client namespace are only placeholders for JS globals
		return;
	}
	stream  << "var " << getName(&G, 0);

	if(G.hasInitializer())
	{
		stream << '=';
		const Constant* C = G.getInitializer();
		POINTER_KIND k = PA.getPointerKindAssert(&G);

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
			stream << "var " << getName(&G, 1);
			stream << "=0";
		}
		else
		{
			if(C->getType()->isPointerTy())
			{
				POINTER_KIND storedKind = PA.getPointerKindForStoredType(C->getType());
				if(storedKind == REGULAR && PA.getConstantOffsetForPointer(&G))
					compilePointerBaseTyped(C, G.getValueType());
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
		if(StructType* st=dyn_cast<StructType>(G.getValueType()))
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
#ifndef NDEBUG
			llvm::errs() << "Expected initializer for ";
			otherGV->dump();
			llvm::errs() << "\n";
#endif
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
#ifndef NDEBUG
		G.dump();
#endif
		return;
	}
	if (symbolicGlobalsAsmJS)
	{
		stream << "var " << getName(&G, 0) << '=';
		uint32_t globalAddr = linearHelper.getGlobalVariableAddress(&G);
		stream << globalAddr << ';' << NewLine;
	}
}

void CheerpWriter::compileGlobalsInitAsmJS()
{
	markHeapNameAsUsed(HEAP8);
	if (asmJSMem)
	{
		ostream_proxy os(*asmJSMem, nullptr, false);

		const LinearMemoryHelper::GlobalDataChunk &chunk = linearHelper.getGlobalDataChunk(0);
		for (uint32_t i = 0; i < chunk.view.size(); i++)
			os << (char)chunk.view[i];
	}
	else
	{
		for (uint32_t i = 0; i < linearHelper.getAmountChunks(); i++)
		{
			const LinearMemoryHelper::GlobalDataChunk &chunk = linearHelper.getGlobalDataChunk(i);
			assert(isHeapNameUsed(HEAP8));
			stream << getHeapName(HEAP8) << ".set([";
			bool first = true;
			for (uint32_t j = 0; j < chunk.view.size(); j++)
			{
				if (!first)
					stream << ",";
				stream << (int)chunk.view[j];
				first = false;
			}
			stream << "]," << chunk.address << ");" << NewLine;
		}
	}
}

void CheerpWriter::compileParamTypeAnnotationsAsmJS(const Function* F)
{
	const Function::const_arg_iterator A=F->arg_begin();
	const Function::const_arg_iterator AE=F->arg_end();
	for(Function::const_arg_iterator curArg=A;curArg!=AE;++curArg)
	{
		stream << getName(&*curArg, 0) << '=';
		Registerize::REGISTER_KIND kind = registerize.getRegKindFromType(curArg->getType(), true);
		switch(kind)
		{
			case Registerize::INTEGER:
				stream << getName(&*curArg, 0) << "|0";
				break;
			case Registerize::INTEGER64:
				if (LinearOutput==AsmJs)
					report_fatal_error("unsupported INTEGER64 register");
				stream << getName(&*curArg, 0);
				break;
			case Registerize::FLOAT:
				stream << namegen.getBuiltinName(NameGenerator::Builtin::FROUND) << '(';
				stream << getName(&*curArg, 0) << ')';
				break;
			case Registerize::DOUBLE:
				stream << '+' << getName(&*curArg, 0);
				break;
			case Registerize::OBJECT:
				stream << getName(&*curArg, 0);
				break;
			case Registerize::VECTOR:
				llvm::report_fatal_error("VECTOR register kind should not appear outside of WASM");
				break;
		}
		stream << ';' << NewLine;
	}
}

void CheerpWriter::compileNullPtrs()
{
	stream << "var oSlot=0;var nullArray=[null];var nullObj={d:nullArray,o:0};" << NewLine;
}

void CheerpWriter::compileCreateClosure()
{
	stream << "function " << namegen.getBuiltinName(NameGenerator::Builtin::CREATE_CLOSURE) << "(func, obj){return function(){var a=Array.prototype.slice.call(arguments);a.unshift(obj);return func.apply(null,a);};}" << NewLine;
	stream << "function " << namegen.getBuiltinName(NameGenerator::Builtin::CREATE_CLOSURE_SPLIT) << "(func, obj, objo){return function(){var a=Array.prototype.slice.call(arguments);a.unshift(obj,objo);return func.apply(null,a);};}" << NewLine;
}

void CheerpWriter::compileHandleVAArg()
{
	stream << "function " << namegen.getBuiltinName(NameGenerator::Builtin::HANDLE_VAARG) << "(ptr){var ret=ptr.d[ptr.o];ptr.o++;return ret;}" << NewLine;
}

void CheerpWriter::compileCheerpException()
{
	stream << "function CheerpException(m){" << NewLine;
	stream << "var instance=new Error('Uncaught C++ exception: '+m);" << NewLine;
	stream << "instance.name='CheerpException';" << NewLine;
	stream << "Object.setPrototypeOf(instance,Object.getPrototypeOf(this));" << NewLine;
	stream << "if(Error.captureStackTrace){" << NewLine;
	stream << "Error.captureStackTrace(instance, CheerpException);" << NewLine;
	stream << "}" << NewLine;
	stream << "return instance;" << NewLine;
	stream << "}" << NewLine;
	stream << "CheerpException.prototype=Object.create(Error.prototype);"<<NewLine;
}

void CheerpWriter::compileBuiltins(bool asmjs)
{
	StringRef math = asmjs?"stdlib.Math.":"Math.";
	if(useMathImul || asmjs)
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::IMUL) << '=' << math << "imul;" << NewLine;
	if(useMathFround)
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

void CheerpWriter::compileCheckDefined(const Value* p, bool needsOffset)
{
	// When compiling a SPIT_REGULAR, if there is an offset, we only check that.
	// If the offset exists the base is guaranteed to exists in the type.
	stream<<"checkDefined(";
	compileGEP(cast<User>(p),COMPLETE_OBJECT, HIGHEST);
	if(needsOffset)
		stream << "o";
	stream<<")";
}

void CheerpWriter::compileCheckBoundsAsmJSHelper()
{
	stream << "function checkBoundsAsmJS(align,size,addr){if((addr&align) || addr>=size || addr<0) throw new Error('OutOfBoundsAsmJS: '+addr);return addr;}" << NewLine;
}

void CheerpWriter::compileCheckBoundsAsmJS(int alignMask)
{
	// NOTE: the caller must add the address argument and the closing ')'
	stream<<"checkBoundsAsmJS(";
	stream<<alignMask<<pointerCoercionSuffix()<<","<<heapSize*1024*1024<<pointerCoercionSuffix()<<",";
}

void CheerpWriter::compileFunctionTablesAsmJS()
{
	for (const auto& table : linearHelper.getFunctionTables())
	{
		stream << "var " << table.second.name << "=[";
		bool first = true;
		uint32_t num = 0;
		for (const auto F : table.second.functions)
		{
			if (!first)
				stream << ',';
			first = false;
			stream << getName(F, 0);
			num++;
		}
		uint32_t mask = linearHelper.getFunctionAddressMask(table.second.functions[0]->getFunctionType());
		for (; num <= mask; num++)
		{
			stream << ',' << getName(table.second.functions[0], 0);
		}
		stream << "];" << NewLine;
	}
}

void CheerpWriter::compileAssignHeaps(bool wasm)
{
	const std::string shortestName = namegen.getShortestLocalName();
	int last = wasm ? LAST_WASM : LAST_ASMJS;

	if (isHeapNameUsed(HEAP64) && !UseBigInts)
	{
		llvm_unreachable("Unexpected BigInt");
	}

	stream << "function " << namegen.getBuiltinName(NameGenerator::Builtin::ASSIGN_HEAPS) << "(" << shortestName << "){" << NewLine;
	for (int i = HEAP8; i<=last; i++)
	{
		if (!isHeapNameUsed(i))
			continue;
		stream << getHeapName(i) << "=new " << typedArrayNames[i] << "(" << shortestName << ");" << NewLine;
	}
	stream << "}" << NewLine;
}

void CheerpWriter::compileGrowMem()
{
	stream << "function " << namegen.getBuiltinName(NameGenerator::Builtin::GROW_MEM) << "(pages){" << NewLine;
	stream << "try{" << NewLine;
	stream << "var ret=";
	if (!LowerAtomics)
	{
		StringRef threadObject = namegen.getBuiltinName(NameGenerator::Builtin::THREADINGOBJECT);
		stream << threadObject << ".";
	}
	stream << namegen.getBuiltinName(NameGenerator::MEMORY) << ".grow(pages);" << NewLine;
	stream << "__heap=";
	if (!LowerAtomics)
	{
		StringRef threadObject = namegen.getBuiltinName(NameGenerator::Builtin::THREADINGOBJECT);
		stream << threadObject << ".";
	}
	stream << namegen.getBuiltinName(NameGenerator::MEMORY) << ".buffer;" << NewLine;
	stream << namegen.getBuiltinName(NameGenerator::Builtin::ASSIGN_HEAPS) << "(__heap);" << NewLine;
	stream << "return ret;" << NewLine;
	stream << "}catch(e){" << NewLine;
	stream << "return -1;" << NewLine;
	stream << '}' << NewLine;
	stream << "}" << NewLine;
}

void CheerpWriter::compileAtomicFunctions()
{
	auto funcName = namegen.getBuiltinName(NameGenerator::Builtin::ATOMICLOAD);
	stream << "function " << funcName << "(bitwidth, addr){" << NewLine;
	stream << "if(bitwidth==8)" << NewLine;
	stream << "return Atomics.load(" << getHeapName(HEAP8) << ", addr);" << NewLine;
	stream << "else if(bitwidth==16)" << NewLine;
	stream << "return Atomics.load(" << getHeapName(HEAP16) << ", addr);" << NewLine;
	stream << "else if(bitwidth==32)" << NewLine;
	stream << "return Atomics.load(" << getHeapName(HEAP32) << ", addr);" << NewLine;
	if (UseBigInts && LinearOutput!=AsmJs)
	{
		stream << "else if(bitwidth==64)" << NewLine;
		stream << "return Atomics.load(" << getHeapName(HEAP64) << ", addr);" << NewLine;
	}
	stream << "else " << NewLine;
	stream << "throw new Error('Wrong bitwidth');" << NewLine;
	stream << "}" << NewLine;
	std::vector<std::string> opNames={"store","add","sub","and","or","xor","exchange"};
	for (uint32_t i = 0; i < opNames.size(); i++)
	{
		auto b = static_cast<NameGenerator::Builtin>(i + NameGenerator::Builtin::ATOMICSTORE);
		auto opName = opNames[i];
		funcName = namegen.getBuiltinName(b);
		stream << "function " << funcName << "(bitwidth, addr, val){" << NewLine;
		stream << "if(bitwidth==8)" << NewLine;
		stream << "return Atomics." << opName << "(" << getHeapName(HEAP8) << ", addr, val);" << NewLine;
		stream << "else if(bitwidth==16)" << NewLine;
		stream << "return Atomics." << opName << "(" << getHeapName(HEAP16) << ", addr, val);" << NewLine;
		stream << "else if(bitwidth==32)" << NewLine;
		stream << "return Atomics." << opName << "(" << getHeapName(HEAP32) << ", addr, val);" << NewLine;
		if (UseBigInts && LinearOutput!=AsmJs)
		{
			stream << "else if(bitwidth==64)" << NewLine;
			stream << "return Atomics." << opName << "(" << getHeapName(HEAP64) << ", addr, val);" << NewLine;
		}
		stream << "else " << NewLine;
		stream << "throw new Error('Wrong bitwidth');" << NewLine;
		stream << "}" << NewLine;
	}
	funcName = namegen.getBuiltinName(NameGenerator::Builtin::ATOMICCMPXCHG);
	stream << "function " << funcName << "(bitwidth, addr, expected, replacement){" << NewLine;
	stream << "if(bitwidth==8)" << NewLine;
	stream << "return Atomics.compareExchange(" << getHeapName(HEAP8) << ", addr, expected, replacement);" << NewLine;
	stream << "else if(bitwidth==16)" << NewLine;
	stream << "return Atomics.compareExchange(" << getHeapName(HEAP16) << ", addr, expected, replacement);" << NewLine;
	stream << "else if(bitwidth==32)" << NewLine;
	stream << "return Atomics.compareExchange(" << getHeapName(HEAP32) << ", addr, expected, replacement);" << NewLine;
	if (UseBigInts && LinearOutput!=AsmJs)
	{
		stream << "else if(bitwidth==64)" << NewLine;
		stream << "return Atomics.compareExchange(" << getHeapName(HEAP64) << ", addr, expected, replacement);" << NewLine;
	}
	stream << "else " << NewLine;
	stream << "throw new Error('Wrong bitwidth');" << NewLine;
	stream << "}" << NewLine;
}

void CheerpWriter::compileMathDeclAsmJS()
{
	stream << "var Infinity=stdlib.Infinity;" << NewLine;
	stream << "var NaN=stdlib.NaN;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ABS_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::ABS) << "=stdlib.Math.abs;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ACOS_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::ACOS) << "=stdlib.Math.acos;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ASIN_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::ASIN) << "=stdlib.Math.asin;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ATAN_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::ATAN) << "=stdlib.Math.atan;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ATAN2_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::ATAN2) << "=stdlib.Math.atan2;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::CEIL_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::CEIL) << "=stdlib.Math.ceil;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::COS_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::COS) << "=stdlib.Math.cos;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::EXP_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::EXP) << "=stdlib.Math.exp;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::FLOOR_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::FLOOR) << "=stdlib.Math.floor;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::LOG_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::LOG) << "=stdlib.Math.log;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::POW_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::POW) << "=stdlib.Math.pow;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::SIN_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::SIN) << "=stdlib.Math.sin;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::SQRT_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::SQRT) << "=stdlib.Math.sqrt;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::TAN_F))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::TAN) << "=stdlib.Math.tan;" << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::CLZ))
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::CLZ32) << "=stdlib.Math.clz32;" << NewLine;
}

void CheerpWriter::compileFetchBuffer()
{
	if (makeModule == MODULE_TYPE::ES6)
		compileES6FetchBuffer();
	else
		compileRegularFetchBuffer();
}

//ES6 modules syntax implies either the presence of fetch or the import() syntax
//Note that IE is NOT supported by ES6 modules
void CheerpWriter::compileES6FetchBuffer()
{
	assert(makeModule == MODULE_TYPE::ES6);

	stream << "function " << namegen.getBuiltinName(NameGenerator::FETCHBUFFER) <<"(p){" << NewLine;
	stream << "return (typeof self==='object')?" << NewLine;
	stream << "fetch(p).then(r=>r.arrayBuffer()):" << NewLine;
	stream << "new Promise((y,n)=>{" << NewLine;
	stream << "import('fs').then(r=>r.readFile(p,(e,d)=>{" << NewLine;
	stream << "if(e)n(e);" << NewLine;
	stream << "else y(d);" << NewLine;
	stream << "}));" << NewLine;
	stream << "});" << NewLine;
	stream << "}" << NewLine;
}

//read is a fall back for IE / legacy browsers, require is available in node.js, and fetch is supported by modern browsers
void CheerpWriter::compileRegularFetchBuffer()
{
	assert(makeModule != MODULE_TYPE::ES6);

	stream << "function " << namegen.getBuiltinName(NameGenerator::FETCHBUFFER) <<"(p){" << NewLine;
	stream << "var b=null;" << NewLine;
	stream << "if(typeof self==='object')b=fetch(p).then(r=>r.arrayBuffer());" << NewLine;
	stream << "else if(typeof require==='function'){" << NewLine;
	stream << "p=require('path').join(__dirname, p);" << NewLine;
	stream << "b=new Promise((y,n)=>{" << NewLine;
	stream << "require('fs').readFile(p,(e,d)=>{" << NewLine;
	stream << "if(e)n(e);" << NewLine;
	stream << "else y(d);" << NewLine;
	stream << "});" << NewLine;
	stream << "});" << NewLine;
	stream << "}else b=new Promise((y,n)=>{" << NewLine;
	stream << "y(read(p,'binary'));" << NewLine;
	stream << "});" << NewLine;
	stream << "return b;" << NewLine;
	stream << "}" << NewLine;
}

void CheerpWriter::compileFetchBufferCall(const std::string& fileName, const std::string& argumentName)
{
	if (makeModule == MODULE_TYPE::ES6)
	{
		stream << "((" << argumentName << "&&" << argumentName << ".buffer)?" << NewLine;
		stream << "Promise.resolve(" << argumentName << ".buffer):" << NewLine;
	}
	stream << namegen.getBuiltinName(NameGenerator::FETCHBUFFER) << "(";
	if (makeModule == MODULE_TYPE::ES6)
	{
		stream << "(" << argumentName << "&&" << argumentName << ".absPath)";
		stream << "?";
		stream << argumentName << ".absPath" << ":";
		stream << "new URL(";
	}
	stream << "'" << fileName << "'";
	if (makeModule == MODULE_TYPE::ES6)
		stream << ", import.meta.url)";
	stream << ")";
  if (makeModule == MODULE_TYPE::ES6)
    stream << ")";
}

void CheerpWriter::compileSourceMapsBegin()
{
	sourceMapGenerator->beginFile();

	NamedMDNode *cu = module.getNamedMetadata("llvm.dbg.cu");
	if (!cu || cu->getNumOperands() == 0) {
		llvm::errs() << "warning: no debug symbols found but source map is requested\n";
	}

	DebugInfoFinder finder;
	finder.processModule(module);

	for (const DISubprogram *method : finder.subprograms()) {
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

		StringRef linkName = method->getLinkageName();
		if (linkName.empty())
			linkName = method->getName();
		auto it = functionToDebugInfoMap.find(linkName);
		if(it == functionToDebugInfoMap.end())
			functionToDebugInfoMap.insert(std::make_pair(linkName, method));
		else if(method->isDefinition() && !it->second->isDefinition())
			it->second = method;
	}
}
void CheerpWriter::compileSourceMapsEnd()
{
	sourceMapGenerator->endFile();
	stream << "//# sourceMappingURL=" << sourceMapGenerator->getSourceMapName();
}

void CheerpWriter::compileTimeToMainBegin()
{
	stream << "var __cheerp_now = typeof dateNow!==\"undefined\"?dateNow:(typeof performance!==\"undefined\"?performance.now:function(){return new Date().getTime()});" << NewLine;
	stream << "var __cheerp_main_time = -0;" << NewLine;
	stream << "var __cheerp_start_time = __cheerp_now();" << NewLine;
}
void CheerpWriter::compileTimeToMainEnd()
{
	stream << "console.log(\"main() called after\", __cheerp_main_time-__cheerp_start_time, \"ms\");" << NewLine;
}

void CheerpWriter::compileModuleClosureBegin()
{
	stream << "(function(){" << NewLine;

	isRootNeeded = true;
}

void CheerpWriter::compileRootIfNeeded()
{
	if (!isRootNeeded)
		return;

	if (hasJSExports()) {
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

}
void CheerpWriter::compileModuleClosureEnd()
{
	stream << "})();" << NewLine;
}

void CheerpWriter::compileHelpers()
{
	// Enable strict mode first
	stream << "\"use strict\";" << NewLine;

	if(addCredits)
		stream << "/*Compiled using Cheerp (R) by Leaning Technologies Ltd*/" << NewLine;

	//Compile the bound-checking function
	if ( checkBounds )
	{
		compileCheckBoundsHelper();
		compileCheckDefinedHelper();
	}

	compileBuiltins(false);

	compileNullPtrs();

	if (!LowerAtomics)
	{
		compileThreadingObject();
	}

	// Utility function for loading files
	if(!wasmFile.empty() || asmJSMem)
		compileFetchBuffer();

	if (isWasmTarget && checkBounds)
	{
		compileCheckBoundsAsmJSHelper();
	}

	if (isWasmTarget)
	{
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::STACKPTR) << '=' <<
			linearHelper.getStackStart() << "|0;" << NewLine;
	}

	if (makeModule == MODULE_TYPE::ES6) {
		stream << "var " <<  EnvironName << "=null,";
		stream << ArgvName << "=null;" << NewLine;
	}
}

void CheerpWriter::compileImports()
{
	for (const Function* imported: globalDeps.asmJSImports())
	{
		stream << getName(imported, 0) << ':';
		if (!imported->empty())
		{
			stream << getName(imported, 0);
		}
		else if (TypeSupport::isClientFunc(imported))
		{
			TypeSupport::ClientFunctionDemangled clientHelper(*imported);
			//Regular call
			if (imported->hasFnAttribute(Attribute::Static))
			{
				stream << clientHelper.namespacedName;	//namespacedName is either empty or ends with '.'
			}
			stream << clientHelper.funcName;
		}
		else
		{
			stream << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY);
		}
		stream << ',' << NewLine;
	}
}

void CheerpWriter::compileAsmJSClosure()
{
	//HEAP8 is used in various special situations (eg. initialization of globals) in AsmJs
	markHeapNameAsUsed(HEAP8);

	// compile boilerplate
	stream << "function asmJS(stdlib, ffi, __heap){" << NewLine;
	stream << "\"use asm\";" << NewLine;

	for (int i = HEAP8; i<=LAST_ASMJS; i++)
	{
		stream << "var "<<getHeapNameWithoutMarking(i)<<"=new stdlib."<<typedArrayNames[i]<<"(__heap);" << NewLine;
	}

	stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::STACKPTR) << "=ffi.stackStart|0;" << NewLine;

	compileMathDeclAsmJS();
	compileBuiltins(true);
	stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY) << "=ffi." << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY) << ";" << NewLine;
	if (checkBounds)
	{
		stream << "var checkBoundsAsmJS=ffi.checkBoundsAsmJS;" << NewLine;
	}
	for (const Function* imported: globalDeps.asmJSImports())
	{
		stream << "var " << getName(imported, 0) << "=ffi." << getName(imported, 0) << ';' << NewLine;
	}
	if (globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::GROW_MEM))
	{

		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::GROW_MEM);
		stream << "=ffi.";
		stream << namegen.getBuiltinName(NameGenerator::Builtin::GROW_MEM);
		stream << ';' << NewLine;
	}
	if (globalDeps.usesAtomics())
	{
		for (int i = NameGenerator::Builtin::ATOMICLOAD; i <= NameGenerator::Builtin::ATOMICCMPXCHG; i++)
		{
			auto b = static_cast<NameGenerator::Builtin>(i);
			stream << "var " << namegen.getBuiltinName(b) << "=ffi." << namegen.getBuiltinName(b) << ";" << NewLine;
		}
	}

	// Declare globals
	for ( const GlobalVariable* GV : linearHelper.globals() )
		compileGlobalAsmJS(*GV);

	for (const Function& F : module.functions())
	{
		if (!F.empty() && F.getSection() == "asmjs")
		{
			compileMethod(F);
		}
	}

	compileFunctionTablesAsmJS();

	stream << "return {" << NewLine;
	for (const Function* exported: globalDeps.asmJSExports())
	{
		StringRef name = getName(exported, 0);
		stream << name << ':' << name << ',' << NewLine;
	}
	stream << "};" << NewLine;
	stream << "};" << NewLine;
}

void CheerpWriter::compileAsmJSffiObject()
{
	stream << "{" << NewLine;
	stream << "heapSize:__heap.byteLength," << NewLine;
	stream << "stackStart:" << linearHelper.getStackStart() << ',' << NewLine;
	stream << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY) << ":" << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY) << "," << NewLine;
	if (checkBounds)
	{
		stream << "checkBoundsAsmJS:checkBoundsAsmJS," << NewLine;
	}
	compileImports();
	if (globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::GROW_MEM))
	{
		stream << namegen.getBuiltinName(NameGenerator::Builtin::GROW_MEM);
		stream << ':';
		stream << namegen.getBuiltinName(NameGenerator::Builtin::GROW_MEM);
		stream << ',' << NewLine;
	}
	if (globalDeps.usesAtomics())
	{
		for (int i = NameGenerator::Builtin::ATOMICLOAD; i <= NameGenerator::Builtin::ATOMICCMPXCHG; i++)
		{
			auto b = static_cast<NameGenerator::Builtin>(i);
			stream << namegen.getBuiltinName(b) << ":" << namegen.getBuiltinName(b) << "," << NewLine;
		}
	}
	stream << "}";
}

void CheerpWriter::compileAsmJSTopLevel()
{
	compileDummies();

	stream << "var __heap = new ";
	if (globalDeps.usesAtomics())
		stream << "Shared";
	stream << "ArrayBuffer(" << heapSize * 1024 * 1024 << ");" << NewLine;
	{
		//Declare used HEAPs variables to null, to be inizializated by a later call to ASSIGN_HEAPS
		bool isFirst = true;
		for (int i = HEAP8; i<=LAST_ASMJS; i++)
		{
			if (!isHeapNameUsed(i))
				continue;

			if (isFirst)
				stream << "var ";
			else
				stream << ",";
			isFirst = false;
			stream << getHeapName(i) << "=null";
		}
		if (!isFirst)
			stream << ";" << NewLine;
	}
	stream << namegen.getBuiltinName(NameGenerator::Builtin::ASSIGN_HEAPS) << "(__heap);" << NewLine;
	if (isWasmTarget)
	{
		stream << "var stdlib = {"<<NewLine;
		stream << "Math:Math,"<<NewLine;
		stream << "Infinity:Infinity,"<<NewLine;
		stream << "NaN:NaN,"<<NewLine;
		for (int i = HEAP8; i<=LAST_ASMJS; i++)
		{
			//Here we forward declare all types, since they may be used even without explicitly mentioning the relative HEAPs
			stream << typedArrayNames[i] << ':' << typedArrayNames[i] << ',' << NewLine;
		}
		stream << "};" << NewLine;
	}
	compileGlobalsInitAsmJS();
}

void CheerpWriter::compileGenericJS()
{
	jsExportedDecls = getJsExportModule(module);
	if (isRootNeeded)
		prependRootToNames(jsExportedDecls);

	jsExportedDecls.getExportsFilter<JsExportClass>([&](llvm::StringRef name, const JsExport& value)
	{
		jsExportedTypes.try_emplace(std::get<JsExportClass>(value).getType(), name);
	});

	for (const Function& F: module.functions())
	{
		if (F.getSection() == "asmjs")
			continue;
		if (!F.empty())
		{
#ifdef CHEERP_DEBUG_POINTERS
			dumpAllPointers(F, PA);
#endif //CHEERP_DEBUG_POINTERS
			compileMethod(F);
		}
	}
	for ( const GlobalVariable & GV : module.getGlobalList() )
	{
		// Skip global ctors array
		if (GV.getName() == "llvm.global_ctors")
			continue;
		if (!TypeSupport::isAsmJSPointer(GV.getType()))
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

	for ( Type * st : globalDeps.dynResizeArrays() )
		compileResizeArrayClassType(st);

	if ( globalDeps.needCreatePointerArray() )
		compileArrayPointerType();
	
	//Compile the closure creation helper
	if ( globalDeps.needCreateClosure() )
		compileCreateClosure();
	
	//Compile handleVAArg if needed
	if( globalDeps.needHandleVAArg() )
		compileHandleVAArg();

	//Compile CheerpException if needed
	if(globalDeps.needCheerpException())
		compileCheerpException();

	//Compile growLinearMemory if needed
	if (globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::GROW_MEM))
		compileGrowMem();

	if (globalDeps.usesAtomics() && wasmFile.empty())
		compileAtomicFunctions();
}

void CheerpWriter::compileDummies()
{
	stream << "function " << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY) << "(){throw new Error('this should be unreachable');};" << NewLine;

	areDummiesDeclared = true;
}

void CheerpWriter::compileWasmLoader()
{
	stream << "var ";
	for (int i = HEAP8; i<=LAST_WASM; i++)
	{
		if (!isHeapNameUsed(i))
			continue;
		stream << getHeapName(i) << "=null,";
	}
	stream << "__asm=null,";
	stream << "__heap=null;";
	compileDummies();

	if (LowerAtomics)
		compileWasmMemory();

	compileDeclareExports();

	StringRef dummyName = namegen.getBuiltinName(NameGenerator::Builtin::DUMMY);
	StringRef threadObject = namegen.getBuiltinName(NameGenerator::Builtin::THREADINGOBJECT);
	const std::string shortestName = namegen.getShortestLocalName();
	compileFetchBufferCall(wasmFile, shortestName);
	if (LowerAtomics)
		stream << ".then(" << shortestName << "=>" << NewLine;
	else
	{
		stream << ";" << NewLine;
		stream << "}else{" << NewLine;
		stream << dummyName << ".promise=Promise.resolve(";
		stream << threadObject << ".module);" << NewLine;
		stream << "}" << NewLine;

		stream << dummyName << ".promise=" << dummyName << ".promise.then(" << shortestName << "=>" << NewLine;
	}
	stream << "WebAssembly.instantiate(" << shortestName << "," << NewLine;
	stream << "{i:{" << NewLine;
	// Import the memory.
	StringRef memoryName = namegen.getBuiltinName(NameGenerator::Builtin::MEMORY);
	if (LowerAtomics)
		stream << memoryName << ':' << memoryName << ',' << NewLine;
	else
		stream << memoryName << ':' << threadObject << '.' << memoryName << ',' << NewLine;
	compileImports();
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ACOS_F))
		stream << namegen.getBuiltinName(NameGenerator::ACOS) << ":Math.acos," << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ASIN_F))
		stream << namegen.getBuiltinName(NameGenerator::ASIN) << ":Math.asin," << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ATAN_F))
		stream << namegen.getBuiltinName(NameGenerator::ATAN) << ":Math.atan," << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::ATAN2_F))
		stream << namegen.getBuiltinName(NameGenerator::ATAN2) << ":Math.atan2," << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::COS_F))
		stream << namegen.getBuiltinName(NameGenerator::COS) << ":Math.cos," << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::EXP_F))
		stream << namegen.getBuiltinName(NameGenerator::EXP) << ":Math.exp," << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::LOG_F))
		stream << namegen.getBuiltinName(NameGenerator::LOG) << ":Math.log," << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::POW_F))
		stream << namegen.getBuiltinName(NameGenerator::POW) << ":Math.pow," << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::SIN_F))
		stream << namegen.getBuiltinName(NameGenerator::SIN) << ":Math.sin," << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::TAN_F))
		stream << namegen.getBuiltinName(NameGenerator::TAN) << ":Math.tan," << NewLine;
	if(globalDeps.needsBuiltin(BuiltinInstr::BUILTIN::GROW_MEM))
	{
		stream << namegen.getBuiltinName(NameGenerator::Builtin::GROW_MEM);
		stream << ':';
		stream << namegen.getBuiltinName(NameGenerator::Builtin::GROW_MEM);
		stream << ',' << NewLine;
	}
	stream << "}})" << NewLine;
	if (LowerAtomics)
	{
		stream << ").then(" << shortestName << "=>{" << NewLine;
		stream << "__asm=" << shortestName << ".instance.exports;" << NewLine;
		stream << "__heap=" << memoryName << ".buffer;" << NewLine;
	}
	else
	{
		stream << ");"<< NewLine;
		stream << "__heap=" << threadObject << "." << memoryName << ".buffer;" << NewLine;
	}
	stream << namegen.getBuiltinName(NameGenerator::Builtin::ASSIGN_HEAPS) << "(__heap);" << NewLine;
	if (!LowerAtomics)
	{
		stream << "if(!" << threadObject << ".inWorker)" << NewLine;
		stream << "{" << NewLine;
		if (makeModule == MODULE_TYPE::COMMONJS)
			stream << "module.exports=" << NewLine;
		else if (makeModule == MODULE_TYPE::ES6)
			stream << "return ";
		stream << dummyName << ".promise.then(" << shortestName << "=>{" << NewLine;
		stream << threadObject << ".module=" << shortestName << ".module;" << NewLine;
		stream << "__asm=" << shortestName << ".instance.exports;" << NewLine;
	}
}

void CheerpWriter::compileDeclareExports()
{
	for (auto i: globalDeps.asmJSExports())
	{
		if(i->empty()) continue;

		stream << "var " << getName(i, 0) << "=null;" << NewLine;
	}
	areAsmJSExportsDeclared = true;

	{
		//Set all jsExportedDecls equal to DUMMY / {}
		jsExportedDecls.getExportsFilter<JsExportClass, JsExportFunction>([&](llvm::StringRef name, const JsExport& value)
		{
			if (!isRootNeeded && !isNamespaced(name))
			{
				areJsExportedExportsDeclared = true;
				stream << "var ";
			}

			stream << name << "=";
			if (areDummiesDeclared)
				stream << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY) <<";" << NewLine;
			else
				stream << "{};" << NewLine;
		});
	}

	if (!LowerAtomics && makeModule != MODULE_TYPE::ES6)
	{
		stream << "if(!" << namegen.getBuiltinName(NameGenerator::Builtin::THREADINGOBJECT) << ".inWorker){" << NewLine;
		compileWasmMemory();
	}

	if (LowerAtomics && makeModule == MODULE_TYPE::COMMONJS)
	{
		stream << "module.exports=" << NewLine;
	}
	else if (makeModule == MODULE_TYPE::ES6)
	{
		const std::string shortestName = namegen.getShortestLocalName();
		stream << "export default function(" << shortestName << "){" << NewLine;
		if (!LowerAtomics)
		{
			stream << "if(!" << namegen.getBuiltinName(NameGenerator::Builtin::THREADINGOBJECT) << ".inWorker){" << NewLine;
			compileWasmMemory();
		}
		stream << EnvironName << "=(typeof " << shortestName << " == 'undefined' ? null : " << shortestName << ".env) || null;" << NewLine;
		stream << ArgvName << "=(typeof " << shortestName << " == 'undefined' ? null : " << shortestName << ".argv) || null;" << NewLine;
		if (!LowerAtomics)
			stream << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY) << ".promise=";
		else
			stream << "return ";
	}
	else
	{
		assert(areDummiesDeclared);
		//Set the promise of DUMMY_WITH_PROMISE
		stream << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY) << ".promise=" << NewLine;
	}
}

void CheerpWriter::compileWasmMemory()
{
	uint32_t maxMemory = heapSize << 4;
	uint32_t minMemory = (linearHelper.getHeapStart() + 65535) >> 16;
	if (noGrowMemory)
		minMemory = maxMemory;
	if (LowerAtomics)
		stream << "var " << namegen.getBuiltinName(NameGenerator::Builtin::MEMORY);
	else
		stream << namegen.getBuiltinName(NameGenerator::Builtin::THREADINGOBJECT) << "." << namegen.getBuiltinName(NameGenerator::Builtin::MEMORY);
	stream << "=new WebAssembly.Memory({initial:" << minMemory << ",maximum:" << maxMemory;
	if (WasmSharedMemory)
		stream << ",shared:true";
	stream << "});" << NewLine;
}

void CheerpWriter::compileNamespaces()
{
	class NestedNamespaceDeclarator
	{
	public:
		NestedNamespaceDeclarator(CheerpWriter& writer, const bool isRootDeclared)
			: writer(writer), stream(writer.stream), isRootDeclared(isRootDeclared), lowestIndex(isRootDeclared?1:0), index(lowestIndex)
		{
		}
		void addName(StringRef name)
		{
			std::vector<std::string> curr = decompose(name);

			//Skip over top level declarations
			if (curr.size() == 1+lowestIndex)
				return;

			while (index >= curr.size())
				doDecrease();

			while (index>lowestIndex && curr[index-1] != last[index-1])
				doDecrease();

			last = curr;

			while (index < curr.size())
				doIncrease();
		}
		~NestedNamespaceDeclarator()
		{
			while (index > lowestIndex)
				doDecrease();
		}
	private:
		std::vector<std::string> decompose(StringRef name)
		{
			std::vector<std::string> layered;
			unsigned int last = 0;
			for (unsigned int i = 0;; i++)
			{
				if (i==name.size() || name[i] == '.')
				{
					assert(i > last);
					layered.push_back(std::string(name.begin()+last, i-last));
					last = i+1;
				}
				if (i == name.size())
					break;
			}
			return layered;
		}
		void doDecrease()
		{
			if (index != last.size())
				stream << "}";
			--index;
			if (index == lowestIndex)
				stream <<";";
			else
				stream <<",";
			stream << writer.NewLine;
		}
		void doIncrease()
		{
			if (index == lowestIndex)
			{
				if (isRootDeclared)
					stream << last[0] << ".";
				else
					stream << "var ";
				stream << last[index] << "=";
			}
			else
				stream << last[index] << ":";
			++index;
			if (index == last.size())
				stream << "null";
			else
				stream << "{" << writer.NewLine;
		}
		CheerpWriter& writer;
		ostream_proxy& stream;
		std::vector<std::string> last;
		StringRef lastWritten;
		const bool isRootDeclared;
		const unsigned int lowestIndex;
		unsigned int index;
	};

	compileRootIfNeeded();

	NestedNamespaceDeclarator NND(*this, isRootNeeded);

	jsExportedDecls.getExportsFilter<JsExportClass, JsExportFunction, JsExportProperty>([&](llvm::StringRef name, const JsExport& value)
	{
		NND.addName(name);
	});
}

void CheerpWriter::compileDefineExports()
{
	const bool alsoDeclareAsmJS = !areAsmJSExportsDeclared;

	for (auto i: globalDeps.asmJSExports())
	{
		if(i->empty()) continue;

		if (alsoDeclareAsmJS)
			stream << "var ";
		stream << getName(i, 0) << "=__asm." << getName(i, 0) <<";" << NewLine;
	}
	areAsmJSExportsDeclared = true;
	//We just did

	compileDeclExportedToJs(/*alsoDeclare*/ !areJsExportedExportsDeclared);
	areJsExportedExportsDeclared = true;

	if (makeModule != MODULE_TYPE::COMMONJS && makeModule != MODULE_TYPE::ES6)
	{
		//CommonJS modules have already the promise on module.exports
		bool anyJSEx = false;
		jsExportedDecls.getExportsFilter<JsExportClass, JsExportFunction>([&](llvm::StringRef name, const JsExport& value)
		{
			anyJSEx = true;
			stream << name << ".promise=" << NewLine;
		});
		if (anyJSEx)
			stream << "Promise.resolve();" << NewLine;
	}
}

void CheerpWriter::compileAsmJSLoader()
{
	compileDeclareExports();

	const std::string shortestName = namegen.getShortestLocalName();
	compileFetchBufferCall(asmJSMemFile, shortestName);
	stream << ".then(" << shortestName << "=>{" << NewLine;
	stream << getHeapName(HEAP8) << ".set(new Uint8Array(" << shortestName << "),";
	stream << linearHelper.getStackStart() << ");" << NewLine;
}

void CheerpWriter::compileLoaderOrModuleEnd()
{
	stream << "});" << NewLine;
}

void CheerpWriter::compileCommonJSModule()
{
	compileDeclareExports();
	stream << "Promise.resolve().then(_=>{" << NewLine;
}

void CheerpWriter::compileCommonJSExports()
{
	assert(!isRootNeeded);

	llvm::StringRef exportName = namegen.getBuiltinName(NameGenerator::Builtin::EXPORT);

	stream << "var " << exportName << "={" << NewLine;

	for (const auto& [name, value] : jsExportedDecls.getExports())
	{
		if (std::holds_alternative<JsExportProperty>(value))
			continue;

		stream << name << ':' << name << ',' << NewLine;
	}

	stream << "};" << NewLine;

	for (const auto& [name, value] : jsExportedDecls.getExports())
	{
		if (auto* prop = std::get_if<JsExportProperty>(&value))
		{
			stream << "Object.defineProperty(" << exportName << ",'" << name << "',";
			compileJsExportProperty(*prop, true, nullptr);
			stream << ");" << NewLine;
		}
	}

	stream << "return " << exportName << ";" << NewLine;
}

void CheerpWriter::compileEntryPoint()
{
	const Function * entryPoint = module.getFunction("_start");
	if (entryPoint)
	{
		if (entryPoint->getSection() == "asmjs")
			stream << "__asm.";
		stream << getName(entryPoint, 0) << "();" << NewLine;
	}
}

void CheerpWriter::compileThreadingObject()
{
		StringRef threadObject = namegen.getBuiltinName(NameGenerator::Builtin::THREADINGOBJECT);
		stream << "if(typeof ";
		if (makeModule == MODULE_TYPE::ES6 || makeModule == MODULE_TYPE::COMMONJS)
			stream << "globalThis.";
		stream << threadObject << "==='undefined'){" << NewLine;
		stream << "var script=";
		if (makeModule == MODULE_TYPE::ES6)
			stream << "import.meta.url;";
		else
		{
			stream << "null;";
			stream << "if(typeof self==='object')";
			stream << "try {throw new Error();}catch(e){";
			stream << "const re = /((?:blob:)?(?:http|https|chrome-extension|file):\\/\\/.*):\\d+:\\d+$/;";
			stream << "script=re.exec(e.stack.trim())[1]";
			stream << "}";
			stream << "else script = __filename;";
		}
		stream << NewLine;
		stream << "var " << threadObject << "={inWorker:false,module:null,script:script,memory:null,func:null,args:null,tls:null,tid:null,stack:null};" << NewLine;
		if (makeModule == MODULE_TYPE::ES6 || makeModule == MODULE_TYPE::COMMONJS)
		{
			stream << "}else{" << NewLine;
			stream << "var " << threadObject << "=globalThis." << threadObject << ";" << NewLine;
		}
		stream << "}" << NewLine;
}

void CheerpWriter::compileWorkerMainScript()
{
	std::string shortestName = namegen.getShortestLocalName();
	StringRef threadObject = namegen.getBuiltinName(NameGenerator::Builtin::THREADINGOBJECT);
	stream << "}else{" << NewLine;
	stream << namegen.getBuiltinName(NameGenerator::Builtin::DUMMY) << ".promise.then(" << shortestName << "=>{" << NewLine;
	stream << "__asm=" << shortestName << ".exports;" << NewLine;
	compileDefineExports();
	stream << "__asm._workerEntry(" << threadObject << ".tls, " << threadObject << ".func, ";
	stream << threadObject << ".args, " << threadObject << ".tid, " << threadObject << ".stack, ";
	stream << threadObject << ".ctid);" << NewLine;
	stream << "});" << NewLine;
}

void CheerpWriter::compileFileBegin(const OptionsSet& options)
{
	if (options[Options::NEED_SOURCE_MAPS])
		compileSourceMapsBegin();
	if (options[Options::MEASURE_TIME_TO_MAIN])
		compileTimeToMainBegin();
	if (options[Options::NEED_MODULE_CLOSURE])
		compileModuleClosureBegin();
}

void CheerpWriter::compileFileEnd(const OptionsSet& options)
{
	if (options[Options::NEED_SOURCE_MAPS])
		compileSourceMapsEnd();
	if (options[Options::MEASURE_TIME_TO_MAIN])
		compileTimeToMainEnd();
	if (options[Options::NEED_MODULE_CLOSURE])
		compileModuleClosureEnd();
}

void CheerpWriter::makeJS()
{
	const bool needWasmLoader = !wasmFile.empty();

	auto initializeOptions = [&]() -> OptionsSet
	{
		OptionsSet options;

		options[Options::NEED_SOURCE_MAPS] = (sourceMapGenerator != nullptr);
		options[Options::MEASURE_TIME_TO_MAIN] = measureTimeToMain;
		options[Options::NEED_MODULE_CLOSURE] = (makeModule == MODULE_TYPE::CLOSURE);

		return options;
	};
	const OptionsSet options = initializeOptions();

	compileFileBegin(options);

	compileHelpers();
	compileGenericJS();

	compileNamespaces();

	bool areExtraParenthesisOpen = true;

	if (needWasmLoader)
		compileWasmLoader();
	else
	{
		if (isWasmTarget)
		{
			compileAsmJSClosure();
			compileAsmJSTopLevel();
		}

		if (isWasmTarget && asmJSMem)
			compileAsmJSLoader();
		else if (makeModule == MODULE_TYPE::COMMONJS || makeModule == MODULE_TYPE::ES6)
			compileCommonJSModule();
		else
			areExtraParenthesisOpen = false;

		if (isWasmTarget)
		{
			stream << "var __asm=asmJS(stdlib, ";
			compileAsmJSffiObject();
			stream << ", __heap);" << NewLine;
		}
	}

	compileDefineExports();
	compileEntryPoint();
	if (makeModule == MODULE_TYPE::COMMONJS || makeModule == MODULE_TYPE::ES6)
		compileCommonJSExports();

	if (areExtraParenthesisOpen)
		compileLoaderOrModuleEnd();

	if (!LowerAtomics && needWasmLoader)
	{
		compileWorkerMainScript();
		stream << "}" << NewLine;
	}

	if (makeModule == MODULE_TYPE::ES6)
		stream << "}" << NewLine;

	if (isWasmTarget)
		compileAssignHeaps(needWasmLoader);

	compileFileEnd(options);
}

bool CheerpWriter::useSwitch(const Instruction* term)
{
	// Consider only switch instructions
	if (!isa<SwitchInst>(term))
		return false;
	const SwitchInst* si = cast<SwitchInst>(term);
	// At least 3 successors
	if (si->getNumSuccessors() < 3)
		return false;
	//In asm.js cases values must be in the range [-2^31,2^31),
	//and the difference between the biggest and the smaller must be < 2^31
	int64_t max = std::numeric_limits<int64_t>::min();
	int64_t min = std::numeric_limits<int64_t>::max();
	for (auto& c: si->cases())
	{
		int64_t curr = c.getCaseValue()->getSExtValue();
		max = std::max(max,curr);
		min = std::min(min,curr);
	}
	if (min >= std::numeric_limits<int32_t>::min() &&
		max <= std::numeric_limits<int32_t>::max() && 
		//NOTE: this number is the maximum allowed by V8 for wasm's br_table,
		// it is not defined in the spec
		max-min <= 32 * 1024 &&
		// Avoid extremely big and extremely sparse tables, require at least 3% fill rate
		(max-min <= 100 || si->getNumCases() * 100 >= 3 * (max-min)))
	{
		return true;
	}
	return false;
}

void CheerpWriter::JSBytesWriter::addByte(uint8_t byte)
{
	if(!first)
		stream << ',';
	stream << (int)byte;
	first = false;
}

void CheerpWriter::AsmJSGepWriter::addValue(const llvm::Value* v, uint32_t size)
{
	offset = true;
	if (size == 1)
	{
		writer.compileOperand(v ,ADD_SUB);
		writer.stream << '+';
	}
	else if (isPowerOf2_32(size))
	{
		writer.stream << '(';
		writer.compileOperand(v, SHIFT);
		writer.stream << "<<";
		writer.stream << Log2_32(size);
		writer.stream << ")+";
	}
	else if (use_imul)
	{
		// NOTE: V8 requires imul to be coerced to int like normal functions
		writer.stream << '(' << writer.namegen.getBuiltinName(NameGenerator::Builtin::IMUL) << '(';
		writer.compileOperand(v ,LOWEST);
		writer.stream << ',' << size << ')';
		writer.stream << "|0)+";
	}
	else
	{
		writer.stream << '(';
		writer.compileOperand(v, MUL_DIV);
		writer.stream << '*';
		writer.stream << size;
		writer.stream << "|0)+";
	}
}

void CheerpWriter::AsmJSGepWriter::addConst(int64_t v)
{
	assert(v);
	// Just make sure that the constant part of the offset is not too big
	assert(v>=std::numeric_limits<int32_t>::min());
	assert(v<=std::numeric_limits<uint32_t>::max());

	offset = true;
	writer.stream << v << '+';
}

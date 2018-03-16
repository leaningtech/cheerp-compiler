//===-- Cheerp/Utility.h - Cheerp common routines -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2015 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_UTILITY_H
#define _CHEERP_UTILITY_H

#include <cctype>
#include <set>
#include <unordered_set>
#include "llvm/ADT/SmallString.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Cheerp/PointerAnalyzer.h"

namespace cheerp
{

bool isNopCast(const llvm::Value* val);
bool isValidVoidPtrSource(const llvm::Value* val, std::set<const llvm::PHINode*>& visitedPhis);

inline bool isValidVoidPtrSource(const llvm::Value* val)
{
	std::set<const llvm::PHINode*> visitedPhis;
	return isValidVoidPtrSource(val, visitedPhis);
}

bool isInlineable(const llvm::Instruction& I, const PointerAnalyzer& PA);

inline bool isBitCast(const llvm::Value* v)
{
	if( llvm::isa< llvm::BitCastInst>(v) )
		return true;
	if(const llvm::ConstantExpr * ce = llvm::dyn_cast<llvm::ConstantExpr>(v) )
		return ce->getOpcode() == llvm::Instruction::BitCast;
	if(const llvm::IntrinsicInst* II=llvm::dyn_cast<llvm::IntrinsicInst>(v))
	{
		switch(II->getIntrinsicID())
		{
			case llvm::Intrinsic::cheerp_cast_user:
			case llvm::Intrinsic::cheerp_upcast_collapsed:
				return true;
			default:
				break;
		}
	}
	return false;
}

inline bool isGEP(const llvm::Value* v)
{
	if( llvm::isa<llvm::GetElementPtrInst>(v) )
		return true;
	if(const llvm::ConstantExpr * ce = llvm::dyn_cast<llvm::ConstantExpr>(v) )
		return ce->getOpcode() == llvm::Instruction::GetElementPtr;
	return false;
}

#define WASM_INTRINSIC_LIST(x) \
	x("f32.abs", 0x8b, "fabsf") \
	x("f32.ceil", 0x8d, "ceilf") \
	x("f32.floor", 0x8e, "floorf") \
	x("f32.trunc", 0x8f, "truncf") \
	x("f32.nearest", 0x90, "roundf") \
	x("f32.sqrt", 0x91, "sqrtf") \
	x("f32.min", 0x96, "fminf") \
	x("f32.max", 0x97, "fmaxf") \
	x("f32.copysign", 0x98, "copysignf") \
	\
	x("f64.abs", 0x99, "fabs") \
	x("f64.ceil", 0x9b, "ceil") \
	x("f64.floor", 0x9c, "floor") \
	x("f64.trunc", 0x9d, "trunc") \
	x("f64.nearest", 0x9e, "round") \
	x("f64.sqrt", 0x9f, "sqrt") \
	x("f64.min", 0xa4, "fmin") \
	x("f64.max", 0xa5, "fmax") \
	x("f64.copysign", 0xa6, "copysign") \


bool isWasmIntrinsic(const llvm::Function* F);

const llvm::ConstantArray* ModuleGlobalConstructors(llvm::Module& M);

bool needsSecondaryName(const llvm::Value*, const PointerAnalyzer& PA);

uint32_t getIntFromValue(const llvm::Value* v);

inline uint32_t getMaskForBitWidth(int width)
{
	return (1 << width) - 1;
}

// Printable name of the llvm type - useful only for debugging
std::string valueObjectName(const llvm::Value * v);

bool hasNonLoadStoreUses(const llvm::Value* v);

llvm::StructType* getGEPContainerStructType(const llvm::User* gep);

class TypeSupport
{
public:
	TypeSupport( const llvm::Module & module ) :
		module(module) {}

	static bool isDerivedStructType(llvm::StructType* derivedType, llvm::StructType* baseType);

	static bool isClientGlobal(const llvm::Value * v)
	{
		return v->getName().startswith("_ZN6client");
	}

	static bool isClientType(llvm::Type* t)
	{
		if ( llvm::StructType * st = llvm::dyn_cast<llvm::StructType>(t) )
			return st->hasName() && st->getName().startswith("class._ZN6client");
		else 
			return false;
	}

	static bool isI32Type(llvm::Type* t)
	{
		return t->isIntegerTy(32);
	}

	static bool isTypedArrayType(llvm::Type* t, bool forceTypedArray)
	{
		return t->isIntegerTy(8) || t->isIntegerTy(16) || t->isIntegerTy(32) ||
			t->isFloatTy() || (forceTypedArray && t->isDoubleTy());
	}

	static bool isImmutableType(llvm::Type* t)
	{
		if(t->isIntegerTy() || t->isFloatTy() || t->isDoubleTy() || t->isPointerTy())
			return true;
		return false;
	}

	static bool hasByteLayout(llvm::Type* t)
	{
		if ( llvm::StructType * st = llvm::dyn_cast<llvm::StructType>(t) )
			return st->hasByteLayout();
		else if ( llvm::ArrayType * at = llvm::dyn_cast<llvm::ArrayType>(t) )
			return hasByteLayout(at->getElementType());
		else
			return false;
	}
	static bool isAsmJSPointer(const llvm::Type* t)
	{
		if (const llvm::PointerType* pt = llvm::dyn_cast<llvm::PointerType>(t))
		{
			t = pt->getPointerElementType();
			if ( const llvm::StructType * st = llvm::dyn_cast<llvm::StructType>(t) )
				return st->hasAsmJS();
			else if ( const llvm::ArrayType * at = llvm::dyn_cast<llvm::ArrayType>(t) )
			{
				if ( const llvm::StructType * st = llvm::dyn_cast<llvm::StructType>(at->getElementType()) )
					return st->hasAsmJS();
			}
		}
		return false;
	}

	static bool hasBasesInfoMetadata(llvm::StructType* t, const llvm::Module & m)
	{
		return getBasesMetadata(t, m) != nullptr;
	}

	// Bridge to the static version
	bool getBasesInfo(const llvm::StructType* t, uint32_t& firstBase, uint32_t& baseCount) const
	{
		return getBasesInfo(module, t, firstBase, baseCount);
	}

	static bool hasVirtualBases(const llvm::Module& module, const llvm::StructType* t);

	/**
	 * Find out if a given member of a struct requires the wrapping array
	 */
	bool useWrapperArrayForMember(const PointerAnalyzer& PA, llvm::StructType* st, uint32_t memberIndex) const;

	/**
	 * Returns the prefix character for an element of the passed type and index
	 */
	char getPrefixCharForMember(const PointerAnalyzer& PA, llvm::StructType* st, uint32_t memberIndex) const;

	static bool getBasesInfo(const llvm::Module& module, const llvm::StructType* t, uint32_t& firstBase, uint32_t& baseCount);

	static bool isJSExportedType(llvm::StructType* st, const llvm::Module& m);

	static std::pair<llvm::StructType*, llvm::StringRef> getJSExportedTypeFromMetadata(llvm::StringRef name, const llvm::Module & module);

	// Returns true if the type is not considered a literal object or array in JS
	static bool isSimpleType(llvm::Type* t, bool forceTypedArrays);

	static llvm::NamedMDNode* getBasesMetadata(const llvm::StructType * t, const llvm::Module & m)
	{
		if(!t->hasName())
			return nullptr;

		return m.getNamedMetadata(llvm::Twine(t->getName(),"_bases"));
	}

	/**
	 * Returns the required alignment for this time in the asmjs section
	 */
	static uint32_t getAlignmentAsmJS(const llvm::DataLayout& dl, llvm::Type* t);
	/**
	 * Returns the cookie size for a c++ dynamic array allocation of the provided type
	 */
	static uint32_t getArrayCookieSizeAsmJS(const llvm::DataLayout& dl, llvm::Type* t)
	{
		return cheerp::TypeSupport::getAlignmentAsmJS(dl, t) > 4? 8 : 4;
	}
private:
	const llvm::Module & module;
};

/*
 * Provide information about a malloc/calloc/etc call
 */
class DynamicAllocInfo
{
public:
	enum AllocType
	{
		not_an_alloc,
		malloc,
		calloc,
		cheerp_allocate,
		cheerp_reallocate,
		opnew, // operator new(unsigned int)
		opnew_array // operator new[](unsigned int)
	};
	
	/**
	 * This constructor works with any instruction.
	 * 
	 * If the passed argument is not a call, or is not an alloc,
	 * isValidAlloc will return false. In this case any other
	 * use of this object is not permitted.
	 */
	DynamicAllocInfo(llvm::ImmutableCallSite, const llvm::DataLayout* DL, bool forceTypedArrays);
	
	bool isValidAlloc() const { return type != not_an_alloc; }
	
	AllocType getAllocType() const { return type; }
	
	static AllocType getAllocType(llvm::ImmutableCallSite);

	/**
	 * Get the call/invoke instruction
	 */
	const llvm::Instruction * getInstruction() const
	{
		return call.getInstruction();
	}

	/**
	 * Every alloc instruction produces an i8*.
	 * This function tries to understand how the result of an alloc
	 * is used, and deduce the actual used type of the allocation.
	 * 
	 * Will report an llvm error if the use of the result is not consistent
	 */
	llvm::PointerType * getCastedType() const { return castedType; }
	
	/**
	 * This argument will never be null
	 */
	const llvm::Value * getByteSizeArg() const;
	
	/**
	 * This can be null if getAllocType() == calloc
	 */
	const llvm::Value * getNumberOfElementsArg() const;

	/**
	 * This can be null if getAllocType() != cheerp_reallocate
	 */
	const llvm::Value * getMemoryArg() const;

	/**
	 * Check if the size of the allocation is known only at runtime
	 */
	bool sizeIsRuntime() const;
	
	/**
	 * Check if the allocation should use a createArray function
	 */
	bool useCreateArrayFunc() const;
	
	/**
	 * Check if the allocation should use a createTypedArray function
	 */
	bool useCreatePointerArrayFunc() const;
	
	/**
	 * Check if the allocation should use typed arrays
	 */
	bool useTypedArray() const;

private:
	llvm::PointerType * computeCastedType() const;
	
	llvm::ImmutableCallSite call;
	AllocType type;
	uint32_t typeSize;
	llvm::PointerType * castedType;
	bool forceTypedArrays;
};

/**
 * Iterator over all the words composed by a given set of symbols.
 * 
 * SymbolTraits model:
 * 
 * struct SymbolTraits {
 * 	static constexpr char first_symbol = ... ; // The first symbol in the symbol order (i.e. 'a' for alphabet, etc.)
 * 	static char next( char c ); // The symbol after c, or first symbol if c is the last symbol
 * 	// Determine if the passed string is valid. 
 * 	// The implementation is free to modify the passed string in order to skip a long sequence of invalid symbols
 * 	template< class String >
 * 	static bool is_valid( String & ); 
 * };
 */
template<class SymbolTraits, unsigned StringSize = 4>
class name_iterator : 
	public std::iterator< std::input_iterator_tag, llvm::SmallString<StringSize> >,
	SymbolTraits // Empty base opt
{
public:
	name_iterator(SymbolTraits st) : 
		SymbolTraits( std::move(st) )
	{
		value_.assign(1, SymbolTraits::first_symbol );
	}
	
	explicit name_iterator(llvm::StringRef s, SymbolTraits st) : 
		SymbolTraits( std::move(st) ),
		value_(s)
	{}
	
	const llvm::SmallString<4>& operator*() const { return value_; }
	const llvm::SmallString<4>* operator->() const { return &value_; }
	
	name_iterator& operator++() { advance(); return *this; }
	name_iterator operator++(int) { name_iterator cpy(*this); advance(); return cpy; }
	
	bool operator==(const name_iterator & other) const { return value_ == other.value_; }
	bool operator!=(const name_iterator & other) const { return ! operator==(other); }
	
private:
	void advance()
	{
		do
		{
			for ( std::size_t i = value_.size(); (i--) > 0; )
			{
				value_[i] = SymbolTraits::next(value_[i]);
				
				if ( i == 0 )
				{
					if ( value_[0] == SymbolTraits::first_symbol )
						value_.insert( value_.begin(), SymbolTraits::first_symbol );
				}
				else if ( value_[i] != SymbolTraits::first_symbol  )
					break;
			}
		}
		while( !SymbolTraits::is_valid( value_ ) );
	}
	
	llvm::SmallString<4> value_;
};

/**
 * Demangles a C++ name by iterating over its (demangled) nested names.
 * We do not need to handle arguments' types (yet!).
 * 
 * Hopefully we can use this everywhere we need demangling, in order
 * to be safe.
 * 
 * Whenever we find a demangling error, we set an error state "error".
 * Whenever this happens, we set ourself to the "end" iterator. So error() can
 * never return true for a non-end iterator.
 * 
 * NOTE we obviously do not honor all the rules of a mangled C++ identifier.
 * We ignore templates, union and probably much more.
 */
struct demangler_iterator : std::iterator< 
	std::forward_iterator_tag,
	llvm::StringRef,
	std::ptrdiff_t,
	const char *,
	llvm::StringRef>
{
	demangler_iterator(): isNested(false),hasFailed(false) {}

	explicit demangler_iterator( llvm::StringRef i ) : tokenSize(0),isNested(false),hasFailed(false)
	{
		if ( i.startswith("_ZN") )
		{
			isNested = true;
			input = i.drop_front(3);
		}
		else if ( i.startswith("_Z") )
		{
			input = i.drop_front(2);
		}
		else
		{
			// Just return the name
			input = i;
			tokenSize = input.size();
			return;
		}

		advance();
	}
	
	llvm::StringRef operator*() const {
		assert( tokenSize <= input.size());
		return llvm::StringRef( input.begin(), tokenSize );
	}
	
	// TODO find a way to safely implement this
// 	const char * operator->() const;
	
	bool operator==(const demangler_iterator & other) const
	{
		return input == other.input;
	}
	
	bool operator!=(const demangler_iterator & other) const
	{
		return !operator==(other);
	}
	
	demangler_iterator& operator++() {
		advance();
		return *this;
	}
	
	demangler_iterator operator++(int) {
		demangler_iterator cpy(*this);
		advance();
		return cpy;
	}
	
	bool error() const { return hasFailed; }
	
private:
	
	void advance()
	{
		// Advance by tokenSize;
		input = input.drop_front(tokenSize);
		
		if ( input.empty() )
		{
			// End of input
			if ( isNested ) hasFailed = true;
			input = llvm::StringRef();
			return;
		}
		
		// We can not use strtol since StringRef is not guaranteed to be null-terminated!
		const char * FirstValid = std::find_if_not( 
			input.begin(),
			input.end(),
			::isdigit );

		bool parseFail = input.drop_back( input.end() -  FirstValid ).getAsInteger(10, tokenSize);

		if ( parseFail )
		{
			// Check if we have a constructor/destructor
			if ( input.size() >= 2 &&
				(input.startswith("C") || input.startswith("D") ) &&
				std::isdigit( input[1] ) )
			{
				tokenSize = 2;
			}
			else
			{
				// Bail out.
				if ( isNested && input.front() != 'E' )
					hasFailed = true;
				input = llvm::StringRef();
			}
		}
		else
		{
			// We successfully parsed tokenSize.
			// Drop all the initial characters which represent the token length
			input = input.drop_front( FirstValid - input.begin() );
			if ( input.size() < tokenSize )
			{
				//Oh oh.. the mangled name lied to us!
				hasFailed = true;
				input = llvm::StringRef();
			}
		}
	}
	
	llvm::StringRef input;
	std::size_t tokenSize;
	bool isNested;
	bool hasFailed;
};

// Forward define the Registerize class
class Registerize;

class EndOfBlockPHIHandler
{
public:
	EndOfBlockPHIHandler(const PointerAnalyzer& PA):PA(PA)
	{
	}
	void runOnEdge(const Registerize& registerize, const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB);
protected:
	const PointerAnalyzer& PA;
	virtual ~EndOfBlockPHIHandler()
	{
	}
private:
	struct PHIRegData
	{
		const llvm::PHINode* phiInst;
		llvm::SmallVector<std::pair<uint32_t, const llvm::Instruction*>,2> incomingRegs;
		enum STATUS { NOT_VISITED=0, VISITING, VISITED };
		STATUS status;
		bool selfReferencing;
		PHIRegData(const llvm::PHINode* p, llvm::SmallVector<std::pair<uint32_t, const llvm::Instruction*>,2>&& r, bool selfReferencing):
			phiInst(p), incomingRegs(std::move(r)), status(NOT_VISITED), selfReferencing(selfReferencing)
		{
		}
	};
	typedef std::map<uint32_t, PHIRegData> PHIRegs;
	void runOnPHI(PHIRegs& phiRegs, uint32_t phiId, const llvm::Instruction* incoming, llvm::SmallVector<std::pair<const llvm::PHINode*, /*selfReferencing*/bool>, 4>& orderedPHIs);
	// Callbacks implemented by derived classes
	virtual void handleRecursivePHIDependency(const llvm::Instruction* incoming) = 0;
	virtual void handlePHI(const llvm::Instruction* phi, const llvm::Value* incoming, bool selfReferencing) = 0;
	// Called for every register which is either assigned or used by PHIs in the edge
	virtual void setRegisterUsed(uint32_t reg) {};
};

template<class U, class V>
struct PairHash
{
		size_t operator()(const std::pair<U, V>& r) const
		{
			return std::hash<U>()(r.first) ^ std::hash<V>()(r.second);
		}
};

}

#endif //_CHEERP_UTILITY_H

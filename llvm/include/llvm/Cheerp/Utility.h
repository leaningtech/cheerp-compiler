//===-- Cheerp/Utility.h - Cheerp common routines -------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_UTILITY_H
#define _CHEERP_UTILITY_H

#include <cctype>
#include <set>
#include <string>
#include <unordered_set>
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/AbstractCallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Cheerp/EdgeContext.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Registerize.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/DeterministicUnorderedSet.h"
#include "llvm/Cheerp/ForbiddenIdentifiers.h"

namespace cheerp
{

const static int V8MaxLiteralDepth = 3;
const static int V8MaxLiteralProperties = 8;

bool isNopCast(const llvm::Value* val);
bool isValidVoidPtrSource(const llvm::Value* val, std::set<const llvm::PHINode*>& visitedPhis);

inline bool isValidVoidPtrSource(const llvm::Value* val)
{
	std::set<const llvm::PHINode*> visitedPhis;
	return isValidVoidPtrSource(val, visitedPhis);
}

inline llvm::Type* getElementType(llvm::Type* t)
{
	if(llvm::isa<llvm::PointerType>(t))
		return t->getPointerElementType();
	else
		return t->getArrayElementType();
}

class InlineableCache
{
	typedef llvm::DenseMap<const llvm::Instruction*, bool> Cache;
	typedef bool(InlineableCache::*InstructionToBoolFunction)(const llvm::Instruction&);
	//TODO: a more general solution would require limiting the cache size, but here the dimension of the cache is limited by the total number of Instruction
public:
	InlineableCache(const PointerAnalyzer& PA) : PA(PA)
	{
	}
	void clearCache()
	{
		cache.clear();
	}
	bool isInlineable(const llvm::Instruction& I);
	template<InstructionToBoolFunction recursiveCall>
	bool isInlineableImpl(const llvm::Instruction& I);
private:
	bool isInlineableWithoutCache(const llvm::Instruction& I);
	Cache cache;
	const PointerAnalyzer& PA;
	friend bool isInlineable(const llvm::Instruction& I, const PointerAnalyzer& PA);
};

bool isInlineable(const llvm::Instruction& I, const PointerAnalyzer& PA);

bool mayContainSideEffects(const llvm::Value* V, const PointerAnalyzer& PA);

// Forward define the Registerize class
class Registerize;

/**
* Returns true if this PHI will render identically for all incoming
* NOTE: If the PHI would completely disappear this returns false
*/
bool canDelayPHI(const llvm::PHINode* phi, const PointerAnalyzer& PA, const Registerize& registerize, const EdgeContext& edgeContext = EdgeContext::emptyContext());

template<uint32_t N>
bool isNumStatementsLessThan(const llvm::BasicBlock* BB,
	const PointerAnalyzer& PA, const Registerize& registerize, const bool skipPhi = false)
{
	bool asmjs = BB->getParent()->getSection() == llvm::StringRef("asmjs");
	uint32_t Count = 0;
	llvm::BasicBlock::const_iterator It = BB->begin();
	while(auto PHI = llvm::dyn_cast<llvm::PHINode>(It))
	{
		// Delayed PHIs are rendered in their parent block
		if (!skipPhi && canDelayPHI(PHI, PA, registerize))
		{
			if (PHI->getType()->isPointerTy() && PA.getPointerKind(PHI) == SPLIT_REGULAR)
				Count++;
			Count++;
		}
		if (Count >= N)
			return false;
		It++;
	}
	for (const auto& I: make_range(It, BB->end()))
	{
		if (I.isTerminator())
			break;
		if (isInlineable(I, PA))
			continue;
		if (const auto* II = llvm::dyn_cast<llvm::IntrinsicInst>(&I))
		{
			//Skip some kind of intrinsics
			if(II->getIntrinsicID()==llvm::Intrinsic::lifetime_start ||
				II->getIntrinsicID()==llvm::Intrinsic::lifetime_end ||
				II->getIntrinsicID()==llvm::Intrinsic::dbg_declare ||
				II->getIntrinsicID()==llvm::Intrinsic::dbg_value)
			{
				continue;
			}
			else if(II->getIntrinsicID()==llvm::Intrinsic::memcpy ||
				II->getIntrinsicID()==llvm::Intrinsic::memmove)
			{
				// In genericjs this compiles to multiple statements
				Count+=!asmjs;
			}
		}
		if (I.getType()->isPointerTy() && PA.getPointerKind(&I) == SPLIT_REGULAR)
			Count++;
		if (auto Store = llvm::dyn_cast<llvm::StoreInst>(&I))
		{
			const llvm::Value* ValueOp = Store->getValueOperand();
			if (ValueOp->getType()->isPointerTy()
				&& PA.getPointerKind(Store) == SPLIT_REGULAR)
			{
				Count++;
			}
		}
		else if (llvm::isa<llvm::VAArgInst>(I))
		{
			Count+= asmjs;
		}
		Count++;
		if (Count >= N)
			return false;
	}
	if (llvm::isa<llvm::ReturnInst>(BB->getTerminator()))
	{
		const llvm::Function* F = BB->getParent();
		if (F->getReturnType()->isPointerTy()
			&& PA.getPointerKindForReturn(F) == SPLIT_REGULAR)
		{
			Count++;
		}
		Count++;
	}
	return Count < N;
}

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

//Utility function that calculate the offset for Structs or Array at a given index of a GEP
int32_t partialOffset(llvm::Type* & curType, const llvm::DataLayout& DL, const int32_t index);

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

llvm::Type* getGEPContainerType(const llvm::User* gep);

inline bool isFreeFunctionName(llvm::StringRef name)
{
	return name=="free" || name=="_ZdlPv" || name=="_ZdaPv";
}

struct LoopWithDepth
{
	LoopWithDepth(const llvm::Loop* loop)
		: loop(loop), depth(calculateDepth(loop))
	{}
	void stepBack()
	{
		assert(depth && loop);
		--depth;
		loop = loop->getParentLoop();
	}
	const llvm::Loop* loop;
	uint32_t depth;
private:
	uint32_t calculateDepth(const llvm::Loop* loop)
	{
		if (loop == nullptr)
			return 0;
		return loop->getLoopDepth();
	}
};

const llvm::Loop* findCommonLoop(const llvm::LoopInfo* LI, const llvm::BasicBlock* A, const llvm::BasicBlock* B);

/**
 * Find out the lowest insertion point for an instruction which is used in multiple places
 *
 * currentInsertionPoint can be NULL, this is useful to start the process
 * I can be NULL as far as user is not a PHI
 */
const llvm::Instruction* findCommonInsertionPoint(const llvm::Instruction* I, const llvm::DominatorTree* DT, const llvm::Instruction* currentInsertionPoint, const llvm::Instruction* user);
llvm::Instruction* findCommonInsertionPoint(const llvm::Instruction* I, const llvm::DominatorTree* DT, llvm::Instruction* currentInsertionPoint, llvm::Instruction* user);

/**
 * Returns the incoming not-inlineable instruction for v
 *
 * Traverses pure-forward intructions such as BitCast and Trunc
 */
const llvm::Instruction* getUniqueIncomingInst(const llvm::Value* v, const PointerAnalyzer& PA);

class TypeSupport
{
public:
	TypeSupport( const llvm::Module & module ) :
		module(module) {}

	static bool isDerivedStructType(llvm::StructType* derivedType, llvm::StructType* baseType);

	static bool isClientGlobal(const llvm::GlobalVariable * v)
	{
		return v->getName().startswith("_ZN6client");
	}

	static bool isClientFuncName(llvm::StringRef ident)
	{
		return ident.startswith("_ZN6client") || ident.startswith("_ZNK6client");
	}

	static bool isClientFunc(const llvm::Function * v)
	{
		return isClientFuncName(v->getName());
	}

	static bool isClientConstructorName(llvm::StringRef ident)
	{
		return ident.startswith("cheerpCreate_ZN6client");
	}

	static bool isClientType(llvm::Type* t)
	{
		if ( llvm::StructType * st = llvm::dyn_cast<llvm::StructType>(t) )
			return st->hasName() && (st->getName().startswith("class._ZN6client") || st->getName().startswith("struct._ZN6client"));
		else 
			return false;
	}

	class ClientFunctionDemangled
	{
	public:
		ClientFunctionDemangled(const char* identifier)
			: demangled(getClientClassAndFunc(identifier)), className(demangled.first), funcName(demangled.second)
		{
		}
		ClientFunctionDemangled(const llvm::Function& F)
			: ClientFunctionDemangled(F.getName().data())
		{
		}
		bool isMethod() const
		{
			return !className.empty();
		}
	private:
		static std::pair<std::string, std::string> getClientClassAndFunc(const char* identifier);
		const std::pair<std::string, std::string> demangled;
	public:
		const std::string& className;
		const std::string& funcName;
	};

	static bool isI32Type(llvm::Type* t)
	{
		return t->isIntegerTy(32);
	}

	static bool isTypedArrayType(llvm::Type* t, bool forceTypedArray)
	{
		return t->isIntegerTy(8) || t->isIntegerTy(16) || t->isIntegerTy(32) || t->isIntegerTy(64) ||
			t->isFloatTy() || (forceTypedArray && t->isDoubleTy());
	}

	static bool isImmutableType(llvm::Type* t)
	{
		if(t->isIntegerTy() || t->isFloatTy() || t->isDoubleTy() || t->isPointerTy())
			return true;
		return false;
	}

	static bool hasByteLayout(const llvm::Type* t)
	{
		if (const llvm::StructType * st = llvm::dyn_cast<llvm::StructType>(t) )
			return st->hasByteLayout();
		else if (const llvm::ArrayType * at = llvm::dyn_cast<llvm::ArrayType>(t) )
			return hasByteLayout(at->getElementType());
		else
			return false;
	}
	// Is this type a pointer to a asmjs struct type?
	static bool isAsmJSPointer(const llvm::Type* t)
	{
		if (const llvm::PointerType* pt = llvm::dyn_cast<llvm::PointerType>(t))
		{
			t = pt->getPointerElementType();
			while ( t->isArrayTy() )
				t = t->getArrayElementType();
			if ( const llvm::StructType * st = llvm::dyn_cast<llvm::StructType>(t) )
				return st->hasAsmJS();
		}
		return false;
	}
	// Is this type a pointer to a genericjs struct type?
	static bool isGenericJSPointer(const llvm::Type* t)
	{
		if (const llvm::PointerType* pt = llvm::dyn_cast<llvm::PointerType>(t))
		{
			t = pt->getPointerElementType();
			while ( t->isArrayTy() )
				t = t->getArrayElementType();
			if ( const llvm::StructType * st = llvm::dyn_cast<llvm::StructType>(t) )
				return !st->isOpaque() && !st->hasAsmJS();
		}
		return false;
	}
	// Is the kind of values of this type RAW, in the given context?
	static bool isRawPointer(const llvm::Type* t, bool asmjs)
	{
		if (isAsmJSPointer(t))
			return true;
		if (!asmjs)
			return false;
		if (!t->isPointerTy())
			return false;
		llvm::Type* et = t->getPointerElementType();
		if (isTypedArrayType(et, true) || et->isIntegerTy(1))
			return true;
		if (et->isPointerTy() || et->isArrayTy())
			return true;
		if (et->isFunctionTy())
			return true;
		if (llvm::isa<llvm::StructType>(et) && llvm::cast<llvm::StructType>(et)->isOpaque())
			return asmjs;
		if (!WasmAnyref)
			llvm::report_fatal_error("Found an externref, but externref support is not enabled");
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

	static std::string getNamespacedFunctionName(llvm::StringRef name);

	static std::pair<llvm::StructType*, std::string> getJSExportedTypeFromMetadata(llvm::StringRef name, const llvm::Module & module);

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
	DynamicAllocInfo(const llvm::CallBase*, const llvm::DataLayout* DL, bool forceTypedArrays);
	
	bool isValidAlloc() const { return type != not_an_alloc; }
	
	AllocType getAllocType() const { return type; }
	
	static AllocType getAllocType(const llvm::CallBase*);

	/**
	 * Get the call/invoke instruction
	 */
	const llvm::Instruction * getInstruction() const
	{
		return call;
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
	
	const llvm::CallBase* call;
	AllocType type;
	uint32_t typeSize;
	llvm::PointerType * castedType;
	bool forceTypedArrays;
};

//replace CallInst(bitcast someFunc, ....) with BitCast(CalInst someFunc), returning whether something has been modified
bool replaceCallOfBitCastWithBitCastOfCall(llvm::CallInst& callInst, bool mayFail = false, bool performPtrIntConversions = false);

void replaceSomeUsesWith(std::vector<llvm::Use*> uses, llvm::Value* toSubstitute);

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
	name_iterator(llvm::StringRef prefix, SymbolTraits st) : 
		SymbolTraits( std::move(st) ), prefixLength(prefix.size())
	{
		value_ = prefix;
		value_.push_back(SymbolTraits::first_symbol);
	}
	
	const llvm::SmallString<StringSize>& operator*() const { return value_; }
	const llvm::SmallString<StringSize>* operator->() const { return &value_; }
	
	name_iterator& operator++() { advance(); return *this; }
	name_iterator operator++(int) { name_iterator cpy(*this); advance(); return cpy; }
	
	bool operator==(const name_iterator & other) const { return value_ == other.value_; }
	bool operator!=(const name_iterator & other) const { return ! operator==(other); }
	
	void advance(std::size_t endPos)
	{
		do
		{
			for ( std::size_t i = endPos; (i--) > prefixLength; )
			{
				value_[i] = SymbolTraits::next(value_[i]);
				
				if ( i == prefixLength )
				{
					if ( value_[i] == SymbolTraits::first_symbol )
						value_.insert( value_.begin() + i, SymbolTraits::first_symbol );
				}
				else if ( value_[i] != SymbolTraits::first_symbol  )
					break;
			}
		}
		while( !SymbolTraits::is_valid( value_, prefixLength != 0 ) );
	}
private:
	void advance()
	{
		advance(value_.size());
	}
	llvm::SmallString<StringSize> value_;
	uint32_t prefixLength;
};

/**
 * Iterate over all the valid JS identifier is much more complicated , because JS uses unicode.
 * Reference for valid JS identifiers:
*  http://stackoverflow.com/questions/1661197/valid-characters-for-javascript-variable-names
*/
struct JSSymbols
{
	enum : char {first_symbol = 'a' };
	static char next( char c )
	{
		return  c == '$' ? 'a' :
			c == 'z' ? 'A' :
			c == 'Z' ? '0' :
			c == '9' ? '_' :
			c == '_' ? '$' :
			++c;
	}

	JSSymbols(const std::vector<std::string>& reservedNames):externallyReservedNames(reservedNames)
	{
	}

	template< class String >
	bool is_valid( String & s, bool hasPrefix )
	{
		// Can not be empty
		if ( s.empty() ) return false;
		
		// Can not start with a digit
		if ( std::isdigit(s.front() ) )
		{
			std::fill( s.begin(), s.end(), '_' );
			return false;
		}
		
		// Check for reserved keywords
		if ( is_reserved_name(s) )
			return false;

		// "null" is used by cheerp internally
		if ( s == "null" )
			return false;
		
		if ( hasPrefix )
			return true;

		// In the rare case that a name is longer than 4 characters, it need to start with an underscore
		// just to be safe.
		if ( s.size() > 4 && s.front() != '_' )
		{
			std::fill( s.begin(), s.end(), '_' );
			return false;
		}
		
		return true;
	}

	template< class String >
	bool is_reserved_name( String& s)
	{
		return cheerp::isForbiddenJSIdentifier(s) ||
			std::binary_search(externallyReservedNames.begin(), externallyReservedNames.end(), s);
	}
	const std::vector<std::string>& externallyReservedNames;
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
		if ( i.startswith("_ZNK") )
		{
			isNested = true;
			input = i.drop_front(4);
		}
		else if ( i.startswith("_ZN") )
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

template<class U, class V>
struct PairHash
{
		size_t operator()(const std::pair<U, V>& r) const
		{
			return std::hash<U>()(r.first) ^ std::hash<V>()(r.second);
		}
};

	// Handy alias for a deterministic set of const llvm::Function* with no erasures
	using DeterministicFunctionSet = cheerp::DeterministicUnorderedSet<const llvm::Function*, RestrictionsLifted::NoPointerStability>;

	// Utility function to create a wrapper for FFI interoperability with
	// asm.js/wasm
	class FFIWrapping {
	public:
		FFIWrapping(llvm::Module& M,
				DeterministicFunctionSet& imports,
				DeterministicFunctionSet& insideModule,
				DeterministicFunctionSet& outsideModule)
				: M(M), imports(imports),
				  insideModule(insideModule), outsideModule(outsideModule)
		{
		}
		void run();
	private:
		llvm::Module& M;
		DeterministicFunctionSet& imports;
		DeterministicFunctionSet& insideModule;
		DeterministicFunctionSet& outsideModule;
	};
}

#endif //_CHEERP_UTILITY_H

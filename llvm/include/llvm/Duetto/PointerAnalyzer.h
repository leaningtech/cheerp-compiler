//===-- Duetto/Analyzer.h - Duetto analyzer code -----------------------------===//
//
//                     Duetto: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2011-2013 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _DUETTO_POINTER_ANALYZER_H
#define _DUETTO_POINTER_ANALYZER_H

#include "llvm/Duetto/NameGenerator.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Instructions.h"
#include <map>
#include <set>

namespace duetto {

/**
 * \addtogroup pointers Pointer implementation
 * \note Functions belonging to this group are implemented in Pointers.cpp
 * 
 * Three type of pointers are used:
 *   - COMPLETE_OBJECT This pointer can point only to a C++ struct/class type. It is implemented 
 *                     by directly binding a JS var to the pointed object. This type of pointer
 *                     does not support arithmetic nor ordering. In order to allow conversion to REGULAR pointer,
 *                     the object bound to a COMPLETE_OBJECT pointer shall contain the member "s", which is a JS ref to the object itself.
 *   - COMPLETE_ARRAY  This pointer points to the first element of a C++ array of struct/class or primitive type.
 *                     It is implemented in JS by binding a var to the pointed array. It does not support
 *                     global ordering.
 *   - REGULAR         Regular pointers are implemented with a JS object of the form: "{ d : OBJ, o : OFFSET }", where
 *                     OBJ is a JS reference to a *container* object of the pointer, and OFFSET is the offset of the pointed
 *                     object inside the container object (it can be a string or a integral value).
 *                     Pointers of type COMPLETE_OBJECT and COMPLETE_ARRAY are always convertible to REGULAR pointers,
 *                     but not viceversa. REGULAR pointers support pointer arithmetic; two REGULAR pointers are ordinable 
 *                     if they have the same container object. 
 * 
 * General rules (which apply both to JS immutable types and to struct/class objects):
 *    -# In order for an object to be bound to a regular pointer, a "container" object must exists. If the 
 *       object being bound to a pointer is a member object of some class/struct type or a member of an array the 
 *       container object naturally exists. Otherwise such object must be created inside an array of size one.
 * 
 *    -# An array of objects can always be bound to a COMPLETE_ARRAY pointer, just by taking its name.
 * 
 *    -# A single object can always be bound to a COMPLETE_OBJECT pointer, just by taking its name.
 * 
 *    -# A COMPLETE_ARRAY pointer can be converted to a REGULAR pointer by setting the "d" member to the COMPLETE_ARRAY pointer
 *       name, and the "o" member to zero.
 * 
 *    -# A COMPLETE_OBJECT pointer can be converted to a REGULAR pointer using the self member. Notice that
 *       arithmetic operations on such pointer are still available, provided the user does not dereference the result, and generate
 *       a on offset value like "s1, s2" etc.
 *       
 * 
 * Optimization:
 *    - no-self-pointer. Avoid the creation of the member ".s" if the conversion to REGULAR pointer is not required, \sa{isNoSelfPointerOptimizable}.
 *    - no-wrapping-array. Avoid the creation of a wrapping array for immutable types if possible, \sa{isNoWrappingArrayOptimizable}.
 * 
 * @{
*/

enum POINTER_KIND {
	UNDECIDED = 0,
	COMPLETE_ARRAY,
	COMPLETE_OBJECT,
	REGULAR
};

// Functionalities provided by a pointer
enum POINTER_USAGE_FLAG {
	POINTER_NONCONST_DEREF = 1, // The pointer is used to modify the pointed object
	POINTER_IS_NOT_UNIQUE_OWNER = (1 << 1), // The pointer is not the only one which points to the object
	POINTER_ARITHMETIC = (1 << 2), // The pointer can be incremented/decremented etc, and/or it is used to access an array (i.e. p[i])
	POINTER_ORDINABLE = (1 << 3), // The pointer is used for a comparison with another pointer
	POINTER_EQUALITY_COMPARABLE = (1 << 4), // The pointer is used for ==/!= comparison with another pointer.
	POINTER_CASTABLE_TO_INT = (1 << 5),  // The pointer is explicitly casted to an integer (usually used to implement pointers hash table)
	
	POINTER_UNKNOWN = (1LL << 32LL) - 1
};

class DuettoPointerAnalyzer {
public:
	
	DuettoPointerAnalyzer( NameGenerator & namegen ) : namegen(namegen) {}
	
	POINTER_KIND getPointerKind(const llvm::Value* v) const;

	// Returns a bitmask of POINTER_USAGE_FLAG
	/**
	 * Memoization wrapper around dfsPointerUsageFlagsComplete
	 */
	uint32_t getPointerUsageFlagsComplete(const llvm::Value * v) const;
	
	// Dump a pointer value info
	void dumpPointer(const llvm::Value * v) const;

#ifdef DUETTO_DEBUG_POINTERS
	void dumpAllPointers() const;
        void dumpAllFunctions() const;
#endif //DUETTO_DEBUG_POINTERS
	
	// Return a string containing the type name of the object V.
	static std::string valueObjectName(const llvm::Value * v);

private:
	/** 
	 * Compute the usage of a single pointer, regardless of the phi nodes
	 */
	//TODO at the moment if it is used in a CallInst it returns POINTER_UNKNOWN.
	// CallInst should be handled inside getPointerUsageFlagsComplete, in order to provide information on how that pointer is used inside the function call.
	// This is especially important at the moment for memset/memcpy/memmove.
	uint32_t getPointerUsageFlags(const llvm::Value* v) const;
	
	/**
	 * Compute the sum of the usages of all the "child" pointers, where "child pointer" means any pointer which can be initialized to this one's value.
	 * \param v The pointer to inspect.
	 * \param openset Set of the visited pointers in order to stop cyclic dependencies in the phi node.
	 */
	uint32_t dfsPointerUsageFlagsComplete(const llvm::Value * v,std::set<const llvm::Value *> & openset) const;
	
	template<class ArgOrInvokeInst>
	uint32_t usageFlagsForStoreAndInvoke(const llvm::Value * v, const ArgOrInvokeInst * I, std::set<const llvm::Value *> & openset) const;

public:
	// Detect if a no-self-pointer optimization is applicable to the pointer value
	bool isNoSelfPointerOptimizable(const llvm::Value * v) const;

	// Detect if a no-wrapping-array optimization is applicable to the pointer value
	bool isNoWrappingArrayOptimizable(const llvm::Value * v) const;

	// Detect if a function can possibly be called indirectly
	bool canBeCalledIndirectly(const llvm::Function * f) const;

private:
	
	bool computeCanBeCalledIndirectly(const llvm::Constant * c) const;
	
	typedef std::map<const llvm::Value *, POINTER_KIND> pointer_kind_map_t;
	typedef std::map<const llvm::Value *, uint32_t> pointer_usage_map_t;
	
	mutable pointer_kind_map_t pointerKindMap;
	mutable pointer_usage_map_t pointerCompleteUsageMap;	
	mutable pointer_usage_map_t pointerUsageMap;
	
	typedef std::map<const llvm::Function *, bool > function_indirect_call_map_t;
	mutable function_indirect_call_map_t functionIndirectCallMap;
	
#ifdef DUETTO_DEBUG_POINTERS
	typedef std::set<const llvm::Value *> known_pointers_t;
	mutable known_pointers_t debugAllPointersSet;
        
        typedef std::set<const llvm::Function *> known_functions_t;
        mutable known_functions_t debugAllFunctionsSet;
#endif //DUETTO_DEBUG_POINTERS
	
	const NameGenerator & namegen;

};

/** @} */

}

#endif //_DUETTO_POINTER_ANALYZER_H

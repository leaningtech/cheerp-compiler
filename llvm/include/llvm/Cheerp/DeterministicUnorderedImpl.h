//===-- Cheerp/DeterministicUnorderedImpl.h - Cheerp utility class ---------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2019-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_DETERMINISTIC_UNORDERED_IMPL_H
#define _CHEERP_DETERMINISTIC_UNORDERED_IMPL_H

#include <unordered_map>
#include <utility>
#include <algorithm>
#include <deque>
#include <list>
#include <cassert>
#include <iostream>
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"

/*
   Deterministic Unordered Container Implementation

   There are 2 choices that are made are compile time:
   -Should the linear container be std::list or std::deque?
   -Should the associative container have Key or Key* as key?


   -Should the linear container be std::list or std::deque?
   std::list is more general, preserve pointer and interator stability.
   std::deque is faster, but works only if additional guarantee are given: either NoErasure will be done (and so it's fine just to append to the deque)
   or if no pointer & iterator stability is needed (and so on erasure elements can be shuffled around)

   At compile time it will be checked whether Value is movable at all, and whether NoErasure or NoPointerStability has been specified,
   and the appropriate container is chosen


   -Should the associative container have Key or Key* as key?
   Generally, it should hold Key* with an appropriate Hash function and Equality function.
   If Key is already a pointer (and this happens in most cases), we can store it directly and save us an additional indirections.
   Also this is chosen at compile time.
*/

namespace deterministicUnorderedImplementation
{


template <typename Key, typename Value, class Hash_Key, template<typename, typename...> class Container, bool CouldErase>
class DeterministicUnorderedImpl
{
public:
	using size_type = unsigned;
	using ContainerLocal = class Container<Value>;
	using iterator = typename ContainerLocal::iterator;
	using const_iterator = typename ContainerLocal::const_iterator;
	DeterministicUnorderedImpl()
	{
	}
	DeterministicUnorderedImpl(DeterministicUnorderedImpl&& other) : DeterministicUnorderedImpl()
	{
		this->operator=(other);
	}
	DeterministicUnorderedImpl& operator=(DeterministicUnorderedImpl&& rhs)
	{
		if (&rhs != this)
		{
			map = std::move(rhs.map);
			container = std::move(rhs.container);
		}
		return *this;
	}
	void swap(DeterministicUnorderedImpl& rhs)
	{
		if (&rhs == this)
			return;
		map.swap(rhs.map);
		container.swap(rhs.container);
	}
	iterator find(const Key& key)
	{
		auto it = map.find(mapped(key));
		if (it == map.end())
			return end();
		return it->second;
	}
	const_iterator find(const Key& key) const
	{
		return const_cast<DeterministicUnorderedImpl*>(this)->find(key);
	}
	bool empty() const
	{
		assert(map.size() == container.size());
		return container.empty();
	}
	void clear()
	{
		map.clear();
		container.clear();
	}
	size_type count(const Key t) const
	{
		return map.count(mapped(t));
	}
	size_type size() const
	{
		assert(map.size() == container.size());
		return container.size();
	}
	const_iterator begin() const
	{
		return container.begin();
	}
	const_iterator end() const
	{
		return container.end();
	}
	iterator begin()
	{
		return container.begin();
	}
	iterator end()
	{
		return container.end();
	}
protected:
	constexpr static bool isKeyPointer()
	{
		return std::is_pointer<Key>();// || std::is_fundamental<Key>();
	}
	typedef typename std::conditional<isKeyPointer(), Key, const Key*>::type Key_ptr;

	template <typename Ret, typename Arg, bool isPointer>
	struct mappedImpl
	{
		static Ret func(const Arg& key);
	};
	template <typename Ret, typename Arg>
	struct mappedImpl<Ret, Arg, true>
	{
		static Ret func(const Arg& key)
		{
			return key;
		}
	};
	template <typename Ret, typename Arg>
	struct mappedImpl<Ret, Arg, false>
	{
		static Ret func(const Arg& key)
		{
			return &key;
		}
	};
	static Key_ptr mapped(const Key& key)
	{
		return mappedImpl<Key_ptr, Key, isKeyPointer()>::func(key);
	}


	void eraseImplFromList(iterator& iter)
	{
		container.erase(iter);
	}
	void eraseImplFromDeque(iterator& iter)
	{
		std::swap((*iter), (container.back()));
		map[*iter] = iter;
		map.erase(container.back());
		container.pop_back();
	}

	template <typename inner_iterator, typename Class, class T>
	struct removeFromImpl
	{
		static void erase(inner_iterator& iter, Class& obj);
	};
	template <typename inner_iterator, typename Class>
	struct removeFromImpl<inner_iterator, Class, std::list<Value>>
	{
		static void erase(inner_iterator& iter, Class& obj)
		{
			obj.eraseImplFromList(iter);
		}
	};
	template <typename inner_iterator, typename Class>
	struct removeFromImpl<inner_iterator, Class, std::deque<Value>>
	{
		static void erase(inner_iterator& iter, Class& obj)
		{
			obj.eraseImplFromDeque(iter);
		}
	};
	template <class T>
	void removeFrom (iterator& iter)
	{
		removeFromImpl<iterator, DeterministicUnorderedImpl, T>::erase(iter, *this);
	}

	//In this cases we store pointers to Key in the map
	struct KeyPtr_Hash
	{
		size_t operator() (const Key_ptr& p) const
		{
			return Hash_Key().operator()((const Key&)(*p));
		}
	};
	struct KeyPtr_Equal
	{
		bool operator() (const Key_ptr& a, const Key_ptr& b) const
		{
				return *a == *b;
		}
	};
	struct DenseMapInfoSpecialPtr {
		using T = const Key;
		static inline T* getEmptyKey()
		{
			uintptr_t Val = static_cast<uintptr_t>(-1);
			return reinterpret_cast<T*>(Val);
		}
		static inline T* getTombstoneKey()
		{
			uintptr_t Val = static_cast<uintptr_t>(-2);
			return reinterpret_cast<T*>(Val);
		}
		static unsigned getHashValue(const T* PtrVal)
		{
			return (unsigned)KeyPtr_Hash().operator()(PtrVal);
		}
		static bool isEqual(const T *LHS, const T *RHS)
		{
			if (LHS == getEmptyKey() || LHS == getTombstoneKey())
				return RHS == LHS;
			if (RHS == getEmptyKey() || RHS == getTombstoneKey())
				return false;
			return KeyPtr_Equal().operator()(LHS, RHS);
		}
	};
	ContainerLocal container;
	typename std::conditional<isKeyPointer(),
		 llvm::DenseMap<Key, iterator, llvm::DenseMapInfo<Key>>,
		 llvm::DenseMap<Key_ptr, iterator, DenseMapInfoSpecialPtr>
			 >::type map;
};

template <typename T>
constexpr bool isMovable()
{
	return std::is_move_assignable<T>();
}

enum class RestrictionsLifted : uint8_t
{
	None = 0, NoErasure = 1, NoPointerStability = 2, NoDeterminism = 4
};

constexpr RestrictionsLifted operator| (const RestrictionsLifted& a, const RestrictionsLifted& b)
{
	return RestrictionsLifted((uint8_t)a | (uint8_t)b);
}

constexpr bool nonEraseable(const RestrictionsLifted& x)
{
	return (uint8_t)x & (uint8_t)RestrictionsLifted::NoErasure;
}

constexpr bool nonStable(const RestrictionsLifted& x)
{
	return (uint8_t)x & (uint8_t)RestrictionsLifted::NoPointerStability;
}

template <typename Value_type>
constexpr bool couldBeDeque(const RestrictionsLifted& restrictionVoided)
{
	//Either there are no Erase or (value_type is movable and pointer stability is not needed)
	return nonEraseable(restrictionVoided) ||
		(isMovable<Value_type>() && nonStable(restrictionVoided) );
}

}

#endif

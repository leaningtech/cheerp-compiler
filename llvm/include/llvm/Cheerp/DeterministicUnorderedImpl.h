//===-- Cheerp/DeterministicUnorderedImpl.h - Cheerp utility class ---------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
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

template <typename Key, typename Value, class Hash_Key, template<typename, typename...> class Container, bool CouldErase>
class DeterministicUnorderedImpl
{
public:
	using size_type = unsigned;
	using ContainerLocal = Container<Value>;
	using iterator = typename ContainerLocal::iterator;
	using const_iterator = typename ContainerLocal::const_iterator;
	constexpr static bool isKeyPointer()
	{
		return std::is_pointer<Key>();// || std::is_fundamental<Key>();
	}
	typedef typename std::conditional<isKeyPointer(), Key, const Key*>::type Key_ptr;
	template <bool isPointer>
	static Key_ptr mappedImpl(const Key& key);
	template <>
	static Key_ptr mappedImpl</*isPointer*/true>(const Key& key)
	{
		return key;
	}
	template <>
	static Key_ptr mappedImpl</*isPointer*/false>(const Key& key)
	{
		return &key;
	}
	static Key_ptr mapped(const Key& key)
	{
		return mappedImpl<isKeyPointer()>(key);
	}
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
	template <class T>
	void removeFrom (iterator& iter);
	template <>
	void removeFrom<std::list<Value>> (iterator& iter)
	{
		container.erase(iter);
	}
	template <>
	void removeFrom<std::deque<Value>> (iterator& iter)
	{
		std::swap(const_cast<Key>(iter->first), const_cast<Key>(container.back()->first));
		std::swap(iter->second, container.back()->second);
		map[mapped(iter->first)] = iter;
		container.pop_back();
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

	//In this cases Key is already pointer-like, so we store it twice, one in the map and one in the deque/list
	struct Key_Hash
	{
		size_t operator() (const Key& p) const
		{
			return Hash_Key().operator()(p);
		}
	};
	struct Key_Equal
	{
		bool operator() (const Key& a, const Key& b) const
		{
				return a == b;
		}
	};
	struct DenseMapInfoSpecial {
		using T = Key;
		static inline T getEmptyKey()
		{
			uintptr_t Val = static_cast<uintptr_t>(-1);
			return reinterpret_cast<T>(Val);
		}
		static inline T getTombstoneKey()
		{
			uintptr_t Val = static_cast<uintptr_t>(-2);
			return reinterpret_cast<T>(Val);
		}
		static unsigned getHashValue(const T PtrVal)
		{
			return (unsigned)Key_Hash().operator()(PtrVal);
		}
		static bool isEqual(const T LHS, const T RHS)
		{
			return Key_Equal().operator()(LHS, RHS);
		}
	};

	ContainerLocal container;
	typename std::conditional<isKeyPointer(),
//		 std::unordered_map<Key, iterator, Key_Hash, Key_Equal>,
//		 std::unordered_map<Key_ptr, iterator, KeyPtr_Hash, KeyPtr_Equal>
		 llvm::DenseMap<Key, iterator, DenseMapInfoSpecial>,
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

#endif

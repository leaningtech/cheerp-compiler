//===-- Cheerp/DeterministicUnorderedMap.h - Cheerp utility class ---------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_DETERMINISTIC_UNORDERED_MAP_H
#define _CHEERP_DETERMINISTIC_UNORDERED_MAP_H

#include "llvm/Cheerp/DeterministicUnorderedImpl.h"

/*
   Deterministic Unordered Map

   Currently implement the most common map-operations (insert, operator[], count, find, erase, clear, empty, swap, operator=)
   with a slight per-operation additional cost over a std::map (roughly 2x in memory size and negligible in time-complexity)
   All operation, iterations comprised, are deterministic

   No operation invalidates "live" iterators (eg. after you call delete, you should not access an iterator)

   The determinism is achieved by keeping 2 different datastructure updated at the same time, a std::map<key,std::list<>::iterator> and a std::list<std::pair<key, mapped>>.
   Iterations are done on the list iterators while the other operations are performed first on the map
*/


namespace cheerp
{

template <typename Key, typename Mapped, class Hash_Key, template<typename, typename...> class Container, bool CouldErase>
class DeterministicUnorderedMapImpl : public
	DeterministicUnorderedImpl<Key, std::pair<const Key, Mapped>, Hash_Key, Container, CouldErase>
{
public:
	typedef typename std::pair<const Key, Mapped> value_type;
	using BaseClass = DeterministicUnorderedImpl<Key, value_type, Hash_Key, Container, CouldErase>;
	using BaseClass::BaseClass;
	using typename BaseClass::iterator;
	using typename BaseClass::const_iterator;
	using BaseClass::map;
	using BaseClass::container;
	std::pair<iterator,bool> emplace(const Key& k, const Mapped& m)
	{
		return insertImpl(k, m);
	}
	std::pair<iterator,bool> insertImpl(const Key& k, const Mapped& m)
	{
		iterator W = this->find(k);
		if (W != BaseClass::end())
			return {W, false};
		container.push_back(std::make_pair(k, m));
		W = --BaseClass::end();
		map[this->mapped(W->first)] = W;
		return {W, true};
	}
	std::pair<iterator,bool> insert(const value_type& value)
	{
		return insertImpl(value.first, value.second);
	}
	template<typename Iter>
	DeterministicUnorderedMapImpl(Iter I, Iter E) : DeterministicUnorderedMapImpl()
	{
		insert<Iter>(I, E);
	}
	DeterministicUnorderedMapImpl(const DeterministicUnorderedMapImpl& other) : DeterministicUnorderedMapImpl()
	{
		this->operator=(other);
	}
	DeterministicUnorderedMapImpl& operator=(const DeterministicUnorderedMapImpl& rhs)
	{
		if (this != &rhs)
		{
			this->clear();
			map.reserve(rhs.size());
			insert(rhs.begin(), rhs.end());
		}
		return *this;
	}
	template<typename It>
	void insert(It b, It e)
	{
		while (b != e)
		{
			insert(*b);
			++b;
		}
	}
	Mapped& operator[](const Key& key)
	{
	auto it = this->find(key);
	if (it == this->end())
		return ((insert(std::make_pair(key,Mapped{}))).first)->second;
	return it->second;
	}
	Mapped& at(const Key& key)
	{
		auto it = this->find(key);
		assert (it != this->end());
		return it->second;
	}
	const Mapped& at(const Key& key) const
	{
		return const_cast<DeterministicUnorderedMapImpl*>(this)->at(key);
	}
	template<bool couldErase>
	bool eraseImpl(const Key& t);
	template<>
	bool eraseImpl<true>(const Key& t)
	{
		iterator W = find(t);
		if (W == BaseClass::end())
			return false;
		map.erase(this->mapped(W->first));
		BaseClass::removeFrom<BaseClass::ContainerLocal>(W);
		return true;
	}
	template<>
	bool eraseImpl<false>(const Key& t)
	{
		static_assert(CouldErase, "No erase are possible");
		return false;
	}
	bool erase(const Key& t)
	{
		return eraseImpl<CouldErase>(t);
	}
};

template <typename Key, typename Mapped, class Hash_Key, bool CouldErase>
class DequeMap : public DeterministicUnorderedMapImpl<Key, Mapped, Hash_Key, std::deque, CouldErase>
{
	using BaseClass = DeterministicUnorderedMapImpl<Key, Mapped, Hash_Key, std::deque, CouldErase>;
	using BaseClass::BaseClass;
};

template <typename Key, typename Mapped, class Hash_Key, bool CouldErase>
class ListMap : public DeterministicUnorderedMapImpl<Key, Mapped, Hash_Key, std::list, CouldErase>
{
	using BaseClass = DeterministicUnorderedMapImpl<Key, Mapped, Hash_Key, std::list, CouldErase>;
	using BaseClass::BaseClass;
};


template <typename Key, typename Mapped, RestrictionsLifted restrictionVoided = RestrictionsLifted::None, class Hash_Key=std::hash<Key>>
class DeterministicUnorderedMap : public 
				  std::conditional<
		couldBeDeque<std::pair<Key,Mapped>>(restrictionVoided),
		DequeMap<Key, Mapped, Hash_Key, !nonEraseable(restrictionVoided)>,
		ListMap<Key, Mapped, Hash_Key, !nonEraseable(restrictionVoided)> >::type
{
	using BaseClass = typename std::conditional<
		couldBeDeque<std::pair<Key,Mapped>>(restrictionVoided),
		DequeMap<Key, Mapped, Hash_Key, !nonEraseable(restrictionVoided)>,
		ListMap<Key, Mapped, Hash_Key, !nonEraseable(restrictionVoided)> >::type;
	using BaseClass::BaseClass;
};


}

#endif

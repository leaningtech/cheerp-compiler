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

   Implement most of unordered_map-interface (insert, operator[], count, find, erase, clear, empty, swap, operator=),
   with similar performance time-wise while having a slight memory overhead
   All operation, iterations comprised, are deterministic

   On construction you may provide an additional template paramether explicitly guarantee that certain restrictions apply,
   and this information is used to improve the speed of the container.
   Restrictions that can be voided are: NoErasure (no erase calls allowed), NoPointerStability (objects can be moved)

   If NoPointerStability is not specified, no operation invalidates "live" iterators (eg. after you call delete, you should not access an iterator.

   The determinism is achieved by keeping 2 different datastructure updated at the same time.
   One is either a std::list or std::deque (list in the general case, deque if additional guarantee are given) of pair<Key, Mapped>
   The other is an unordered_map from key (or key*) to iterators in the other container with an appropriate Equality function.
   Iterations are done on the list/deque iterators while the other operations are performed first on the map
*/


namespace cheerp
{

using namespace deterministicUnorderedImplementation;

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
	std::pair<iterator,bool> emplace(const Key& k, const Mapped& m)
	{
		return insertImpl(k, m);
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
	bool erase(const Key& t)
	{
		return eraseImpl<CouldErase>(t);
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
private:
	using BaseClass::map;
	using BaseClass::container;
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
class DeterministicUnorderedMap : public std::conditional<
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

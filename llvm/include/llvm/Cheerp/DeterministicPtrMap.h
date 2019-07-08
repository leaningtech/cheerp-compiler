//===-- Cheerp/DeterministicPtrMap.h - Cheerp utility class ---------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_DETERMINISTIC_PTR_MAP_H
#define _CHEERP_DETERMINISTIC_PTR_MAP_H

#include <map>
#include <list>
#include <cassert>

/*
   Deterministic Pointer Map

   Currently implement the most common map-operations (insert, operator[], count, find, erase, clear, empty, swap, operator=)
   with a slight per-operation additional cost over a std::map (roughly 2x in memory size and negligible in time-complexity)
   All operation, iterations comprised, are deterministic

   No operation invalidates "live" iterators (eg. after you call delete, you should not access an iterator)

   The determinism is achieved by keeping 2 different datastructure updated at the same time, a std::map<key,std::list<>::iterator> and a std::list<std::pair<key, mapped>>.
   Iterations are done on the list iterators while the other operations are performed first on the map
*/


namespace cheerp
{

template <typename Key, typename Mapped>
class DeterministicPtrMap
{
	//This is a specialized set only meant to work with pointers
	static_assert(std::is_pointer<Key>::value, "DeterministicPtrMap currently index only pointer types");
	using size_type = unsigned;
public:
	typedef typename std::pair<const Key, Mapped> value_type;
	typedef typename std::list<value_type>::iterator iterator;
	typedef typename std::list<value_type>::const_iterator const_iterator;
	DeterministicPtrMap()
	{
	}
	DeterministicPtrMap(const DeterministicPtrMap& other) : DeterministicPtrMap()
	{
		this->operator=(other);
	}
	DeterministicPtrMap(DeterministicPtrMap&& other) : DeterministicPtrMap()
	{
		this->operator=(other);
	}
	template<typename Iter>
	DeterministicPtrMap(Iter I, Iter E) : DeterministicPtrMap()
	{
		insert<Iter>(I, E);
	}
	DeterministicPtrMap& operator=(const DeterministicPtrMap& rhs)
	{
		insert(rhs.begin(), rhs.end());
		return *this;
	}
	DeterministicPtrMap& operator=(DeterministicPtrMap&& rhs)
	{
		if (&rhs != this)
		{
			map = std::move(rhs.map);
			list = std::move(rhs.list);
		}
		return *this;
	}
	void swap(DeterministicPtrMap& rhs)
	{
		if (&rhs == this)
			return;
		map.swap(rhs.map);
		list.swap(rhs.list);
	}
	Mapped& operator[](const Key& key)
	{
		return ((insert(std::make_pair(key,Mapped{}))).first)->second;
	}
	Mapped& at(const Key& key)
	{
		assert(count(key));
		return operator[](key);
	}
	const Mapped& at(const Key& key) const
	{
		return const_cast<DeterministicPtrMap&>(this)->at(key);
	}
	iterator find(const Key& key)
	{
		if (count(key) == 0)
			return end();
		return map.at(key);
	}
	const_iterator find(const Key& key) const
	{
		return const_cast<DeterministicPtrMap*>(this)->find(key);
	}
	std::pair<iterator,bool> insert(const value_type& value)
	{
		iterator W = find(value.first);
		if (W != end())
			return {W, false};
		list.push_back(value);
		W = --end();
		map[value.first] = W;
		return {W, true};
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
		iterator W = find(t);
		if (W == end())
			return false;
		map.erase(t);
		list.erase(W);
		return true;
	}
	bool empty() const
	{
		assert(map.size() == list.size());
		return list.empty();
	}
	void clear()
	{
		map.clear();
		list.clear();
	}
	size_type count(const Key t) const
	{
		return map.count(t);
	}
	size_type size() const
	{
		assert(map.size() == list.size());
		return list.size();
	}
	const_iterator begin() const
	{
		return list.begin();
	}
	const_iterator end() const
	{
		return list.end();
	}
	iterator begin()
	{
		return list.begin();
	}
	iterator end()
	{
		return list.end();
	}
private:
	std::list<value_type> list;
	std::map<const Key, iterator> map;
};

}

#endif

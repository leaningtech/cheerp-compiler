//===-- Cheerp/DeterministicPtrSet.h - Cheerp utility class ---------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_DETERMINISTIC_PTR_SET_H
#define _CHEERP_DETERMINISTIC_PTR_SET_H

#include <unordered_map>
#include <deque>
#include <cassert>

/*
   Deterministic Pointer Set

   Currently implement the most common set-operations (insert, count, find, erase, clear, empty, swap, operator=)
   with a slight per-operation additional cost over a std::unordered_set (roughly 2x in memory size and negligible in time-complexity)
   while allowing faster iterations over the collection of items.
   All operation, iterations comprised, are deterministic
   Note that for operations that returns iterators, it means that the returned iterator - begin() is constant over different executions.

   erase() invalidates iterators, so combining iterations with erase should be avoided.
   The same is true for indirect operations that relies on iteration+erase, line remove_if()

   The determinism is achieved by keeping 2 different datastructure updated at the same time, a std::unordered_map<pointer,std::deque<pointer>::iterator> and a std::deque<pointer>.
   Iterations are done on the deque iterators while the other operations are performed first on the unordered_map
*/


namespace cheerp
{

template <typename T>
class DeterministicPtrSet
{
	//This is a specialized set only meant to work with pointers
	static_assert(std::is_pointer<T>::value, "Indexer currently index only pointer types");
	using size_type = unsigned;
public:
	typedef typename std::deque<T>::iterator iterator;
	typedef typename std::deque<T>::const_iterator const_iterator;
	DeterministicPtrSet()
	{
	}
	DeterministicPtrSet(const DeterministicPtrSet& other) : DeterministicPtrSet()
	{
		this->operator=(other);
	}
	DeterministicPtrSet(DeterministicPtrSet&& other) : DeterministicPtrSet()
	{
		this->operator=(other);
	}
	template<typename Iter>
	DeterministicPtrSet(Iter I, Iter E) : DeterministicPtrSet()
	{
		insert<Iter>(I, E);
	}
	DeterministicPtrSet& operator=(const DeterministicPtrSet& rhs)
	{
		map = rhs.map;
		vec = rhs.vec;
		normalize();
		return *this;
	}
	DeterministicPtrSet& operator=(DeterministicPtrSet&& rhs)
	{
		if (&rhs != this)
		{
			this->swap(rhs);
			rhs.clear();
		}
		return *this;
	}
	void swap(DeterministicPtrSet& rhs)
	{
		if (&rhs == this)
			return;
		map.swap(rhs.map);
		vec.swap(rhs.vec);
		normalize();
		rhs.normalize();
	}
	iterator find(const T t)
	{
		if (count(t) == 0)
			return end();
		return map.at(t);
	}
	const_iterator find(const T t) const
	{
		return const_cast<DeterministicPtrSet*>(this)->find(t);
	}
	std::pair<iterator,bool> insert(T t)
	{
		iterator W = find(t);
		if (W != end())
			return {W, false};
		vec.push_back(t);
		W = vec.end()-1;
		map[t] = W;
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
	bool erase(T t)
	{
		iterator W = find(t);
		if (W == end())
			return false;
		iterator K = find(vec.back());
		assert(K != end());
		std::swap<T>(*K, *W);
		map.at(*W) = W;
		assert(*K == t);
		map.erase(*K);
		assert(vec.back() == t);
		vec.pop_back();
		return true;
	}
	bool empty() const
	{
		assert(map.size() == vec.size());
		return vec.empty();
	}
	void clear()
	{
		map.clear();
		vec.clear();
	}
	size_type count(const T t) const
	{
		return map.count(t);
	}
	size_type size() const
	{
		assert(map.size() == vec.size());
		return vec.size();
	}
	const_iterator begin() const
	{
		return vec.begin();
	}
	const_iterator end() const
	{
		return vec.end();
	}
	iterator begin()
	{
		return vec.begin();
	}
	iterator end()
	{
		return vec.end();
	}
private:
	void normalize()
	{
		iterator i = begin();
		iterator e = end();
		while (i != e)
		{
			map[*i] = i;
			++i;
		}
	}
	std::deque<T> vec;
	std::unordered_map<T, iterator> map;
};

}

#endif

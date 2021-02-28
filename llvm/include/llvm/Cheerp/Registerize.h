//===-- Cheerp/Registerize.h - Cheerp utility code ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2019 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#ifndef _CHEERP_REGISTERIZE_H
#define _CHEERP_REGISTERIZE_H

#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/Support/Debug.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/ADT/BitVector.h"
#include "llvm/ADT/IntEqClasses.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/DeterministicUnorderedMap.h"
#include "llvm/Cheerp/EdgeContext.h"
#include <array>
#include <set>
#include <unordered_map>
#include <vector>
#include <deque>
#include <queue>

//#define REGISTERIZE_STATS
//#define REGISTERIZE_DEBUG
//#define REGISTERIZE_DEBUG_EXAUSTIVE_SEARCH

#define MAXIMAL_NUMBER_OF_ITERATIONS_VERTEX_COLORER 100

namespace cheerp
{
/**
 * VertexColorer - Given an undirect graph, color the vertex in a (close to) optimal way as to have neighbours always of different color
 * Optimal means a combinations of 2 metrics: using the smaller number of colors as possible, and breaking as few "soft" constraints as possible
 *
 * There are two main ways in which the problem is solved, either reducing the problem to a smaller/simpler one or do a search of the solution space, looking for the best solution.
 *
 *	1. Given a problem, try to find a redution to a subproblem or to multiple simpler subproblems
 *		1a. If the problem is unconnected, solve every connected component separately
 *		1b. If the problem have many constraints, such that the inverted graph becomes disconnected, solve the components separately
 *		1c. If the problem has an articulation point, split the problem there
 *		1d. Same thing as 1c, but with a clique
 *		1e. If there are nodes with few constraints, they can be removed and post-processed
 *		1f. If there are dominated nodes, they can be merged to their dominator
 *		1g. If the problem is represented by two cliques, split it and assign friendships optimally
 *
 *      All this redunctions guarantee optimality, so they can be safely applied withoud losing any information
 *
 *	2. Search for a good solution
 *		Do a DFS with iterative deepening while pruning as soon as possible.
 *		For pruning, we check that we will not be able to beat the current best solution,
 *			comparing it against lower bound on chromatic number * C + sum weighted friendships already broken
 *		If you happen to visit all leaf nodes, it's guaranteed you have found a optimal solution,
 *		otherwise there may be better solution to be found (teoretically, but in practice you have already a close to optimal solution).
 *		For generating the current solutions (needed both for pruning and for having something to show for when the tree is too deep to be fully visited)
 *		a greedy strategy is used:
 *		- as long as there are positive weight friendships: pick the heavier satisfayable friendships and satisfy it
 *		- then, group greedily the remaining colors, trying to use as fewer as possible
 *
 *	The computational complexity for most calls in this class is O(N^2), basically checking constraints matrix. Exeptions:
 *		-maximalGeneratedClique() that is O(N^2 * number of blocks)
 *		-removeDominatedRows that is O(N^3), but it's called as few times as possible and there is a sort of hashing mechanism to speed up the computations
 *	As a consequence, determining a full solution is O(N^3) in degenerated cases, but closer to O(N^2) in usual cases.
 */
class VertexColorer
{
	typedef std::vector<uint32_t> Coloring;
	typedef std::pair<uint32_t, std::pair<uint32_t, uint32_t>> Friendship;
	struct Link
	{
		Link(const uint32_t a, const uint32_t b)
			: Link(0, a, b)
		{
		}
		Link(const uint32_t weight, const uint32_t a, const uint32_t b)
			: weight(weight), first(a), second(b)
		{
		}
		uint32_t weight;
		uint32_t first;
		uint32_t second;
	};
	typedef std::pair<uint32_t, uint32_t> Friend;
public:
	VertexColorer(const uint32_t N, const uint32_t costPerColor, const uint32_t maximalNumberExploredLeafs, const uint32_t costPerPhiEdge = 3)
		: N(N), costPerColor(costPerColor), costPerPhiEdge(costPerPhiEdge), parent(N), constraints(N, llvm::BitVector(N)), friends(N), depthRecursion(0)
	{
		for (uint32_t i=0; i<N; i++)
		{
			parent[i] = i;
			constraints[i].reset(i);
		}
		times = maximalNumberExploredLeafs;
		lowerBoundChromaticNumber = ((N==0)?0:1);
		howManyWaysHasLowerBoundBeenEvaluated = 0;
		isOptimal = true;
	}
	void dump() const
	{
		for (uint32_t i=0; i<N; i++)
		{
			llvm::errs() << "| | ";
			for (uint32_t j=0; j<N; j++)
			{
				bool isFriend = false;
				uint32_t C = 0;
				for (const Friend& f : friends[i])
				{
					if (f.first == j)
					{
						isFriend = true;
						C++;
					}
				}
				assert(C<2);
				if (isFriend)
					llvm::errs() << "x";
				else if (constraints[i][j])
					llvm::errs() << "1";
				else
					llvm::errs() << ".";
			}
			llvm::errs()<<"\n";
		}
		llvm::errs() << "| | 1 = constraints |  x = friendships | . = nothing\n| L"<<std::string(40, '-')<<"\n";
	}
	const Coloring& getSolution() const
	{
		return retColors;
	}
	void solve();
	void addWeightedFriendship(const uint32_t a, const uint32_t b, const uint32_t weight)
	{
		addFriendship(weight, a, b);
	}
	void addAllowed(const uint32_t a, const uint32_t b)
	{
		addFriendship(0, a, b);
	}
	void addNewEdge()
	{
		groupedLinks.push_back(std::vector<Link>());
	}
	void popLastEdge()
	{
		groupedLinks.pop_back();
	}
	void addOnEdge(const uint32_t a, const uint32_t b)
	{
		if (a != b && !constraints[a][b])
			groupedLinks.back().push_back(Link(3, a, b));
	}
	void addConstraint(const uint32_t a, const uint32_t b)
	{
		assert(a < N && b < N);
		if (a == b)
			return;
		constraints[a].set(b);
		constraints[b].set(a);
	}
	class IterationsCounter
	{
	public:
		IterationsCounter(const uint32_t maximal)
			: maxNumber(maximal), currNumber(0)
		{
		}
		void consumeIteration()
		{
			++currNumber;
		}
		void consumeIterations(const uint32_t X)
		{
			currNumber += X;
		}
		uint32_t evaluationsDone() const
		{
			assert(currNumber <= maxNumber);
			return currNumber;
		}
		uint32_t remaining() const
		{
			assert(currNumber <= maxNumber);
			return maxNumber - currNumber;
		}
	private:
		const uint32_t maxNumber;
		uint32_t currNumber;
	};
	static bool hasAnythingBeenMerged(const Coloring& coloring)
	{
		if (coloring.empty())
			return false;
		return computeNumberOfColors(coloring) != coloring.size();
	}
	void setAll(const bool conflicting)
	{
		for (uint32_t i=0; i<N; i++)
		{
			if (conflicting)
				constraints[i].set();
			else
				constraints[i].reset();
		}
		for (uint32_t i=0; i<N; i++)
		{
			constraints[i].reset(i);
		}
	}
	VertexColorer(const uint32_t N, const VertexColorer& parent)
		: VertexColorer(N, parent.costPerColor, parent.times, parent.costPerPhiEdge)
	{
		depthRecursion = parent.depthRecursion + 1;
	}
private:
	template <bool constraints, bool zeroWeight, bool positiveWeight, bool singleRow>
	class ConstFriendIterator
	{
	public:
		ConstFriendIterator(const VertexColorer& instance_, bool isBegin = true, uint32_t startingIndex = 0)
			: instance(instance_), currentLink(instance.N, instance.N)
		{
			static_assert(constraints || zeroWeight || positiveWeight, "You should iterate over something");
			assert(singleRow || startingIndex == 0);
			if (!isBegin)
			{
				i = instance.N;
			}
			else
			{
				i = startingIndex;
				while (!isTheEnd())
				{
					setToStartOfRow();
					setToFirstValidInRow();
					if (!isTheEndLocal())
						break;
					goToNextRow();
				}
			}
		}
		const Link& operator*()
		{
			assert(!isTheEnd());
			assert(!isTheEndLocal());
			if (!positiveWeight)
				setWeight();
			else if (onlyPositive() || (!isTheEndPositive() && j>=it->first))
			{
				if (onlyPositive())
				{
					j = it->first;
				}
				assert(j == it->first);
				setWeight(it->second);
			}
			else
				setWeight();
			return currentLink;
		}
		void dump() const
		{
			llvm::errs() << i << "/" << instance.N << "\t";
			if (!isTheEnd())
			{
				if (positiveWeight)
				{
					if (isTheEndPositive())
						llvm::errs() << "end";
					else
						llvm::errs() << it->first;
					llvm::errs() << "-" << instance.friends[i].size()<<"\t";
				}
				else
					llvm::errs() << "\t\t";
				if (zeroWeight)
					llvm::errs() << j;
				llvm::errs() << "\t";
			}
			llvm::errs() << "\n";
		}
		ConstFriendIterator& operator++()
		{
			assert(!isTheEnd());
			assert(!isTheEndLocal());
			if (onlyPositive())
				++it;
			else
			{
				if (positiveWeight && !isTheEndPositive() && j >= it->first)
					++it;
				++j;
				setToFirstValidInRow();
			}
			while (isTheEndLocal())
			{
				goToNextRow();
				if (isTheEnd())
					break;
				setToStartOfRow();
				setToFirstValidInRow();
			}
			return *this;
		}
		bool operator!=(const ConstFriendIterator& other) const
		{
			if (isTheEnd() && other.isTheEnd())
				return false;
			if (isTheEnd() || other.isTheEnd())
				return true;
			if (positiveWeight && it != other.it)
				return false;
			if (onlyPositive())
				return true;
			return j != other.j;
		}
	private:
		void setWeight(uint32_t w = 0)
		{
			currentLink.weight = w;
		}
		void goToNextRow()
		{
			if (singleRow)
				i = instance.N;
			else
				++i;
		}
		constexpr bool onlyPositive() const
		{
			return positiveWeight && !constraints && !zeroWeight;
		}
		void setToStartOfRow()
		{
			if (positiveWeight)
				it = instance.friends[i].begin();
			if (!onlyPositive())
				j = 0;
		}
		uint32_t hardLimitOnJ() const
		{
			if (singleRow)
				return instance.N;
			else
				return i;
		}
		uint32_t limitOnJ() const
		{
			if (positiveWeight && !isTheEndPositive())
				return std::min(hardLimitOnJ(), it->first);
			else
				return hardLimitOnJ();
		}
		bool isTheEndPositive() const
		{
			if (it == instance.friends[i].end())
				return true;
			return it->first >= i;
		}
		void setToFirstValidInRow()
		{
			if (onlyPositive())
				return;
			if (constraints && zeroWeight)
				return;
			const uint32_t limit = limitOnJ();
			if (constraints)
			{
				if (!instance.constraints[i][j])
					j = std::min(limit, (uint32_t)(instance.constraints[i].find_next(j)));
				return;
			}
			while (j < limit)
			{
				if (!instance.constraints[i][j] && zeroWeight)
					break;
				++j;
			}
		}
		bool isTheEnd() const
		{
			return i >= instance.friends.size();
		}
		bool isTheEndLocal() const
		{
			assert(!isTheEnd());
			if (onlyPositive())
				return isTheEndPositive();
			return j >= hardLimitOnJ();
		}
		uint32_t& i = currentLink.second;
		uint32_t& j = currentLink.first;
		std::vector<Friend>::const_iterator it;
		const VertexColorer& instance;
		Link currentLink;
	};
	template <bool constraints, bool zeroWeight, bool positiveWeight>
	class ConstGenericFriendshipIterator
	{
		typedef ConstFriendIterator<constraints, zeroWeight, positiveWeight, /*singleRow*/false> Iterator;
	public:
		ConstGenericFriendshipIterator(const VertexColorer& instance_)
			: instance(instance_)
		{
		}
		Iterator begin() const
		{
			return Iterator(instance, /*isBegin*/true);
		}
		Iterator end() const
		{
			return Iterator(instance, /*isBegin*/false);
		}
	private:
		const VertexColorer& instance;
	};
	template <bool constraints, bool zeroWeight, bool positiveWeight>
	class ConstGenericFriendsIterator
	{
		typedef ConstFriendIterator<constraints, zeroWeight, positiveWeight, /*singleRow*/true> Iterator;
	public:
		ConstGenericFriendsIterator(const VertexColorer& instance_, uint32_t row)
			: instance(instance_), row(row)
		{
		}
		Iterator begin() const
		{
			return Iterator(instance, /*isBegin*/true, row);
		}
		Iterator end() const
		{
			return Iterator(instance, /*isBegin*/false, row);
		}
	private:
		const VertexColorer& instance;
		const uint32_t row;
	};

	//Friendships Iterable
	ConstGenericFriendshipIterator</*constraints*/false,/*zeroWeight*/false,/*positiveWeight*/true> positiveWeightFriendshipIterable() const
	{
		return ConstGenericFriendshipIterator</*constraints*/false,/*zeroWeight*/false,/*positiveWeight*/true>(*this);
	}
	ConstGenericFriendshipIterator</*constraints*/false,/*zeroWeight*/true,/*positiveWeight*/false> zeroWeightFriendshipIterable() const
	{
		return ConstGenericFriendshipIterator</*constraints*/false,/*zeroWeight*/true,/*positiveWeight*/false>(*this);
	}
	ConstGenericFriendshipIterator</*constraints*/true,/*zeroWeight*/false,/*positiveWeight*/true> constraintOrFriendshipIterable() const
	{
		return ConstGenericFriendshipIterator</*constraints*/true,/*zeroWeight*/false,/*positiveWeight*/true>(*this);
	}
	ConstGenericFriendshipIterator</*constraints*/true,/*zeroWeight*/false,/*positiveWeight*/false> constraintIterable() const
	{
		return ConstGenericFriendshipIterator</*constraints*/true,/*zeroWeight*/false,/*positiveWeight*/false>(*this);
	}
	ConstGenericFriendshipIterator</*constraints*/false,/*zeroWeight*/true,/*positiveWeight*/true> allFriendshipIterable() const
	{
		return ConstGenericFriendshipIterator</*constraints*/false,/*zeroWeight*/true,/*positiveWeight*/true>(*this);
	}

	//Friends Iterable
	ConstGenericFriendsIterator</*constraints*/false,/*zeroWeight*/false,/*positiveWeight*/true> positiveWeightFriendsIterable(const uint32_t row) const
	{
		return ConstGenericFriendsIterator</*constraints*/false,/*zeroWeight*/false,/*positiveWeight*/true>(*this, row);
	}
	ConstGenericFriendsIterator</*constraints*/false,/*zeroWeight*/true,/*positiveWeight*/false> zeroWeightFriendsIterable(const uint32_t row) const
	{
		return ConstGenericFriendsIterator</*constraints*/false,/*zeroWeight*/true,/*positiveWeight*/false>(*this, row);
	}
	ConstGenericFriendsIterator</*constraints*/true,/*zeroWeight*/false,/*positiveWeight*/true> constraintOrFriendsIterable(const uint32_t row) const
	{
		return ConstGenericFriendsIterator</*constraints*/true,/*zeroWeight*/false,/*positiveWeight*/true>(*this, row);
	}
	ConstGenericFriendsIterator</*constraints*/false,/*zeroWeight*/true,/*positiveWeight*/true> allFriendsIterable(const uint32_t row) const
	{
		return ConstGenericFriendsIterator</*constraints*/false,/*zeroWeight*/true,/*positiveWeight*/true>(*this, row);
	}
	std::vector<uint32_t> findAlreadyDiagonalized() const;
	void solveInvariantsAlreadySet();
	static uint32_t computeNumberOfColors(const Coloring& coloring)
	{
		if (coloring.empty())
			return 0;
		uint32_t res = 0;
		for (uint32_t c : coloring)
		{
			if (res < c)
				res = c;
		}
		return res+1;
	}
	void establishInvariantsGroupedLinks()
	{
		for (std::vector<Link> & V : groupedLinks)
		{
			for (Link& link : V)
			{
				if (link.first > link.second)
					std::swap(link.first, link.second);
			}
			llvm::sort(V.begin(), V.end(), [](const Link& a, const Link& b) -> bool
					{
						if (a.first != b.first)
							return (a.first < b.first);
						return a.second < b.second;
					}
					);
			for (uint32_t i=0; i<V.size();)
			{
				if (constraints[V[i].first][V[i].second])
				{
					assert(false);
					V[i].weight = 0;
					++i;
					continue;
				}
				uint32_t j=i+1;
				while (j<V.size() && V[j].first == V[i].first  && V[j].second == V[i].second)
				{
					V[j].weight = 0;
					j++;
				}
				i = j;
			}
			V.erase(std::remove_if(V.begin(), V.end(), [](const Link& link) -> bool
					{
						return link.weight == 0;
					}
					), V.end());
		}
		groupedLinks.erase(std::remove_if(groupedLinks.begin(), groupedLinks.end(), [&](const std::vector<Link>& linkVector) -> bool
				{
					if (linkVector.size() == 1)
						addFriendship(linkVector.front().weight, linkVector.front().first, linkVector.front().second);
					return linkVector.size() <= 1;
				}
				), groupedLinks.end());

		areGroupedStillGood.resize(groupedLinks.size(), 0);
	}
	void establishInvariants();
	void establishInvariantsFriendships();
	void establishInvariantsFriends();
	static void establishInvariantsFriend(std::vector<Friend>& singleRowFriends);
	void addFriendship(const uint32_t weight, const uint32_t a, const uint32_t b)
	{
		assert(a < N && b < N);
		if (a == b)
			return;
		constraints[a].reset(b);
		constraints[b].reset(a);
		if (weight > 0)
		{
			friends[a].push_back({b, weight});
			friends[b].push_back({a, weight});
		}
	}
	void buildOrderedLinks();
	void buildFriendships()
	{
		if (!friendships.empty())
			return;
		for (const Link& link : allFriendshipIterable())
		{
			friendships.push_back(std::make_pair(link.weight, std::make_pair(link.first, link.second)));
		}
	}
	typedef std::pair<uint32_t, Coloring> Solution;
	struct SearchState
	{
		SearchState(Solution& best, uint32_t minimalNumberOfColors, uint32_t nodesToEvaluate, const uint32_t targetDepth,
				const uint32_t alreadyProcessedDepth, const uint32_t numWeightedFriendships, const uint32_t costPerColor)
			: currentBest(best), minimalNumberOfColors(minimalNumberOfColors),
			iterationsCounter(nodesToEvaluate),
			targetDepth(targetDepth), processedDepth(alreadyProcessedDepth), numWeightedFriendships(numWeightedFriendships), costPerColor(costPerColor)
		{
			processedFriendships = 0;
			processedLinks = 0;
			leafs = 0;
			choicesMade = llvm::BitVector(0);
			currentScore = 0;
		}
		Solution& currentBest;
		uint32_t minimalNumberOfColors;
		IterationsCounter iterationsCounter;
		const uint32_t targetDepth;
		const uint32_t processedDepth;
		const uint32_t numWeightedFriendships;
		uint32_t processedFriendships;
		uint32_t processedLinks;
		uint32_t leafs;
		uint32_t currentScore;
		llvm::BitVector choicesMade;
#ifdef REGISTERIZE_DEBUG
		std::array<uint32_t, 4> debugStats{};
#endif
		const uint32_t costPerColor;
		bool improveScore(const Solution& local)
		{
			if (couldImproveScore(local.first))
			{
				currentBest = local;
				return true;
			}
			return false;
		}
		bool couldImproveScore(const uint32_t score) const
		{
			return currentBest.second.size() == 0 || score < currentBest.first;
		}
		bool couldCurrentImproveScore(const uint32_t score) const
		{
			return currentBest.second.size() == 0 || currentScore + score + costPerColor*minimalNumberOfColors < currentBest.first;
		}
		bool shouldBeEvaluated() const
		{
			return processedFriendships == targetDepth;
		}
		bool isEvaluationAlreadyDone() const
		{
			if (choicesMade.empty())
				return false;
			assert(targetDepth > 0);
			assert(targetDepth == choicesMade.size());

			if (targetDepth >= numWeightedFriendships)
				return false;

			//Here we use the fact that if all previous choices (up to processedDepth) are "merge two registers",
			//we already have tried this path (while evaluating at processedDepth) and we can now skip it
			for (uint32_t k = targetDepth; ; )
			{
				k--;
				if (!choicesMade[k])
					return false;
				if (k == processedDepth)
					break;
			}
			return true;
		}
		int leafsEvaluated() const
		{
			return leafs;
		}
		void printChoicesMade() const
		{
			if (choicesMade.empty())
			{
				llvm::errs() << "none";
			}
			for (uint32_t i=0; i<choicesMade.size(); i++)
			{
				llvm::errs() << (choicesMade[i]?"1":"0");
			}
		}
	};
	std::vector<uint32_t> keepMerging(SearchState& state);
	void DFSwithLimitedDepth(SearchState& state);
	bool areMergeable(const uint32_t a, const uint32_t b) const
	{
		return !constraints[a][b];
	}
	void doContraction(const uint32_t a, const uint32_t b)
	{
		assert(isAlive(a) && isAlive(b));
		parent[b] =a;
		assert(constraints[a][b] == constraints[b][a] && !constraints[a][b]);
		constraints.push_back(constraints[a]);
#ifdef REGISTERIZE_DEBUG
		debugStats[CONTRACTIONS]++;
#endif
		constraints[a] |= constraints[b];
		for (uint32_t i = 0; i<N; i++)
		{
			if (!isAlive(i))
				continue;
			constraints[i][a] = constraints[a][i];
		}
	}
	void undoContraction(const uint32_t a, const uint32_t b)
	{
		constraints[a] = constraints.back();
		constraints.pop_back();
		for (uint32_t i = 0; i<N; i++)
		{
			if (!isAlive(i))
				continue;
			constraints[i][a] = constraints[a][i];
		}
		parent[b] = b;
	}
	void setAdditionalConstraint(const uint32_t a, const uint32_t b, bool direct)
	{
#ifdef REGISTERIZE_DEBUG
		if (direct)
			debugStats[SEPARATIONS]++;
#endif
		assert(constraints[a][b] == constraints[b][a] && constraints[b][a] != direct);
		constraints[a].flip(b);
		constraints[b].flip(a);
	}
	void iterativeDeepening(IterationsCounter& counter);
	std::vector<uint32_t> assignGreedily() const;
	uint32_t computeScore(const Coloring& coloring, const uint32_t lowerBound = 0) const
	{
		assert(coloring.size() == N);
		uint32_t res = std::max(computeNumberOfColors(coloring), lowerBound) * costPerColor;
		for (const auto& p : friendships)
		{
			if (coloring[p.second.first] != coloring[p.second.second])
				res += p.first;
		}
		for (const auto& edge : groupedLinks)
		{
			for (const auto& E : edge)
			{
				if (coloring[E.first] != coloring[E.second])
				{
					res += costPerPhiEdge;
					break;
				}
			}
		}
		return res;
	}
	Coloring getColors(const std::vector<uint32_t>& P) const
	{
		Coloring colors(N, N);
		uint32_t firstUnused = 0;
		for (uint32_t i=0; i<N; i++)
		{
			if (P[i] == i)
				colors[i] = firstUnused++;
		}
		for (uint32_t i=0; i<N; i++)
		{
			if (colors[i] < N)
				continue;
			std::vector<uint32_t> V;
			uint32_t x = i;
			while (P[x] != x)
			{
				V.push_back(x);
				x = P[x];
			}
			for (uint32_t v : V)
			{
				colors[v] = colors[x];
			}
		}
		return colors;
	}
	struct HopcroftTarjanData
	{
		HopcroftTarjanData(const VertexColorer& subsolution)
			: sol(subsolution), visited(sol.N, false), numChildren(sol.N, 0), depth(sol.N, 0), low(sol.N, 0), isArticulation(sol.N, false), parent(sol.N, sol.N)
		{
		}
		const VertexColorer& sol;
		llvm::BitVector visited;
		std::vector<uint32_t> numChildren;
		std::vector<uint32_t> depth;
		std::vector<uint32_t> low;
		llvm::BitVector isArticulation;
		std::vector<uint32_t> parent;
		void visit(const uint32_t i, const uint32_t d);
		void processChildren(const uint32_t i, const uint32_t j, const uint32_t d);
	};
	std::vector<uint32_t> getArticulationPoints() const;
	void floodFillOnBitsWithArticulationPoints(llvm::BitVector& region, const uint32_t start, const bool conflicting, const llvm::BitVector& isArticulationPoint) const;
	void floodFillOnBits(llvm::BitVector& region, const uint32_t start, const bool conflicting) const;
	bool isDominatingFriend(const uint32_t a, const uint32_t b) const;
	bool removeRowsWithFewConstraints();
	bool canBeAddedToClique(const uint32_t index, const llvm::BitVector& unionConstraint, const llvm::BitVector& used) const;
	void addToClique(const uint32_t index, llvm::BitVector& unionConstraint, llvm::BitVector& used) const;
	void improveLowerBound(const uint32_t x);
	uint32_t chromaticNumberWithNoFriends(uint32_t lowerBound, uint32_t minimalColors) const;
	uint32_t maximalGeneratedClique() const;
	uint32_t lowerBoundOnNumberOfColors(const bool forceEvaluation = false);
	bool checkConstraintsAreRespected(const Coloring& colors) const;
	bool friendshipsInvariantsHolds() const;
	bool friendInvariantsHolds() const;
	uint32_t findParent(const uint32_t index) const
	{
		//TODO possibly implement shortening (while it could invalidate parent, so it should be done on other data)
		if (parent[index] == index)
			return index;
		else
			return findParent(parent[index]);
	}
	bool areAllAlive() const;
	bool isAlive(const uint32_t index) const
	{
		assert(index < N);
		return parent[index] == index;
	}
	bool isSolutionOptimal() const
	{
		return isOptimal;
	}
	static void permuteFirstElements(Coloring& coloring, const uint32_t N);
	const uint32_t N;
	const uint32_t costPerColor;
	const uint32_t costPerPhiEdge;
	std::vector<uint32_t> parent;
	Coloring retColors;
	std::vector<llvm::BitVector> constraints;
	std::vector<std::vector<Link>> groupedLinks;
	std::vector<uint32_t> areGroupedStillGood;
	std::vector<std::pair<Link,uint32_t>> orderedLinks;
	std::vector<Friendship> friendships;
	std::vector<std::vector<Friend>> friends;
	uint32_t lowerBoundChromaticNumber;
	uint32_t howManyWaysHasLowerBoundBeenEvaluated;
	bool isOptimal;
public:
	enum ReductionPasses{SPLIT_CONFLICTING, SPLIT_UNCONNECTED, SPLIT_ARTICULATION, REMOVE_DOMINATED, REMOVE_SMALL, ENUMERATE_PHI_EDGES,
		REDUCTIONPASSES_FINAL_ELEMENT};
	static std::string reductionPassesNames(const uint32_t i)
	{
		switch (i) {
		case SPLIT_CONFLICTING:
			return "Split conflicting              ";
		case SPLIT_UNCONNECTED:
			return "Split unconnected              ";
		case SPLIT_ARTICULATION:
			return "Split articulation             ";
		case REMOVE_DOMINATED:
			return "Remove nodes dominated         ";
		case REMOVE_SMALL:
			return "Remove nodes with few conflicts";
		case ENUMERATE_PHI_EDGES:
			return "Enumerate phi edges            ";
		default:
			return "Unknown reduction              ";
		};
	}
private:
	std::array<bool, REDUCTIONPASSES_FINAL_ELEMENT> avoidPass{};
	uint32_t depthRecursion{};
	template <typename T> friend class Reduction;
	friend class EnumerateAllPhiEdges;
	friend class RemoveFewConstraints;
	friend class RemoveDominated;
	friend class SplitArticulation;
	template <typename T> friend class SplitConflictingBase;
	friend class SplitUnconnected;
	friend class SplitInverseUnconnected;
public:
#ifdef REGISTERIZE_DEBUG
	enum PrintStatistics{GREEDY_EVALUATIONS=0, NODE_VISITED=1, CONTRACTIONS=2, SEPARATIONS=3};
	std::array<uint32_t, 4> debugStats{};
#endif
	uint32_t times{};
};

//Base class accordingly to Curiously Recurring Template Paramether pattern
template<typename Derived>
class Reduction
{
public:
	std::string reductionName() const
	{
		return VertexColorer::reductionPassesNames(getDerived().id());
	}
	bool couldBeAvoided() const
	{
		return instance.avoidPass[getDerived().id()];
	}
	bool perform();
	void dumpDescription() const
	{
		llvm::errs() <<"|"<<std::string(instance.depthRecursion, ' ')<< reductionName() << "   ";
		getDerived().dumpSpecificDescription();
		llvm::errs() << "\n";
	}
	VertexColorer& instance;
	std::vector<uint32_t> computeLeaders(llvm::IntEqClasses& eqClasses, const uint32_t N) const;
	std::vector<uint32_t> computeLeaders(llvm::IntEqClasses& eqClasses) const;
	void assignIndexes(const std::vector<uint32_t>& whichSubproblems, std::vector<uint32_t>& numerositySubproblem, std::vector<uint32_t>& newIndexes, const uint32_t startingFrom=0);
	//Constructor is private, and only accessible from the Derived class
	friend Derived;
private:
	Reduction(VertexColorer& instance)
		: instance(instance)
	{}
	Derived& getDerived()
	{
		return static_cast<Derived&>(*this);
	}
	const Derived& getDerived() const
	{
		return static_cast<const Derived&>(*this);
	}
};

class EnumerateAllPhiEdges : public Reduction<EnumerateAllPhiEdges>
{
public:
	EnumerateAllPhiEdges(VertexColorer& instance)
		: Reduction(instance)
	{}
	bool couldBePerformed();
	bool couldBePerformedPhiEdges();
	void relabelNodes();
	void reduce();
	void dumpSpecificDescription() const;
	void preprocessing(VertexColorer& subsolution) const;
	void postprocessing(VertexColorer& subsolution);
	void buildSubproblems();
	uint32_t id() const
	{
		return VertexColorer::ENUMERATE_PHI_EDGES;
	}
private:
	std::vector<uint32_t> newIndex;
	std::deque<VertexColorer> subproblems;
	llvm::IntEqClasses eqClasses;
	bool goodIsValid{};
};

class RemoveFewConstraints : public Reduction<RemoveFewConstraints>
{
public:
	RemoveFewConstraints(VertexColorer& instance)
		: Reduction(instance), toBePostProcessed(instance.N, false), newIndex(instance.N, 0)
	{}
	bool couldBePerformed();
	bool couldBePerformedPhiEdges();
	void relabelNodes();
	void reduce();
	void dumpSpecificDescription() const;
	void preprocessing(VertexColorer& subsolution) const;
	void postprocessing(VertexColorer& subsolution);
	void buildSubproblems();
	uint32_t id() const
	{
		return VertexColorer::REMOVE_SMALL;
	}
private:
	uint32_t howManyOnCutoff{};
	std::vector<bool> toBePostProcessed;
	std::vector<uint32_t> alive;
	std::vector<uint32_t> newIndex;
	std::deque<VertexColorer> subproblems;
};

class RemoveDominated : public Reduction<RemoveDominated>
{
public:
	RemoveDominated(VertexColorer& instance)
		: Reduction(instance), isNodeAlive(instance.N, true), newIndex(instance.N, 0)
	{}
	bool couldBePerformed();
	bool couldBePerformedPhiEdges();
	void relabelNodes();
	void reduce();
	void dumpSpecificDescription() const;
	void preprocessing(VertexColorer& subsolution) const;
	void postprocessing(VertexColorer& subsolution);
	void buildSubproblems();
	uint32_t id() const
	{
		return VertexColorer::REMOVE_DOMINATED;
	}
private:
	static uint64_t computeSample(const llvm::BitVector&A)
	{
		uint64_t res = 0;

		uint32_t spacing = std::max(A.size() / 64, uint32_t(1));

		for (uint32_t i=0; i<A.size(); i+=spacing)
		{
			res = 2*res + (A[i]?1:0);
		}
		return res;
	}
	static bool isSubset(const llvm::BitVector& A, const llvm::BitVector& B)
	{
		assert(A.size() == B.size());
		for (uint32_t i = 0; i<A.size(); i++)
		{
			if (A[i] && !B[i])
				return false;
		}
		return true;
	}
	static bool isSubset(const uint64_t A, const uint64_t B)
	{
		return A == (A&B);
	}
	bool isAlive(const uint32_t a) const;
	bool phiEdgesWouldBeBroken(const uint32_t dominator, const uint32_t dominated);
	bool mergeIfDominated(const uint32_t dominator, const uint32_t dominated);
	std::vector<uint32_t> whoIsDominatingFriend(const uint32_t a) const;
	struct SamplesData
	{
		std::vector<uint64_t> samples;
		std::vector<std::pair<uint64_t, std::vector<uint32_t>>> bucketsSameSample;
	};
	SamplesData precomputeSamples() const;
	llvm::IntEqClasses eqClasses;
	llvm::BitVector isNodeAlive;
	std::vector<uint32_t> newIndex;
	std::deque<VertexColorer> subproblems;
};

class SplitArticulation : public Reduction<SplitArticulation>
{
public:
	SplitArticulation(VertexColorer& instance, const bool limitSize)
		: Reduction(instance), limitSize(limitSize), blockNumber(limitSize ?
										instance.parent :
										instance.findAlreadyDiagonalized()),
		blocks(blockNumber.size() + 1, instance),
		newIndex(instance.N, 0), whichSubproblem(instance.N, 0), numerositySubproblem(instance.N, 0)
	{}
	const bool limitSize;
	bool couldBePerformed();
	bool couldBePerformedPhiEdges();
	void relabelNodes();
	void reduce();
	void dumpSubproblems() const;
	void dumpSpecificDescription() const;
	void preprocessing(VertexColorer& subsolution) const;
	void postprocessing(VertexColorer& subsolution);
	void buildSubproblems();
	uint32_t id() const
	{
		return VertexColorer::SPLIT_ARTICULATION;
	}
private:
	const std::vector<uint32_t> blockNumber;
	VertexColorer blocks;
	std::vector<uint32_t> start, end;
	llvm::IntEqClasses eqClasses;
	std::vector<uint32_t> newIndex;
	std::vector<uint32_t> whichSubproblem;
	std::vector<uint32_t> numerositySubproblem;
	std::vector<uint32_t> articulations;
	std::deque<VertexColorer> subproblems;
};

template<typename Derived>
class SplitConflictingBase : public Reduction<SplitConflictingBase<Derived>>
{
public:
	bool couldBePerformed();
	bool couldBePerformedPhiEdges();
	void reduce();
	void relabelNodes();
	void dumpSubproblems() const;
	void dumpSpecificDescription() const;
	void buildSubproblems();
	uint32_t id() const
	{
		return getDerived().id();
	}
private:
	friend Derived;
	SplitConflictingBase(VertexColorer& instance, const bool conflicting)
		: Reduction<SplitConflictingBase<Derived>>(instance), conflicting(conflicting), newIndex(instance.N, 0), whichSubproblem(instance.N, 0), numerositySubproblem(instance.N, 0)
	{}
	Derived& getDerived()
	{
		return *static_cast<Derived*>(this);
	}
	const Derived& getDerived() const
	{
		return *static_cast<const Derived*>(this);
	}
	const bool conflicting;
	llvm::IntEqClasses eqClasses;
	std::vector<uint32_t> newIndex;
	std::vector<uint32_t> whichSubproblem;
	std::vector<uint32_t> numerositySubproblem;
	std::deque<VertexColorer> subproblems;
};

class SplitUnconnected : public SplitConflictingBase<SplitUnconnected>
{
public:
	SplitUnconnected(VertexColorer& instance)
		: SplitConflictingBase(instance, /*conflicting*/false), sumColors(0)
	{}
	void preprocessing(VertexColorer& subsolution) const;
	void postprocessing(VertexColorer& subsolution);
	uint32_t id() const
	{
		return VertexColorer::SPLIT_UNCONNECTED;
	}
private:
	uint32_t sumColors;
};

class SplitInverseUnconnected : public SplitConflictingBase<SplitInverseUnconnected>
{
public:
	SplitInverseUnconnected(VertexColorer& instance)
		: SplitConflictingBase(instance, /*conflicting*/true)
	{}
	void preprocessing(VertexColorer& subsolution) const;
	void postprocessing(VertexColorer& subsolution);
	uint32_t id() const
	{
		return VertexColorer::SPLIT_CONFLICTING;
	}
};

#ifdef REGISTERIZE_STATS
template<uint32_t N>
class StatisticCollector
{
public:
	void add(const uint32_t n)
	{
		totalNumber++;
		if (n > biggest)
			biggest = n;
		uint32_t log = 0;
		uint32_t high = 1;
		while (n >= high)
		{
			++log;
			high *= 2;
		}
		assert(log < N);
		data[log]++;
	}
	void report(const std::string& name) const
	{
		if (totalNumber == 0)
			return;
		llvm::errs() << "-----   " << name << "   -----";
		llvm::errs() << totalNumber << " samples collected with " << biggest << " as the biggest\nBreakdown in buckets: ";
		bool isFirst = true;
		uint32_t low = 0;
		uint32_t high = 1;
		for (uint32_t i=0; i<N; i++)
		{
			if (data[i])
			{
				if (!isFirst)
					llvm::errs() << ", ";
				if (low < high -1)
					llvm::errs() << low << "-";
				llvm::errs() <<high-1 << ":" << data[i];
				isFirst = false;
			}
			low = high;
			high *= 2;
		}
		llvm::errs() << "\n\n";
	}
private:
	std::array<uint32_t, N> data;
	uint32_t totalNumber{};
	uint32_t biggest{};
};
void reportRegisterizeStatistics();
#endif

/**
 * Registerize - Map not-inlineable instructions to the minimal number of local variables
 */
class Registerize : public llvm::ModulePass
{
public:
	struct LiveRangeChunk
	{
		// [start,end)
		uint32_t start;
		uint32_t end;
		LiveRangeChunk(uint32_t s, uint32_t e):start(s),end(e)
		{
		}
		bool operator<(const LiveRangeChunk& r) const
		{
			return start < r.start;
		}
		bool empty() const
		{
			return start == end;
		}
	};
	struct LiveRange: public llvm::SmallVector<LiveRangeChunk, 4>
	{
		LiveRange()
		{
		}
		template<class Iterator>
		LiveRange(const Iterator& begin, const Iterator& end):llvm::SmallVector<LiveRangeChunk, 4>(begin,end)
		{
		}
		bool doesInterfere(const LiveRange& other) const;
		bool doesInterfere(uint32_t id) const;
		void merge(const LiveRange& other);
		void dump() const;
		bool invariantsHold() const;
		uint32_t peekLast() const
		{
			return back().end;
		}
		void extendOrPush(const LiveRangeChunk& chunk)
		{
			if (empty() || peekLast() < chunk.start)
				push_back(chunk);
			else
				back().end = std::max(back().end, chunk.end);
		}
	};

	static char ID;
	
	explicit Registerize(bool froundAvailable = false, bool wasm = false) : ModulePass(ID), froundAvailable(froundAvailable), wasm(wasm)
#ifndef NDEBUG
			, RegistersAssigned(false)
#endif
	{ }
	
	void getAnalysisUsage(llvm::AnalysisUsage & AU) const override;

	bool runOnModule(llvm::Module& M) override;
	
	llvm::StringRef getPassName() const override;

	bool hasRegister(const llvm::Instruction* I) const;
	uint32_t getRegisterId(const llvm::Instruction* I, const EdgeContext& edgeContext) const;
	uint32_t getSelfRefTmpReg(const llvm::Instruction* I, const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB) const;

	void assignRegisters(llvm::Module& M, cheerp::PointerAnalyzer& PA);
	void computeLiveRangeForAllocas(const llvm::Function& F);
	void invalidateLiveRangeForAllocas(const llvm::Function& F);

	const LiveRange& getLiveRangeForAlloca(const llvm::AllocaInst* alloca) const
	{
		assert(allocaLiveRanges.count(alloca));
		return allocaLiveRanges.find(alloca)->second;
	}

	// Registers should have a consistent JS type
	enum REGISTER_KIND { OBJECT=0, INTEGER, INTEGER64, DOUBLE, FLOAT };

	struct RegisterInfo
	{
		// Try to save bits, we may need more flags here
		const REGISTER_KIND regKind : 3;
		int needsSecondaryName : 1;
		RegisterInfo(REGISTER_KIND k, bool n):regKind(k),needsSecondaryName(n)
		{
		}
	};

	const std::vector<RegisterInfo>& getRegistersForFunction(const llvm::Function* F) const
	{
		assert(registersForFunctionMap.count(F));
		return registersForFunctionMap.find(F)->second;
	}

	REGISTER_KIND getRegKindFromType(const llvm::Type*, bool asmjs) const;

	llvm::LoopInfo* LI;
	llvm::DominatorTree* DT;
	llvm::PostDominatorTree* PDT;
public:
	template <typename T>
	class Indexer
	{
		//This is a bidirectional map between T and uint32_t, it could be queried both on T or the index
		static_assert(std::is_pointer<T>::value, "Indexer currently index only pointer types");
	public:
		void insert(T t)
		{
			if (count(t))
				return;
			map[t] = size();
			vec.push_back(t);
		}
		uint32_t count(const T t) const
		{
			return map.count(t);
		}
		uint32_t size() const
		{
			return vec.size();
		}
		uint32_t id(const T t) const
		{
			assert(count(t));
			return map.at(t);
		}
		T at(const uint32_t i) const
		{
			assert(i < size());
			return vec[i];
		}
		typename std::vector<T>::iterator begin()
		{
			return vec.begin();
		}
		typename std::vector<T>::iterator end()
		{
			return vec.end();
		}
	private:
		std::vector<T> vec;
		cheerp::DeterministicUnorderedMap<T, uint32_t, RestrictionsLifted::NoErasure | RestrictionsLifted::NoDeterminism> map;
	};
private:
	// Final data structures
	struct InstOnEdge
	{
		const llvm::BasicBlock* fromBB;
		const llvm::BasicBlock* toBB;
		uint32_t registerId;
		InstOnEdge(const llvm::BasicBlock* f, const llvm::BasicBlock* t, uint32_t r):fromBB(f),toBB(t),registerId(r)
		{
		}
		bool operator==(const InstOnEdge& r) const
		{
			return fromBB==r.fromBB && toBB==r.toBB && registerId==r.registerId;
		}
		struct Hash
		{
			size_t operator()(const InstOnEdge& i) const
			{
				return std::hash<const llvm::BasicBlock*>()(i.fromBB) ^
					std::hash<const llvm::BasicBlock*>()(i.toBB) ^
					std::hash<uint32_t>()(i.registerId);
			}
		};
	};
	llvm::DenseMap<const llvm::Instruction*, uint32_t> registersMap;

	//Class that keeps track for a single instruction which register it occupies at any given point
	class RegisterUpdates
	{
	public:
		RegisterUpdates(uint32_t incomingRegister)
		{
			//Fall back option: the value of the incomingRegister
			updates.push_back({0u, incomingRegister});
		}
		void update(uint32_t assigmentIndex, uint32_t registerId)
		{
			if (updates.back().second == registerId)
				return;
			updates.push_back({assigmentIndex, registerId});
		}
		uint32_t findId(const uint32_t index) const
		{
			uint32_t i=0;
			//TODO: possibly logarithmic, but number should in any case be small
			while (i+1 < updates.size() && updates[i+1].first <= index)
			{
				i++;
			}
			return updates[i].second;
		}
		void dump() const
		{
			for (auto& pair : updates)
			{
				llvm::errs() << pair.first << "->" << pair.second <<"\t";
			}
			llvm::errs() << "\n";
		}
	private:
		std::vector<std::pair<uint32_t,uint32_t>> updates;
	};

	//Class that keeps track for all Istructions which registers they occupy at any given point (in phi-edges, temporary registers may be required)
	class EdgeRegistersMap
	{
	public:
		uint32_t findCurrentRegisterId(uint32_t originalId, const EdgeContext& edgeContext) const
		{
			auto it=edgeRegistersMap.find(buildInstOnEdge(edgeContext, originalId));
			if (it!=edgeRegistersMap.end())
				return it->second.findId(edgeContext.assigmentIndex);
			return originalId;
		}
		void insertUpdate(uint32_t originalId, uint32_t nextId, const EdgeContext& edgeContext)
		{
			auto it=edgeRegistersMap.insert({buildInstOnEdge(edgeContext, originalId), originalId}).first;
			it->second.update(edgeContext.assigmentIndex, nextId);
		}
		uint32_t count(uint32_t originalId, const EdgeContext& edgeContext) const
		{
			return edgeRegistersMap.count(buildInstOnEdge(edgeContext, originalId));
		}
		void dump() const
		{
			for (auto& X : edgeRegistersMap)
			{
				llvm::errs() << "Register ID: "<< (X.first.registerId) << " with registers with this history :\t";
				X.second.dump();
				llvm::errs() << "\nOf edge between ";
				X.first.fromBB->printAsOperand(llvm::errs(), false);
				llvm::errs() << " and ";
				X.first.toBB->printAsOperand(llvm::errs(), false);
				llvm::errs() << "\n";
			}
		}
	private:
		static InstOnEdge buildInstOnEdge(const EdgeContext& edgeContext, uint32_t regId)
		{
			return InstOnEdge(edgeContext.fromBB, edgeContext.toBB, regId);
		}
		cheerp::DeterministicUnorderedMap<InstOnEdge, RegisterUpdates, RestrictionsLifted::NoErasure | RestrictionsLifted::NoDeterminism, InstOnEdge::Hash> edgeRegistersMap;
	};
	EdgeRegistersMap edgeRegistersMap;

	llvm::DenseMap<const llvm::AllocaInst*, LiveRange> allocaLiveRanges;
	llvm::DenseMap<const llvm::Function*, std::vector<RegisterInfo>> registersForFunctionMap;
	cheerp::DeterministicUnorderedMap<InstOnEdge, uint32_t, RestrictionsLifted::NoErasure | RestrictionsLifted::NoDeterminism, InstOnEdge::Hash> selfRefRegistersMap;
	const bool froundAvailable;
	const bool wasm;
#ifndef NDEBUG
	bool RegistersAssigned;
#endif
	// Temporary data structure used to compute the live range of an instruction
	struct InstructionLiveRange
	{
		// codePathId is used to efficently coalesce uses in a sequential range when possible
		uint32_t codePathId;
		LiveRange range;
		InstructionLiveRange(uint32_t c): codePathId(c)
		{
		}
		void addUse(uint32_t codePathId, uint32_t thisIndex);
	};
	// Map from instructions to their unique identifier
	typedef llvm::DenseMap<const llvm::Instruction*, uint32_t> InstIdMapTy;
	struct CompareInstructionByID
	{
	private:
		const InstIdMapTy* instIdMap;
	public:
		CompareInstructionByID(const InstIdMapTy& i):instIdMap(&i)
		{
		}
		bool operator()(const llvm::Instruction* L, const llvm::Instruction* R) const
		{
			auto l = instIdMap->find(L);
			assert(l != instIdMap->end());
			auto r = instIdMap->find(R);
			assert(r != instIdMap->end());
			return l->second < r->second;
		}
	};
	// Map from instructions to their live ranges
	typedef std::map<const llvm::Instruction*, InstructionLiveRange, CompareInstructionByID> LiveRangesTy;
	struct RegisterRange
	{
		LiveRange range;
		RegisterInfo info;
		RegisterRange(const LiveRange& range, REGISTER_KIND k, bool n):range(range),info(k, n)
		{
		}
	};
	static bool couldBeMerged(const RegisterRange& a, const RegisterRange& b);
	static bool couldBeMerged(const std::pair<const RegisterRange&, uint32_t>& a, const std::pair<const RegisterRange&, uint32_t>& b);
	static void mergeRegisterInPlace(RegisterRange& a, const RegisterRange& b);
	static RegisterRange mergeRegister(const RegisterRange& a, const RegisterRange& b);
	class FrequencyInfo
	{
	public:
		FrequencyInfo(llvm::Function& F, llvm::LoopInfo* LI)
			: LI(LI)
		{
			//TODO: possibly store the computed values to avoid recalculations
		}
		uint32_t getWeight(const llvm::BasicBlock* from, const llvm::BasicBlock* to) const;
	private:
		llvm::LoopInfo* LI;
	};
	typedef std::pair<uint32_t, uint32_t> Friend;
	typedef std::pair<uint32_t, std::pair<uint32_t, uint32_t>> Friendship;
	typedef std::vector<Friendship> Friendships;

	class RegisterAllocatorInst
	{
	public:
		RegisterAllocatorInst(llvm::Function& F_, const InstIdMapTy& instIdMap, const LiveRangesTy& liveRanges, const PointerAnalyzer& PA, Registerize* registerize);
		uint32_t getSumPointsAvailable() const;
		void solve();
		void dump()
		{
			if (emptyFunction)
				return;
			for (uint32_t i = 0; i<size(); i++)
				llvm::dbgs() << findParent(i) << "\t";
			llvm::dbgs() << "\n\n";
		}
		void materializeRegisters(llvm::SmallVectorImpl<RegisterRange>& registers)
		{
			if (emptyFunction)
				return;
			std::vector<uint32_t> indexMaterializedRegisters(size());
			//Materialize virtual registers and set the proper index
			for (uint32_t i = 0; i<size(); i++)
			{
				if (!isAlive(i))
					continue;
				indexMaterializedRegisters[i] = registers.size();
				registers.push_back(virtualRegisters[i]);
			}
			//Assign every instruction to his own materialized register
			for (uint32_t i = 0; i<indexer.size(); i++)
			{
				registerize->registersMap[indexer.at(i)] = indexMaterializedRegisters[findParent(i)];
			}
		}
	private:
		bool couldAvoidToBeMaterialized(const llvm::BasicBlock& BB) const;
		void computeBitsetConstraints()
		{
			bitsetConstraint.clear();
			for (uint32_t i = 0; i<numInst(); i++)
			{
				bitsetConstraint.push_back(llvm::BitVector(numInst()));
				if (!isAlive(i))
					continue;
				for (uint32_t j = 0; j < numInst(); j++)
				{
					if (i != j && isAlive(j))
						bitsetConstraint[i][j] = !couldBeMerged(i, j);
				}
			}
		}
		void buildEdgesData(llvm::Function& F);
		void buildFriendsSinglePhi(const uint32_t phi, const PointerAnalyzer& PA);
		void createSingleFriendship(const uint32_t i, const llvm::Value* operand);
		void buildFriendsSingleCompressibleInstr(const uint32_t i);
		void buildFriends(const PointerAnalyzer& PA)
		{
			friends.resize(numInst());
			for (uint32_t i = 0; i< numInst(); i++)
			{
				if (isAlive(i))
				{
					buildFriendsSinglePhi(i, PA);
					buildFriendsSingleCompressibleInstr(i);
				}
			}

			for (uint32_t i = 0; i<numInst(); i++)
			{
				for (const Friend& x : friends[i])
				{
					if (i < x.first)
						friendsEdges.push_back({x.second, {i, x.first}});
				}
			}
		}
		void addFriendship(const uint32_t a, const uint32_t b, const uint32_t weight)
		{
			assert(a<numInst() && b<numInst());
			if (a==b || bitsetConstraint[a][b])
				return;
			friends[a].push_back({b, weight});
			friends[b].push_back({a, weight});
		}
		uint32_t computeWeightBrokenEdges()
		{
			uint32_t res = 0;
			for (const auto& V : edges)
			{
				for (const auto& p : V)
				{
					if (findParent(p.first) != findParent(p.second))
					{
						++res;
						break;
					}
				}
			}
			return res;
		}
		uint32_t computeWeigthBrokenNeighbours()
		{
			uint32_t res = 0;
			for (uint32_t i = 0; i < numInst(); ++i)
			{
				for (const auto& p : friends[i])
				{
					if (findParent(i) != findParent(p.first))
						res += p.second;
				}
			}
			return res/2;
		}
		bool couldBeMerged(const uint32_t a, const uint32_t b) const
		{
			assert(a < size() && b < size());
			//Only unmerged registers could be merged
			assert(isAlive(a));
			assert(isAlive(b));
			if (a == b)
				return false;
			if (std::max(a, b) < numInst())
				return Registerize::couldBeMerged({virtualRegisters[a], instructionLocations[a]}, {virtualRegisters[b], instructionLocations[b]});
			return Registerize::couldBeMerged(virtualRegisters[a], virtualRegisters[b]);
		}
		void mergeVirtual(const uint32_t a, const uint32_t b)
		{
			assert(couldBeMerged(a, b));
			const uint32_t index = size();
			virtualRegisters.push_back(mergeRegister(virtualRegisters[a], virtualRegisters[b]));
			parentRegister[a] = index;
			parentRegister[b] = index;
			parentRegister.push_back(index);
		}
		uint32_t findParent(uint32_t x) const
		{
			//Path compression on the parents tree
			if (x != parentRegister[x])
			{
				parentRegister[x] = findParent(parentRegister[x]);
			}
			return parentRegister[x];
		}
		bool isAlive(uint32_t x) const
		{
			return x < size() && findParent(x) == x;
		}
		uint32_t registersNeeded()
		{
			uint32_t res = 0;
			for (uint32_t i = 0; i<size(); i++)
			{
				if (isAlive(i))
					res++;
			}
			return res;
		}
		uint32_t numInst() const
		{
			return indexer.size();
		}
		uint32_t size() const
		{
			return virtualRegisters.size();
		}
		llvm::Function& F;
		Registerize* registerize;
		const PointerAnalyzer& PA;
		Indexer<const llvm::Instruction*> indexer;
		llvm::SmallVector<RegisterRange, 4> virtualRegisters;
		llvm::SmallVector<uint32_t, 4> instructionLocations;
		std::vector<llvm::BitVector> bitsetConstraint;
		std::vector<std::vector<Friend>> friends;
		std::vector<Friendship> friendsEdges;
		std::vector<std::vector<std::pair<uint32_t,uint32_t>>> edges;
		mutable std::vector<uint32_t> parentRegister;
		bool emptyFunction;
		const FrequencyInfo frequencyInfo;
	};
	// Temporary data structures used while exploring the CFG
	struct BlockState
	{
		llvm::Instruction* inInst;
		llvm::SmallVector<llvm::Instruction*, 4> outSet;
		void addLiveOut(llvm::Instruction* I)
		{
			if(outSet.empty() || outSet.back()!=I)
				outSet.push_back(I);
		}
		void setLiveIn(llvm::Instruction* I)
		{
			inInst=I;
		}
		bool isLiveOut(llvm::Instruction* I) const
		{
			return !outSet.empty() && outSet.back()==I;
		}
		bool isLiveIn(llvm::Instruction* I) const
		{
			return inInst==I;
		}
		bool completed;
		BlockState():inInst(NULL),completed(false)
		{
		}
	};
	typedef cheerp::DeterministicUnorderedMap<llvm::BasicBlock*, BlockState, RestrictionsLifted::NoErasure | RestrictionsLifted::NoDeterminism> BlocksState;
	// Temporary data used to registerize allocas
	typedef std::vector<const llvm::AllocaInst*> AllocaSetTy;
	typedef std::map<uint32_t, uint32_t> RangeChunksTy;
	class FloodFillState
	{
	public:
		enum SegmentKind
		{
			Begin = 1, Middle = 2, End = 4
		};
	private:
		struct BasicBlockInfo
		{
			bool hasLifetimeStart = false;
			bool hasLifetimeEnd = false;
			bool visitedForward = false;
			bool visitedBackward = false;
			SegmentKind segmentsOnVisitForward = SegmentKind(0);
			SegmentKind segmentsOnVisitBackward = SegmentKind(0);
			SegmentKind visitedBothWays()
			{
				return SegmentKind(segmentsOnVisitForward & segmentsOnVisitBackward);
			}
		};
	public:
		FloodFillState(const llvm::Function& F) : F(F) {}
		void addSource(const llvm::BasicBlock* BB);
		void addSink(const llvm::BasicBlock* BB);
		void addLifetimeEnd(const llvm::BasicBlock* BB);
		void addLifetimeStart(const llvm::BasicBlock* BB);
		void processAlloca();
		std::vector<std::pair<const llvm::BasicBlock*, const SegmentKind>> aliveSegments();
	private:
		void processForward(const llvm::BasicBlock* BB);
		template <bool isComplete>
		void processBackward(const llvm::BasicBlock* BB);
		void cleanupBBInfo(BasicBlockInfo& info);
		template <bool isForwardVisit>
		void visitSegment(BasicBlockInfo& info, const SegmentKind& segmentKind);
		template <bool isForwardVisit>
		void toBeVisited(const llvm::BasicBlock* BB);
		template <bool isForwardVisit>
		std::queue<const llvm::BasicBlock*>& getToProcessQueue();

		std::queue<const llvm::BasicBlock*> toProcessForward;
		std::queue<const llvm::BasicBlock*> toProcessBackward;

		BasicBlockInfo& getInfo(const llvm::BasicBlock* BB);
		const llvm::Function& F;
		std::unordered_map<const llvm::BasicBlock*, BasicBlockInfo> mapBBtoInfo;
		std::vector<const llvm::BasicBlock*> doublyVisitedBlocks;
	};


	LiveRangesTy computeLiveRanges(llvm::Function& F, const InstIdMapTy& instIdMap, cheerp::PointerAnalyzer& PA);
	void doUpAndMark(BlocksState& blocksState, llvm::BasicBlock* BB, llvm::Instruction* I);
	static void assignInstructionsIds(InstIdMapTy& instIdMap, const llvm::Function& F, AllocaSetTy& allocaSet, const PointerAnalyzer* PA);
	uint32_t dfsLiveRangeInBlock(BlocksState& blockState, LiveRangesTy& liveRanges, const InstIdMapTy& instIdMap,
					llvm::BasicBlock& BB, cheerp::PointerAnalyzer& PA, uint32_t nextIndex, uint32_t codePathId);
	void extendRangeForUsedOperands(llvm::Instruction& I, LiveRangesTy& liveRanges, cheerp::PointerAnalyzer& PA,
					uint32_t thisIndex, uint32_t codePathId, bool splitRegularDest);
	uint32_t assignToRegisters(llvm::Function& F, const InstIdMapTy& instIdMap, const LiveRangesTy& liveRanges, const PointerAnalyzer& PA);
	void handlePHI(const llvm::Instruction& I, const LiveRangesTy& liveRanges, llvm::SmallVector<RegisterRange, 4>& registers, const PointerAnalyzer& PA);
	uint32_t findOrCreateRegister(llvm::SmallVector<RegisterRange, 4>& registers, const InstructionLiveRange& range,
					REGISTER_KIND kind, bool needsSecondaryName);
	bool addRangeToRegisterIfPossible(RegisterRange& regRange, const InstructionLiveRange& liveRange, REGISTER_KIND kind, bool needsSecondaryName);
	void computeAllocaLiveRanges(AllocaSetTy& allocaSet, const InstIdMapTy& instIdMap);
	typedef std::set<llvm::Instruction*, CompareInstructionByID> InstructionSetOrderedByID;
	InstructionSetOrderedByID gatherDerivedMemoryAccesses(const llvm::AllocaInst* rootI, const InstIdMapTy& instIdMap, FloodFillState& floodFillState);

	void assignRegistersToInstructions(llvm::Function& F, cheerp::PointerAnalyzer& PA);
};

llvm::ModulePass *createRegisterizePass(bool froundAvailable, bool wasm = false);

}

#endif

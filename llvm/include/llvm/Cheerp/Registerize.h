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
#include "llvm/ADT/BitVector.h"
#include "llvm/Cheerp/CommandLine.h"
#include <array>
#include <set>
#include <unordered_map>
#include <vector>
#include <deque>

//#define REGISTERIZE_DEBUG
//#define REGISTERIZE_DEBUG_EXAUSTIVE_SEARCH

namespace cheerp
{
/**
 * VertexColorer - Given an undirect graph, color the vertex in a (close to) optimal way as to have neighbours always of different color
 * Optimal means a combinations of 2 metrics: using the smaller number of colors as possible, and breaking as few "soft" constraints as possible
 */
class VertexColorer
{
	typedef std::vector<uint32_t> Coloring;
	typedef std::pair<uint32_t, std::pair<uint32_t, uint32_t>> Friendship;
	struct Link
	{
		uint32_t weight;
		uint32_t first;
		uint32_t second;
	};
	typedef std::pair<uint32_t, uint32_t> Friend;
public:
	VertexColorer(const uint32_t N, const uint32_t costPerColor, const uint32_t maximalNumberExploredLeafs)
		: N(N), costPerColor(costPerColor), parent(N), constraints(N, llvm::BitVector(N, true)), friends(N)
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
			llvm::errs()<<"\t\t"<<constraints[i].count() << "\t"<<friends[i].size() << "\n";
		}
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
	void setAll(const bool color)
	{
		for (uint32_t i=0; i<N; i++)
		{
			if (color)
				constraints[i].set();
			else
				constraints[i].reset();
		}
		for (uint32_t i=0; i<N; i++)
		{
			constraints[i].reset(i);
		}
	}
private:
	VertexColorer(const uint32_t N, const VertexColorer& parent)
		: VertexColorer(N, parent.costPerColor, parent.times)
	{
	}
	template <bool constraints, bool zeroWeight, bool positiveWeight, bool singleRow>
	class ConstFriendIterator
	{
	public:
		ConstFriendIterator(const VertexColorer& instance_, bool isBegin = true, uint32_t startingIndex = 0)
			: instance(instance_)
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
		constexpr bool onlyPositive()
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
			const uint32_t limit = limitOnJ();
			while (j < limit)
			{
				if (instance.constraints[i][j] && constraints)
					break;
				if (!instance.constraints[i][j] && zeroWeight)
					break;
				++j;
				//TODO: possible use BitVector::goToFirst(un)set to speed up
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
	//TODO: possibly take a filter, and return only unfiltered couples

	//Friendships Iterable
	ConstGenericFriendshipIterator</*constraints*/false,false,true> positiveWeightFriendshipIterable() const
	{
		return ConstGenericFriendshipIterator<false,false,true>(*this);
	}
	ConstGenericFriendshipIterator<false,true,false> zeroWeightFriendshipIterable() const
	{
		return ConstGenericFriendshipIterator<false,true,false>(*this);
	}
	ConstGenericFriendshipIterator<true,false,true> constraintOrFriendshipIterable() const
	{
		return ConstGenericFriendshipIterator<true,false,true>(*this);
	}
	ConstGenericFriendshipIterator<false,true,true> allFriendshipIterable() const
	{
		return ConstGenericFriendshipIterator<false,true,true>(*this);
	}

	//Friends Iterable
	ConstGenericFriendsIterator<false,false,true> positiveWeightFriendsIterable(const uint32_t row) const
	{
		return ConstGenericFriendsIterator<false,false,true>(*this, row);
	}
	ConstGenericFriendsIterator<false,true,false> zeroWeightFriendsIterable(const uint32_t row) const
	{
		return ConstGenericFriendsIterator<false,true,false>(*this, row);
	}
	ConstGenericFriendsIterator<true,false,true> constraintOrFriendsIterable(const uint32_t row) const
	{
		return ConstGenericFriendsIterator<true,false,true>(*this, row);
	}
	ConstGenericFriendsIterator<false,true,true> allFriendsIterable(const uint32_t row) const
	{
		return ConstGenericFriendsIterator<false,true,true>(*this, row);
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
		SearchState(Solution& best, uint32_t minimalNumberOfColors, uint32_t nodesToEvaluate, const uint32_t targetDepth, const uint32_t alreadyProcessedDepth, const uint32_t costPerColor)
			: currentBest(best), minimalNumberOfColors(minimalNumberOfColors),
			iterationsCounter(nodesToEvaluate),
			targetDepth(targetDepth), processedDepth(alreadyProcessedDepth), costPerColor(costPerColor)
		{
			processedFriendships = 0;
			leafs = 0;
			choicesMade = llvm::BitVector(0);
			currentScore = 0;
		}
		Solution& currentBest;
		uint32_t minimalNumberOfColors;
		IterationsCounter iterationsCounter;
		const uint32_t targetDepth;
		const uint32_t processedDepth;
		uint32_t processedFriendships;
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
		assert(constraints[a][b] == constraints[b][a] && !constraints[a][b]);
		constraints.push_back(constraints[b]);
		constraints.push_back(constraints[a]);
#ifdef REGISTERIZE_DEBUG
		debugStats[CONTRACTIONS]++;
#endif
		constraints[a] |= constraints[b];
		for (uint32_t i = 0; i<N; i++) constraints[i][a] = constraints[a][i];
		constraints[b] = llvm::BitVector(N);
		for (uint32_t i = 0; i<N; i++) constraints[i][b] = constraints[b][i];
		assert(parent[b] == b);
		parent[b] =a;
	}
	void undoContraction(const uint32_t a, const uint32_t b)
	{
		parent[b] = b;

		constraints[a] = constraints.back();
		constraints.pop_back();
		for (uint32_t i = 0; i<N; i++) constraints[i][a] = constraints[a][i];

		constraints[b] = constraints.back();
		constraints.pop_back();
		for (uint32_t i = 0; i<N; i++) constraints[i][b] = constraints[b][i];
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
	static uint64_t computeSample(const llvm::BitVector&A)
	{
		uint64_t res = 0;
		assert(A.size() >= 64);

		uint32_t spacing = A.size() / 64;

		for (uint32_t i=0; i<64; i++)
		{
			res = 2*res + (A[i*spacing]?1:0);
		}
		return res;
	}
	static bool isSubset(const uint64_t A, const uint64_t B)
	{
		return A == (A&B);
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
	uint32_t computeScore(const Coloring& coloring, const uint32_t lowerBound) const
	{
		assert(coloring.size() == N);
		uint32_t res = std::max(computeNumberOfColors(coloring), lowerBound) * costPerColor;
		for (const auto& p : friendships)
		{
			if (coloring[p.second.first] != coloring[p.second.second])
				res += p.first;
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
	void floodFill(std::vector<uint32_t>& regions, const uint32_t start, const bool conflicting, const uint32_t articulationPoint = -1) const;
	void floodFillOnBitsWithArticulationPoints(llvm::BitVector& region, const uint32_t start, const bool conflicting, const llvm::BitVector& isArticulationPoint) const;
	void floodFillOnBits(llvm::BitVector& region, const uint32_t start, const bool conflicting) const;
	bool isDominatingFriend(const uint32_t a, const uint32_t b) const;
	std::vector<uint32_t> whoIsDominatingFriend(const uint32_t a) const;
	bool removeDominatedRows();
	bool removeRowsWithFewConstraints();
	bool canBeAddedToClique(const uint32_t index, const llvm::BitVector& unionConstraint, const llvm::BitVector& used) const;
	void addToClique(const uint32_t index, llvm::BitVector& unionConstraint, llvm::BitVector& used) const;
	void improveLowerBound(const uint32_t x);
	uint32_t chromaticNumberWithNoFriends(uint32_t lowerBound, uint32_t minimalColors) const;
	uint32_t maximalGeneratedClique() const;
	uint32_t lowerBoundOnNumberOfColors(const bool forceEvaluation = false);
	bool splitOnArticulationClique(const bool keepSingleNodes = false);
	bool splitConflicting(const bool conflicting);
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
	const uint32_t N;
	const uint32_t costPerColor;
	std::vector<uint32_t> parent;
	Coloring retColors;
	std::vector<llvm::BitVector> constraints;
	std::vector<Friendship> friendships;
	std::vector<std::vector<Friend>> friends;
	uint32_t lowerBoundChromaticNumber;
	uint32_t howManyWaysHasLowerBoundBeenEvaluated;
	bool isOptimal;
	enum ReductionPasses{SPLIT_CONFLICTING, SPLIT_UNCONNECTED, SPLIT_ARTICULATION, REMOVE_DOMINATED, REMOVE_SMALL};
	std::array<bool, 5> avoidPass{};
public:
#ifdef REGISTERIZE_DEBUG
	enum PrintStatistics{GREEDY_EVALUATIONS=0, NODE_VISITED=1, CONTRACTIONS=2, SEPARATIONS=3};
	std::array<uint32_t, 4> debugStats{};
#endif
	uint32_t times;
};

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
			if (empty() || peekLast() != chunk.start)
				push_back(chunk);
			else
				back().end = chunk.end;
		}
	};

	static char ID;
	
	explicit Registerize(bool useFloats = false) : ModulePass(ID), useFloats(useFloats)
#ifndef NDEBUG
			, RegistersAssigned(false)
#endif
	{ }
	
	void getAnalysisUsage(llvm::AnalysisUsage & AU) const override;

	bool runOnModule(llvm::Module& M) override;
	
	const char *getPassName() const override;

	uint32_t getRegisterId(const llvm::Instruction* I) const;
	uint32_t getRegisterIdForEdge(const llvm::Instruction* I, const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB) const;
	uint32_t getSelfRefTmpReg(const llvm::Instruction* I, const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB) const;

	void assignRegisters(llvm::Module& M, cheerp::PointerAnalyzer& PA);
	void computeLiveRangeForAllocas(llvm::Function& F);
	void invalidateLiveRangeForAllocas(llvm::Function& F);

	const LiveRange& getLiveRangeForAlloca(const llvm::AllocaInst* alloca) const
	{
		assert(allocaLiveRanges.count(alloca));
		return allocaLiveRanges.find(alloca)->second;
	}

	// Registers should have a consistent JS type
	enum REGISTER_KIND { OBJECT=0, INTEGER, DOUBLE, FLOAT };

	struct RegisterInfo
	{
		// Try to save bits, we may need more flags here
		const REGISTER_KIND regKind : 2;
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

	// Context used to disambiguate temporary values used in PHI resolution
	void setEdgeContext(const llvm::BasicBlock* fromBB, const llvm::BasicBlock* toBB)
	{
		assert(edgeContext.isNull());
		edgeContext.fromBB=fromBB;
		edgeContext.toBB=toBB;
	}

	void clearEdgeContext()
	{
		edgeContext.clear();
	}

	llvm::LoopInfo* LI;
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
		std::unordered_map<T, uint32_t> map;
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
	std::unordered_map<const llvm::Instruction*, uint32_t> registersMap;
	std::unordered_map<InstOnEdge, uint32_t, InstOnEdge::Hash> edgeRegistersMap;
	std::unordered_map<const llvm::AllocaInst*, LiveRange> allocaLiveRanges;
	std::unordered_map<const llvm::Function*, std::vector<RegisterInfo>> registersForFunctionMap;
	std::unordered_map<InstOnEdge, uint32_t, InstOnEdge::Hash> selfRefRegistersMap;
	struct EdgeContext
	{
		const llvm::BasicBlock* fromBB;
		const llvm::BasicBlock* toBB;
		EdgeContext():fromBB(NULL), toBB(NULL)
		{
		}
		bool isNull() const
		{
			return fromBB==NULL;
		}
		void clear()
		{
			fromBB=NULL;
			toBB=NULL;
		}
	};
	EdgeContext edgeContext;
	bool useFloats;
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
	typedef std::unordered_map<const llvm::Instruction*, uint32_t> InstIdMapTy;
	struct CompareInstructionByID
	{
	private:
		const InstIdMapTy* instIdMap;
	public:
		CompareInstructionByID(const InstIdMapTy& i):instIdMap(&i)
		{
		}
		bool operator()(const llvm::Instruction* l, const llvm::Instruction*r) const
		{
			assert(instIdMap->count(l) && instIdMap->count(r));
			return instIdMap->find(l)->second < instIdMap->find(r)->second;
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
	static void mergeRegisterInPlace(RegisterRange& a, const RegisterRange& b);
	static RegisterRange mergeRegister(const RegisterRange& a, const RegisterRange& b);
	struct Result
	{
		Result() : registersNeeded(-1)
		{}
		std::vector<int> parents;
		int registersNeeded;
		int phiUseBroken;
		int phiEdgeBroken;
		bool operator<(const Result& other)const
		{
			if (registersNeeded == -1)
				return false;
			if (other.registersNeeded == -1)
				return true;
			return score() < other.score();
		}
		uint32_t score() const
		{
			//TODO: fix weights
			//TODO: reintroduce phiEdgeBroken
			return phiUseBroken + 6 * registersNeeded + 0*phiEdgeBroken;
		}
	};
	class FrequencyInfo
	{
	public:
		FrequencyInfo(llvm::Function& F, llvm::LoopInfo* LI)
			: LI(LI)
		{
			//TODO: possibly store the computed values to avoid recalculations
		}
		uint32_t getWeight(const llvm::BasicBlock* A, const llvm::BasicBlock* B) const
		{
			//Takes a phi_edge as input, return his weight

			//TODO: change this to find the common loop between the two edges (or it is already like this?)
			const uint32_t depth = std::min(LI->getLoopDepth(A), LI->getLoopDepth(B));
			uint32_t res = 1;
			for (uint32_t i=0; i<depth && res < 100000; i++)
			{
				res *= 10;
			}
			return res;
		}
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
		Result getResult()
		{
			Result res;
			res.parents.resize(numInst());
			for (uint32_t i = 0; i<numInst(); i++)
			{
				res.parents[i] = findParent(i);
			}
			res.registersNeeded = registersNeeded();
			res.phiUseBroken = computeWeigthBrokenNeighbours();
			res.phiEdgeBroken = computeWeightBrokenEdges();
			return res;
		}
	private:
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
		Indexer<const llvm::Instruction*> indexer;
		llvm::SmallVector<RegisterRange, 4> virtualRegisters;
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
	typedef std::unordered_map<llvm::BasicBlock*, BlockState> BlocksState;
	// Temporary data used to registerize allocas
	typedef std::vector<const llvm::AllocaInst*> AllocaSetTy;
	typedef std::map<uint32_t, uint32_t> RangeChunksTy;
	struct AllocaBlockState
	{
		bool liveOut:1;
		// If notLiveOut is true neither this block or the blocks above do not use the alloca
		bool notLiveOut:1;
		bool liveIn:1;
		// If notLiveIn is true we know that the alloca is reset using lifetime_start in the block
		bool notLiveIn:1;
		// Is hasUse is true there is a use for the alloca inside the block
		bool hasUse:1;
		// upAndMarkId is used for various purposes:
		// 1) Is non-zero if the block is currently being explored
		// 2) Is non-zero if the block is in the pending list, the value is the lowest upAndMarkId on which the block state depend
		uint32_t upAndMarkId;
		AllocaBlockState():liveOut(false),notLiveOut(false),liveIn(false),notLiveIn(false),hasUse(false),upAndMarkId(0)
		{
		}
	};
	struct AllocaBlocksState: public std::unordered_map<llvm::BasicBlock*, AllocaBlockState>
	{
		std::vector<llvm::BasicBlock*> pendingBlocks;
		void markPendingBlocksAsLiveOut(uint32_t index)
		{
			for(uint32_t i=index;i<pendingBlocks.size();i++)
			{
				find(pendingBlocks[i])->second.liveOut = true;
				find(pendingBlocks[i])->second.upAndMarkId = 0;
			}
			pendingBlocks.resize(index);
		}
		void markPendingBlocksAsNotLiveOut(uint32_t index)
		{
			for(uint32_t i=index;i<pendingBlocks.size();i++)
			{
				find(pendingBlocks[i])->second.notLiveOut = true;
				find(pendingBlocks[i])->second.upAndMarkId = 0;
			}
			pendingBlocks.resize(index);
		}
		void discardPendingBlocks(uint32_t index)
		{
			for(uint32_t i=index;i<pendingBlocks.size();i++)
				find(pendingBlocks[i])->second.upAndMarkId = 0;
			pendingBlocks.resize(index);
		}
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
	InstructionSetOrderedByID gatherDerivedMemoryAccesses(const llvm::AllocaInst* rootI, const InstIdMapTy& instIdMap);
	enum UP_AND_MARK_ALLOCA_STATE { USE_FOUND = 0, USE_NOT_FOUND, USE_UNKNOWN };
	struct UpAndMarkAllocaState
	{
		uint32_t state;
		UpAndMarkAllocaState(uint32_t s):state(s)
		{
		}
		UpAndMarkAllocaState& operator|=(const UpAndMarkAllocaState& rhs)
		{
			UpAndMarkAllocaState& lhs = *this;
			// 1) FOUND | Any = FOUND
			// 2) UNKNOWN | Any = UNKNOWN
			// 3) NOT_FOUND | NOT_FOUND = NOT_FOUND
			if (lhs.state == USE_FOUND || rhs.state == USE_FOUND)
			{
				lhs.state = USE_FOUND;
				return lhs;
			}
			// Return the smaller one as it is the one closest to the start
			if (lhs.state >= USE_UNKNOWN && rhs.state >= USE_UNKNOWN)
			{
				if (rhs.state < lhs.state)
					lhs.state = rhs.state;
				return lhs;
			}
			if (rhs.state >= USE_UNKNOWN)
			{
				lhs.state = rhs.state;
				return lhs;
			}
			if (lhs.state >= USE_UNKNOWN)
			{
				return lhs;
			}
			assert(lhs.state == USE_NOT_FOUND && rhs.state == USE_NOT_FOUND);
			return lhs;
		}
		bool operator==(UP_AND_MARK_ALLOCA_STATE r) const
		{
			return state == r;
		}
		bool operator!=(UP_AND_MARK_ALLOCA_STATE r) const
		{
			return state != r;
		}
	};
	UpAndMarkAllocaState doUpAndMarkForAlloca(AllocaBlocksState& blocksState, llvm::BasicBlock* BB, uint32_t upAndMarkId);
	void assignRegistersToInstructions(llvm::Function& F, cheerp::PointerAnalyzer& PA);
};

llvm::ModulePass *createRegisterizePass(bool useFloats);

}

#endif

//===-- LowerSwitch.cpp - Cheerp optimization pass ------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2019-2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/Transforms/Utils/LowerSwitch.h"
#include "llvm/Cheerp/CFGPasses.h"
#include <unordered_map>
#include <unordered_set>

using namespace llvm;

namespace {

class CheerpLowerSwitch: public LowerSwitch {
public:
	CheerpLowerSwitch(bool onlyLowerI64 = true): onlyLowerI64(onlyLowerI64)
	{
	}
	StringRef getPassName() const override {
		return "CheerpLowerSwitch";
	}
	static char ID;
private:
	bool onlyLowerI64;

        void processSwitchInst(SwitchInst *SI,
                           SmallPtrSetImpl<BasicBlock *> &DeleteList,
                           AssumptionCache *AC, LazyValueInfo *LVI) override;
	bool keepSwitch(const SwitchInst* si);
};

}

static int64_t getCaseValue(const ConstantInt* c, uint32_t bitWidth)
{
	return bitWidth >= 32 ? c->getSExtValue() : c->getZExtValue();
};

bool CheerpLowerSwitch::keepSwitch(const SwitchInst* si)
{
	// At least 3 successors
	if (si->getNumSuccessors() < 3)
		return false;
	uint32_t bitWidth = si->getCondition()->getType()->getIntegerBitWidth();
	// No 64 bit
	if (bitWidth == 64)
		return false;
	if (onlyLowerI64)
		return true;
	//In asm.js cases values must be in the range [-2^31,2^31),
	//and the difference between the biggest and the smaller must be < 2^31
	int64_t max = std::numeric_limits<int64_t>::min();
	int64_t min = std::numeric_limits<int64_t>::max();
	for (auto& c: si->cases())
	{
		int64_t curr = getCaseValue(c.getCaseValue(), bitWidth);
		max = std::max(max,curr);
		min = std::min(min,curr);
	}
	if (min >= std::numeric_limits<int32_t>::min() &&
		max <= std::numeric_limits<int32_t>::max() && 
		//NOTE: this number is the maximum allowed by V8 for wasm's br_table,
		// it is not defined in the spec
		max-min <= 32 * 1024 &&
		// Avoid extremely big and extremely sparse tables, require at least 3% fill rate
		(max-min <= 100 || si->getNumCases() * 100 >= 3 * (max-min)))
	{
		return true;
	}
	return false;
}

//Class to represent a range low-high (both included, and possibly equal to each other) such that each value in the range should branch to BasicBlock dest
class RangeDest
{
public:
	RangeDest(int64_t low, int64_t high, BasicBlock* dest)
		: low(low), high(high), dest(dest)
	{
		assert(dest);
		assert(low <= high);
	}
	RangeDest(int64_t val, BasicBlock* dest)
		: RangeDest(val, val, dest)
	{
	}
	bool couldExtend(int64_t x)
	{
		return (high+1 == x);
	}
	void extend(int64_t x)
	{
		assert(couldExtend(x));
		high = x;
	}
	BasicBlock* getDest() const
	{
		return dest;
	}
	bool isSingleValue() const
	{
		return low == high;
	}
	int64_t low;
	int64_t high;
private:
	BasicBlock* dest;
};

//Class to represent a sorted collection of RangeDest, to be build at constuction starting from a SwitchInst*
//Note that SI can't be const since we will need to query for the destinations BasicBlock and modify those
class OrderedRanges
{
public:
	OrderedRanges(SwitchInst* SI)
		: SI(SI), bitWidth(SI->getCondition()->getType()->getIntegerBitWidth())
	{
		populateOrderedCases();
		populateRanges();
	}
	const RangeDest& getRange(uint32_t i) const
	{
		return orderedRanges[i];
	}
	uint32_t size() const
	{
		return orderedRanges.size();
	}
private:
	bool hasDefault() const
	{
		return SI->getDefaultDest();
	}
	void populateOrderedCases()
	{
		for (auto& c: SI->cases())
		{
			int64_t curr = getCaseValue(c.getCaseValue(), bitWidth);
			orderedCases.push_back({curr, c.getCaseSuccessor()});
		}

		std::sort(orderedCases.begin(), orderedCases.end());
	}
	void populateRanges()
	{
		//TODO: Check overflow undetermined??
		int64_t minimum = (((uint64_t)1)<<63);
		const int64_t maximum = (((uint64_t)1)<<63) - 1;
		for (const auto& p : orderedCases)
		{
			if (!orderedRanges.empty() && orderedRanges.back().getDest() == p.second)
			{
				if (orderedRanges.back().couldExtend(p.first) || !hasDefault())
				{
					minimum = p.first+1;
					orderedRanges.back().extend(p.first);
					continue;
				}
			}
			if (hasDefault() && minimum < p.first)
			{
				orderedRanges.push_back(RangeDest(minimum, p.first-1, SI->getDefaultDest()));
			}
			minimum = p.first + 1;
			orderedRanges.push_back(RangeDest(p.first, p.second));
		}
		if (hasDefault() && orderedRanges.back().high < maximum)
		{
			orderedRanges.push_back(RangeDest(orderedRanges.back().high+1, maximum, SI->getDefaultDest()));
		}
	}
	SwitchInst* SI;
	const uint32_t bitWidth;
	std::vector<RangeDest> orderedRanges;
	std::vector<std::pair<int64_t, BasicBlock*>> orderedCases;
};

//Class to compute all relevant informations about a SwitchInst and be able to answer queries given a bitset representation
struct DataOnSwitch
{
	DataOnSwitch(SwitchInst* SI)
		: SI (SI), orderedRanges(SI)
	{
	}
	uint32_t cardinalityDestinationsSet(const int64_t representation) const
	{
		std::unordered_set<const BasicBlock*> possibleDestinations;
		for (int64_t pow = 1, id = 0; pow <= representation; pow *=2, id++)
		{
			if (representation & pow)
				possibleDestinations.insert(getRange(id).getDest());
		}

		return possibleDestinations.size();
	}
	llvm::BasicBlock* getUniqueDestination(const int64_t representation) const
	{
		llvm::BasicBlock* uniqueDestination = nullptr;

		for (int64_t pow = 1, id = 0; pow <= representation; pow *=2, id++)
		{
			if (representation & pow)
			{
				llvm::BasicBlock* currDest = getRange(id).getDest();
				if (uniqueDestination == nullptr)
					uniqueDestination = currDest;
				else if (uniqueDestination != currDest)
					return nullptr;
			}
		}

		return uniqueDestination;
	}
	double getApproximateCost(const int64_t representation) const
	{
		return std::max(0.0, cardinalityDestinationsSet(representation) - 1.0);
	}
	uint32_t numRanges() const
	{
		return orderedRanges.size();
	}
	const RangeDest& getRange(uint32_t i) const
	{
		return orderedRanges.getRange(i);
	}
	uint64_t startingRepresentation() const
	{
		int64_t sum = 0;
		int64_t pow = 1;
		for (uint32_t i = 0; i<numRanges(); i++)
		{
			sum += pow;
			pow *= 2;
		}
		return sum;
	}
private:
	bool hasDefault() const
	{
		return SI->getDefaultDest();
	}
	SwitchInst* SI;
	const OrderedRanges orderedRanges;
};

class GreedyLowering
{
public:
	GreedyLowering(SwitchInst* SI)
		: SI(SI), condition(SI->getCondition())
	{
		//We sort of assume that we will be here only with >=2 outgoing
	}
	double calculatedCost(const DataOnSwitch& data)
	{
		if (greedyLoweringIsValid)
			return costCalculated;

		return evaluateCost(data);
	}
	void lowerGreedily(DataOnSwitch& data)
	{
		assert(isValid());

		int64_t start = data.startingRepresentation();

		saveAndRemoveOutgoingPHI();

		recursiveLowerGreedy(data, start, nullptr);
	}
	bool isValid() const
	{
		return greedyLoweringIsValid;
	}
private:
	double evaluateCost(const DataOnSwitch& data)
	{
		greedyLoweringMap.clear();
		if (isBetterThanBinaryLowering(data))
		{
			greedyLoweringIsValid = true;
			costCalculated = evaluateCost(data, data.startingRepresentation());
		}
		else
			costCalculated = 1e9;

		return costCalculated;
	}
	static bool isBetterThanBinaryLowering(const DataOnSwitch& data)
	{
		return data.numRanges() < 12;
	}
	//Map representing every PHINode from which we have subtracted a value, so that it could be restored later
	std::unordered_map<PHINode*, Value*> mapOutgoingValues;
	void saveAndRemoveOutgoingPHI()
	{
		//Here we iterate the successors of SI (only ONCE for every successor), iterate all the phis on each BB (again, ONCE per phi)
		//and remove the incoming value from SI->getParent(), saving it to be reintroduced later
		for (BasicBlock* BB : successors(SI->getParent()))
		{
			for (PHINode& phi : BB->phis())
			{
				mapOutgoingValues[&phi] = phi.removeIncomingValue(SI->getParent(), false);
			}
		}

		//recursiveLowerGreedy will take care (while visiting the leaf nodes) to restore the incoming values

		//Note that generally the number of incomings on the phis may increase, since the lowering might have multiple leaf branching to the same BB
	}
	enum TestKind {EQUALITY, RANGE_MEMBERSHIP, HALF_OPEN_RANGE_MEMBERSHIP, PAIR_MEMBERSHIP};
	struct Transformation
	{
		Transformation(int64_t succ, int64_t fail, TestKind testKind)
			: representation_success(succ), representation_failure(fail), testKind(testKind)
		{
		}
		Transformation(const Transformation& t)
			: representation_success(t.representation_success), representation_failure(t.representation_failure), testKind(t.testKind)
		{
			a = t.a;
			b = t.b;
			c = t.c;
		}
		//Cost of the various splitting possibility
		//The costs are relative to the equality test (a == b), that requires no normalization step
		double getOperationCost() const
		{
			switch (testKind)
			{
				case TestKind::EQUALITY:
					return 1.0;
				case TestKind::RANGE_MEMBERSHIP:
					return 1.2;
				case TestKind::HALF_OPEN_RANGE_MEMBERSHIP:
					return 1.05;
				case TestKind::PAIR_MEMBERSHIP:
					return 1.4;
			}
			llvm_unreachable("TestKind not handled in getOperationCost");
		}
		const int64_t representation_success;
		const int64_t representation_failure;
		const TestKind testKind;
		int64_t a;
		int64_t b;
		int64_t c;
	};

	static bool isGoodSplit(const int64_t A, const int64_t B, const int64_t representation)
	{
		//Each value should be NON 0, otherwise no work will be done
		if (A == 0 || B == 0)
			return false;

		//Each bit in representation should be either in A or B
		if ((A | B) != representation)
			return false;

		//No bit out of representation should be present
		if ((A & representation) != A)
			return false;
		if ((B & representation) != B)
			return false;

		return true;
	}
	//Enumerate all possible ways of splitting in two subcases from a given situation in the lowering
	std::vector<Transformation> enumerateTransformations(const DataOnSwitch& data, const int64_t representation) const
	{
		std::vector<Transformation> possibleTransformations;

		for (int64_t pow = 1, id = 0; pow <= representation; pow *=2, id++)
		{
			{
				//Test for membership of a given range (possibly a single value);
				const int64_t A = pow;
				const int64_t B = representation ^ A;
				if (isGoodSplit(A, B, representation))
				{
					const RangeDest& range = data.getRange(id);
					if (range.isSingleValue())
					{
						possibleTransformations.push_back(Transformation(A, B, TestKind::EQUALITY));
						possibleTransformations.back().a = range.low;
					}
					else
					{
						possibleTransformations.push_back(Transformation(A, B, TestKind::RANGE_MEMBERSHIP));
						possibleTransformations.back().a = range.low;
						possibleTransformations.back().b = range.high;
					}
				}
			}
			for (int64_t pow2 = 1, id2=0; pow2 < pow; pow2 *=2, id2++)
			{
				const int64_t A = (pow-pow2) & representation;
				const int64_t B = representation ^ A;
				if (isGoodSplit(A, B, representation))
				{
					possibleTransformations.push_back(Transformation(A, B, TestKind::RANGE_MEMBERSHIP));
					//TODO -> Find higher and lower bits!!!!

					const RangeDest& range = data.getRange(id-1);
					const RangeDest& range2 = data.getRange(id2);
					possibleTransformations.back().a = range2.low;
					possibleTransformations.back().b = range.high;
				}
			}
			{
				//Test for lower than a given value
				const int64_t A = (pow-1) & representation;
				const int64_t B = representation ^ A;
				if (isGoodSplit(A, B, representation))
				{
					const RangeDest& range = data.getRange(id);
					possibleTransformations.push_back(Transformation(A, B, TestKind::HALF_OPEN_RANGE_MEMBERSHIP));
					possibleTransformations.back().a = range.low - 1;
				}
			}
			for (int64_t pow2 = 1, id2 = 0; pow2 < pow; pow2 *=2, id2++)
			{
				const int64_t A = (pow + pow2) & representation;
				const int64_t B = representation ^ A;
				if (A > 0 && B > 0 && (representation & A) == A && (representation & B) == B && (A | B) == representation)
				{
					const RangeDest& range = data.getRange(id);
					const RangeDest& range2 = data.getRange(id2);

					if (!range.isSingleValue() || !range2.isSingleValue())
						continue;

					possibleTransformations.push_back(Transformation(A, B, TestKind::PAIR_MEMBERSHIP));

					possibleTransformations.back().a = range.low;
					possibleTransformations.back().b = range2.low;
				}
			}
		}

		return possibleTransformations;
	}
	static double combineCosts(const double testCost, const double ifSuccessCost, const double ifFailureCost)
	{
		//The formula should be symmetric between ifSuccessCost and ifFailureCost + should be linear in both terms + convex

		//The only choice left are the coefficients, testCost is arbitrary scalable, so it could be scaled such that it has the same coefficient of std::max()
		//And that coefficient can be scaled arbitrarily to 1.0

		//The only degree of freedom is the coefficient of std::min(), closer to 1.0 means that we minimize the total number of tests (=> the code size), closer to 0.0 means we minimize the worst case (=> the performance)
		const double penality = 0.2;

		assert(penality >= 0.0);
		assert(penality < 1.0);

		return testCost + std::max(ifSuccessCost, ifFailureCost) + penality*std::min(ifSuccessCost, ifFailureCost);
	}

	//evaluateCost computes the cost of a given lowering AND saves the intermediate choices made in the process
	//(so that they can be reconstructed later in exactly the same lowering)
	double evaluateCost(const DataOnSwitch& data, const int64_t representation)
	{
		//Empty subset has cost 0
		if (representation == 0)
			return 0.0;

		//Subset will all the same destination has cost 0
		if (data.getUniqueDestination(representation))
			return 0.0;

		//Collect a the possible transofrmations doable from a given subset
		const std::vector<Transformation> possibleTransformations = enumerateTransformations(data, representation);

		//Calculate the best among the possibleTransofrmations, but computing only the approximate cost
		std::pair<double, int> bestTransformation{1e9, -1};
		for (uint32_t index = 0; index < possibleTransformations.size(); index++)
		{
			const auto& t = possibleTransformations[index];
			const double succ = data.getApproximateCost(t.representation_success);
			const double fail = data.getApproximateCost(t.representation_failure);

			const double currScore = combineCosts(t.getOperationCost(), succ, fail);

			const std::pair<double, int> currCandidate{currScore, index};

			bestTransformation = std::min(bestTransformation, currCandidate);
		}

		if (bestTransformation.second < 0)
			return 1e9;

		//Now greedily take what seems to be the bestTransfomation (according to the approximate cost)
		//And split in two subproblems
		const auto& t = possibleTransformations[bestTransformation.second];
		const double succ = evaluateCost(data, t.representation_success);
		const double fail = evaluateCost(data, t.representation_failure);

		greedyLoweringMap.insert({representation, t});

		//Here we know the actual cost of lowering through this split
		const double evaluatedCost = combineCosts(t.getOperationCost(), succ, fail);

		return evaluatedCost;
	}
	//Dispatch between various methods of generating a comparions
	ICmpInst* generateComparison(BasicBlock& bb, Value* incoming, const Transformation& transformation)
	{
		switch (transformation.testKind)
		{
			case TestKind::EQUALITY:
				return testForEquality(bb, incoming, transformation.a);
			case TestKind::RANGE_MEMBERSHIP:
				return testForRangeMembership(bb, incoming, transformation.a, transformation.b);
			case TestKind::HALF_OPEN_RANGE_MEMBERSHIP:
				return testForHalfOpenRangeMembership(bb, incoming, transformation.a);
			case TestKind::PAIR_MEMBERSHIP:
				return testForPairMembership(bb, incoming, transformation.a, transformation.b);
		}
		llvm_unreachable("TestKind not handled in generateComparison");
	}
	//Given that we have already computed what is the greedy choice in each visited state, do that choice, splitting in two subproblems
	llvm::BasicBlock* recursiveLowerGreedy(DataOnSwitch& data, const int64_t representation, llvm::BasicBlock* from)
	{
		//IFF there is a single destination, no more choices are needed and we are in a leaf.
		//Add the incoming values to the phis in the destination's BasicBlock
		if (llvm::BasicBlock* to = data.getUniqueDestination(representation))
		{
			assert(from); //Here is the constraint that impose that the SwitchInst should be non-obvious (eg. all values branch to the same BB)

			//Fix incoming from's PHINodes from origin to to

			for (PHINode& phi : to->phis())
			{
				phi.addIncoming(mapOutgoingValues.at(&phi), from);
			}

			return to;
		}

		//Find the transofrmation that seemed the best at this stage
		const auto& t = greedyLoweringMap.at(representation);

		const bool isOriginalBB = (from == nullptr);

		//Either we are in the originalBB or we create a new one
		llvm::BasicBlock* currBB = isOriginalBB ?
			SI->getParent() :
			BasicBlock::Create(SI->getFunction()->getContext(), "lowerswitch", SI->getFunction());

		//Recursively walk the rest of the lowering tree
		llvm::BasicBlock* success = recursiveLowerGreedy(data, t.representation_success, currBB);
		llvm::BasicBlock* failure = recursiveLowerGreedy(data, t.representation_failure, currBB);

		Value* incoming = SI->getCondition();

		if (isOriginalBB)
		{
			//remove original SwitchInst
			SI->eraseFromParent();
		}

		//Add comparison
		ICmpInst* test = generateComparison(*currBB, incoming, t);

		//Add Branch
		BranchInst::Create(success, failure, test, currBB);

		return currBB;
	}
	ICmpInst* testForEquality(BasicBlock& bb, Value* incoming, const int64_t a)
	{
		return new ICmpInst(bb, ICmpInst::ICMP_EQ, incoming, getConstantInt(a), "testEquality");
	}
	ICmpInst* testForPairMembership(BasicBlock& bb, Value* incoming, int64_t a, int64_t b)
	{
		if (a > b)
			std::swap(a,b);
		assert(a != b);
		const uint64_t diff = b-a;

		auto greatestPowerOf2 = [](uint64_t diff) -> uint64_t
		{
			assert(diff >= 0);
			uint64_t pow = 1;
			while ((diff & pow) == 0)
				pow *= 2;

			return pow;
		};

		const uint64_t pow = greatestPowerOf2(diff);
		assert((pow&(pow-1)) == 0);
		//pow is the greatest power of 2 that divides diff (that generally is equal to 2**b * odd_number)

		//		-------
		//How to test for (x == A) || (x == B) in a single comparison?

		//There are 2 cases that turns out when combined covers all possibilities

		//1. The difference is a power of 2
		//1a. First we need to normalize {A, B} -> {0, diff} by adding (-A) (=subtracting A)
		//1b. Then we note that difference is a power of 2, so we can do |= diff that leads from 0|diff = diff, diff|diff = diff
		//1c. and all other values will map to something different than diff (since diff = 2**b it will have a single bit set)
		//1d. Now with a single equality against diff we could discover whether (x==A) || (x == B) holds

		//2. The difference is odd
		//2a. First we need to normalize {A, B} -> {-n-1, n} by adding (-A-n-1) for n = diff/2
		//2b. Then we do ^= n. n^n -> 0, and (-n-1)^n -> -1 [Why?]
		//		We start from		-n == (not n) + 1
		//		Then we move the 1	-n-1 == not n
		//		Then we xor by n	(-n-1)^n == not n ^ n == -1	(the last equality since not n and n have no bits in common)
		//2c. Now we add 1, and we go from {-1, 0} to {0, 1}
		//2d. Now a single unsigned less than 2 can discriminate between either A or B or all other values

		//3. If the difference is neither odd nor power of 2, we scompose the difference in odd_number * 2**bits
		//	then we apply the same strategy as in 2 but multipliying each step by 2**bits
		//	The basic idea is that we leave the lower bits untouched, and we ends up at the step c with {0, 2**bits}
		//	Now we use the same idea of 1, doing an OR + one equality

		const uint64_t n = (diff-pow)/2;

		Value* previous = incoming;

		if (diff == 1)
		{
			//difference = 1 means two consecutive values
			return testForRangeMembership(bb, incoming, a, b);
		}
		else if (pow != diff)
		{
			if (a + n + pow != 0)
			{
				previous = BinaryOperator::CreateAdd(previous, getConstantInt(-(a + n + pow)), previous->getName()+".off", &bb);
			}
			assert(n > 0);
			//Now we mapped one number to (-n-pow) and the other to (n)

			previous = BinaryOperator::CreateXor(previous, getConstantInt(n), previous->getName()+".xor", &bb);
			//Now we mapped one number to (-pow) and the other to (0)

			previous = BinaryOperator::CreateAdd(previous, getConstantInt(pow), previous->getName()+".plus1", &bb);
			//Now we mapped one number to (0) and the other to (pow)
		}
		else
		{
			if (a != 0)
			{
				previous = BinaryOperator::CreateAdd(previous, getConstantInt(-a), previous->getName()+".off", &bb);
			}
			//Now we mapped one number to (0) and the other to (pow)
		}

		if (pow > 1)
		{
			//If we are here we have {0, 2**bit}
			Instruction* Or = BinaryOperator::CreateOr(previous, getConstantInt(pow), previous->getName()+".or", &bb);
			//After the or we have {2**bit, 2**bit}, so a single equality works

			return new ICmpInst(bb, ICmpInst::ICMP_EQ, getConstantInt(pow), Or, "membershipPairGenericDistance");
		}
		else
		{
			//If we are here we have {0, 1}, so a single unsigned <= 1 works
			return new ICmpInst(bb, ICmpInst::ICMP_ULE, previous, getConstantInt(pow), "membershipPairOddDistance");
		}
	}
	ICmpInst* testForRangeMembership(BasicBlock& bb, Value* incoming, int64_t a, int64_t b)
	{
		Value* toCompare = incoming;

		assert(b > a);

		if (a != 0)
		{
			toCompare = BinaryOperator::CreateAdd(incoming, getConstantInt(-a), incoming->getName()+".off", &bb);
			b += -a;
			a += -a;
			//Now a is normalized to 0, and b is a non-negative number
		}

		return new ICmpInst(bb, ICmpInst::ICMP_ULE, toCompare, getConstantInt(b), "testRangeMembership");
	}
	ICmpInst* testForHalfOpenRangeMembership(BasicBlock& bb, Value* incoming, const int64_t a)
	{
		return new ICmpInst(bb, ICmpInst::ICMP_SLE, condition, getConstantInt(a), "testHalfOpenRangeMembership");
	}
	Constant* getConstantInt(const int64_t a)
	{
		return ConstantInt::get(condition->getType(), a);
	}

	SwitchInst* SI;
	Value* condition;
	std::unordered_map<int64_t, Transformation> greedyLoweringMap;

	bool greedyLoweringIsValid{false};
	double costCalculated;
};

void CheerpLowerSwitch::processSwitchInst(SwitchInst *SI, SmallPtrSetImpl<BasicBlock*> &DeleteList, AssumptionCache *AC, LazyValueInfo *LVI)
{
	DataOnSwitch data(SI);
	GreedyLowering lowering(SI);

	const bool isConvenientToLower = (lowering.calculatedCost(data) < 5.0);

	if(!isConvenientToLower && keepSwitch(SI))
		return;

	if (lowering.isValid())
		lowering.lowerGreedily(data);
	else
		LowerSwitch::processSwitchInst(SI, DeleteList, AC, LVI);
}

char CheerpLowerSwitch::ID = 0;

namespace llvm {

FunctionPass* createCheerpLowerSwitchPass(bool onlyLowerI64)
{
	return new CheerpLowerSwitch(onlyLowerI64);
}

}

INITIALIZE_PASS_BEGIN(CheerpLowerSwitch, "CheerpLowerSwitch", "Lower switches too sparse or big into if/else branch chains",
                      false, false)
INITIALIZE_PASS_END(CheerpLowerSwitch, "CheerpLowerSwitch", "Lower switches too sparse or big into if/else branch chains",
                    false, false)

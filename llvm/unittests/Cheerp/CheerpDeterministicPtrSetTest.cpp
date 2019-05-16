//===- llvm/unittest/Cheerp/CheerpPointerAnalyzerTest.cpp -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/DeterministicPtrSet.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"

namespace llvm {
namespace {

template<typename T>
void areIdentical(cheerp::DeterministicPtrSet<T>& A, cheerp::DeterministicPtrSet<T>& B)
{
	auto itA = A.begin();
	auto itB = B.begin();
	while (itA != A.end() && itB != B.end())
	{
		ASSERT_TRUE(*itA == *itB);
		++itA;
		++itB;
	}
	ASSERT_TRUE(itA == A.end());
	ASSERT_TRUE(itB == B.end());
}

template<typename T>
void areEquivalent(cheerp::DeterministicPtrSet<T>& A, cheerp::DeterministicPtrSet<T>& B)
{
	for (auto x : A)
	{
		EXPECT_EQ(1u, B.count(x));
	}
	for (auto x : B)
	{
		EXPECT_EQ(1u, A.count(x));
	}
}

using namespace cheerp;

TEST(CheerpTest, DeterministicPtrSetTest) {

	LLVMContext C;
	SMDiagnostic Err;

	std::unique_ptr<Module> M = parseIRFile( "test1.ll", Err, C );
	ASSERT_TRUE( M.get() );

	DeterministicPtrSet<const Function*> functionSet;

	EXPECT_EQ( 0u, functionSet.size() );
	for (const Function& F : *M)
	{
		functionSet.erase(&F);
	}
	EXPECT_EQ( 0u, functionSet.size() );
	ASSERT_TRUE(functionSet.begin() == functionSet.end());
	for (const Function& F : *M)
	{
		functionSet.insert(&F);
	}
	for (const Function& F : *M)
	{
		EXPECT_EQ( 1u, functionSet.count(&F));
	}
	for (const Function& F : *M)
	{
		ASSERT_TRUE(functionSet.erase(&F));
		EXPECT_EQ( 0u, functionSet.count(&F));
		ASSERT_TRUE(functionSet.find(&F) == functionSet.end());
	}
	EXPECT_EQ( 0u, functionSet.size() );
	ASSERT_TRUE(functionSet.begin() == functionSet.end());
	for (const Function& F : *M)
	{
		functionSet.insert(&F);
	}
	DeterministicPtrSet<const Function*> functionSet2;
	for (const Function& F : *M)
	{
		functionSet2.insert(&F);
	}
	for (const Function& F : *M)
	{
		functionSet2.insert(&F);
	}
	areIdentical<const Function*>(functionSet, functionSet2);
	DeterministicPtrSet<const Function*> functionSet3(functionSet2);
	areIdentical<const Function*>(functionSet, functionSet3);

	for (const Function& F : *M)
	{
		if (rand()%2)
			continue;
		functionSet3.erase(&F);
	}
	for (const Function& F : *M)
	{
		functionSet3.insert(&F);
	}
	areEquivalent<const Function*>(functionSet, functionSet3);
	functionSet.clear();
	EXPECT_EQ( 0u, functionSet.size() );
	for (const Function& F : *M)
	{
		functionSet3.erase(&F);
	}
	EXPECT_EQ( 0u, functionSet3.size() );

	std::vector<const llvm::Instruction*> instructionList;
	for (const Function& F : *M)
	{
		for (const llvm::BasicBlock& BB : F)
		{
			for (const llvm::Instruction& I : BB)
			{
				instructionList.push_back(&I);
			}
		}
	}

	DeterministicPtrSet<const Instruction*> instructionSet1(instructionList.begin(), instructionList.end()), instructionSet3(instructionSet1);
	DeterministicPtrSet<const Instruction*> instructionSetReverse(instructionList.rbegin(), instructionList.rend());
	instructionSetReverse.swap(instructionSet3);
	instructionSetReverse.swap(instructionSet1);
	//Now instructionSet3 is the reversed one, and the others should be equals

	areEquivalent<const Instruction*>(instructionSet3, instructionSet1);
	areEquivalent<const Instruction*>(instructionSet1, instructionSetReverse);
	areEquivalent<const Instruction*>(instructionSet3, instructionSetReverse);
	areIdentical<const Instruction*>(instructionSet1, instructionSetReverse);

	for (unsigned int x = 0; x<instructionList.size(); x++)
	{
		if (x%2)
			continue;
		instructionSet1.erase(instructionList[x]);
		instructionSet1.insert(instructionList[x]);
		instructionSet1.erase(instructionList[x]);
		instructionSet3.erase(instructionList[x]);
		instructionSetReverse.erase(instructionList[x]);
	}

	for (unsigned int x =0; x<100; x++)
	{
		const Instruction* r = (const Instruction*)((unsigned long long)rand());
		instructionSet1.erase(r);
		instructionSet3.erase(r);
		instructionSetReverse.erase(r);
	}

	std::swap(instructionSetReverse, instructionSet3);
	std::swap(instructionSetReverse, instructionSet1);
	//Now instructionSet1 is the reversed one, and the others should be equals

	areEquivalent<const Instruction*>(instructionSet3, instructionSet1);
	areEquivalent<const Instruction*>(instructionSet1, instructionSetReverse);
	areEquivalent<const Instruction*>(instructionSet3, instructionSetReverse);
	areIdentical<const Instruction*>(instructionSet3, instructionSetReverse);
}

}
}

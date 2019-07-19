//===- llvm/unittest/Cheerp/CheerpPointerAnalyzerTest.cpp -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/DeterministicPtrMap.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"

namespace llvm {
namespace {

template<typename T, typename M>
void areIdentical(cheerp::DeterministicPtrMap<T, M>& A, cheerp::DeterministicPtrMap<T, M>& B)
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

template<typename T, typename M>
void areEquivalent(cheerp::DeterministicPtrMap<T, M>& A, cheerp::DeterministicPtrMap<T, M>& B)
{
	for (auto x : A)
	{
		EXPECT_EQ(1u, B.count(x.first));
		EXPECT_EQ(x.second, B[x.first]);
	}
	for (auto x : B)
	{
		EXPECT_EQ(1u, A.count(x.first));
		EXPECT_EQ(x.second, A[x.first]);
	}
}

using namespace cheerp;

TEST(CheerpTest, DeterministicPtrMapTest) {

	LLVMContext C;
	SMDiagnostic Err;

	std::unique_ptr<Module> M = parseIRFile( "test1.ll", Err, C );
	ASSERT_TRUE( M.get() );

	DeterministicPtrMap<const Function*, llvm::StringRef> functionMap;

	EXPECT_EQ( 0u, functionMap.size() );
	for (const Function& F : *M)
	{
		functionMap.erase(&F);
	}
	EXPECT_EQ( 0u, functionMap.size() );
	ASSERT_TRUE(functionMap.begin() == functionMap.end());
	for (const Function& F : *M)
	{
		functionMap.insert({&F, F.getName()});
	}
	for (const Function& F : *M)
	{
		EXPECT_EQ( 1u, functionMap.count(&F));
	}
	for (const Function& F : *M)
	{
		ASSERT_TRUE(functionMap.erase(&F));
		EXPECT_EQ( 0u, functionMap.count(&F));
		ASSERT_TRUE(functionMap.find(&F) == functionMap.end());
	}
	EXPECT_EQ( 0u, functionMap.size() );
	ASSERT_TRUE(functionMap.begin() == functionMap.end());
	for (const Function& F : *M)
	{
		functionMap.insert({&F, F.getName()});
	}
	DeterministicPtrMap<const Function*, llvm::StringRef> functionMap2;
	for (const Function& F : *M)
	{
		functionMap2.insert({&F, F.getName()});
	}
	for (const Function& F : *M)
	{
		functionMap2.insert({&F, F.getName()});
	}
	areIdentical<const Function*, llvm::StringRef>(functionMap, functionMap2);
	DeterministicPtrMap<const Function*, llvm::StringRef> functionMap3(functionMap2);
	areIdentical<const Function*, llvm::StringRef>(functionMap, functionMap3);

	for (const Function& F : *M)
	{
		if (rand()%2)
			continue;
		functionMap3.erase(&F);
	}
	for (const Function& F : *M)
	{
		functionMap3.insert({&F, F.getName()});
	}
	areEquivalent<const Function*, llvm::StringRef>(functionMap, functionMap3);
	functionMap.clear();
	EXPECT_EQ( 0u, functionMap.size() );
	for (const Function& F : *M)
	{
		functionMap3.erase(&F);
	}
	EXPECT_EQ( 0u, functionMap3.size() );

	std::vector<std::pair<const llvm::Instruction*, const BasicBlock*>> instructionList;
	for (const Function& F : *M)
	{
		for (const llvm::BasicBlock& BB : F)
		{
			for (const llvm::Instruction& I : BB)
			{
				instructionList.push_back({&I, I.getParent()});
			}
		}
	}

	DeterministicPtrMap<const Instruction*, const BasicBlock*> instructionMap1(instructionList.begin(), instructionList.end()), instructionMap3(instructionMap1);
	DeterministicPtrMap<const Instruction*, const BasicBlock*> instructionMapReverse(instructionList.rbegin(), instructionList.rend());
	instructionMapReverse.swap(instructionMap3);
	instructionMapReverse.swap(instructionMap1);
	//Now instructionMap3 is the reversed one, and the others should be equals

	areEquivalent<const Instruction*, const BasicBlock*>(instructionMap3, instructionMap1);
	areEquivalent<const Instruction*, const BasicBlock*>(instructionMap1, instructionMapReverse);
	areEquivalent<const Instruction*, const BasicBlock*>(instructionMap3, instructionMapReverse);
	areIdentical<const Instruction*, const BasicBlock*>(instructionMap1, instructionMapReverse);

	for (unsigned int x = 0; x<instructionList.size(); x++)
	{
		if (x%2)
			continue;
		instructionMap1.erase(instructionList[x].first);
		instructionMap1.insert({instructionList[x].first, instructionList[x].first->getParent()});
		instructionMap1.erase(instructionList[x].first);
		instructionMap3.erase(instructionList[x].first);
		instructionMapReverse.erase(instructionList[x].first);
	}

	for (unsigned int x =0; x<100; x++)
	{
		const Instruction* r = (const Instruction*)((unsigned long long)rand());
		instructionMap1.erase(r);
		instructionMap3.erase(r);
		instructionMapReverse.erase(r);
	}

	std::swap(instructionMapReverse, instructionMap3);
	std::swap(instructionMapReverse, instructionMap1);
	//Now instructionMap1 is the reversed one, and the others should be equals

	areEquivalent<const Instruction*, const BasicBlock*>(instructionMap3, instructionMap1);
	areEquivalent<const Instruction*, const BasicBlock*>(instructionMap1, instructionMapReverse);
	areEquivalent<const Instruction*, const BasicBlock*>(instructionMap3, instructionMapReverse);
	areIdentical<const Instruction*, const BasicBlock*>(instructionMap3, instructionMapReverse);
}

}
}

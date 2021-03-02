//===- llvm/unittest/Cheerp/CheerpPointerAnalyzerTest.cpp -----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/PointerAnalyzer.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IRReader/IRReader.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"

namespace llvm {
namespace {

const Value * getValueByName( const Function * F, StringRef name )
{
	for ( auto & BB : *F )
		for ( const Value & V : BB )
		{
			if ( V.getName() == name )
				return &V;
		}
	return nullptr;
}

using namespace cheerp;

TEST(CheerpTest, PointerAnalyzerTest) {

	LLVMContext C;
	SMDiagnostic Err;

	std::unique_ptr<Module> M = parseIRFile( "test/test1.ll", Err, C );
	ASSERT_TRUE( M.get() );
	
	const Function * webMain = M->getFunction("_Z7webMainv");
	const Function * f1 = M->getFunction("f1");
	const Function * f2 = M->getFunction("f2");
	const Function * f3 = M->getFunction("f3");
	const Function * f4 = M->getFunction("f4");
	const Function * f5 = M->getFunction("f5");

	ASSERT_TRUE( webMain );
	ASSERT_TRUE( f1 );
	ASSERT_TRUE( f2);
	ASSERT_TRUE( f3 );
	ASSERT_TRUE( f4 );
	ASSERT_TRUE( f5 );
	
	PointerAnalyzer PA;
	
	/** Check f1 **/
	{
		auto it = f1->arg_begin();
		const Argument * a = &*(it++);
		const Argument * obj = &*(it++);
		const Argument * obj2 = &*(it++);
		
		EXPECT_EQ( SPLIT_REGULAR, PA.getPointerKind(a) );
		EXPECT_EQ( SPLIT_REGULAR, PA.getPointerKind(obj) );
		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKind(obj2) );
	}
	
	/** Check f2 **/
	{
		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKindForReturn(f2) );
		
		auto it = f2->arg_begin();
		const Argument * obj = &*(it++);
		const Argument * b = &*(it++);
		
		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKind(obj) );
		EXPECT_EQ( SPLIT_REGULAR, PA.getPointerKind(b) );
	}
	
	/** Check f3 **/
	{
		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKindForReturn(f3) );
		
		auto it = f3->arg_begin();
		const Argument * obj = &*(it++);
		const Argument * b = &*(it++);
		
		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKind(obj) );
		EXPECT_EQ( SPLIT_REGULAR, PA.getPointerKind(b) );
	}
	
	/** Check f4 **/
	{
		auto it = f4->arg_begin();
		const Argument * p = &*(it++);
		const IntrinsicInst * d = dyn_cast<IntrinsicInst>(f4->begin()->begin());
		
		ASSERT_TRUE(d);
		ASSERT_EQ( Intrinsic::cheerp_downcast, d->getIntrinsicID() );
		
		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKind(p) );
		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKind(d) );
	}
	
	/** Check f5 **/
	{
		auto it = f4->arg_begin();
		const Argument * p = &*(it++);
		const IntrinsicInst * d = dyn_cast<IntrinsicInst>(f5->begin()->begin());
		
		ASSERT_TRUE(d);
		ASSERT_EQ( Intrinsic::cheerp_downcast, d->getIntrinsicID() );
		
		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKind(p) );
		EXPECT_EQ( SPLIT_REGULAR, PA.getPointerKind(d) );
	}
	
	/** Check webMain **/
	{
		const Value * a1 = getValueByName(webMain, "a1");
		const Value * a2 = getValueByName(webMain, "a2");
		const Value * a3 = getValueByName(webMain, "a3");
		const Value * b1 = getValueByName(webMain, "b1");
		const Value * c = getValueByName(webMain, "c");
		const Value * d1 = getValueByName(webMain, "d1");
		
		ASSERT_TRUE(a1);
		ASSERT_TRUE(a2);
		ASSERT_TRUE(a3);
		ASSERT_TRUE(b1);
		ASSERT_TRUE(c);
		ASSERT_TRUE(d1);

		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKind(a1) );
		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKind(a2) );
		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKind(a3) );
		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKind(b1) );
		EXPECT_EQ( SPLIT_REGULAR, PA.getPointerKind(c) );
		EXPECT_EQ( COMPLETE_OBJECT, PA.getPointerKind(d1) );
	}
	
#ifndef NDEBUG
	/** Just dump everything, and see if some assertion fails **/
	dumpAllPointers(*f1, PA);
	dumpAllPointers(*f2, PA);
	dumpAllPointers(*f3, PA);
	dumpAllPointers(*f4, PA);
	dumpAllPointers(*f5, PA);
	dumpAllPointers(*webMain, PA);
#endif
}

}
}

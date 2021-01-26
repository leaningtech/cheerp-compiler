//===-- I64Lowering.cpp - Cheerp helper -------------------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/InitializePasses.h"
#include "llvm/Cheerp/I64Lowering.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;


struct HighInt
{
	Value* high;
	Value* low;

	HighInt(Value* high, Value* low): high(high), low(low)
	{
	}
	HighInt(): HighInt(nullptr, nullptr)
	{
	}
	bool isNull()
	{
		return high == nullptr;
	}
};
struct I64LoweringVisitor: public InstVisitor<I64LoweringVisitor, HighInt>
{

	Module& M;
	LLVMContext& Ctx;
	Type* Int32Ty;
	Type* Int64Ty;
	Type* HighIntTy;
	bool Changed;
	SmallVector<Instruction*, 16> ToDelete;
	SmallVector<PHINode*, 16> DelayedPHIs;
	DenseMap<Value*, HighInt> Cache;

	I64LoweringVisitor(Module& M): M(M), Ctx(M.getContext()), Changed(false)
	{
		Int32Ty = IntegerType::get(Ctx, 32);
		Int64Ty = IntegerType::get(Ctx, 64);
		HighIntTy = ArrayType::get(Int32Ty, 2);
	}

	~I64LoweringVisitor()
	{
		removeToDelete();
	}

	void removeToDelete()
	{
		for (auto I: ToDelete)
		{
			I->replaceAllUsesWith(UndefValue::get(I->getType()));
			I->eraseFromParent();
		}
		ToDelete.clear();
	}

	HighInt visitPHINode(PHINode& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		unsigned Ninc = I.getNumIncomingValues();
		PHINode* PHILow = Builder.CreatePHI(Int32Ty, Ninc);
		PHINode* PHIHigh = Builder.CreatePHI(Int32Ty, Ninc);
		HighInt Self(PHIHigh, PHILow);
		// In order to handle recursive PHIs, we lazily build them here,
		// and finish them later at the end of the function
		DelayedPHIs.push_back(&I);
		return Self;
	}

	HighInt visitICmpInst(ICmpInst& cmpI)
	{
		if(!cmpI.getOperand(0)->getType()->isIntegerTy(64))
			return HighInt();

		Value* result = nullptr;
		IRBuilder<> Builder(&cmpI);
		Value *LHS = cmpI.getOperand(0);
		Value *RHS = cmpI.getOperand(1);

		HighInt HL = visitValue(LHS);
		HighInt HR = visitValue(RHS);

		llvm::Value *lhsHigh = HL.high;
		llvm::Value *lhsLow = HL.low;
		llvm::Value *rhsHigh = HR.high;
		llvm::Value *rhsLow = HR.low;

		switch(cmpI.getPredicate())
		{
			case ICmpInst::ICMP_SLT:
			case ICmpInst::ICMP_ULT:
				result = Builder.CreateOr(
					Builder.CreateICmp(cmpI.getPredicate(), lhsHigh, rhsHigh),
					Builder.CreateAnd(
						Builder.CreateICmp(llvm::ICmpInst::ICMP_EQ, lhsHigh, rhsHigh),
						Builder.CreateICmp(llvm::ICmpInst::ICMP_ULT, lhsLow, rhsLow)
				));
				break;
			case ICmpInst::ICMP_SLE:
			case ICmpInst::ICMP_ULE:
				result = Builder.CreateOr(
					Builder.CreateICmp(cmpI.getPredicate() == ICmpInst::ICMP_ULE ? ICmpInst::ICMP_ULT : ICmpInst::ICMP_SLT, lhsHigh, rhsHigh),
					Builder.CreateAnd(
						Builder.CreateICmp(llvm::ICmpInst::ICMP_EQ, lhsHigh, rhsHigh),
						Builder.CreateICmp(llvm::ICmpInst::ICMP_ULE, lhsLow, rhsLow)
					));
				break;
			case ICmpInst::ICMP_SGT:
			case ICmpInst::ICMP_UGT:
				result = Builder.CreateOr(
					Builder.CreateICmp(cmpI.getPredicate(), lhsHigh, rhsHigh),
					Builder.CreateAnd(
						Builder.CreateICmp(llvm::ICmpInst::ICMP_EQ, lhsHigh, rhsHigh),
						Builder.CreateICmp(llvm::ICmpInst::ICMP_UGT, lhsLow, rhsLow)
					));
				break;
			case ICmpInst::ICMP_SGE:
			case ICmpInst::ICMP_UGE:
				result = Builder.CreateOr(
					Builder.CreateICmp(cmpI.getPredicate() == ICmpInst::ICMP_UGE ? ICmpInst::ICMP_UGT : ICmpInst::ICMP_SGT, lhsHigh, rhsHigh),
					Builder.CreateAnd(
						Builder.CreateICmp(llvm::ICmpInst::ICMP_EQ, lhsHigh, rhsHigh),
						Builder.CreateICmp(llvm::ICmpInst::ICMP_UGE, lhsLow, rhsLow)
					));
				break;
			case ICmpInst::ICMP_EQ:
				result = Builder.CreateAnd(
					Builder.CreateICmp(llvm::ICmpInst::ICMP_EQ, lhsHigh, rhsHigh),
					Builder.CreateICmp(llvm::ICmpInst::ICMP_EQ, lhsLow, rhsLow)
				);
				break;
			case ICmpInst::ICMP_NE:
				result = Builder.CreateOr(
					Builder.CreateICmp(llvm::ICmpInst::ICMP_NE, lhsHigh, rhsHigh),
					Builder.CreateICmp(llvm::ICmpInst::ICMP_NE, lhsLow, rhsLow)
				);
				break;
			default: llvm_unreachable("unexpected comparison type");
		}

		cmpI.replaceAllUsesWith(result);
		ToDelete.push_back(&cmpI);
		Changed = true;
		return HighInt();
	}
	HighInt visitSIToFPInst(SIToFPInst& I)
	{
		return visitIToFPInst(I, I.getOpcode());
	}
	HighInt visitUIToFPInst(UIToFPInst& I)
	{
		return visitIToFPInst(I, I.getOpcode());
	}
	HighInt visitIToFPInst(Instruction& I, int opcode)
	{
		if(!I.getOperand(0)->getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		HighInt H = visitValue(I.getOperand(0));
		Type *Ty = I.getType();
		H.low = Builder.CreateUIToFP(H.low, Ty);

		if(opcode == Instruction::SIToFP)
			H.high = Builder.CreateSIToFP(H.high, Ty);
		else
			H.high = Builder.CreateUIToFP(H.high, Ty);

		Value *c = llvm::ConstantFP::get(Ty, (double)(1LL << 32));
		Value* result = Builder.CreateFMul(c, H.high);
		result = Builder.CreateFAdd(result, H.low);

		I.replaceAllUsesWith(result);
		ToDelete.push_back(&I);
		Changed = true;
		return HighInt();
	}


	HighInt visitShl(BinaryOperator& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		// { h, l } << N =>
		// { (N >= 32) ? l << (N - 32) : (h << N) | (l >> (32 - N)),
		//   (N >= 32) ? 0 : l << N }
		IRBuilder<> Builder(&I);
		HighInt LHS = visitValue(I.getOperand(0));
		// NOTE: we only use the low part
		Value *RHS = visitValue(I.getOperand(1)).low;

		Value* NGE32 = Builder.CreateICmpUGE(RHS, Builder.getInt32(32));
		Value* NMinus32 = Builder.CreateSub(RHS, Builder.getInt32(32));
		Value* _32MinusN = Builder.CreateSub(Builder.getInt32(32), RHS);

		// Compute values for N >= 32, case A
		Value* shlHForCaseA = Builder.CreateShl(LHS.low, NMinus32);
		Value* shlLForCaseA = Builder.getInt32(0);

		// Compute values for N < 32, case B
		Value* shlHForCaseB = Builder.CreateOr(
			Builder.CreateShl(LHS.high, RHS),
			Builder.CreateLShr(LHS.low, _32MinusN)
		);
		Value* shlLForCaseB = Builder.CreateShl(LHS.low, RHS);

		// Compute final values
		Value* shlH = Builder.CreateSelect(NGE32, shlHForCaseA, shlHForCaseB);
		Value* shlL = Builder.CreateSelect(NGE32, shlLForCaseA, shlLForCaseB);

		Value* NEQ0 = Builder.CreateICmpEQ(RHS, Builder.getInt32(0));
		shlH = Builder.CreateSelect(NEQ0, LHS.high, shlH);
		shlL = Builder.CreateSelect(NEQ0, LHS.low, shlL);

		ToDelete.push_back(&I);
		Changed = true;
		return HighInt(shlH, shlL);
	}

	HighInt visitAShr(BinaryOperator& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		// { h, l } >> N =>
		// { ( N >= 32) ? 0 : h >> N ,
		//   ( N >= 32) ? h >> (N - 32) : h << (32 - N) | L >> N }
		IRBuilder<> Builder(&I);
		HighInt LHS = visitValue(I.getOperand(0));
		Value *RHS = visitValue(I.getOperand(1)).low;

		Value* NGE32 = Builder.CreateICmpUGE(RHS, Builder.getInt32(32));
		Value* NMinus32 = Builder.CreateSub(RHS, Builder.getInt32(32));
		Value* _32MinusN = Builder.CreateSub(Builder.getInt32(32), RHS);

		// Compute values for N >= 32, case A
		Value* shrHForCaseA, *shrLForCaseA;
		shrHForCaseA = Builder.CreateAShr(LHS.high, Builder.getInt32(31));

		shrLForCaseA = Builder.CreateAShr(LHS.high, NMinus32);

		// Compute values for N < 32, case B
		Value* shrHForCaseB;
		shrHForCaseB = Builder.CreateAShr(LHS.high, RHS);
		Value* shrLForCaseB = Builder.CreateOr(
			Builder.CreateShl(LHS.high, _32MinusN),
			Builder.CreateLShr(LHS.low, RHS)
		);

		// Compute final values
		Value* shrH = Builder.CreateSelect(NGE32, shrHForCaseA, shrHForCaseB);
		Value* shrL = Builder.CreateSelect(NGE32, shrLForCaseA, shrLForCaseB);

		Value* NEQ0 = Builder.CreateICmpEQ(RHS, Builder.getInt32(0));
		shrH = Builder.CreateSelect(NEQ0, LHS.high, shrH);
		shrL = Builder.CreateSelect(NEQ0, LHS.low, shrL);

		ToDelete.push_back(&I);
		Changed = true;
		return HighInt(shrH, shrL);
	}
	HighInt visitLShr(BinaryOperator& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		// { h, l } >> N =>
		// { ( N >= 32) ? 0 : h >> N ,
		//   ( N >= 32) ? h >> (N - 32) : h << (32 - N) | L >> N }
		IRBuilder<> Builder(&I);
		HighInt LHS = visitValue(I.getOperand(0));
		Value *RHS = visitValue(I.getOperand(1)).low;

		Value* NGE32 = Builder.CreateICmpUGE(RHS, Builder.getInt32(32));
		Value* NMinus32 = Builder.CreateSub(RHS, Builder.getInt32(32));
		Value* _32MinusN = Builder.CreateSub(Builder.getInt32(32), RHS);

		// Compute values for N >= 32, case A
		Value* shrHForCaseA, *shrLForCaseA;
		shrHForCaseA = Builder.getInt32(0);

		shrLForCaseA = Builder.CreateLShr(LHS.high, NMinus32);

		// Compute values for N < 32, case B
		Value* shrHForCaseB;
		shrHForCaseB = Builder.CreateLShr(LHS.high, RHS);
		Value* shrLForCaseB = Builder.CreateOr(
			Builder.CreateShl(LHS.high, _32MinusN),
			Builder.CreateLShr(LHS.low, RHS)
		);

		// Compute final values
		Value* shrH = Builder.CreateSelect(NGE32, shrHForCaseA, shrHForCaseB);
		Value* shrL = Builder.CreateSelect(NGE32, shrLForCaseA, shrLForCaseB);

		Value* NEQ0 = Builder.CreateICmpEQ(RHS, Builder.getInt32(0));
		shrH = Builder.CreateSelect(NEQ0, LHS.high, shrH);
		shrL = Builder.CreateSelect(NEQ0, LHS.low, shrL);

		ToDelete.push_back(&I);
		Changed = true;
		return HighInt(shrH, shrL);
	}
	HighInt visitAnd(BinaryOperator& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		HighInt LHS = visitValue(I.getOperand(0));
		HighInt RHS = visitValue(I.getOperand(1));

		Value *high = Builder.CreateAnd(LHS.high, RHS.high, "andHigh");
		Value *low = Builder.CreateAnd(LHS.low, RHS.low, "andLow");

		ToDelete.push_back(&I);
		Changed = true;
		return HighInt(high, low);
	}

	HighInt visitOr(BinaryOperator& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		HighInt LHS = visitValue(I.getOperand(0));
		HighInt RHS = visitValue(I.getOperand(1));

		Value *high = Builder.CreateOr(LHS.high, RHS.high, "orHigh");
		Value *low = Builder.CreateOr(LHS.low, RHS.low, "orLow");


		ToDelete.push_back(&I);
		Changed = true;
		return HighInt(high, low);
	}

	HighInt visitXor(BinaryOperator& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		HighInt LHS = visitValue(I.getOperand(0));
		HighInt RHS = visitValue(I.getOperand(1));

		Value *high = Builder.CreateXor(LHS.high, RHS.high, "orHigh");
		Value *low = Builder.CreateXor(LHS.low, RHS.low, "orLow");


		ToDelete.push_back(&I);
		Changed = true;
		return HighInt(high, low);
	}

	HighInt visitAdd(BinaryOperator& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		HighInt LHS = visitValue(I.getOperand(0));
		HighInt RHS = visitValue(I.getOperand(1));

		Value *highNormal = Builder.CreateAdd(LHS.high, RHS.high, "highNormal");
		Value *low = Builder.CreateAdd(LHS.low, RHS.low, "low");

		Value *one = Builder.getInt32(1);
		Value *highPlusOne = Builder.CreateAdd(highNormal, one, "highPlusOne");

		// Check if the low bits of the highint will overflow, and add one to the
		// high bits if it does overflow.
		Value *max = Builder.getInt32(0xffffffff);
		Value *difference = Builder.CreateSub(max, RHS.low);
		Value *overflow = Builder.CreateICmpUGT(LHS.low, difference);
		Value *high = Builder.CreateSelect(overflow, highPlusOne, highNormal, "addSel");


		ToDelete.push_back(&I);
		Changed = true;
		return HighInt(high, low);
	}

	HighInt doSub(IRBuilder<>& Builder, HighInt LHS, HighInt RHS)
	{
		llvm::Value *highNormal = Builder.CreateSub(LHS.high, RHS.high, "sub");
		llvm::Value *low = Builder.CreateSub(LHS.low, RHS.low, "sub");

		llvm::Value *one = Builder.getInt32(1);
		llvm::Value *highMinusOne = Builder.CreateSub(highNormal, one, "sub");

		llvm::Value *overflow = Builder.CreateICmpUGT(RHS.low, LHS.low);
		llvm::Value *high = Builder.CreateSelect(overflow, highMinusOne, highNormal, "subSel");

		return HighInt(high, low);
	}

	HighInt visitSub(BinaryOperator& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		HighInt LHS = visitValue(I.getOperand(0));
		HighInt RHS = visitValue(I.getOperand(1));

		HighInt Res = doSub(Builder, LHS, RHS);

		ToDelete.push_back(&I);
		Changed = true;
		return Res;
	}

	HighInt visitMul(BinaryOperator& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		//    Split the numbers according to this:
		//    low bits                                               high bits
		//    0000000000000001111111111111111122222222222222223333333333333333
		//    0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef
		//    \----a4/b4-----/\-----a3/b3----/
		//    \--------RHS.low/LHS.low---------/\---------RHS.high/LHS.high------/

		//    Then multiply each of {a3,a4} with each of {b3,b4} -> 4 i32 mult
		//    And then RHS.low * LHS.high + RHS.high * LHS.low       -> 2 i32 mult
		IRBuilder<> Builder(&I);
		HighInt LHS = visitValue(I.getOperand(0));
		HighInt RHS = visitValue(I.getOperand(1));

		llvm::Value *a3 = Builder.CreateLShr(LHS.low, Builder.getInt32(16));
		llvm::Value *a4 = Builder.CreateAnd(LHS.low, Builder.getInt32(0xffff));

		llvm::Value *b3 = Builder.CreateLShr(RHS.low, Builder.getInt32(16));
		llvm::Value *b4 = Builder.CreateAnd(RHS.low, Builder.getInt32(0xffff));

		llvm::Value *high = Builder.CreateAdd(
			Builder.CreateAdd(
					Builder.CreateMul(LHS.high, RHS.low),
					Builder.CreateMul(LHS.low, RHS.high)
			),
			Builder.CreateMul(a3, b3)
		);

		llvm::Value *lh1 = Builder.CreateMul(a3, b4);
		llvm::Value *lh2 = Builder.CreateMul(b3, a4);

		llvm::Value *lowHigh = Builder.CreateAdd(lh1, lh2);
		llvm::Value *lowLow = Builder.CreateMul(a4, b4);

		// Check if the most left bits of the highint's lower part bits will
		// overflow, and add one to the high bits of the highint if it does.
		llvm::Value *diff = Builder.CreateSub(Builder.getInt32(0xffffffff), lh2);
		llvm::Value *overflow1 = Builder.CreateICmpUGT(lh1, diff);
		llvm::Value *two16 = Builder.getInt32(65536);
		llvm::Value *highPlusTwo16 = Builder.CreateAdd(high, two16);
		high = Builder.CreateSelect(overflow1, highPlusTwo16, high);

		llvm::Value *l1 = Builder.CreateShl(lowHigh, Builder.getInt32(16));
		llvm::Value *l2 = lowLow;

		llvm::Value *diff2 = Builder.CreateSub(Builder.getInt32(0xffffffff), l2);
		llvm::Value *overflow2 = Builder.CreateICmpUGT(l1, diff2);
		high = Builder.CreateAdd(
			high,
			Builder.CreateZExt(overflow2, Int32Ty)
		);

		high = Builder.CreateAdd(
			high,
			Builder.CreateLShr(lowHigh, Builder.getInt32(16))
		);

		llvm::Value *low = Builder.CreateAdd(l1, l2);

		ToDelete.push_back(&I);
		Changed = true;
		return HighInt(high, low);
	}


	HighInt visitFPToI(Instruction& I, int opcode)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		Value* Elt = I.getOperand(0);

		// 1. if number is negative: multiply by -1
		Value *negative = Builder.CreateFCmp(llvm::CmpInst::FCMP_OLT,
			Elt, ConstantFP::get(Elt->getType(), (double) 0), "cmp");
		Value *minusOne = llvm::ConstantFP::get(Elt->getType(), (double) -1);
		Value *EltNegated = Builder.CreateFMul(Elt, minusOne);
		Elt = Builder.CreateSelect(negative, EltNegated, Elt);
		// 2. div high, mod low
		Value *c = llvm::ConstantFP::get(Elt->getType(), (double)(1LL << 32));
		Value *high = Builder.CreateFDiv(Elt, c);
		high = Builder.CreateFPToUI(high, Int32Ty, "conv");
		Value *low = Builder.CreateFRem(Elt, c);
		low = Builder.CreateFPToUI(low, Int32Ty, "conv");
		// 3. if number was negative: invert number
		HighInt Res(high, low);
		HighInt Zero(Builder.getInt32(0), Builder.getInt32(0));
		HighInt ResInv = doSub(Builder, Zero, Res);

		// Emit unary minus with doSub so we handle overflow cases etc.
		Res.high = Builder.CreateSelect(negative, ResInv.high, Res.high);
		Res.low = Builder.CreateSelect(negative, ResInv.low, Res.low);

		ToDelete.push_back(&I);
		Changed = true;
		return Res;
	}

	HighInt visitFPToSI(FPToSIInst& I)
	{
		return visitFPToI(I, I.getOpcode());
	}

	HighInt visitFPToUI(FPToUIInst& I)
	{
		return visitFPToI(I, I.getOpcode());
	}

	HighInt visitSExt(SExtInst& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		Value* low = I.getOperand(0);
		if (low->getType()->getIntegerBitWidth() < 32)
		{
			low = Builder.CreateSExt(low, Int32Ty);
		}
		Value* high = Builder.CreateAShr(low, 31);
		HighInt Res(high, low);

		ToDelete.push_back(&I);
		Changed = true;
		return Res;
	}
	HighInt visitZExt(ZExtInst& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		Value* low = I.getOperand(0);
		if (low->getType()->getIntegerBitWidth() < 32)
		{
			low = Builder.CreateZExt(low, Int32Ty);
		}
		Value* high = Builder.getInt32(0);
		HighInt Res(high, low);

		ToDelete.push_back(&I);
		Changed = true;
		return Res;
	}
	HighInt visitTrunc(TruncInst& I)
	{
		if(!I.getOperand(0)->getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		HighInt H = visitValue(I.getOperand(0));
		Value* Res = H.low;
		if (Res->getType() != I.getType())
		{
			Res = Builder.CreateTrunc(Res, I.getType());
		}

		I.replaceAllUsesWith(Res);
		ToDelete.push_back(&I);
		Changed = true;
		return HighInt();
	}
	HighInt visitPtrToInt(PtrToIntInst &I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		Value* ptr = I.getOperand(0);
		Value* low = Builder.CreatePtrToInt(ptr, Int32Ty);
		Value* high = Builder.getInt32(0);
		HighInt Res(high, low);

		ToDelete.push_back(&I);
		Changed = true;
		return Res;
	}
	HighInt visitIntToPtr(IntToPtrInst &I)
	{
		if(!I.getOperand(0)->getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		HighInt H = visitValue(I.getOperand(0));
		Value* Res = H.low;
		Res = Builder.CreateIntToPtr(Res, I.getType());

		I.replaceAllUsesWith(Res);
		ToDelete.push_back(&I);
		Changed = true;
		return HighInt();
	}
	HighInt visitSelectInst(SelectInst& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		HighInt RHS = visitValue(I.getTrueValue());
		HighInt LHS = visitValue(I.getFalseValue());
		Value* high = Builder.CreateSelect(I.getCondition(), RHS.high, LHS.high);
		Value* low = Builder.CreateSelect(I.getCondition(), RHS.low, LHS.low);
		HighInt Res(high, low);

		ToDelete.push_back(&I);
		Changed = true;
		return Res;
	}

	HighInt visitCallInst(CallInst& I)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		Function* calledFunc = I.getCalledFunction();
		if (calledFunc == nullptr)
			return HighInt();

		Intrinsic::ID intrinsicId = calledFunc->getIntrinsicID();

		HighInt Res;
		IRBuilder<> Builder(&I);
		auto ctlz_cttz_impl = [&Builder, &Res, intrinsicId, &I, this](Value* firstHalf, Value* secondHalf)
		{
				Function* Intr = Intrinsic::getDeclaration(&M, intrinsicId, Int32Ty);
				Value* Cond = Builder.CreateICmpEQ(firstHalf, ConstantInt::get(Int32Ty, 0));
				Value* Args1[] = { firstHalf, I.getOperand(1) };
				Value* Args2[] = { secondHalf, I.getOperand(1) };
				Value* Call1 = Builder.CreateCall(Intr, Args1);
				Value* Call2 = Builder.CreateCall(Intr, Args2);
				Value* Sum = Builder.CreateAdd(ConstantInt::get(Int32Ty, 32), Call2);
				Res.high = ConstantInt::get(Int64Ty, 0);
				Res.low = Builder.CreateSelect(Cond, Sum, Call1, "sel.ctlz");
		};
		switch (intrinsicId)
		{
			case Intrinsic::ctlz:
			{
				HighInt Arg = visitValue(I.getOperand(0));
				ctlz_cttz_impl(Arg.high, Arg.low);
				break;
			}
			case Intrinsic::cttz:
			{
				HighInt Arg = visitValue(I.getOperand(0));
				ctlz_cttz_impl(Arg.low, Arg.high);
				break;
			}
			default:
			{
				report_fatal_error("Unsupported 64 bit intrinsic");
				return HighInt();
			}
		}
		ToDelete.push_back(&I);
		Changed = true;
		return Res;
	}

	HighInt visitDivRem(Instruction& I, const char* fname)
	{
		if(!I.getType()->isIntegerTy(64))
			return HighInt();

		IRBuilder<> Builder(&I);
		HighInt LHS = visitValue(I.getOperand(0));
		HighInt RHS = visitValue(I.getOperand(1));

		llvm::Type *ArgTypes[] = { Int32Ty, Int32Ty, Int32Ty, Int32Ty };
		llvm::FunctionType *FuncTy = llvm::FunctionType::get(Int32Ty, ArgTypes, false);
		llvm::Function* Func = cast<Function>(M.getOrInsertFunction(fname, FuncTy).getCallee());

		llvm::Value *Args[] = {
			LHS.low,
			LHS.high,
			RHS.low,
			RHS.high
		};

		Value* Low = Builder.CreateCall(Func, Args);
		GlobalVariable* Sret = cast<GlobalVariable>(M.getOrInsertGlobal("cheerpSretSlot", Int32Ty));
		Value* High = Builder.CreateLoad(Sret); 

		ToDelete.push_back(&I);
		Changed = true;
		return HighInt(High, Low);
	}

	HighInt visitSRem(BinaryOperator& I)
	{
		return visitDivRem(I, "__modti3");
	}
	HighInt visitURem(BinaryOperator& I)
	{
		return visitDivRem(I, "__umodti3");
	}
	HighInt visitSDiv(BinaryOperator& I)
	{
		return visitDivRem(I, "__divti3");
	}
	HighInt visitUDiv(BinaryOperator& I)
	{
		return visitDivRem(I, "__udivti3");
	}

	HighInt visitInstruction (Instruction &I)
	{
		if(I.getType()->isIntegerTy(64))
		{
			llvm::errs() << "TODO: Lower 64-bit" << I << "\n";
		}
		return HighInt();
	}

	HighInt visit(Instruction& I)
	{
		if (Cache.count(&I))
			return HighInt();
		HighInt ret = InstVisitor::visit(I);

		if (!ret.isNull())
		{
			Cache.insert(std::make_pair(&I, ret));
		}
		return ret;
	}
	void visit(Function& F)
	{
		InstVisitor::visit(F);
		for (PHINode* P: DelayedPHIs)
		{
			HighInt Self = visitValue(P);
			PHINode* PHILow = cast<PHINode>(Self.low);
			PHINode* PHIHigh = cast<PHINode>(Self.high);
			SmallVector<HighInt, 2> IncomingHigh;
			for (Value* Inc: P->incoming_values())
			{
				IncomingHigh.push_back(visitValue(Inc));
			}
			unsigned Ninc = P->getNumIncomingValues();
			for (unsigned i = 0; i < Ninc; ++i)
			{
				BasicBlock* BB = P->getIncomingBlock(i);
				PHILow->addIncoming(IncomingHigh[i].low, BB);
				PHIHigh->addIncoming(IncomingHigh[i].high, BB);
			}

			ToDelete.push_back(P);
		}
	}
	using InstVisitor::visit;

	HighInt visitValue (Value *V)
	{
		if (!V->getType()->isIntegerTy(64))
			return HighInt();

		auto it = Cache.find(V);

		if (it != Cache.end())
			return it->second;

		HighInt ret;
		if (Instruction* I = dyn_cast<Instruction>(V))
		{
			ret = visit(*I);
			return ret;
		}
		else if (ConstantInt* I = dyn_cast<ConstantInt>(V))
		{
			uint64_t i = I->getZExtValue();
			uint32_t low = i;
			uint32_t high = i >> 32;
			ret = HighInt(ConstantInt::get(Int32Ty, high), ConstantInt::get(Int32Ty, low));
		}
		else if (Constant* C = dyn_cast<Constant>(V))
		{
			Constant* CLow = ConstantExpr::getTrunc(C, Int32Ty);
			Constant* CHigh = ConstantExpr::getLShr(C, ConstantInt::get(Int64Ty, 32));
			CHigh = ConstantExpr::getTrunc(CHigh, Int32Ty);
			ret = HighInt(CLow, CHigh);
		}

		if (!ret.isNull())
		{
			Cache.insert(std::make_pair(V, ret));
		}
		else
		{
			llvm::errs()<<"Unhandled value: "<<*V<<"\n";
		}

		return ret;
	}
};

namespace cheerp
{

bool I64Lowering::runOnFunction(Function& F)
{
	bool lowerAsmJSSection = LinearOutput.getValue() == AsmJs;
	bool lowerGenericJSSection = !UseBigInts;
	bool asmjs = F.getSection() == StringRef("asmjs");
	if ((!lowerAsmJSSection && asmjs) || (!lowerGenericJSSection && !asmjs))
		return false;

	bool Changed = false;

	I64LoweringVisitor Visitor(*F.getParent());

	Visitor.visit(F);

	Changed = Visitor.Changed;

	return Changed;
}

StringRef I64LoweringPass::getPassName() const {
	return "I64LoweringPass";
}

bool I64LoweringPass::runOnFunction(Function& F)
{
	return Lowerer.runOnFunction(F);
}

char I64LoweringPass::ID = 0;

FunctionPass *createI64LoweringPass() { return new I64LoweringPass(); }

}

using namespace cheerp;

INITIALIZE_PASS_BEGIN(I64LoweringPass, "I64Lowering", "Converts 64-bit integer operations into 32-bit ones",
                      false, false)
INITIALIZE_PASS_END(I64LoweringPass, "I64Lowering", "Converts 64-bit integer operations into 32-bit ones",
                    false, false)

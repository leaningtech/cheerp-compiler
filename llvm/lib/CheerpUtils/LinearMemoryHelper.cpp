//===-- LinearMemoryHelper.cpp - The Cheerp JavaScript generator ----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the Apache License v2.0 with LLVM Exceptions.
// See LICENSE.TXT for details.
//
// Copyright 2017-2023 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/LinearMemoryHelper.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Module.h"
#include "llvm/Cheerp/Utility.h"

using namespace cheerp;
using namespace llvm;
typedef LinearMemoryHelperInitializer::FunctionAddressMode FunctionAddressMode;

void LinearMemoryHelper::compileConstantAsBytes(const Constant* c, bool asmjs, ByteListener* listener, int32_t offset) const
{
	const auto& targetData = module->getDataLayout();
	if(const ConstantDataSequential* CD = dyn_cast<ConstantDataSequential>(c))
	{
		assert(offset==0);
		for(uint32_t i=0;i<CD->getNumElements();i++)
			compileConstantAsBytes(CD->getElementAsConstant(i), asmjs, listener);
	}
	else if(const UndefValue* U = dyn_cast<UndefValue>(c))
	{
		assert(offset==0);
		uint32_t size = targetData.getTypeAllocSize(U->getType());
		for (uint32_t i = 0; i < size; i++)
			listener->addByte(0);
	}
	else if(isa<ConstantArray>(c))
	{
		assert(offset==0);
		for(uint32_t i=0;i<c->getNumOperands();i++)
			compileConstantAsBytes(cast<Constant>(c->getOperand(i)), asmjs, listener);
	}
	else if(const ConstantStruct* CS = dyn_cast<ConstantStruct>(c))
	{
		assert(offset==0);
		int64_t currentOffset = 0;
		StructType* ST = CS->getType();
		for(int64_t i=0;i<c->getNumOperands();i++)
		{
			const StructLayout* SL = targetData.getStructLayout( ST );
			int64_t elementOffset =  SL->getElementOffset(i);
			Type* elementType =  ST->getElementType(i);
			int64_t elementSize = targetData.getTypeAllocSize(elementType);
			for (int64_t p = currentOffset; p < elementOffset; ++p)
				listener->addByte(0);
			currentOffset = elementOffset + elementSize;
			compileConstantAsBytes(cast<Constant>(c->getOperand(i)), asmjs, listener);
		}
		int64_t structSize = targetData.getTypeAllocSize(ST);
		for (int64_t p = currentOffset; p < structSize; ++p)
			listener->addByte(0);
	}
	else if(const ConstantFP* f=dyn_cast<ConstantFP>(c))
	{
		assert(offset==0);
		const APFloat& flt = f->getValueAPF();
		const APInt& integerRepresentation = flt.bitcastToAPInt();
		uint64_t val = integerRepresentation.getLimitedValue();
		uint32_t bitWidth = integerRepresentation.getBitWidth();
		for(uint32_t i=0;i<bitWidth;i+=8)
			listener->addByte((val>>i)&255);
	}
	else if(const ConstantInt* i=dyn_cast<ConstantInt>(c))
	{
		const APInt& integerRepresentation = i->getValue();
		uint64_t val = integerRepresentation.getLimitedValue() + offset;
		uint32_t bitWidth = integerRepresentation.getBitWidth();
		for(uint32_t i=0;i<bitWidth;i+=8)
			listener->addByte((val>>i)&255);
	}
	else if (asmjs)
	{
		if(const ConstantAggregateZero* Z = dyn_cast<ConstantAggregateZero>(c))
		{
			assert(offset==0);
			uint32_t size = targetData.getTypeAllocSize(Z->getType());
			for (uint32_t i = 0; i < size; i++)
				listener->addByte(0);
		}
		else if(dyn_cast<ConstantPointerNull>(c))
		{
			for(uint32_t i=0;i<32;i+=8)
				listener->addByte((offset>>i)&255);
		}
		else if(const Function* F = dyn_cast<Function>(c))
		{
			assert(offset==0);
			uint32_t addr = 0;
			if (functionHasAddress(F))
			{
				addr = getFunctionAddress(F);
			}
			for(uint32_t i=0;i<32;i+=8)
				listener->addByte((addr>>i)&255);
			// Encode the offset in the table as the offset, treating the table section as a single global
			if(WasmSharedModule)
				listener->addRelocation(nullptr, addr);
		}
		else if(isa<ConstantExpr>(c))
		{
			const ConstantExpr* ce = cast<ConstantExpr>(c);
			switch(ce->getOpcode())
			{
				case Instruction::GetElementPtr:
				{
					Type* curTy = ce->getOperand(0)->getType();
					SmallVector< const Value *, 8 > indices ( std::next(ce->op_begin()), ce->op_end() );
					for (uint32_t i=0; i<indices.size(); i++)
					{
						uint32_t index = cast<ConstantInt>(indices[i])->getZExtValue();
						if (StructType* ST = dyn_cast<StructType>(curTy))
						{
							const StructLayout* SL = targetData.getStructLayout( ST );
							offset += SL->getElementOffset(index);
							curTy = ST->getElementType(index);
						}
						else
						{
							offset += index*targetData.getTypeAllocSize(getElementType(curTy, cast<const GEPOperator>(ce)->getSourceElementType()));
							curTy = getElementType(curTy, cast<const GEPOperator>(ce)->getSourceElementType());
						}
					}
					compileConstantAsBytes(ce->getOperand(0), asmjs, listener, offset);
					break;
				}
				case Instruction::IntToPtr:
				{
					assert(isa<ConstantInt>(ce->getOperand(0)));
					const ConstantInt* i = cast<ConstantInt>(ce->getOperand(0));
					const APInt& integerRepresentation = i->getValue();
					uint64_t val = integerRepresentation.getLimitedValue() + offset;
					for(uint32_t i=0;i<32;i+=8)
						listener->addByte((val>>i)&255);
					break;
				}
				case Instruction::PtrToInt:
				case Instruction::BitCast:
				{
					compileConstantAsBytes(ce->getOperand(0), asmjs, listener, offset);
					break;
				}
				default:
					// TODO: It could make sense to emit the right number of zeroes anyway
					llvm::errs() << "warning: Unsupported constant expr in asm.js module :" << ce->getOpcodeName() << '\n';
			}
		}
		else if(isa<GlobalVariable>(c))
		{
			const GlobalVariable* g = cast<GlobalVariable>(c);
			if (globalAddresses.count(g) != 1)
			{
				llvm::errs() << "global variable not found:" << g->getName() << "\n";
				llvm::report_fatal_error("please report a bug");
			}
			uint32_t val = globalAddresses.at(g)+offset;
			for(uint32_t i=0;i<32;i+=8)
				listener->addByte((val>>i)&255);
			if(WasmSharedModule)
				listener->addRelocation(g, offset);
		}
		else
		{
			// TODO: It could make sense to emit the right number of zeroes anyway
			llvm::errs() << "Unsupported constant type for bytes in asm.js module :" << *c->getType() << "\n";
		}
	}
	else
	{
		// TODO: It could make sense to emit the right number of zeroes anyway
		llvm::errs() << "Unsupported constant type for bytes " << *c << "\n";
	}
}

bool LinearMemoryHelper::isZeroInitializer(const llvm::Constant* c) const
{
	if (const ConstantDataSequential* CD = dyn_cast<ConstantDataSequential>(c))
	{
		for (uint32_t i = 0; i < CD->getNumElements(); i++) {
			if (!isZeroInitializer(CD->getElementAsConstant(i)))
				return false;
		}
		return true;
	}

	if (isa<UndefValue>(c))
		return true;

	if (isa<ConstantArray>(c) || isa<ConstantStruct>(c))
	{
		for (uint32_t i = 0; i < c->getNumOperands(); i++) {
			if (!isZeroInitializer(cast<Constant>(c->getOperand(i))))
				return false;
		}
		return true;
	}

	if (const ConstantFP* f=dyn_cast<ConstantFP>(c))
	{
		const APFloat& flt = f->getValueAPF();
		const APInt& integerRepresentation = flt.bitcastToAPInt();
		uint64_t val = integerRepresentation.getLimitedValue();
		uint32_t bitWidth = integerRepresentation.getBitWidth();
		for (uint32_t i = 0; i < bitWidth; i += 8) {
			if ((val>>i)&255)
				return false;
		}
		return true;
	}

	if (const ConstantInt* i=dyn_cast<ConstantInt>(c))
	{
		const APInt& integerRepresentation = i->getValue();
		uint64_t val = integerRepresentation.getLimitedValue();
		uint32_t bitWidth = integerRepresentation.getBitWidth();
		for (uint32_t i = 0; i < bitWidth; i += 8) {
			if ((val>>i)&255)
				return false;
		}
		return true;
	}

	if(isa<ConstantAggregateZero>(c) || isa<ConstantPointerNull>(c))
		return true;

	if(const Function* F = dyn_cast<Function>(c))
	{
		if (!functionHasAddress(F))
		{
			return true;
		}
		uint32_t addr = getFunctionAddress(F);
		return addr == 0;
	}

	if(isa<ConstantExpr>(c))
		return false;

	if(isa<GlobalVariable>(c))
	{
		// Since globals do not start at offset zero (heapStart is non-zero)
		// the address of a global variable can never be zero.
		assert(heapStart);
		return false;
	}

#ifndef NDEBUG
	c->dump();
#endif
	llvm_unreachable("Unsupported constant type");
}

const llvm::Value* LinearMemoryHelper::compileGEP(const llvm::Value* p, GepListener* listener, const PointerAnalyzer* PA) const
{
	return compileGEP(module, p, listener, PA);
}

int64_t LinearMemoryHelper::compileGEPOperand(const llvm::Value* idxVal, uint32_t size, GepListener* listener, bool invert)
{
	if (const ConstantInt* idx = dyn_cast<ConstantInt>(idxVal))
	{
		int64_t ret = idx->getSExtValue()*size;
		if(invert)
			ret = -ret;
		return ret;
	}
	else
	{
		if (isa<Instruction>(idxVal) && listener->isInlineable(idxVal))
		{
			const Instruction* idxI = cast<Instruction>(idxVal);
			// We can look into Adds and Subs to merge away more constants
			if (idxI->getOpcode() == Instruction::Add)
			{
				int64_t ret = 0;
				ret += compileGEPOperand(idxI->getOperand(0), size, listener, invert);
				ret += compileGEPOperand(idxI->getOperand(1), size, listener, invert);
				return ret;
			}
			else if (listener->hasSubValue() && idxI->getOpcode() == Instruction::Sub)
			{
				int64_t ret = 0;
				ret += compileGEPOperand(idxI->getOperand(0), size, listener, invert);
				ret += compileGEPOperand(idxI->getOperand(1), size, listener, !invert);
				return ret;
			}
			// We can also look into Shls and Muls and merge a constant into the size
			else if(idxI->getOpcode() == Instruction::Shl)
			{
				if(isa<ConstantInt>(idxI->getOperand(1)))
				{
					uint32_t shiftAmount = cast<ConstantInt>(idxI->getOperand(1))->getZExtValue();
					return compileGEPOperand(idxI->getOperand(0), (1<<shiftAmount)*size, listener, invert);
				}
			}
			else if(idxI->getOpcode() == Instruction::Mul)
			{
				const llvm::Value* first = idxI->getOperand(0);
				const llvm::Value* second = idxI->getOperand(1);

				if(isa<ConstantInt>(first))
					std::swap(first, second);

				//Now if there is a Constant, it will be in the second operand
				if(const ConstantInt* C = dyn_cast<ConstantInt>(second))
				{
					int32_t mulAmount = C->getZExtValue();
					if (mulAmount < 0 && listener->hasSubValue())
						return compileGEPOperand(first, -mulAmount*size, listener, !invert);
					else
						return compileGEPOperand(first, mulAmount*size, listener, invert);
				}
			}
		}
		if(invert)
			listener->subValue(idxVal, size);
		else
			listener->addValue(idxVal, size);
		return 0;
	}
}

const llvm::Value* LinearMemoryHelper::compileGEP(const llvm::Module* module, const llvm::Value* p, GepListener* listener, const PointerAnalyzer* PA)
{
	const auto& targetData = module->getDataLayout();
	int64_t constPart = 0;
	while ( isBitCast(p) || isGEP(p) )
	{
		const User * u = cast<User>(p);
		if (isGEP(p))
		{
			Type* curType = u->getOperand(0)->getType();
			SmallVector< const Value *, 8 > indices ( std::next(u->op_begin()), u->op_end() );
			for (uint32_t i=0; i<indices.size(); i++)
			{
				if (StructType* ST = dyn_cast<StructType>(curType))
				{
					int64_t index = cast<ConstantInt>( indices[i] )->getZExtValue();
					const StructLayout* SL = targetData.getStructLayout( ST );
					curType = ST->getElementType(index);
					int64_t offset =  SL->getElementOffset(index);
					constPart += offset;
				}
				else
				{
					curType = getElementType(curType, cast<const GEPOperator>(p)->getSourceElementType());
					uint32_t size = targetData.getTypeAllocSize(curType);
					constPart += compileGEPOperand(indices[i], size, listener, false);
				}
			}
		}
		else if (PA != nullptr)
		{
			POINTER_KIND kind = PA->getPointerKindAssert(p);
			POINTER_KIND prevKind = PA->getPointerKind(cast<User>(p)->getOperand(0));
			assert(kind==RAW);
			if (prevKind != RAW)
				break;
		}
		p = u->getOperand(0);

		if (!listener->isInlineable(p))
			break;
	}
	if (constPart != 0)
		listener->addConst(constPart);
	return p;
}

bool LinearMemoryHelper::hasNonZeroInitialiser(const GlobalVariable* G) const
{
	if (!G->hasInitializer())
		return false;

	const Constant* init = G->getInitializer();
	return !isZeroInitializer(init);
}

void LinearMemoryHelper::addGlobals()
{
	if (LowerAtomics)
		generateGlobalizedGlobalsUsage();

	const auto& targetData = module->getDataLayout();
	// The global variable list has a special order:
	// 1. Move non-initialised and zero-initialised variables to end of
	// global variable list.
	// 2. Sort non-zero initialised variables on alignment to reduce the number
	// of padding bytes.
	for (const auto& G: module->globals())
	{
		if (G.getSection() != StringRef("asmjs")) continue;
		asmjsGlobals.push_back(&G);
	}

	std::sort(asmjsGlobals.begin(), asmjsGlobals.end(),
		[targetData,this] (const GlobalVariable* a, const GlobalVariable* b) {
			// Encode thread-local globals at the end.
			uint32_t isThreadLocalA = a->isThreadLocal();
			uint32_t isThreadLocalB = b->isThreadLocal();
			if (isThreadLocalA != isThreadLocalB)
				return isThreadLocalA < isThreadLocalB;
			// Encode all constant globals at the start when building shared modules,
			// we don't need to assign names to them and can batch them as a single block.
			if(WasmSharedModule)
			{
				uint32_t isConstantA = a->isConstant();
				uint32_t isConstantB = b->isConstant();
				if(isConstantA != isConstantB)
					return isConstantA > isConstantB;
			}
			// Encode zero-initialized globals after all the others
			uint32_t nonZeroInitializedA = hasNonZeroInitialiser(a);
			uint32_t nonZeroInitializedB = hasNonZeroInitialiser(b);
			if(nonZeroInitializedA != nonZeroInitializedB)
				return nonZeroInitializedA > nonZeroInitializedB;
			Type* aTy = a->getValueType();
			Type* bTy = b->getValueType();
			uint32_t typeAlignA = TypeSupport::getAlignmentAsmJS(targetData, aTy);
			uint32_t typeAlignB = TypeSupport::getAlignmentAsmJS(targetData, bTy);
			uint32_t alignA = std::max<uint32_t>(typeAlignA, a->getAlignment());
			uint32_t alignB = std::max<uint32_t>(typeAlignB, b->getAlignment());
			// Bigger alignment should be stored before smaller alignment.
			return alignA > alignB;
		}
	);

	// Compute the global variable addresses.
	// Also, for thread locals, calculate offsets to the image start, and the total size of the image.
	threadLocalStart = 0;
	globalsStart = 0;
	for (const auto G: asmjsGlobals) {
		//Globalized globals do not need an address
		if (globalizedGlobalsUsage.count(G))
			continue;
		Type* ty = G->getValueType();
		uint32_t size = targetData.getTypeAllocSize(ty);
		// Ensure the right alignment for the type
		uint32_t alignment = std::max<uint32_t>(TypeSupport::getAlignmentAsmJS(targetData, ty), G->getAlignment());
		// The following is correct if alignment is a power of 2 (which it should be)
		heapStart = (heapStart + alignment - 1) & ~(alignment - 1);
		if (globalsStart == 0)
			globalsStart = heapStart;
		globalAddresses.emplace(G, heapStart);
		inverseGlobalAddresses.emplace(heapStart, G);
		if (G->isThreadLocal())
		{
			asmjsThreadLocals.push_back(G);
			if (threadLocalStart == 0)
				threadLocalStart = heapStart;
		}
		heapStart += size;
	}
	heapStart = (heapStart + 7) & ~7;
	threadLocalImageSize = heapStart - threadLocalStart;
	// Align the thread local storage to 8 bytes.
}

void LinearMemoryHelper::generateGlobalizedGlobalsUsage()
{
	// TODO: add globalizedGlobals in the JS writer
	if (mode == FunctionAddressMode::AsmJS)
		return;
	if (WasmNoGlobalization)
		return;
	// Identify all globals which are only ever accessed with with load/store, we can promote those to globals
	for (const GlobalVariable& GV: module->globals())
	{
		// Don't deal with undefined variables
		if(!GV.hasInitializer())
			continue;
		uint32_t useCount = 0;
		for(const Use& U: GV.uses())
		{
			useCount++;
			const User* user = U.getUser();
			if ((isa<StoreInst>(user) && U.getOperandNo()==1) ||
				isa<LoadInst>(user))
			{
				if (cast<Instruction>(user)->getFunction()->getSection() == StringRef("asmjs"))
					continue;
			}
			useCount = 0;
			break;
		}
		// useCount == 0 means either access from outside linear memory, non-load/store user, or no user at all
		if(useCount == 0)
			continue;
		// We want to globalize this global, add it to the final map with his use count
		globalizedGlobalsUsage.insert(std::make_pair(&GV, useCount));
	}
}

void LinearMemoryHelper::addFunctions()
{
	// Construct the list of asmjs functions. Make sure that __wasm_nullptr is
	// the first list entry, if defined.
	if (mode == FunctionAddressMode::Wasm)
	{
		llvm::Function* wasmNullptr = module->getFunction(StringRef(wasmNullptrName));
		if (wasmNullptr)
			asmjsFunctions_.push_back(wasmNullptr);
	}

	std::vector<const llvm::Function*> unsorted;
	for (auto& F: module->functions())
	{
		if (F.getSection() != StringRef("asmjs"))
			continue;

		// Do not add __wasm_nullptr twice.
		if (mode == FunctionAddressMode::Wasm && F.getName() == StringRef(wasmNullptrName))
			continue;

		// Adding empty functions here will only cause a crash later
		if (F.empty())
			continue;

		// WebAssembly has some builtin functions (sqrt, abs, copysign, etc.)
		// which should be omitted, and is therefore a subset of the asmjs
		// function list.
		if (mode == FunctionAddressMode::Wasm && TypedBuiltinInstr::isWasmIntrinsic(&F) && !F.hasAddressTaken()) {
			continue;
		}

		unsorted.push_back(&F);
	}

	if (LinearOutput == LinearOutputTy::Wasm)
	{
		// __memory_init will be populated in a later pass, with data calculated in LinearMemoryHelper.
		// But it should be compiled in wasm, so manually add it to the list.
		Function* memoryInit = module->getFunction("__memory_init");
		if (memoryInit)
			unsorted.push_back(memoryInit);
	}

	// Sort the list of functions by their usage.
	std::sort(unsorted.begin(), unsorted.end(),
		[] (const Function* a, const Function* b) {
			return a->getNumUses() > b->getNumUses();
		}
	);

	for (auto F : unsorted)
		asmjsFunctions_.push_back(F);

	// Add the asm.js imports to the function type list. The non-imported
	// asm.js functions will be added below.
#define ADD_FUNCTION_TYPE(fTy) \
if (!functionTypeIndices.count(fTy)) { \
	uint32_t idx = functionTypeIndices.size(); \
	functionTypeIndices[fTy] = idx; \
	functionTypes.push_back(fTy); \
	assert(idx < functionTypes.size()); \
}
#define ADD_BUILTIN(x, sig) if(globalDeps->needsBuiltin(BuiltinInstr::BUILTIN::x)) { needs_ ## sig = true; builtinIds[BuiltinInstr::x] = maxFunctionId++; }

	for (const Function* F : globalDeps->asmJSImports()) {
		const FunctionType* fTy = F->getFunctionType();
		ADD_FUNCTION_TYPE(fTy);
		functionIds.insert(std::make_pair(F, maxFunctionId++));
	}
	if(!NoNativeJavaScriptMath && mode == FunctionAddressMode::Wasm)
	{
		// Synthetize the function type for float/double builtins
		Type* f64 = Type::getDoubleTy(module->getContext());
		Type* f64_1[] = { f64 };
		Type* f64_2[] = { f64, f64 };
		FunctionType* f64_f64_1 = FunctionType::get(f64, f64_1, false);
		FunctionType* f64_f64_2 = FunctionType::get(f64, f64_2, false);
		bool needs_f64_f64_1 = false;
		bool needs_f64_f64_2 = false;
		ADD_BUILTIN(ACOS_F, f64_f64_1);
		ADD_BUILTIN(ASIN_F, f64_f64_1);
		ADD_BUILTIN(ATAN_F, f64_f64_1);
		ADD_BUILTIN(ATAN2_F, f64_f64_2);
		ADD_BUILTIN(COS_F, f64_f64_1);
		ADD_BUILTIN(EXP_F, f64_f64_1);
		ADD_BUILTIN(LOG_F, f64_f64_1);
		ADD_BUILTIN(POW_F, f64_f64_2);
		ADD_BUILTIN(SIN_F, f64_f64_1);
		ADD_BUILTIN(TAN_F, f64_f64_1);
		if(needs_f64_f64_1)
			ADD_FUNCTION_TYPE(f64_f64_1);
		if(needs_f64_f64_2)
			ADD_FUNCTION_TYPE(f64_f64_2);
	}
	Type* i32 = Type::getInt32Ty(module->getContext());
	Type* i32_1[] = { i32 };
	FunctionType* i32_i32_1 = FunctionType::get(i32, i32_1, false);
	bool needs_i32_i32_1 = false;
	ADD_BUILTIN(GROW_MEM, i32_i32_1);
	if(needs_i32_i32_1)
		ADD_FUNCTION_TYPE(i32_i32_1);
#undef ADD_BUILTIN
#undef ADD_FUNCTION_TYPE

	// Build the function tables first
	for (const Function* F : asmjsFunctions_)
	{
		const FunctionType* fTy = F->getFunctionType();
		if (F->hasAddressTaken() || F->getName() == StringRef(wasmNullptrName)) {
			auto it = functionTables.find(fTy);
			if (it == functionTables.end())
			{
				functionTableOrder.push_back(fTy);
				it = functionTables.emplace(fTy,FunctionTableInfo()).first;
			}
			it->second.functions.push_back(F);
		}

		functionIds.insert(std::make_pair(F, maxFunctionId++));

		const auto& found = functionTypeIndices.find(fTy);
		if (found == functionTypeIndices.end()) {
			uint32_t idx = functionTypeIndices.size();
			functionTypeIndices[fTy] = idx;
			functionTypes.push_back(fTy);
			assert(idx < functionTypes.size());
		}

		// When building shared modules new functions can be used from outside the module,
		// so we should assume any called type is a valid
		if (WasmSharedModule)
		{
			for (const BasicBlock& bb : *F)
			{
				for (const Instruction& I : bb)
				{
					const CallBase* ci = dyn_cast<CallBase>(&I);
					if (!ci || ci->isInlineAsm())
						continue;
					Value* calledValue = ci->getCalledOperand();
					if (isa<Function>(calledValue))
						continue;
					const FunctionType* fTy = ci->getFunctionType();
					auto it = functionTables.find(fTy);
					if (it == functionTables.end())
					{
						functionTableOrder.push_back(fTy);
						functionTables.emplace(fTy, FunctionTableInfo());
					}
					const auto& found = functionTypeIndices.find(fTy);
					if (found == functionTypeIndices.end()) {
						uint32_t idx = functionTypeIndices.size();
						functionTypeIndices[fTy] = idx;
						functionTypes.push_back(fTy);
						assert(idx < functionTypes.size());
					}
				}
			}
		}
	}

	// Then assign addresses in the order that the function tables are created.
	// Without the creation order, it is possible that __wasm_nullptr will not
	// get the first function address (= 0), since std::unordered_map could
	// have any traversal order.
	uint32_t offset = 0;
	for (const FunctionType* fTy: functionTableOrder)
	{
		const auto FT = functionTables.find(fTy);
		if (mode == FunctionAddressMode::AsmJS)
			offset += 1<<16;
		uint32_t addr = 0;
		for (const auto F: FT->second.functions)
		{
			functionAddresses.emplace(F, addr+offset);
			addr++;
		}
		if (mode == FunctionAddressMode::Wasm)
			offset += FT->second.functions.size();
	}

	// Finish the function tables.
	offset = 0;
	for (auto& t: functionTables)
	{
		t.second.offset = offset;
		offset += t.second.functions.size();
		
		size_t typeIndex = 0;
		for (auto& fTy : functionTypes) {
			if (FunctionSignatureCmp(/*isStrict*/false)(t.first, fTy))
				break;
			typeIndex++;
		}
		t.second.typeIndex = typeIndex;
		assert(typeIndex < functionTypes.size());
	}
}

void LinearMemoryHelper::addStack()
{
	heapStart += stackSize + stackOffset;
	stackStart = heapStart - 8;
}

void LinearMemoryHelper::checkMemorySize()
{
	if (mode == FunctionAddressMode::AsmJS && memorySize > 2147483648U)
		report_fatal_error("Cheerp: -cheerp-linear-heap-size greater than 2048 is not supported with -cheerp-linear-output=asmjs");
	if (heapStart < memorySize)
		return;
	// Not enough memory, error
	report_fatal_error("Cheerp: Not enough linear memory. Try to increase it with -cheerp-linear-heap-size");
}

void LinearMemoryHelper::setGlobalPtrIfPresent(llvm::StringRef name, uint32_t ptr)
{
	if (GlobalVariable* G = module->getNamedGlobal(name))
	{
		ConstantInt* value = ConstantInt::get(IntegerType::getInt32Ty(module->getContext()), ptr, false);
		Constant* initializer = ConstantExpr::getIntToPtr(value, G->getValueType(), false);

		G->setInitializer(initializer);
		G->setSection("asmjs");
	}
}

void LinearMemoryHelper::setGlobalUInt32IfPresent(llvm::StringRef name, uint32_t value)
{
	if (GlobalVariable* G = module->getNamedGlobal(name))
	{
		ConstantInt* initializer = ConstantInt::get(IntegerType::getInt32Ty(module->getContext()), value, false);
		G->setInitializer(initializer);
	}
}

void LinearMemoryHelper::addMemoryInfo()
{
	setGlobalPtrIfPresent("_stackBottom", stackStart);
	setGlobalPtrIfPresent("_stackTop", stackStart + 8 - stackSize);
	setGlobalPtrIfPresent("_globalsStart", globalsStart);

	//Align to 8 bytes
	heapStart = (heapStart + 7) & ~7;
	setGlobalPtrIfPresent("_heapStart", heapStart);

	uint32_t heapEnd = growMem ? heapStart : memorySize;
	// Align heapEnd to a wasm page size
	heapEnd = (heapEnd + 65535) & ~65535;
	setGlobalPtrIfPresent("_heapEnd", heapEnd);

	// Set the values for the thread local storage.
	setGlobalUInt32IfPresent("__tlsImage", threadLocalStart);
	setGlobalUInt32IfPresent("__tlsImageSize", threadLocalImageSize);
}

void LinearMemoryHelper::VectorWriter::addByte(uint8_t b)
{
	rawData[address] = b;
	if (b == 0)
	{
		currentZeroStreak++;
		address++;
		return;
	}
	// If no data was available yet, we mark it now.
	if (!isDataAvailable)
	{
		isDataAvailable = true;
		startOfChunk = address;
	}
	// If data was already available, and we hit the split threshold, we split a chunk.
	else if (currentZeroStreak >= splitThreshold)
	{
		bool split = splitChunk();
		// We also start a new chunk with this byte.
		if (split)
			startOfChunk = address;
	}
	lastNonZero = address;
	address++;
	currentZeroStreak = 0;
}

void LinearMemoryHelper::VectorWriter::addRelocation(const llvm::GlobalVariable* GV, uint32_t offset)
{
	// NOTE: The relocation is added after the value, The -4 offset compensate for that.
	relocations.emplace_back(curGlobal, startAddress + address - curGlobalAddress - 4, GV, offset);
}

bool LinearMemoryHelper::VectorWriter::splitChunk(bool force, bool hasAsmjsMem)
{
	if (force && !isDataAvailable)
		return false;
	if (!force && chunks.size() + 1 == maxChunks)
		return false;
	uint32_t address = startAddress + startOfChunk;
	uint32_t startPosition = hasAsmjsMem ? 0 : startOfChunk;
	uint32_t length = hasAsmjsMem ? rawData.size() : lastNonZero - startPosition + 1;
	chunks.emplace_back(address, curGlobal, rawData, startPosition, length, std::move(relocations));
	if(force)
		isDataAvailable = false;
	return true;
}

void LinearMemoryHelper::populateGlobalData()
{
	// costToSplit is based on the cost of characters needed to encode data.
	uint32_t costToSplit = (mode == FunctionAddressMode::AsmJS) ? 9 : 19;

	// maxChunks differs. In Wasm this is 1e5 (V8 and SpiderMonkey apparently have a hard limit there)
	// In AsmJS with extra memory there is only 1 chunk, and without extra memory we can have infinite.
	uint32_t maxChunks = UINT_MAX;
	if (mode == FunctionAddressMode::Wasm)
		maxChunks = 1e5;
	else if (hasAsmjsMem)
		maxChunks = 1;

	// This vector will keep the raw byte data of the globals.
	rawGlobalData.resize(heapStart - stackSize);
	VectorWriter vectorWriter(rawGlobalData, globalDataChunks, costToSplit, maxChunks, stackStart);

	// Now loop over all the globals and compile them into the vector.
	for (const GlobalVariable *GV : globals())
	{
		auto it = globalAddresses.find(GV);
		if (GV->hasInitializer() && it != globalAddresses.end())
		{
			const Constant* init = GV->getInitializer();
			uint32_t curAddress = it->second;
			vectorWriter.setAddress(GV, curAddress);
			compileConstantAsBytes(init,/* asmjs */ true, &vectorWriter);
		}
		// If we need to link globals we need to make sure the
		// initialization data is in different chunks
		// TODO: We could avoid splitting for constants
		if(WasmSharedModule)
			vectorWriter.splitChunk(true, hasAsmjsMem);
	}
	// Flush any pending data
	if(!WasmSharedModule)
		vectorWriter.splitChunk(true, hasAsmjsMem);
}

const LinearMemoryHelper::GlobalDataChunk &LinearMemoryHelper::getGlobalDataChunk(uint32_t number) const
{
	assert(number < globalDataChunks.size());
	return globalDataChunks[number];
}

uint32_t LinearMemoryHelper::getGlobalVariableAddress(const GlobalVariable* G) const
{
	assert(globalAddresses.count(G));
	return globalAddresses.find(G)->second;
}

const llvm::GlobalVariable* LinearMemoryHelper::getGlobalVariableFromAddress(Value* C) const
{
	int addr = 0;
	if (isa<ConstantInt>(C))
		addr = cast<ConstantInt>(C)->getZExtValue();
	else
		return nullptr;

	auto it = inverseGlobalAddresses.find(addr);
	if (it != inverseGlobalAddresses.end())
		return it->second;

	return nullptr;
}

int32_t LinearMemoryHelper::getThreadLocalOffset(const GlobalVariable* G) const
{
	assert(globalAddresses.count(G) && G->isThreadLocal());
	return globalAddresses.find(G)->second - threadLocalStart - threadLocalImageSize;
}

uint32_t LinearMemoryHelper::getFunctionAddress(const llvm::Function* F) const
{
	assert(functionAddresses.count(F));
	return functionAddresses.find(F)->second;
}
bool LinearMemoryHelper::functionHasAddress(const llvm::Function* F) const
{
	return functionAddresses.count(F);
}
uint32_t LinearMemoryHelper::getFunctionAddressMask(const llvm::FunctionType* Fty) const
{
	auto t = functionTables.find(Fty);
	assert (t != functionTables.end());
	uint32_t mask = t->second.functions.size();
	uint32_t next_power_of_2 = 1;
	while(next_power_of_2 < mask)
		next_power_of_2 <<= 1;
	mask = next_power_of_2 - 1;
	return mask;

}

bool LinearMemoryHelper::LinearGepListener::isInlineable(const llvm::Value* p)
{
	if (const auto I = dyn_cast<Instruction>(p))
		return ::isInlineable(*I, PA);

	return true;
}

AnalysisKey LinearMemoryAnalysis::Key;
LinearMemoryHelper* LinearMemoryHelperWrapper::innerPtr{nullptr};

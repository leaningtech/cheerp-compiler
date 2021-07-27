//===-- LinearMemoryHelper.cpp - The Cheerp JavaScript generator ----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017-2020 Leaning Technologies
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
							offset += index*targetData.getTypeAllocSize(getElementType(curTy));
							curTy = getElementType(curTy);
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
				if(isa<ConstantInt>(idxI->getOperand(0)))
				{
					uint32_t mulAmount = cast<ConstantInt>(idxI->getOperand(0))->getZExtValue();
					return compileGEPOperand(idxI->getOperand(1), mulAmount*size, listener, invert);
				}
				else if(isa<ConstantInt>(idxI->getOperand(1)))
				{
					uint32_t mulAmount = cast<ConstantInt>(idxI->getOperand(1))->getZExtValue();
					return compileGEPOperand(idxI->getOperand(0), mulAmount*size, listener, invert);
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
					curType = getElementType(curType);
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
			// Encode zero-initialized globals after all the others
			uint32_t nonZeroInitializedA = hasNonZeroInitialiser(a);
			uint32_t nonZeroInitializedB = hasNonZeroInitialiser(b);
			if(nonZeroInitializedA != nonZeroInitializedB)
				return nonZeroInitializedA > nonZeroInitializedB;
			Type* aTy = a->getType()->getPointerElementType();
			Type* bTy = b->getType()->getPointerElementType();
			uint32_t typeAlignA = TypeSupport::getAlignmentAsmJS(targetData, aTy);
			uint32_t typeAlignB = TypeSupport::getAlignmentAsmJS(targetData, bTy);
			uint32_t alignA = std::max(typeAlignA, a->getAlignment());
			uint32_t alignB = std::max(typeAlignB, b->getAlignment());
			// Bigger alignment should be stored before smaller alignment.
			return alignA > alignB;
		}
	);

	// Compute the global variable addresses.
	for (const auto G: asmjsGlobals) {
		//Globalized globals do not need an address
		if (globalizedGlobalsUsage.count(G))
			continue;
		asmjsAddressableGlobals.push_back(G);
		Type* ty = G->getType();
		uint32_t size = targetData.getTypeAllocSize(ty->getPointerElementType());
		// Ensure the right alignment for the type
		uint32_t alignment = std::max(TypeSupport::getAlignmentAsmJS(targetData, ty->getPointerElementType()), G->getAlignment());
		// The following is correct if alignment is a power of 2 (which it should be)
		heapStart = (heapStart + alignment - 1) & ~(alignment - 1);
		globalAddresses.emplace(G, heapStart);
		heapStart += size;
	}
}

void LinearMemoryHelper::generateGlobalizedGlobalsUsage()
{
	// TODO: add globalizedGlobals in the JS writer
	if (mode == FunctionAddressMode::AsmJS)
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
	for (auto* F: globalDeps->insideModule())
	{
		if (F->getSection() != StringRef("asmjs"))
			continue;

		// Do not add __wasm_nullptr twice.
		if (mode == FunctionAddressMode::Wasm && F->getName() == StringRef(wasmNullptrName))
			continue;

		// Adding empty functions here will only cause a crash later
		if (F->empty())
			continue;

		// WebAssembly has some builtin functions (sqrt, abs, copysign, etc.)
		// which should be omitted, and is therefore a subset of the asmjs
		// function list.
		if (mode == FunctionAddressMode::Wasm && TypedBuiltinInstr::isWasmIntrinsic(F) && !F->hasAddressTaken()) {
			continue;
		}

		unsorted.push_back(F);
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
	if (!wasmOnly) {
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
	}

	// Check if the __genericjs__free function is present. If so, consider
	// "free()" as if its address is taken
	bool freeTaken = module->getFunction("__genericjs__free") != nullptr;
	// Build the function tables first
	for (const Function* F : asmjsFunctions_)
	{
		const FunctionType* fTy = F->getFunctionType();
		if (F->hasAddressTaken() || F->getName() == StringRef(wasmNullptrName) || (freeTaken && F->getName() == StringRef("free"))) {
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
			if (FunctionSignatureCmp<>()(t.first, fTy))
				break;
			typeIndex++;
		}
		t.second.typeIndex = typeIndex;
		assert(typeIndex < functionTypes.size());
	}
}

void LinearMemoryHelper::addStack()
{
	heapStart += stackSize;
	stackStart = heapStart - 8;
}

void LinearMemoryHelper::checkMemorySize()
{
	if (heapStart < memorySize)
		return;
	// Not enough memory, error
	report_fatal_error("Cheerp: Not enough linear memory. Try to increase it with -cheerp-linear-heap-size");
}
void LinearMemoryHelper::addHeapStartAndEnd()
{
	GlobalVariable* heapStartVar = module->getNamedGlobal("_heapStart");
	GlobalVariable* heapEndVar = module->getNamedGlobal("_heapEnd");

	if (heapStartVar)
	{
		assert(heapEndVar && "No _heapEnd global variable found");
		// Align to 8 bytes
		heapStart = (heapStart + 7) & ~7;
		ConstantInt* startAddr = ConstantInt::get(IntegerType::getInt32Ty(module->getContext()), heapStart, false);
		Constant* startInit = ConstantExpr::getIntToPtr(startAddr, heapStartVar->getType()->getElementType(), false);
		heapStartVar->setInitializer(startInit);
		heapStartVar->setSection("asmjs");

		uint32_t heapEnd = growMem ? heapStart : memorySize;
		ConstantInt* endAddr = ConstantInt::get(IntegerType::getInt32Ty(module->getContext()), heapEnd, false);
		Constant* endInit = ConstantExpr::getIntToPtr(endAddr, heapEndVar->getType()->getElementType(), false);
		heapEndVar->setInitializer(endInit);
		heapEndVar->setSection("asmjs");
	}
}

uint32_t LinearMemoryHelper::getGlobalVariableAddress(const GlobalVariable* G) const
{
	assert(globalAddresses.count(G));
	return globalAddresses.find(G)->second;
}
uint32_t LinearMemoryHelper::getFunctionAddress(const llvm::Function* F) const
{
	if (F->getName() == StringRef("__genericjs__free"))
	{
		const Function* ffree = module->getFunction("free");
		assert(ffree);
		F = ffree;
	}
	assert(functionAddresses.count(F));
	return functionAddresses.find(F)->second;
}
bool LinearMemoryHelper::functionHasAddress(const llvm::Function* F) const
{
	if (F->getName() == StringRef("__genericjs__free"))
	{
		const Function* ffree = module->getFunction("free");
		assert(ffree);
		F = ffree;
	}
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

void LinearMemoryHelper::getAnalysisUsage(llvm::AnalysisUsage & AU) const
{
	AU.addRequired<cheerp::GlobalDepsAnalyzer>();
	AU.setPreservesAll();
	llvm::Pass::getAnalysisUsage(AU);
}

llvm::ModulePass *cheerp::createLinearMemoryHelperPass(LinearMemoryHelper::FunctionAddressMode mode,
		uint32_t memorySize,uint32_t stackSize, bool wasmOnly, bool growMem)
{
	return new LinearMemoryHelper(mode, memorySize, stackSize, wasmOnly, growMem);
}

char LinearMemoryHelper::ID = 0;

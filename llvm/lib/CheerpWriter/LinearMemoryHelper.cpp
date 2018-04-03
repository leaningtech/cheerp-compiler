//===-- LinearMemoryHelper.cpp - The Cheerp JavaScript generator ----------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2017 Leaning Technologies
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
	const auto& targetData = module.getDataLayout();
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
	else if(isa<ConstantArray>(c) || isa<ConstantStruct>(c))
	{
		assert(offset==0);
		for(uint32_t i=0;i<c->getNumOperands();i++)
			compileConstantAsBytes(cast<Constant>(c->getOperand(i)), asmjs, listener);
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
			assert(offset==0);
			listener->addByte(0);
			listener->addByte(0);
			listener->addByte(0);
			listener->addByte(0);
		}
		else if(const Function* F = dyn_cast<Function>(c))
		{
			assert(offset==0);
			if (!functionAddresses.count(F))
			{
				llvm::errs() << "function not in table: " << F->getName() <<"\n";
				llvm::report_fatal_error("please report a bug");
			}
			uint32_t addr = getFunctionAddress(F);
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
							offset += index*targetData.getTypeAllocSize(curTy->getSequentialElementType());
							curTy = curTy->getSequentialElementType();
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
			llvm::errs() << "Unsupported constant type for bytes in asm.js module :";
			c->getType()->dump();
		}
	}
	else
	{
		// TODO: It could make sense to emit the right number of zeroes anyway
		llvm::errs() << "Unsupported constant type for bytes ";
		c->dump();
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
		if (!functionAddresses.count(F))
		{
			llvm::errs() << "function not in table: " << F->getName() <<"\n";
			llvm::report_fatal_error("please report a bug");
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

	c->dump();
	llvm_unreachable("Unsupported constant type");
}

const llvm::Value* LinearMemoryHelper::compileGEP(const llvm::Value* p, GepListener* listener) const
{
	const auto& targetData = module.getDataLayout();
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
					curType = curType->getSequentialElementType();
					uint32_t size = targetData.getTypeAllocSize(curType);
					if (const ConstantInt* idx = dyn_cast<ConstantInt>(indices[i]))
					{
						constPart += idx->getSExtValue()*size;
					}
					else
					{
						listener->addValue(indices[i], size);
					}
				}
			}
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
	const auto& targetData = module.getDataLayout();
	// The global variable list has a special order:
	// 1. Move non-initialised and zero-initialised variables to end of
	// global variable list.
	// 2. Sort non-zero initialised variables on alignment to reduce the number
	// of padding bytes.
	for (const auto& G: module.globals())
	{
		if (G.getSection() != StringRef("asmjs")) continue;
		if (!hasNonZeroInitialiser(&G)) continue;

		asmjsGlobals.push_back(&G);
	}

	std::sort(asmjsGlobals.begin(), asmjsGlobals.end(),
		[targetData] (const GlobalVariable* a, const GlobalVariable* b) {
			Type* aTy = a->getType()->getPointerElementType();
			Type* bTy = b->getType()->getPointerElementType();
			// Bigger alignment should be stored before smaller alignment.
			return TypeSupport::getAlignmentAsmJS(targetData, aTy) >
				TypeSupport::getAlignmentAsmJS(targetData, bTy);
		}
	);

	for (const auto& G: module.globals())
	{
		if (G.getSection() != StringRef("asmjs")) continue;
		if (hasNonZeroInitialiser(&G)) continue;
		asmjsGlobals.push_back(&G);
	}

	// Compute the global variable addresses.
	for (const auto G: asmjsGlobals) {
		Type* ty = G->getType();
		uint32_t size = targetData.getTypeAllocSize(ty->getPointerElementType());
		// Ensure the right alignment for the type
		uint32_t alignment = TypeSupport::getAlignmentAsmJS(targetData, ty->getPointerElementType());
		// The following is correct if alignment is a power of 2 (which it should be)
		heapStart = (heapStart + alignment - 1) & ~(alignment - 1);
		globalAddresses.emplace(G, heapStart);
		heapStart += size;
	}
}

void LinearMemoryHelper::addFunctions()
{
	// Construct the list of asmjs functions. Make sure that __wasm_nullptr is
	// the first list entry, if defined.
	if (mode == FunctionAddressMode::Wasm)
	{
		llvm::Function* wasmNullptr = module.getFunction(StringRef(wasmNullptrName));
		if (wasmNullptr)
			asmjsFunctions_.push_back(wasmNullptr);
	}

	std::vector<llvm::Function*> unsorted;
	for (auto& F: module.functions())
	{
		if (F.getSection() != StringRef("asmjs"))
			continue;

		// Do not add __wasm_nullptr twice.
		if (mode == FunctionAddressMode::Wasm && F.getName() == StringRef(wasmNullptrName))
			continue;

		// WebAssembly has some builtin functions (sqrt, abs, copysign, etc.)
		// which should be omitted, and is therefore a subset of the asmjs
		// function list.
		if (mode == FunctionAddressMode::Wasm && isWasmIntrinsic(&F)) {
			assert(!F.hasAddressTaken());
			continue;
		}

		unsorted.push_back(&F);
	}

	// Sort the list of functions by their usage.
	std::sort(unsorted.begin(), unsorted.end(),
		[] (Function* a, Function* b) {
			return a->getNumUses() > b->getNumUses();
		}
	);

	for (auto F : unsorted)
		asmjsFunctions_.push_back(F);

	// Add the asm.js imports to the function type list. The non-imported
	// asm.js functions will be added below.
	if (!WasmLoader.empty()) {
		for (const Function* F : globalDeps.asmJSImports()) {
			const FunctionType* fTy = F->getFunctionType();
			const auto& found = functionTypeIndices.find(fTy);
			if (found == functionTypeIndices.end()) {
				uint32_t idx = functionTypeIndices.size();
				functionTypeIndices[fTy] = idx;
				functionTypes.push_back(fTy);
				assert(idx < functionTypes.size());
			}
			functionIds.insert(std::make_pair(F, functionIds.size()));
		}
	}

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
				it->second.name = getFunctionTableName(fTy);
			}
			it->second.functions.push_back(F);
		}

		functionIds.insert(std::make_pair(F, functionIds.size()));

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
			if (FunctionSignatureCmp()(t.first, fTy))
				break;
			typeIndex++;
		}
		t.second.typeIndex = typeIndex;
		assert(typeIndex < functionTypes.size());
	}
}

void LinearMemoryHelper::addHeapStart()
{
	GlobalVariable* heapStartVar = module.getNamedGlobal("_heapStart");

	if (heapStartVar)
	{
		// Align to 8 bytes
		heapStart = (heapStart + 7) & ~7;
		ConstantInt* addr = ConstantInt::get(IntegerType::getInt32Ty(module.getContext()), heapStart, false);
		Constant* heapInit = ConstantExpr::getIntToPtr(addr, heapStartVar->getType()->getElementType(), false);
		heapStartVar->setInitializer(heapInit);
		heapStartVar->setSection("asmjs");
	}
}

uint32_t LinearMemoryHelper::getGlobalVariableAddress(const GlobalVariable* G) const
{
	assert(globalAddresses.count(G));
	return globalAddresses.find(G)->second;
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

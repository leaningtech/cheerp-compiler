//===-- NameGenerator.cpp - The Cheerp name generator ---------------------===//
//
//                     Cheerp: The C++ compiler for the Web
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Copyright 2014-2020 Leaning Technologies
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/StringExtras.h"
#include "llvm/Cheerp/NameGenerator.h"
#include "llvm/Cheerp/GlobalDepsAnalyzer.h"
#include "llvm/Cheerp/PHIHandler.h"
#include "llvm/Cheerp/JsExport.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InlineAsm.h"
#include <functional>
#include <set>

using namespace llvm;

namespace cheerp {

NameGenerator::NameGenerator(const Module& M, const GlobalDepsAnalyzer& gda, Registerize& r,
				const PointerAnalyzer& PA,  LinearMemoryHelper& linearHelper,
				const std::vector<std::string>& rn, bool makeReadableNames):
				registerize(r), PA(PA), reservedNames(buildReservedNamesList(M, rn))
{
	if ( makeReadableNames )
		generateReadableNames(M, gda, linearHelper);
	else
		generateCompressedNames(M, gda, linearHelper);
}

llvm::StringRef NameGenerator::getNameForEdge(const llvm::Value* v, const EdgeContext& edgeContext) const
{
	if (const Instruction* I=dyn_cast<Instruction>(v))
	{
		uint32_t regId = registerize.getRegisterId(I, edgeContext);
		return regNamemap.at(std::make_pair(I->getParent()->getParent(), regId));
	}
	return namemap.at(v);
}

llvm::StringRef NameGenerator::getSecondaryNameForEdge(const llvm::Value* v, const EdgeContext& edgeContext) const
{
	if (const Instruction* I=dyn_cast<Instruction>(v))
	{
		uint32_t regId = registerize.getRegisterId(I, edgeContext);
		return regSecondaryNamemap.at(std::make_pair(I->getParent()->getParent(), regId));
	}
	return secondaryNamemap.at(v);
}

SmallString< 4 > NameGenerator::filterLLVMName(StringRef s, NAME_FILTER_MODE filterMode)
{
	SmallString< 4 > ans;
	ans.reserve( s.size() + 1 );

	//Add an '_', 'L' or 'M' to skip reserved names
	switch(filterMode)
	{
		case GLOBAL:
			ans.push_back( '_' );
			break;
		case GLOBAL_SECONDARY:
			ans.push_back( '$' );
			break;
		case LOCAL:
			ans.push_back( 'L' );
			break;
		case LOCAL_SECONDARY:
			ans.push_back( 'M' );
			break;
	}

	for ( char c : s )
	{
		//We need to escape invalid chars
		switch(c)
		{
			case '.':
				ans.append("$p");
				break;
			case '-':
				ans.append("$m");
				break;
			case ':':
				ans.append("$c");
				break;
			case '<':
				ans.append("$l");
				break;
			case '>':
				ans.append("$r");
				break;
			case ' ':
				ans.append("$s");
				break;
			default:
				ans.push_back(c);
		}
	}

	return ans;
}

void NameGenerator::generateCompressedNames(const Module& M, const GlobalDepsAnalyzer& gda, LinearMemoryHelper& linearHelper)
{
	typedef std::pair<unsigned, const GlobalValue *> useGlobalPair;
	// We either encode arguments in the Value or a pair of (Function, register id)
	// This will be unified to the second case when we registerize args
	struct localData
	{
		const llvm::Value* argOrFunc;
		uint32_t regId;
		bool needsSecondaryName;
	};
	typedef std::pair<unsigned, localData> useLocalPair;
	typedef std::pair<unsigned, std::vector<localData>> useLocalsPair;
	typedef std::vector<useLocalPair> useLocalVec;
	typedef std::vector<useLocalsPair> useLocalsVec;
	typedef std::pair<unsigned, Type*> useTypesPair;

	typedef std::set<useTypesPair, std::greater<useTypesPair>> useTypesSet;
        
	// Class to handle giving names to temporary variables needed for recursively dependent PHIs
	class CompressedPHIHandler: public PHIHandlerUsingTemp
	{
	public:
		CompressedPHIHandler(NameGenerator& n, EdgeContext& edgeContext, useLocalVec& l):
			PHIHandlerUsingTemp(n.PA, edgeContext), namegen(n), thisFunctionLocals(l)
		{
		}
	private:
		NameGenerator& namegen;
		useLocalVec& thisFunctionLocals;
		void handleRecursivePHIDependency(const Instruction* incoming) override
		{
			assert(edgeContext.isNull() == false);
			uint32_t registerId = namegen.registerize.getRegisterId(incoming, edgeContext);
			assert(registerId < thisFunctionLocals.size());
			useLocalPair& regData = thisFunctionLocals[registerId];
			// Assume it is used once
			regData.first++;
			// Set the register information if required
			assert(regData.second.argOrFunc);
			if(cheerp::needsSecondaryName(incoming, namegen.PA))
				assert(regData.second.needsSecondaryName);
		}
		void handlePHI(const PHINode* phi, const Value* incoming, bool selfReferencing) override
		{
			// Nothing to do here, we have already given names to all PHIs
		}
	};
	/**
	 * Collect the types that need a constructor.
	 * 
	 * We use a frequency count of 1, because counting the actual number of uses
	 * is not trivial, and the benefits in code size would not be significant
	 */
	useTypesSet classTypes;
	useTypesSet constructorTypes;
	useTypesSet arrayTypes;
	useTypesSet resizeTypes;
	for(Type* T: gda.classesWithBaseInfo())
	{
		classTypes.insert(std::make_pair(1,T));
	}
	for(Type* T: gda.classesUsed())
	{
		constructorTypes.insert(std::make_pair(1,T));
	}
	for(Type* T: gda.dynAllocArrays())
	{
		arrayTypes.insert(std::make_pair(1,T));
	}
	for(Type* T: gda.dynResizeArrays())
	{
		resizeTypes.insert(std::make_pair(1,T));
	}

	/**
	 * Collect the local values.
	 * 
	 * We sort them by uses, then store together those in the same position.
	 * i.e. allLocalValues[0].second will contain all the most used local values
	 * for each function, and allLocalValues[0].first will be the sum of the uses
	 * of all those local values.
	 */
        
	useLocalsVec allLocalValues;
        
	/**
	 * We use a std::set to automatically sort the global values by number of uses
	 */
	std::set< useGlobalPair, std::greater< useGlobalPair > > allGlobalValues;

	for (const Function & f : M.getFunctionList() )
	{
		unsigned nUses = f.getNumUses();

		if ( f.getName() == "_Z7webMainv" )
			++nUses; // We explicitly invoke the webmain

		// Constructors are also explicitly invoked
		if ( std::find(gda.constructors().begin(), gda.constructors().end(), &f ) != gda.constructors().end() )
			++nUses;

		allGlobalValues.emplace( nUses, &f );

		/**
		 * TODO, some cheerp-internals functions are actually generated even with an empty IR.
		 * They should be considered here.
		 */
		if ( f.empty() ) 
			continue;

		// The first part of this vector is for register based locals, after those there are arguments
		useLocalVec thisFunctionLocals;
		const std::vector<Registerize::RegisterInfo>& regsInfo = registerize.getRegistersForFunction(&f);
		thisFunctionLocals.reserve(regsInfo.size());
		for(unsigned regId = 0; regId < regsInfo.size(); regId++)
		{
			thisFunctionLocals.emplace_back(0, localData{&f, regId, bool(regsInfo[regId].needsSecondaryName)});
		}

		// Insert all the instructions
		for (const BasicBlock & bb : f)
		{
			for (const Instruction & I : bb)
			{
				if ( needsName(I, PA) )
				{
					uint32_t registerId = registerize.getRegisterId(&I, EdgeContext::emptyContext());
					assert(registerId < thisFunctionLocals.size());
					useLocalPair& regData = thisFunctionLocals[registerId];
					// Add the uses for this instruction to the total count for the register
					regData.first+=I.getNumUses();
					assert(regData.second.argOrFunc);
					if(needsSecondaryName(&I, PA))
						assert(	regData.second.needsSecondaryName);
				}
			}
			// Handle the special names required for the edges between blocks
			const Instruction* term=bb.getTerminator();
			for(uint32_t i=0;i<term->getNumSuccessors();i++)
			{
				EdgeContext localEdgeContext;
				const BasicBlock* succBB=term->getSuccessor(i);
				CompressedPHIHandler(*this, localEdgeContext, thisFunctionLocals).runOnEdge(registerize, &bb, succBB);
			}
		}

		uint32_t currentArgPos=thisFunctionLocals.size();
		thisFunctionLocals.resize(currentArgPos+f.arg_size());
		// Insert the arguments
		for ( auto& arg: f.args())
		{
			thisFunctionLocals[currentArgPos].first = f.getNumUses();
			thisFunctionLocals[currentArgPos].second = localData{&arg, 0, needsSecondaryName(&arg, PA)};
			currentArgPos++;
		}

		// Resize allLocalValues so that we have empty useValuesPair at the end of the container
		if ( thisFunctionLocals.size() > allLocalValues.size() )
			allLocalValues.resize( thisFunctionLocals.size() );

		std::sort(thisFunctionLocals.begin(),thisFunctionLocals.end(), [](const useLocalPair& lhs, const useLocalPair& rhs) { return lhs.first < rhs.first; });
		auto dst_it = allLocalValues.begin();

		for (auto src_it = thisFunctionLocals.rbegin(); src_it != thisFunctionLocals.rend(); ++src_it, ++dst_it )
		{
			dst_it->first += src_it->first;
			dst_it->second.push_back(src_it->second);
		}
	}
        
	assert( std::is_sorted( 
		allLocalValues.rbegin(), 
		allLocalValues.rend(),
		[] (const useLocalsPair & lhs, const useLocalsPair & rhs) { return lhs.first < rhs.first; } 
		) );

	for ( const GlobalValue & GV : M.getGlobalList() )
	{
		if ( isa<GlobalVariable>(GV) && TypeSupport::isClientGlobal(cast<GlobalVariable>(&GV)) )
		{
			demangler_iterator dmg( GV.getName() );
			assert(*dmg == "client");
			
			namemap.emplace( &GV, *(++dmg) );
			
			continue;
		}

		allGlobalValues.emplace( GV.getNumUses(), &GV );
	}

	/*
	 * Helper class to deal with name iteration logic, we want to apply the prefix (if any)
	 * only to global names. Local names should be allowed to use any name.
	 * When generating a local name we will skip anything which starts with the prefix
	 *
	 * Note that there will never be a global with the same name as a local.
	 * This is suboptimal, since in theory we could check out for the uses
	 * of the global inside the function.
	 */
	class NameHelper
	{
	private:
		name_iterator<JSSymbols> global_name_iterator;
		name_iterator<JSSymbols> local_name_iterator;
		StringRef globalPrefix;
	public:
		NameHelper(StringRef p, const std::vector<std::string>& reservedNames):global_name_iterator(p, JSSymbols(reservedNames)),
					local_name_iterator(StringRef(), JSSymbols(reservedNames)),
					globalPrefix(p)
		{
		}
		SmallString<4> makeGlobalName()
		{
			return *global_name_iterator++;
		}
		SmallString<4> makeLocalName()
		{
			if(globalPrefix.empty())
			{
				// We need to use the global iterator
				return makeGlobalName();
			}
			if(local_name_iterator->startswith(globalPrefix))
			{
				// Skip over all this sub-part of the range
				local_name_iterator.advance(globalPrefix.size());
				assert(!local_name_iterator->startswith(globalPrefix));
			}
			return *local_name_iterator++;
		}
	};

	NameHelper nameHelper(GlobalPrefix, reservedNames);

	// Generate HEAP names first to keep them short
	if(gda.needAsmJS())
	{
		builtins[HEAP8] = nameHelper.makeGlobalName();
		builtins[HEAP16] = nameHelper.makeGlobalName();
		builtins[HEAP32] = nameHelper.makeGlobalName();
		builtins[HEAP64] = nameHelper.makeGlobalName();
		builtins[HEAPF32] = nameHelper.makeGlobalName();
		builtins[HEAPF64] = nameHelper.makeGlobalName();
	}
	else
	{
		builtins[HEAP8] = "null";
		builtins[HEAP16] = "null";
		builtins[HEAP32] = "null";
		builtins[HEAP64] = "null";
		builtins[HEAPF32] = "null";
		builtins[HEAPF64] = "null";
	}

	// We need to iterate over allGlobalValues and allLocalValues
	// at the same time incrementing selectively only one of the iterators
	
	std::set< useGlobalPair >::const_iterator global_it = allGlobalValues.begin();
	useLocalsVec::const_iterator local_it = allLocalValues.begin();
	useTypesSet::const_iterator class_it = classTypes.begin();
	useTypesSet::const_iterator constructor_it = constructorTypes.begin();
	useTypesSet::const_iterator array_it = arrayTypes.begin();
	useTypesSet::const_iterator resize_it = resizeTypes.begin();

	bool globalsFinished = global_it == allGlobalValues.end();
	bool localsFinished = local_it == allLocalValues.end();
	bool classTypesFinished = class_it == classTypes.end();
	bool constructorTypesFinished = constructor_it == constructorTypes.end();
	bool arrayTypesFinished = array_it == arrayTypes.end();
	bool resizeTypesFinished = resize_it == resizeTypes.end();
	for ( ; !globalsFinished || !localsFinished || !classTypesFinished || !constructorTypesFinished || !arrayTypesFinished || !resizeTypesFinished; )
	{
		if ( !globalsFinished &&
			(localsFinished || global_it->first >= local_it->first) &&
			(classTypesFinished || global_it->first >= class_it->first) &&
			(constructorTypesFinished || global_it->first >= constructor_it->first) &&
			(arrayTypesFinished || global_it->first >= array_it->first) &&
			(resizeTypesFinished || global_it->first >= resize_it->first))
		{
			// Assign this name to a global value
			namemap.emplace( global_it->second, nameHelper.makeGlobalName() );
			// We need to consume another name to assign the secondary one
			if(needsSecondaryName(global_it->second, PA))
			{
				secondaryNamemap.emplace( global_it->second, nameHelper.makeGlobalName() );
			}
			++global_it;
		}
		else if ( !localsFinished &&
			(globalsFinished || local_it->first >= global_it->first) &&
			(classTypesFinished || local_it->first >= class_it->first) &&
			(constructorTypesFinished || local_it->first >= constructor_it->first) &&
			(arrayTypesFinished || local_it->first >= array_it->first) &&
			(resizeTypesFinished || local_it->first >= resize_it->first))
		{
			// Assign this name to all the local values
			SmallString<4> primaryName = nameHelper.makeLocalName();
			SmallString<4> secondaryName;
			for ( const localData& v : local_it->second )
			{
				if(v.needsSecondaryName && secondaryName.empty())
				{
					secondaryName = nameHelper.makeLocalName();
				}
				if(const llvm::Function* f = dyn_cast<llvm::Function>(v.argOrFunc))
				{
					regNamemap.emplace( std::make_pair( f, v.regId ), primaryName);
					if(v.needsSecondaryName)
						regSecondaryNamemap.emplace( std::make_pair( f, v.regId ), secondaryName);
				}
				else
				{
					namemap.emplace( v.argOrFunc, primaryName );
					// We need to consume another name to assign the secondary one
					if(v.needsSecondaryName)
						secondaryNamemap.emplace( v.argOrFunc, secondaryName );
				}
				assignLocalName(primaryName);
			}
			
			++local_it;
		}
		else if ( !classTypesFinished &&
			(globalsFinished || class_it->first >= global_it->first) &&
			(localsFinished || class_it->first >= local_it->first) &&
			(constructorTypesFinished || class_it->first >= constructor_it->first) &&
			(arrayTypesFinished || class_it->first >= array_it->first) &&
			(resizeTypesFinished || class_it->first >= resize_it->first))
		{
			SmallString<4> name = nameHelper.makeGlobalName();
			classmap.emplace(class_it->second, name);
			++class_it;
		}
		else if ( !constructorTypesFinished &&
			(globalsFinished || constructor_it->first >= global_it->first) &&
			(localsFinished || constructor_it->first >= local_it->first) &&
			(classTypesFinished || constructor_it->first >= class_it->first) &&
			(arrayTypesFinished || constructor_it->first >= array_it->first) &&
			(resizeTypesFinished || constructor_it->first >= resize_it->first))
		{
			SmallString<4> name = nameHelper.makeGlobalName();
			constructormap.emplace(constructor_it->second, name);
			++constructor_it;
		}
		else if ( !arrayTypesFinished &&
			(globalsFinished || array_it->first >= global_it->first) &&
			(localsFinished || array_it->first >= local_it->first) &&
			(classTypesFinished || array_it->first >= class_it->first) &&
			(constructorTypesFinished || class_it->first >= constructor_it->first) &&
			(resizeTypesFinished || array_it->first >= resize_it->first))
		{
			SmallString<4> name = nameHelper.makeGlobalName();
			arraymap.emplace(array_it->second, name);
			++array_it;
		}
		else
		{
			SmallString<4> name = nameHelper.makeGlobalName();
			resizemap.emplace(resize_it->second, name);
			++resize_it;
		}

		globalsFinished = global_it == allGlobalValues.end();
		localsFinished = local_it == allLocalValues.end();
		classTypesFinished = class_it == classTypes.end();
		constructorTypesFinished = constructor_it == constructorTypes.end();
		arrayTypesFinished = array_it == arrayTypes.end();
		resizeTypesFinished = resize_it == resizeTypes.end();
	}
	for(auto& tableIt: linearHelper.getFunctionTables())
	{
		tableIt.second.name = nameHelper.makeGlobalName();
	}
	// Generate the rest of the builtins
	for(int i=IMUL;i<=FETCHBUFFER;i++)
		builtins[i] = nameHelper.makeGlobalName();

	if (shortestLocalName.size() == 0)
		assignLocalName(nameHelper.makeLocalName());
}

void NameGenerator::generateReadableNames(const Module& M, const GlobalDepsAnalyzer& gda, LinearMemoryHelper& linearHelper)
{
	for (const Function & f : M.getFunctionList() )
	{
		namemap.emplace( &f, filterLLVMName( f.getName(), GLOBAL ) );
		if ( f.empty() )
			continue;
		const std::vector<Registerize::RegisterInfo>& regsInfo = registerize.getRegistersForFunction(&f);
		std::vector<bool> doneRegisters(regsInfo.size(), false);
		for (const BasicBlock & bb : f)
		{
			for (const Instruction & I : bb)
			{
				if ( needsName(I, PA) )
				{
					uint32_t registerId = registerize.getRegisterId(&I, EdgeContext::emptyContext());
					if(doneRegisters[registerId])
						continue;
					if (!I.hasName())
						continue;
					// If this instruction has a name, use it
					auto& name = regNamemap.emplace( std::make_pair(&f, registerId), filterLLVMName(I.getName(), LOCAL) ).first->second;
					assignLocalName(name);
					if(regsInfo[registerId].needsSecondaryName)
						regSecondaryNamemap.emplace( std::make_pair(&f, registerId), StringRef((name+"o").str()));
					doneRegisters[registerId] = true;
				}
			}
		}

		// Assign a name to any register which still needs one
		for(unsigned registerId=0;registerId<regsInfo.size();registerId++)
		{
			if(doneRegisters[registerId])
				continue;
			auto& name = regNamemap.emplace( std::make_pair(&f, registerId), StringRef( "tmp" + std::to_string(registerId) ) ).first->second;
			assignLocalName(name);
			if(regsInfo[registerId].needsSecondaryName)
				regSecondaryNamemap.emplace( std::make_pair(&f, registerId), StringRef((name+"o").str()));
		}

		for ( auto& arg: f.args())
		{
			bool needsTwoNames = needsSecondaryName(&arg, PA);
			if ( arg.hasName() )
			{
				namemap.emplace( &arg, filterLLVMName(arg.getName(), LOCAL) );
				if(needsTwoNames)
					secondaryNamemap.emplace( &arg, filterLLVMName(arg.getName(), LOCAL_SECONDARY) );
			}
			else
			{
				namemap.emplace( &arg, StringRef( "Larg" + std::to_string(arg.getArgNo()) ) );
				if(needsTwoNames)
					secondaryNamemap.emplace( &arg, StringRef( "Marg" + std::to_string(arg.getArgNo()) ) );
			}
			assignLocalName(namemap.at(&arg));
		}
	}

	for (const GlobalVariable & GV : M.getGlobalList() )
	{
		if (TypeSupport::isClientGlobal(&GV) )
		{
			demangler_iterator dmg( GV.getName() );
			assert(*dmg == "client");
			
			namemap.emplace( &GV, *(++dmg) );
			
		}
		else
		{
			namemap.emplace( &GV, filterLLVMName( GV.getName(), GLOBAL ) );
			bool needsTwoNames = needsSecondaryName(&GV, PA);
			if(needsTwoNames)
				secondaryNamemap.emplace( &GV, filterLLVMName(GV.getName(), GLOBAL_SECONDARY) );
		}
	}

	for(Type* T: gda.classesWithBaseInfo())
	{
		if(isa<StructType>(T) && cast<StructType>(T)->hasName())
		{
			llvm::SmallString<4> name = llvm::SmallString<4>("create");
			name.append(filterLLVMName(cast<StructType>(T)->getName(), GLOBAL).str());
			classmap.insert(std::make_pair(T, name));
		}
		else
			classmap.insert(std::make_pair(T, StringRef("class_literal" + std::to_string(classmap.size()))));
	}
	for(Type* T: gda.classesUsed())
	{
		if(isa<StructType>(T) && cast<StructType>(T)->hasName())
		{
			llvm::SmallString<4> name = llvm::SmallString<4>("constructor");
			name.append(filterLLVMName(cast<StructType>(T)->getName(), GLOBAL).str());
			constructormap.insert(std::make_pair(T, name));
		}
		else
			constructormap.insert(std::make_pair(T, StringRef("construct_literal" + std::to_string(constructormap.size()))));
	}
	for(Type* T: gda.dynAllocArrays())
	{
		if(isa<StructType>(T) && cast<StructType>(T)->hasName())
		{
			llvm::SmallString<4> name = llvm::SmallString<4>("createArray");
			name.append(filterLLVMName(cast<StructType>(T)->getName(), GLOBAL).str());
			arraymap.insert(std::make_pair(T, name));
		}
		else
			arraymap.insert(std::make_pair(T, StringRef("createArray_literal" + std::to_string(arraymap.size()))));
	}
	for(Type* T: gda.dynResizeArrays())
	{
		if(isa<StructType>(T) && cast<StructType>(T)->hasName())
		{
			llvm::SmallString<4> name = llvm::SmallString<4>("resizeArray");
			name.append(filterLLVMName(cast<StructType>(T)->getName(), GLOBAL).str());
			resizemap.insert(std::make_pair(T, name));
		}
		else
			resizemap.insert(std::make_pair(T, StringRef("resizeArray_literal" + std::to_string(resizemap.size()))));
	}
	for(auto& tableIt: linearHelper.getFunctionTables())
	{
		tableIt.second.name = "__FUNCTION_TABLE_" + LinearMemoryHelper::getFunctionTableName(tableIt.first);
	}
	// Builtin funcions
	builtins[IMUL] = "__imul";
	builtins[FROUND] = "__fround";
	builtins[ABS] = "abs";
	builtins[ACOS] = "acos";
	builtins[ASIN] = "asin";
	builtins[ATAN] = "atan";
	builtins[ATAN2] = "atan2";
	builtins[CEIL] = "ceil";
	builtins[COS] = "cos";
	builtins[EXP] = "exp";
	builtins[FLOOR] = "floor";
	builtins[LOG] = "log";
	builtins[POW] = "pow";
	builtins[SIN] = "sin";
	builtins[SQRT] = "sqrt";
	builtins[TAN] = "tan";
	builtins[CLZ32] = "clz32";
	builtins[CREATE_CLOSURE] = "cheerpCreateClosure";
	builtins[CREATE_CLOSURE_SPLIT] = "cheerpCreateClosureSplit";
	builtins[CREATE_POINTER_ARRAY] = "createPointerArray";
	builtins[GROW_MEM] = "growLinearMemory";
	builtins[ASSIGN_HEAPS] = "assignHeaps";
	builtins[DUMMY] = "__dummy";
	builtins[MEMORY] = "memory";
	builtins[HANDLE_VAARG] = "handleVAArg";
	builtins[FETCHBUFFER] = "fetchBuffer";
	builtins[LABEL] = "label";
	builtins[STACKPTR] = "__stackPtr";
	builtins[HEAP8] = "HEAP8";
	builtins[HEAP16] = "HEAP16";
	builtins[HEAP32] = "HEAP32";
	builtins[HEAP64] = "HEAP64";
	builtins[HEAPF32] = "HEAPF32";
	builtins[HEAPF64] = "HEAPF64";

	if (shortestLocalName.size() == 0)
		assignLocalName("Z");
}

bool NameGenerator::needsName(const Instruction & I, const PointerAnalyzer& PA) const
{
	return !isInlineable(I, PA) && !I.getType()->isVoidTy() && !I.use_empty();
}

std::vector<std::string> buildJsExportedNamesList(const Module& M)
{
	std::vector<std::string> names;

	const auto & jsexportedDecl = CheerpWriter::buildJsExportedNamedDecl(M);

	for (const auto& jsex : jsexportedDecl)
	{
		//This basically is the same as finding the '.' or the first-past-the-last
		unsigned int i = 0;
		while (true)
		{
			if (i==jsex.name.size() || jsex.name[i] == '.')
			{
				names.push_back(std::string(&jsex.name[0],i));
				break;
			}
			i++;
		}
	}

	return names;
}

std::unordered_set<std::string> buildAsmClobberNamesList(const Module& M)
{
	//We use an (unordered_)set since we expect repetitions
	std::unordered_set<std::string> ret;

	// Look for clobber lists from inline asm calls
	for(const Function& F: M)
	{
		for(const BasicBlock& BB: F)
		{
			for(const Instruction& I: BB)
			{
				const CallInst* CI = dyn_cast<CallInst>(&I);
				if(!CI)
					continue;
				const InlineAsm* IA = dyn_cast<InlineAsm>(CI->getCalledOperand());
				if(!IA)
					continue;
				auto constraints = IA->ParseConstraints();
				for(const auto& c: constraints)
				{
					if(c.Type != InlineAsm::isClobber)
						continue;
					for(const auto& s: c.Codes)
					{
						if(s.size() < 2 || s.front() != '{' || s.back() != '}')
							continue;
						ret.emplace(s.begin()+1, s.end()-1);
					}
				}
			}
		}
	}

	return ret;
}

std::vector<std::string> buildReservedNamesList(const Module& M, const std::vector<std::string>& fromOption)
{
	std::set<std::string> ret(fromOption.begin(), fromOption.end());

	const auto& asmClobber = buildAsmClobberNamesList(M);
	ret.insert(asmClobber.begin(), asmClobber.end());

	const auto& jsExported = buildJsExportedNamesList(M);
	ret.insert(jsExported.begin(), jsExported.end());

	return std::vector<std::string>(ret.begin(), ret.end());
}

}

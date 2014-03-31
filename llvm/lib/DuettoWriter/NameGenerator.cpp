
#include "llvm/Duetto/NameGenerator.h"

using namespace llvm;

namespace duetto {

uint32_t NameGenerator::getUniqueIndexForValue(const Value* v) const
{
	std::map<const Value*,uint32_t>::iterator it=unnamedValueMap.find(v);
	if(it==unnamedValueMap.end())
		it=unnamedValueMap.insert(std::make_pair(v, currentUniqueIndex++)).first;
	return it->second;
}

uint32_t NameGenerator::getUniqueIndex()
{
	return currentUniqueIndex++;
}

}

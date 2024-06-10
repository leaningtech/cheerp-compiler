//
// Created by c on 22-5-24.
//

#include <algorithm>
#include <limits>

#include "llvm/MC/MCWasmStreamer.h"
#include "CFGStackifier.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Cheerp/BuiltinInstructions.h"
#include "llvm/Cheerp/CommandLine.h"
#include "llvm/Cheerp/PHIHandler.h"
#include "llvm/Cheerp/NameGenerator.h"
#include "llvm/Cheerp/WasmMCStreamer.h"
#include "llvm/Cheerp/Writer.h"
#include "llvm/IR/IntrinsicsWebAssembly.h"
#include "llvm/IR/ProfDataUtils.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/LEB128.h"

using namespace cheerp;
using namespace llvm;
using namespace std;

static uint32_t COMPILE_METHOD_LIMIT = 10000;

static inline void encodeF32(float f, WasmBuffer& stream)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  //TODO: check the function used to encode float type
}

static inline void encodeF64(float f, WasmBuffer& stream)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  //TODO: check the function used to encode double type
}

static inline void encodeRegisterKind(Registerize::REGISTER_KIND regKind, WasmBuffer& stream)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  //TODO: check the function used to encode registerize type
}

static uint32_t getValType(const Type* t)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  //TODO: check if there is any functions used to return the type code
  if (t->isIntegerTy(64))
    return 0x7e;
  else if (t->isIntegerTy() || TypeSupport::isRawPointer(t, true))
    return 0x7f;
  else if (t->isFloatTy())
    return 0x7d;
  else if (t->isDoubleTy())
    return 0x7c;
  else if (t->isPointerTy())
    return 0x6f;
  else if (t->isVectorTy())
    return 0x7b;
  else
  {
#ifndef NDEBUG
    llvm::errs() << "Unsupported type ";
    t->dump();
#endif
    llvm_unreachable("Unsuppored type");
  }
}

static void encodeValType(const Type* t, WasmBuffer& stream)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  //TODO: check the type transfer from Type* to MCExpr*
  // stream.emitULEB128Value();
}

static void encodeLiteralType(const Type* t, WasmBuffer& stream)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  //TODO: check the type transfer & the implementation in MCStream
  if (t->isIntegerTy(64))
    return;//encodeULEB128(0x42, stream);
  else if (t->isIntegerTy() || TypeSupport::isRawPointer(t, true))
    return;//encodeULEB128(0x41, stream);
  else if(t->isFloatTy())
    return;//encodeULEB128(0x43, stream);
  else if(t->isDoubleTy())
    return;//encodeULEB128(0x44, stream);
  else
  {
#ifndef NDEBUG
    llvm::errs() << "Unsupported type: ";
    t->dump();
#endif
    llvm_unreachable("Unsuppored type");
  }
}

std::string string_to_hex(StringRef input)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  static const char* const lut = "0123456789abcdef";
  size_t len = input.size();

  std::string output;
  output.reserve(2 * len);
  for (size_t i = 0; i < len; ++i)
  {
    const unsigned char c = input[i];
    output.push_back(lut[c >> 4]);
    output.push_back(lut[c & 15]);
    if ((i & 1) == 1 && (i + 1) < len)
      output.push_back(' ');
  }
  return output;
}

enum class BranchHint
{
  Likely,
  Unlikely,
  Neutral,
};

static BranchHint shouldBranchBeHinted(const llvm::BranchInst* bi, const bool IfNot)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  uint64_t weight_false = 0;
  uint64_t weight_true = 0;
  if (extractBranchWeights(*bi, weight_true, weight_false))
  {
    if (IfNot)
      std::swap(weight_false, weight_true);

    const uint64_t total = weight_true + weight_false;
    if (total == 0)
      return BranchHint::Neutral;

    auto isConvenientToHint = [](uint64_t part, uint64_t total) -> bool {
      return (part*1.0 / total) > 0.8;
    };

    if (isConvenientToHint(weight_true, total))
      return BranchHint::Likely;

    if (isConvenientToHint(weight_false, total))
      return BranchHint::Unlikely;
  }
  return BranchHint::Neutral;
}

/**TODO: this function should be replaced with the section change inside MCStream
* @{
*/
Section::Section(uint32_t sectionId, const char* sectionName, CheerpWasmWriter* writer)
    : hasName(sectionName), name(hasName ? (sectionName) : ""), sectionId(sectionId), writer(writer)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  // Custom sections have a section name.
  if (!sectionId) {
    encodeULEB128(strlen(sectionName), *this);
    (*this) << sectionName;
  }
}

void Section::encode()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  assert(state == State::GENERATING);

#if WASM_DUMP_SECTIONS
  uint64_t start = writer->stream.tell();
  if (hasName)
    llvm::errs() << name << " ";
  else
    llvm::errs() << "unnamed section ";
  llvm::errs() << "id=" << sectionId << " ";
  llvm::errs() << "start=" << start << " ";
  llvm::errs() << "end=" << start + tell() << " ";
  llvm::errs() << "size=" << tell() << "\n";
#if WASM_DUMP_SECTION_DATA
  llvm::errs() << "section: " << string_to_hex(str()) << '\n';
#endif
#endif
  encodeULEB128(sectionId, writer->stream);

  encodeULEB128(tell(), writer->stream);
  writer->stream << str();

  state = State::ENCODED;
}

void Section::discard()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  assert(state == State::GENERATING);
  state = State::DISCARDED;
}

Section::~Section()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  assert(state == State::ENCODED || state == State::DISCARDED);
}
/** @} */

enum ConditionRenderMode {
  NormalCondition = 0,
  InvertCondition
};

uint32_t CheerpWasmWriter::findDepth(const llvm::Value *v) const
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO: using MCWasmStreamer
  return 0;
}

void CheerpWasmWriter::filterNop(llvm::SmallVectorImpl<char> &buffer, std::function<void(uint32_t, char)> filterCallback) const
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO: using MCWasmStreamer
}

void CheerpWasmWriter::encodeBinOp(const llvm::Instruction& I, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmOpcode opcode, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmS32Opcode opcode, int32_t immediate, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmS64Opcode opcode, int64_t immediate, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmU32Opcode opcode, uint32_t immediate, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmFCU32Opcode opcode, uint32_t immediate, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmFCU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmSIMDOpcode opcode, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmSIMDU32Opcode opcode, uint32_t immediate, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmSIMDU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmSIMDU32U32U32Opcode opcode, uint32_t i1, uint32_t i2, uint32_t i3, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmThreadsU32Opcode opcode, uint32_t immediate, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmThreadsU32U32Opcode opcode, uint32_t i1, uint32_t i2, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeInst(WasmInvalidOpcode opcode, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeVectorConstantZero(WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeConstantDataVector(WasmBuffer& code, const llvm::ConstantDataVector* cdv)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeConstantVector(WasmBuffer& code, const llvm::ConstantVector* cv)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeExtractLane(WasmBuffer& code, const llvm::ExtractElementInst& eei)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeReplaceLane(WasmBuffer& code, const llvm::InsertElementInst& iei)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeVectorTruncation(WasmBuffer& code, const llvm::Instruction& I)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeLoadingShuffle(WasmBuffer& code, const llvm::FixedVectorType* vecType)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeStoringShuffle(WasmBuffer& code, const llvm::FixedVectorType* vecType)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeBranchHint(const llvm::BranchInst* BI, const bool IfNot, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodePredicate(const llvm::Type* ty, const llvm::CmpInst::Predicate predicate, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeLoad(llvm::Type* ty, uint32_t offset,
                                  WasmBuffer& code, bool signExtend, bool atomic)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeWasmIntrinsic(WasmBuffer& code, const llvm::Function* F)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

bool CheerpWasmWriter::requiresExplicitAssigment(const Instruction* phi, const Value* incoming)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return 0;
}

int CheerpWasmWriter::gainOfHandlingPhiOnTheEdge(const PHINode* phi, const Value* incoming) const
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return 0;
}

int CheerpWasmWriter::gainOfHandlingPhiOnTheEdge(const PHINode* phi) const
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return 0;
}

void CheerpWasmWriter::compilePHIOfBlockFromOtherBlock(WasmBuffer& code, const BasicBlock* to, const BasicBlock* from, const PHINode* phiHandledAsResult)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

const char* CheerpWasmWriter::getTypeString(const Type* t)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return nullptr;
}

void CheerpWasmWriter::compileGEP(WasmBuffer& code, const llvm::User* gep_inst, bool standalone)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeBranchTable(WasmBuffer& code, std::vector<uint32_t> table, int32_t defaultBlock)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileSignedInteger(WasmBuffer& code, const llvm::Value* v, bool forComparison)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileUnsignedInteger(WasmBuffer& code, const llvm::Value* v)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileTypedZero(WasmBuffer& code, const llvm::Type* t)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileConstantExpr(WasmBuffer& code, const ConstantExpr* ce)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

static uint32_t getSLEBEncodingLength(int64_t val)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return 0;
}

static uint32_t getULEBEncodingLength(int64_t val)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return 0;
}

void CheerpWasmWriter::compileFloatToText(WasmBuffer& code, const APFloat& f, uint32_t precision)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileConstant(WasmBuffer& code, const Constant* c, bool forGlobalInit)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileGetLocal(WasmBuffer& code, const llvm::Instruction* I, uint32_t elemIdx)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileAggregateElem(WasmBuffer& code, const llvm::Value* v, uint32_t elemIdx)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileOperand(WasmBuffer& code, const llvm::Value* v)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

const char* CheerpWasmWriter::getIntegerPredicate(llvm::CmpInst::Predicate p)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return nullptr;
}

bool CheerpWasmWriter::isSignedLoad(const Value* V) const
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return false;
}

void CheerpWasmWriter::compileICmp(const Value* op0, const Value* op1, const CmpInst::Predicate p,
                                   WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileICmp(const ICmpInst& ci, const CmpInst::Predicate p,
                                   WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileFCmp(const Value* lhs, const Value* rhs, CmpInst::Predicate p, WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileDowncast(WasmBuffer& code, const CallBase* callV)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

uint32_t CheerpWasmWriter::compileLoadStorePointer(WasmBuffer& code, const Value* ptrOp)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return 0;
}

void CheerpWasmWriter::compileLoad(WasmBuffer& code, const LoadInst& li, bool signExtend)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileStore(WasmBuffer& code, const StoreInst& si)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

bool CheerpWasmWriter::compileInstruction(WasmBuffer& code, const Instruction& I)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return false;
}

bool CheerpWasmWriter::isTailCall(const CallInst& ci) const
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return false;
}

bool CheerpWasmWriter::isReturnPartOfTailCall(const Instruction& ti) const
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return false;
}

void CheerpWasmWriter::checkAndSanitizeDependencies(InstructionToDependenciesMap& dependencies) const
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::flushGeneric(WasmBuffer& code, const Instruction& I, const InstructionToDependenciesMap& dependencies)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::flushMemoryDependencies(WasmBuffer& code, const Instruction& I)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::flushSetLocalDependencies(WasmBuffer& code, const Instruction& I)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

bool CheerpWasmWriter::compileInlineInstruction(WasmBuffer& code, const Instruction& I)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return false;
}

void CheerpWasmWriter::compileInstructionAndSet(WasmBuffer& code, const llvm::Instruction& I)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

bool CheerpWasmWriter::shouldDefer(const llvm::Instruction* I) const
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return false;
}

void CheerpWasmWriter::compileBB(WasmBuffer& code, const BasicBlock& BB, const PHINode* phiHandledAsResult)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::renderDeferred(WasmBuffer& code, const vector<const llvm::Instruction*>& deferred)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileMethodLocals(WasmBuffer& code, const vector<int>& locals)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileMethodParams(WasmBuffer& code, const FunctionType* fTy)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileMethodResult(WasmBuffer& code, const Type* ty)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileCondition(WasmBuffer& code, const llvm::Value* cond, bool booleanInvert)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileBranchTable(WasmBuffer& code, const llvm::SwitchInst* si,
                                          const std::vector<std::pair<int, int>>& cases)
{
  ///TODO
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
}

static int getResultKind(const Token& T)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return 0;
}

const BasicBlock* CheerpWasmWriter::compileTokens(WasmBuffer& code,
                                                  const TokenList& Tokens)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return nullptr;
}

std::map<const llvm::BasicBlock*, const llvm::PHINode*> CheerpWasmWriter::selectPHINodesHandledAsResult(const std::vector<const llvm::BasicBlock*>& possibleBB) const
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return std::map<const llvm::BasicBlock*, const llvm::PHINode*>();
}

void CheerpWasmWriter::compileMethod(WasmBuffer& code, const Function& F)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::checkImplicitedAssignedPhi(const llvm::Function& F)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::generateNOP(WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileTypeSection()
{
  if (linearHelper.getFunctionTypes().empty())
    return;
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";

//  MCStreamer->changeSection(nullptr, nullptr);///TODO: mcsection & mcexpr
//
//  MCStreamer->emitULEB128Value(nullptr);///TODO: size into Value
//
//  for (const auto& fTy : linearHelper.getFunctionTypes()){
//    MCStreamer->emitULEB128Value(nullptr);///TODO: 0x60 into value
//    MCStreamer->emitCOFFSymbolType(NULL);///TODO: or maybe use existing funcitons
//  }
  ///TODO
}

void CheerpWasmWriter::compileImport(WasmBuffer& code, StringRef funcName, FunctionType* fTy)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileImportMemory(WasmBuffer& code)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileImportSection()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileFunctionSection()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileTableSection()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

CheerpWasmWriter::GLOBAL_CONSTANT_ENCODING CheerpWasmWriter::shouldEncodeConstantAsGlobal(const Constant* C, uint32_t useCount, uint32_t getGlobalCost)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
  return CheerpWasmWriter::GLOBAL_CONSTANT_ENCODING::NONE;
}

void CheerpWasmWriter::compileMemorySection()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileGlobalSection()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileExportSection()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileElementSection()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileDataCountSection()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileCodeSection()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::encodeDataSectionChunk(WasmBuffer& data, uint32_t address, StringRef buf)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileDataSection()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileNameSection()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO
}

void CheerpWasmWriter::compileModule()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  ///TODO: use the functions inside streamer to compile magic number of wasm
  /// use the functions insede streamer to compile version number
  /// or it is not necessary for MCStreamer

  compileTypeSection();

  compileImportSection();

  compileFunctionSection();

  compileTableSection();

  if (!useWasmLoader)
    compileMemorySection();

  compileGlobalSection();

  compileExportSection();

  compileElementSection();

  compileDataCountSection();

  compileCodeSection();

  compileDataSection();

  if (prettyCode) {
    compileNameSection();
  }
}

void CheerpWasmWriter::makeWasm()
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  compileModule();
}

void CheerpWasmWriter::WasmGepWriter::addValue(const llvm::Value* v, uint32_t size)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  addedValues.emplace_back(v, size);
}

void CheerpWasmWriter::WasmGepWriter::subValue(const llvm::Value* v, uint32_t size)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  subbedValues.emplace_back(v, size);
}

void CheerpWasmWriter::WasmGepWriter::compileValue(const llvm::Value* v, uint32_t size) const
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  writer.compileOperand(code, v);
  if (size > 1)
  {
    if (isPowerOf2_32(size))
    {
      writer.encodeInst(WasmS32Opcode::I32_CONST, Log2_32(size), code);
      writer.encodeInst(WasmOpcode::I32_SHL, code);
    }
    else
    {
      writer.encodeInst(WasmS32Opcode::I32_CONST, size, code);
      writer.encodeInst(WasmOpcode::I32_MUL, code);
    }
  }
}

uint32_t CheerpWasmWriter::WasmGepWriter::compileValues(bool positiveOffsetAllowed) const
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  struct ValuesToAdd
  {
    ValuesToAdd(const llvm::Value* v, bool toInvert, uint32_t multiplier)
        : v(v), toInvert(toInvert), multiplier(multiplier)
    {
    }
    const llvm::Value* v;
    bool toInvert;
    uint32_t multiplier;
  };

  std::vector<ValuesToAdd> V;

  for (auto& it: addedValues)
  {
    V.push_back(ValuesToAdd(it.first, /*toInvert*/false, it.second));
  }

  for (auto& it: subbedValues)
  {
    V.push_back(ValuesToAdd(it.first, /*toInvert*/true, it.second));
  }
  //Since we first insert addedValues and only then subbedValues
  //And the sorting method is stable, for a given multiplier class,
  //IFF there are values with toInvert == false they have to be clustered at the beginning

  std::sort(V.begin(), V.end(), [](const ValuesToAdd& A, const ValuesToAdd& B) -> bool {
    if (A.multiplier != B.multiplier)
      return (A.multiplier > B.multiplier);
    return false;
  }
  );

  struct GroupedValuesToAdd
  {
    GroupedValuesToAdd(uint32_t multiplier)
        : constantPart(0), multiplier(multiplier)
    {
    }
    void add(const ValuesToAdd& v)
    {
      assert(v.multiplier == multiplier);
      if (v.toInvert)
        valuesToInvert.push_back(v.v);
      else
        valuesToAdd.push_back(v.v);
    }
    void addConstant(uint32_t c)
    {
      assert(c % multiplier == 0);
      assert(c > 0);
      constantPart += c/multiplier;
    }
    bool hasPositive() const
    {
      return constantPart != 0 || !valuesToAdd.empty();
    }
    void sort(const CheerpWasmWriter& writer)
    {
      sortImpl(writer, valuesToAdd);
      sortImpl(writer, valuesToInvert);
    }
  private:
    void sortImpl(const CheerpWasmWriter& writer, std::vector<const llvm::Value*>& toSort)
    {
      std::vector<std::pair<uint32_t, const llvm::Value*>> V;
      for (const llvm::Value* v : toSort)
      {
        V.push_back({writer.findDepth(v), v});
      }
      std::sort(V.begin(), V.end(), [](const std::pair<uint32_t, const llvm::Value*>& A, const std::pair<uint32_t, const llvm::Value*>&B) -> bool
                {
                  if (A.first != B.first)
                    return A.first < B.first;
                  return false;
                });
      for (uint32_t i=0; i<V.size(); i++)
      {
        toSort[i] = V[i].second;
      }

    }
  public:
    uint32_t constantPart;
    uint32_t multiplier;
    std::vector<const llvm::Value*> valuesToAdd;
    std::vector<const llvm::Value*> valuesToInvert;
  };

  std::vector<GroupedValuesToAdd> V2;

  for (auto& v: V)
  {
    if (v.multiplier == 0)
      continue;
    if (V2.empty() || V2.back().multiplier != v.multiplier)
    {
      V2.push_back(GroupedValuesToAdd(v.multiplier));
    }
    V2.back().add(v);
  }

  auto initializeYetToBeEncodedOffset = [this, &positiveOffsetAllowed](std::vector<GroupedValuesToAdd>& V2, bool& first) -> uint32_t
  {
    if (!positiveOffsetAllowed)
    {
      uint32_t toBeHandled = constPart;
      //In these cases constantPart should not be used, so it has to be merged in the GroupedValuesToAdd's vector
      if (toBeHandled != 0)
      {
        for (auto& grouped : V2)
        {
          if (toBeHandled % grouped.multiplier == 0)
          {
            grouped.addConstant(toBeHandled);
            toBeHandled = 0;
            break;
          }
        }
      }
      if (toBeHandled != 0)
      {
        V2.push_back(GroupedValuesToAdd(1));
        V2.back().addConstant(toBeHandled);
        toBeHandled = 0;
      }
      assert(toBeHandled == 0);
      return 0;
    }
    else if (std::none_of(V2.begin(), V2.end(), [](const GroupedValuesToAdd& g)->bool{return g.hasPositive();}))
    {
      //If we have to put a 0, it's always beneficial to put the maximum value that comes in the single byte encoding
      writer.encodeInst(WasmS32Opcode::I32_CONST, 0, code);
      first = false;
      return constPart;
    }
    else
    {
      return constPart;
    }
  };

  bool first = true;
  //The call to the lambda will properly initialize yetToBeEncodedOffset AND set first to true if something has been written in the stack
  const uint32_t yetToBeEncodedOffset = initializeYetToBeEncodedOffset(V2, first);

  std::stable_partition(V2.begin(), V2.end(), [](const GroupedValuesToAdd& g) -> bool { return g.hasPositive(); });

  if (first)
    assert(!V2.empty() && V2.front().hasPositive());

  for (GroupedValuesToAdd& p : V2)
  {
    p.sort(writer);
  }

  bool singlePositiveValue = true;
  for (const GroupedValuesToAdd& p : V2)
  {
    const bool invertSign = (p.hasPositive() == false) && (first == false);
    bool is_first = true;

    for (const llvm::Value* v : p.valuesToAdd)
    {
      writer.compileOperand(code, v);
      if (writer.needsUnsignedTruncation(v, /*asmjs*/true))
        singlePositiveValue = false;
      if (!is_first)
      {
        writer.encodeInst(WasmOpcode::I32_ADD, code);
        singlePositiveValue = false;
      }
      is_first = false;
    }

    if (p.constantPart)
    {
      writer.encodeInst(WasmS32Opcode::I32_CONST, p.constantPart, code);
      if (!is_first)
        writer.encodeInst(WasmOpcode::I32_ADD, code);
      singlePositiveValue = false;
      is_first = false;
    }

    for (const llvm::Value* v : p.valuesToInvert)
    {
      writer.compileOperand(code, v);
      if (!is_first)
      {
        if (invertSign)
          writer.encodeInst(WasmOpcode::I32_ADD, code);
        else
          writer.encodeInst(WasmOpcode::I32_SUB, code);
      }
      singlePositiveValue = false;
      is_first = false;
    }

    const uint32_t sizeCurr = p.multiplier * (invertSign ? -1 : 1);
    if (sizeCurr > 1)
    {
      if (isPowerOf2_32(sizeCurr))
      {
        writer.encodeInst(WasmS32Opcode::I32_CONST, Log2_32(sizeCurr), code);
        writer.encodeInst(WasmOpcode::I32_SHL, code);
      }
      else
      {
        writer.encodeInst(WasmS32Opcode::I32_CONST, sizeCurr, code);
        writer.encodeInst(WasmOpcode::I32_MUL, code);
      }
      singlePositiveValue = false;
    }

    if(!first)
    {
      if (invertSign || p.hasPositive())
        writer.encodeInst(WasmOpcode::I32_ADD, code);
      else
        writer.encodeInst(WasmOpcode::I32_SUB, code);
    }
    first = false;
  }

  //In any case we have put something on the stack
  assert(!first);

  // We assume no access to the first 4k is legal, so a small offset can be safely encoded in the load/store
  if(!singlePositiveValue && yetToBeEncodedOffset > 4096)
  {
    writer.encodeInst(WasmS32Opcode::I32_CONST, yetToBeEncodedOffset, code);
    writer.encodeInst(WasmOpcode::I32_ADD, code);
    return 0;
  }

  return yetToBeEncodedOffset;
}

void CheerpWasmWriter::WasmGepWriter::addConst(int64_t v)
{
  errs()<<std::string(__PRETTY_FUNCTION__)+"\n";
  assert(v);
  // Just make sure that the constant part of the offset is not too big
  // TODO: maybe use i64.const here instead of crashing
  assert(v>=std::numeric_limits<int32_t>::min());
  assert(v<=std::numeric_limits<int32_t>::max());

  constPart += v;
}


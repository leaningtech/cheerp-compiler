#include "llvm/Cheerp/CommandLine.h"

llvm::cl::opt<std::string> WasmLoader("cheerp-wasm-loader", llvm::cl::Optional,
  llvm::cl::desc("If specified, the file name of the wasm loader"), llvm::cl::value_desc("filename"));

llvm::cl::opt<std::string> WasmFile("cheerp-wasm-file", llvm::cl::Optional,
  llvm::cl::desc("If specified, the file name of the wasm file"), llvm::cl::value_desc("filename"));

llvm::cl::opt<std::string> AsmJSMemFile("cheerp-asmjs-mem-file", llvm::cl::Optional,
  llvm::cl::desc("If specified, the file name of the asm.js module initialized memory dump"), llvm::cl::value_desc("filename"));

llvm::cl::opt<std::string> SourceMap("cheerp-sourcemap", llvm::cl::Optional,
  llvm::cl::desc("If specified, the file name of the source map"), llvm::cl::value_desc("filename"));

llvm::cl::opt<std::string> SourceMapPrefix("cheerp-sourcemap-prefix", llvm::cl::Optional,
  llvm::cl::desc("If specified, this prefix will be removed from source map file paths"), llvm::cl::value_desc("path"));

llvm::cl::opt<std::string> MakeModule("cheerp-make-module", llvm::cl::Optional, llvm::cl::desc("Create a [closure/commonjs] module around the generated code.") );

llvm::cl::opt<bool> SourceMapStandAlone("cheerp-sourcemap-standalone", llvm::cl::desc("Generate a standalone sourcemap by including _all_ sources in the map file") );

llvm::cl::opt<bool> PrettyCode("cheerp-pretty-code", llvm::cl::desc("Generate human-readable JS") );

llvm::cl::opt<bool> SymbolicGlobalsAsmJS("cheerp-asmjs-symbolic-globals", llvm::cl::desc("Compile global variables addresses as js variables in the asm.js module") );

llvm::cl::opt<bool> RegisterizeLegacy("cheerp-registerize-legacy", llvm::cl::desc("Use the legacy algorithm to assign registers") );

llvm::cl::opt<bool> NoNativeJavaScriptMath("cheerp-no-native-math", llvm::cl::desc("Disable native JavaScript math functions") );

llvm::cl::opt<bool> NoJavaScriptMathImul("cheerp-no-math-imul", llvm::cl::desc("Disable JavaScript Math.imul") );

llvm::cl::opt<bool> NoJavaScriptMathFround("cheerp-no-math-fround", llvm::cl::desc("Disable JavaScript Math.fround") );

llvm::cl::opt<bool> NoCredits("cheerp-no-credits", llvm::cl::desc("Disable Cheerp credits in JS") );

llvm::cl::opt<bool> MeasureTimeToMain("cheerp-measure-time-to-main", llvm::cl::desc("Print time elapsed until the first line of main() is executed") );

llvm::cl::opt<bool> ForceTypedArrays("cheerp-force-typed-arrays", llvm::cl::desc("Use typed arrays instead of normal arrays for arrays of doubles") );

llvm::cl::list<std::string> ReservedNames("cheerp-reserved-names", llvm::cl::value_desc("list"), llvm::cl::desc("A list of JS identifiers that should not be used by Cheerp"), llvm::cl::CommaSeparated);

llvm::cl::opt<std::string> GlobalPrefix("cheerp-global-prefix", llvm::cl::Optional, llvm::cl::desc("Prefix all global names with the given string"));

llvm::cl::opt<unsigned> CheerpHeapSize("cheerp-linear-heap-size", llvm::cl::init(8), llvm::cl::desc("Desired heap size for the cheerp wasm/asmjs module (in MB)") );

llvm::cl::opt<unsigned> CheerpStackSize("cheerp-linear-stack-size", llvm::cl::init(1), llvm::cl::desc("Desired stack size for the cheerp wasm/asmjs module (in MB)") );

llvm::cl::opt<bool> CheerpNoICF("cheerp-no-icf", llvm::cl::init(0), llvm::cl::desc("Disable identical code folding for wasm/asmjs") );

llvm::cl::opt<bool> BoundsCheck("cheerp-bounds-check", llvm::cl::desc("Generate debug code for bounds-checking arrays") );

llvm::cl::opt<bool> CfgLegacy("cheerp-cfg-legacy", llvm::cl::desc("Use the legacy relooper algorithm to render the cfg") );

llvm::cl::opt<bool> AvoidWasmTraps("cheerp-avoid-wasm-traps", llvm::cl::desc("Avoid traps from WebAssembly by generating more verbose code") );

llvm::cl::opt<bool> AggressiveGepOptimizer("cheerp-aggressive-gep-optimizer", llvm::cl::desc("Speculatively hoist part of GEPs when possible") );

llvm::cl::opt<bool> FixWrongFuncCasts("cheerp-fix-wrong-func-casts", llvm::cl::Optional, llvm::cl::desc("Generate wrappers for functions casted to types with more arguments") );

llvm::cl::opt<std::string> StrictLinking("cheerp-strict-linking", llvm::cl::Optional, llvm::cl::desc("Emit warnings/errors on missing symbols"), llvm::cl::value_desc("warning/error") );


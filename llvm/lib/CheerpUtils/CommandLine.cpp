#include "llvm/Cheerp/CommandLine.h"

llvm::cl::opt<LinearOutputTy> LinearOutput("cheerp-linear-output", llvm::cl::Optional,
  llvm::cl::desc("The output format of the linear memory part of the program [wasm/asmjs]. Default: wasm"),
  llvm::cl::value_desc("output"),
  llvm::cl::values(
    clEnumValN(Wasm, "wasm", "wasm linear output"),
    clEnumValN(AsmJs, "asmjs", "asmjs linear output")),
  llvm::cl::init(Wasm));

llvm::cl::opt<std::string> SecondaryOutputFile("cheerp-secondary-output-file", llvm::cl::Optional,
  llvm::cl::desc("If specified, the file name of the secondary output file"), llvm::cl::value_desc("filename"));

llvm::cl::opt<std::string> SecondaryOutputPath("cheerp-secondary-output-path", llvm::cl::Optional,
  llvm::cl::desc("If specified, the runtime relative path of the secondary output file"), llvm::cl::value_desc("filename"));

llvm::cl::opt<std::string> SourceMap("cheerp-sourcemap", llvm::cl::Optional,
  llvm::cl::desc("If specified, the file name of the source map"), llvm::cl::value_desc("filename"));

llvm::cl::opt<std::string> SourceMapPrefix("cheerp-sourcemap-prefix", llvm::cl::Optional,
  llvm::cl::desc("If specified, this prefix will be removed from source map file paths"), llvm::cl::value_desc("path"));

llvm::cl::opt<std::string> MakeModule("cheerp-make-module", llvm::cl::Optional, llvm::cl::desc("Create a [closure/commonjs] module around the generated code.") );

llvm::cl::opt<bool> WasmOnly("cheerp-wasm-only", llvm::cl::desc("Generate only the wasm module without the genericjs functions and the JS glue code") );

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

llvm::cl::opt<bool> AvoidWasmTraps("cheerp-avoid-wasm-traps", llvm::cl::desc("Avoid traps from WebAssembly by generating more verbose code") );

llvm::cl::opt<bool> AggressiveGepOptimizer("cheerp-aggressive-gep-optimizer", llvm::cl::desc("Speculatively hoist part of GEPs when possible") );

llvm::cl::opt<bool> FixWrongFuncCasts("cheerp-fix-wrong-func-casts", llvm::cl::Optional, llvm::cl::desc("Generate wrappers for functions casted to types with more arguments") );

llvm::cl::opt<std::string> StrictLinking("cheerp-strict-linking", llvm::cl::Optional, llvm::cl::desc("Emit warnings/errors on missing symbols"), llvm::cl::value_desc("warning/error") );

llvm::cl::opt<bool> WasmSharedMemory("cheerp-wasm-shared-memory", llvm::cl::desc("Enable sharing wasm module memory between workers"));

llvm::cl::opt<bool> WasmNoGrowMemory("cheerp-wasm-no-grow-memory", llvm::cl::desc("Disable memory growth and allocate all the wasm module memory upfront"));

llvm::cl::opt<bool> WasmExportedTable("cheerp-wasm-exported-table", llvm::cl::desc("Export the function table from the wasm module as 'tbl'"));

llvm::cl::opt<bool> WasmBranchHints("cheerp-wasm-branch-hinting", llvm::cl::desc("Enable generating branch hinting"));

llvm::cl::opt<bool> WasmAnyref("cheerp-wasm-externref", llvm::cl::desc("Enable support for the externref value type in wasm"));

llvm::cl::opt<bool> WasmReturnCalls("cheerp-wasm-return-calls", llvm::cl::desc("Enable return-call and return-call-indirect opcodes"));

llvm::cl::opt<bool> UseBigInts("cheerp-use-bigints", llvm::cl::desc("Use the BigInt type in JS to represent i64 values"));

llvm::cl::opt<bool> KeepInvokes("cheerp-keep-invokes", llvm::cl::desc("Don't lower invokes to calls"));

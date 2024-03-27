#include "llvm/Cheerp/JsExport.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Constants.h"

namespace cheerp {
	static std::string getJsName(llvm::StringRef name) {
		demangler_iterator demangler(name);
		const char* delim = "";
		std::string result;

		while (demangler != demangler_iterator()) {
			result += delim;
			result += *demangler++;
			delim = ".";
		}

		return result;
	}

	static llvm::StringRef getBaseName(llvm::StringRef name) {
		demangler_iterator demangler(name);
		llvm::StringRef result = *demangler++;

		while (demangler != demangler_iterator())
			result = *demangler++;

		return result;
	}

	static llvm::StringRef dropRecordPrefix(llvm::StringRef name) {
		if (name.startswith("class.")) {
			return name.drop_front(6);
		}

		assert(name.startswith("struct."));
		return name.drop_front(7);
	}

	template<class T>
	static llvm::iterator_range<JsExportIterator<T>> getJsExportRange(const llvm::Module& module, const llvm::Twine& name) {
		const auto* metadata = module.getNamedMetadata(name);
		JsExportIterator<T> begin(module, metadata ? metadata->op_begin() : llvm::NamedMDNode::const_op_iterator());
		JsExportIterator<T> end(module, metadata ? metadata->op_end() : llvm::NamedMDNode::const_op_iterator());
		return llvm::make_range(begin, end);
	}
	
	JsExportRecord::JsExportRecord(const llvm::Module& module, const llvm::MDNode* node) {
		name = llvm::cast<llvm::MDString>(node->getOperand(0))->getString();
		type = llvm::StructType::getTypeByName(module.getContext(), name);

		if (!type) {
			type = llvm::StructType::create(module.getContext(), name);
		}
	}

	llvm::StringRef JsExportRecord::getName() const {
		return name;
	}

	llvm::StructType* JsExportRecord::getType() const {
		return type;
	}

	std::string JsExportRecord::getJsName() const {
		return cheerp::getJsName(dropRecordPrefix(name));
	}
	
	JsExportFunction::JsExportFunction(const llvm::Module& module, const llvm::MDNode* node) {
		function = llvm::cast<llvm::Function>(llvm::cast<llvm::ConstantAsMetadata>(node->getOperand(0))->getValue());
		flags = llvm::cast<llvm::ConstantInt>(llvm::cast<llvm::ConstantAsMetadata>(node->getOperand(1))->getValue())->getZExtValue();
	}

	llvm::Function* JsExportFunction::getFunction() const {
		return function;
	}

	llvm::StringRef JsExportFunction::getBaseName() const {
		return cheerp::getBaseName(function->getName());
	}

	llvm::StringRef JsExportFunction::getPropertyName() const {
		assert(isGetter() || isSetter());
		return getBaseName().substr(13);
	}

	std::string JsExportFunction::getJsName() const {
		return cheerp::getJsName(function->getName());
	}

	bool JsExportFunction::isStatic() const {
		return flags & 1;
	}

	bool JsExportFunction::isConstructor() const {
		return getBaseName() == "new";
	}

	bool JsExportFunction::isGetter() const {
		return getBaseName().startswith("__cheerp_get_");
	}

	bool JsExportFunction::isSetter() const {
		return getBaseName().startswith("__cheerp_set_");
	}

	llvm::iterator_range<JsExportRecordIterator> getJsExportRecords(const llvm::Module& module) {
		return getJsExportRange<JsExportRecord>(module, "jsexport_records");
	}

	llvm::iterator_range<JsExportFunctionIterator> getJsExportFunctions(const llvm::Module& module) {
		return getJsExportRange<JsExportFunction>(module, "jsexport_functions");
	}
}

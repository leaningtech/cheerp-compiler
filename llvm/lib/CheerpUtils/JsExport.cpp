#include "llvm/Cheerp/JsExport.h"
#include "llvm/Cheerp/Utility.h"
#include "llvm/IR/Constants.h"

namespace cheerp {
	template<class T>
	static llvm::iterator_range<JsExportIterator<T>> getJsExportRange(const llvm::Module& module, const llvm::Twine& name) {
		const auto* metadata = module.getNamedMetadata(name);
		JsExportIterator<T> begin(module, metadata ? metadata->op_begin() : llvm::NamedMDNode::const_op_iterator());
		JsExportIterator<T> end(module, metadata ? metadata->op_end() : llvm::NamedMDNode::const_op_iterator());
		return llvm::make_range(begin, end);
	}

	JsExportName::JsExportName(llvm::StringRef name) : name(name) {
	}

	llvm::StringRef JsExportName::string() const {
		return name;
	}

	llvm::StringRef JsExportName::base() const {
		demangler_iterator demangler(name);
		llvm::StringRef result = *demangler++;

		while (demangler != demangler_iterator())
			result = *demangler++;

		return result;
	}

	llvm::StringRef JsExportName::root() const {
		return *demangler_iterator(name);
	}

	std::vector<llvm::StringRef> JsExportName::split() const {
		return std::vector(demangler_iterator(name), demangler_iterator());
	}

	std::string JsExportName::js() const {
		demangler_iterator demangler(name);
		std::string result(*demangler++);

		while (demangler != demangler_iterator()) {
			result += ".";
			result += *demangler++;
		}

		return result;
	}

	JsExportRecord::JsExportRecord(const llvm::Module& module, const llvm::MDNode* node) {
		llvm::StringRef name = llvm::cast<llvm::MDString>(node->getOperand(0))->getString();
		type = llvm::StructType::getTypeByName(module.getContext(), name);

		if (!type)
			type = llvm::StructType::create(module.getContext(), name);
	}

	JsExportName JsExportRecord::getName() const {
		llvm::StringRef name = type->getStructName();

		if (name.startswith("class."))
			return name.drop_front(6);

		assert(name.startswith("struct."));
		return name.drop_front(7);
	}

	llvm::StructType* JsExportRecord::getType() const {
		return type;
	}

	JsExportFunction::JsExportFunction(const llvm::Module& module, const llvm::MDNode* node) {
		function = llvm::cast<llvm::Function>(llvm::cast<llvm::ConstantAsMetadata>(node->getOperand(0))->getValue());
		flags = llvm::cast<llvm::ConstantInt>(llvm::cast<llvm::ConstantAsMetadata>(node->getOperand(1))->getValue())->getZExtValue();
	}

	llvm::Function* JsExportFunction::getFunction() const {
		return function;
	}

	JsExportName JsExportFunction::getName() const {
		return function->getName();
	}

	llvm::StringRef JsExportFunction::getPropertyName() const {
		assert(isProperty());
		return getName().base().substr(13);
	}

	bool JsExportFunction::isStatic() const {
		return flags & 1;
	}

	bool JsExportFunction::isConstructor() const {
		return getName().base() == "new";
	}

	bool JsExportFunction::isGetter() const {
		return getName().base().startswith("__cheerp_get_");
	}

	bool JsExportFunction::isSetter() const {
		return getName().base().startswith("__cheerp_set_");
	}

	bool JsExportFunction::isProperty() const {
		return isGetter() || isSetter();
	}

	llvm::StringRef JsExportProperty::getName() const {
		return getter->getPropertyName();
	}

	llvm::Type* JsExportProperty::getType() const {
		return getter->getFunction()->getReturnType();
	}

	bool JsExportProperty::hasGetter() const {
		return getter.has_value();
	}

	bool JsExportProperty::hasSetter() const {
		return setter.has_value();
	}

	const JsExportFunction& JsExportProperty::getGetter() const {
		return *getter;
	}

	const JsExportFunction& JsExportProperty::getSetter() const {
		return *setter;
	}

	bool JsExportProperty::isStatic() const {
		return getter->isStatic();
	}

	void JsExportProperty::insert(JsExportFunction&& func) {
		assert(func.isProperty());

		if (func.isGetter())
			getter = std::move(func);
		else
			setter = std::move(func);
	}

	JsExportClass::JsExportClass(llvm::StructType* type) : type(type) {
	}

	llvm::StructType* JsExportClass::getType() const {
		return type;
	}

	const JsExportMap<JsExportFunction>& JsExportClass::getMethods() const {
		return methods;
	}

	const JsExportMap<JsExportProperty>& JsExportClass::getProperties() const {
		return properties;
	}

	JsExportFunction& JsExportClass::insert(llvm::StringRef name, JsExportFunction&& method) {
		return methods.try_emplace(name, std::move(method)).first->second;
	}

	JsExportProperty& JsExportClass::insert(llvm::StringRef name, JsExportProperty&& property) {
		return properties.try_emplace(name, std::move(property)).first->second;
	}

	const JsExportMap<JsExport>& JsExportModule::getExports() const {
		return exports;
	}

	bool JsExportModule::hasTypes() const {
		return types;
	}

	JsExportRef JsExportModule::insert(llvm::ArrayRef<llvm::StringRef> name, JsExport&& ex) {
		if (std::holds_alternative<JsExportClass>(ex))
			types = true;

		if (name.size() == 1) {
			JsExport& value = exports.try_emplace(name[0], std::move(ex)).first->second;
			return std::visit([](auto& ex) -> JsExportRef { return &ex; }, value);
		}

		JsExportModule& module = std::get<JsExportModule>(exports[name[0]]);
		return module.insert(name.slice(1), std::move(ex));
	}

	llvm::iterator_range<JsExportRecordIterator> getJsExportRecords(const llvm::Module& module) {
		return getJsExportRange<JsExportRecord>(module, "jsexport_records");
	}

	llvm::iterator_range<JsExportFunctionIterator> getJsExportFunctions(const llvm::Module& module) {
		return getJsExportRange<JsExportFunction>(module, "jsexport_functions");
	}

	JsExportModule getJsExportModule(const llvm::Module& module) {
		JsExportModule exports;

		for (auto record : getJsExportRecords(module)) {
			auto name = record.getName().split();
			exports.insert(name, JsExportClass(record.getType()));
		}

		for (auto function : getJsExportFunctions(module)) {
			auto nameVector = function.getName().split();
			auto name = nameVector.back();
			JsExportRef parent = &exports;

			if (nameVector.size() != 1)
				parent = exports.insert(llvm::ArrayRef<llvm::StringRef>(nameVector).drop_back(), JsExportModule());

			if (function.isProperty()) {
				llvm::StringRef name = function.getPropertyName();
				JsExportProperty* property;

				if (auto* ex = std::get_if<JsExportClass*>(&parent))
					property = &(*ex)->insert(name, JsExportProperty());
				else if (auto* ex = std::get_if<JsExportModule*>(&parent))
					property = std::get<JsExportProperty*>((*ex)->insert(name, JsExportProperty()));

				property->insert(std::move(function));
			} else if (auto* ex = std::get_if<JsExportClass*>(&parent)) {
				(*ex)->insert(name, std::move(function));
			} else if (auto* ex = std::get_if<JsExportModule*>(&parent)) {
				(*ex)->insert(name, std::move(function));
			}
		}

		return exports;
	}
}

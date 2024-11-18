#include "scheme_script_loader.h"
#include "scheme_script.h"
#include <godot_cpp/classes/file_access.hpp>
#include <godot_cpp/core/class_db.hpp>

using namespace godot;

void SchemeScriptLoader::_bind_methods() {}

SchemeScriptLoader::SchemeScriptLoader() {}

SchemeScriptLoader::~SchemeScriptLoader() {}

PackedStringArray SchemeScriptLoader::_get_recognized_extensions() const {
	auto extensions = PackedStringArray();
	extensions.append("scm");
	return extensions;
}

bool SchemeScriptLoader::_handles_type(const StringName &type) const {
	return type == String("SchemeScript");
}

Variant SchemeScriptLoader::_load(
		const String &path,
		const String &original_path,
		bool use_sub_threads,
		int32_t cache_mode) const {
	auto code = FileAccess::get_file_as_string(path);
	return memnew(SchemeScript(code));
}

String SchemeScriptLoader::_get_resource_type(const String &p_path) const {
	if (p_path.get_extension().to_lower() == "scm") {
		return "SchemeScript";
	}
	return "";
}
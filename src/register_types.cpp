#include "register_types.h"
#include "scheme.hpp"
#include "scheme_object.hpp"
#include "scheme_script.hpp"
#include "scheme_script_loader.hpp"
#include <gdextension_interface.h>
#include <godot_cpp/classes/resource_loader.hpp>
#include <godot_cpp/core/class_db.hpp>
#include <godot_cpp/core/defs.hpp>
#include <godot_cpp/godot.hpp>

using namespace godot;

static Ref<SchemeScriptLoader> script_loader;

void initialize_gdextension_types(ModuleInitializationLevel p_level) {
	if (p_level != MODULE_INITIALIZATION_LEVEL_SCENE) {
		return;
	}
	GDREGISTER_CLASS(SchemeScript);
	GDREGISTER_CLASS(SchemeScriptLoader);
	GDREGISTER_CLASS(Scheme);
	GDREGISTER_CLASS(SchemeObject);

	script_loader.instantiate();
	ResourceLoader::get_singleton()->add_resource_format_loader(script_loader);
}

void uninitialize_gdextension_types(ModuleInitializationLevel p_level) {
	if (p_level != MODULE_INITIALIZATION_LEVEL_SCENE) {
		return;
	}

	ResourceLoader::get_singleton()->remove_resource_format_loader(script_loader);
	script_loader.unref();
}

extern "C" {
GDExtensionBool GDE_EXPORT godot_s7_scheme_library_init(
		GDExtensionInterfaceGetProcAddress p_get_proc_address,
		GDExtensionClassLibraryPtr p_library,
		GDExtensionInitialization *r_initialization) {
	GDExtensionBinding::InitObject init_obj(p_get_proc_address, p_library, r_initialization);
	init_obj.register_initializer(initialize_gdextension_types);
	init_obj.register_terminator(uninitialize_gdextension_types);
	init_obj.set_minimum_library_initialization_level(MODULE_INITIALIZATION_LEVEL_SCENE);

	return init_obj.init();
}
}

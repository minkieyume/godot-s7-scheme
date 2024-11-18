#ifndef GODOT_S7_SCHEME_SCHEME_SCRIPT_H
#define GODOT_S7_SCHEME_SCHEME_SCRIPT_H

#include <godot_cpp/classes/resource.hpp>

namespace godot {
class SchemeScript : public Resource {
  GDCLASS(SchemeScript, Resource)

public:
  SchemeScript();
  SchemeScript(const String &code) : code(code) {}
  ~SchemeScript();

  const String &get_code() const { return code; }

protected:
  static void _bind_methods();

private:
  String code;
};

} // namespace godot

#endif //GODOT_S7_SCHEME_SCHEME_SCRIPT_H

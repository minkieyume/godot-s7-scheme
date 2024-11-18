#include "debug_macros.h"

#if DEBUG_LOG

#include "ffi.h"
#include <iostream>

using namespace godot;

s7_pointer watch_s7_value(
    s7_scheme *sc, const char *func, int line, const char *e, s7_pointer v) {
  std::cout << func << ":" << line << ":" << e << ":";
  auto type = scheme_object_to_godot_string(sc, s7_type_of(sc, v));
  auto str = scheme_object_to_godot_string(sc, v);
  std::cout << type.utf8() << ":" << str.utf8() << std::endl;
  return v;
}

const Variant &watch_variant(const char *func, int line, const char *e, const Variant &v) {
  std::cout << func << ":" << line << ":" << e << ":" << v.stringify().utf8() << std::endl;
  return v;
}
#endif
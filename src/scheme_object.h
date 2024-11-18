
#ifndef SCHEME_OBJECT_H
#define SCHEME_OBJECT_H

#include "s7.hpp"

#include <godot_cpp/classes/ref_counted.hpp>

namespace godot {
class SchemeObject : public RefCounted {
  GDCLASS(SchemeObject, RefCounted)

public:
  SchemeObject() : sc(nullptr), scheme_ptr(nullptr) {}
  SchemeObject(s7_scheme* sc, s7_pointer shared) : sc(sc), scheme_ptr(std::move(s7_gc_protected(sc, shared))){}

  bool belongs_to(const s7_scheme* scheme) const {
    return sc == scheme;
  }

  [[nodiscard]] s7_pointer get_scheme_ptr() const {
    return scheme_ptr.get();
  }
protected:
  static void _bind_methods();

private:
  const s7_scheme* sc;
  s7_protected_ptr scheme_ptr;
};
}

#endif //SCHEME_OBJECT_H

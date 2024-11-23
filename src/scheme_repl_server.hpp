#ifndef GODOT_S7_SCHEME_SCHEME_REPL_SERVER_H
#define GODOT_S7_SCHEME_SCHEME_REPL_SERVER_H

#include <godot_cpp/classes/object.hpp>
#include <godot_cpp/classes/thread.hpp>

namespace godot {

class SchemeReplServer : public Object {
  GDCLASS(SchemeReplServer, Object);

  static SchemeReplServer *singleton;

private:
  mutable bool exit_thread;
  Ref<Thread> thread;

public:
  static SchemeReplServer *get_singleton();
  SchemeReplServer();
  Error init();
  void finish();

protected:
  static void _bind_methods();
  void server_loop();
};
} //namespace godot
#endif //GODOT_S7_SCHEME_SCHEME_REPL_SERVER_H

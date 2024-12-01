#ifndef GODOT_S7_SCHEME_SCHEME_REPL_SERVER_H
#define GODOT_S7_SCHEME_SCHEME_REPL_SERVER_H

#include "repl/mediator.hpp"
#include <godot_cpp/classes/object.hpp>
#include <godot_cpp/classes/thread.hpp>

namespace godot {

class Scheme;

class SchemeReplServer : public Object {
  GDCLASS(SchemeReplServer, Object);

public: // public API
  static SchemeReplServer *get_singleton();
  void publish_node(const Scheme *node);
  void unpublish_node(const Scheme *node);
  void reply(String result, uint64_t connection_id);

private:
  static SchemeReplServer *singleton;
  bool exit_thread;
  Ref<Thread> thread;
  ReplMediator::MessageQueue message_queue;

public: // extension initialization API
  SchemeReplServer();
  ~SchemeReplServer();
  Error start();
  void stop();

protected:
  static void _bind_methods();
  void server_loop(int tcp_port, const String &tcp_bind_address);
};
} //namespace godot
#endif //GODOT_S7_SCHEME_SCHEME_REPL_SERVER_H

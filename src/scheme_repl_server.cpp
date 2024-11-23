#include "scheme_repl_server.hpp"
#include "repl/mediator.hpp"
#include <godot_cpp/classes/os.hpp>
#include <godot_cpp/variant/utility_functions.hpp>

using namespace godot;
using gd = godot::UtilityFunctions;

void SchemeReplServer::server_loop() {
  // TODO: only start server when --s7-tcp-port=<number> and/or --s7-tcp-address=<address> are present in OS::get_cmdline_args()

  String tcp_bind_address = "127.0.0.1";
  int tcp_port = 0;

  Ref<TCPServer> tcp_server;
  tcp_server.instantiate();

  auto error = tcp_server->listen(tcp_port, tcp_bind_address);
  ERR_FAIL_COND_MSG(
      error != OK,
      ("Failed to start scheme repl server: " + gd::error_string(error)));

  gd::print("Scheme repl server listening on local port ", tcp_server->get_local_port());

  auto mediator = ReplMediator(tcp_server);
  while (!exit_thread) {
    mediator.mediate();
    OS::get_singleton()->delay_msec(50);
  }
  tcp_server->stop();
}

Error SchemeReplServer::init() {
  ERR_FAIL_COND_V_MSG(thread.is_valid(), ERR_BUG, "Scheme repl server can only be started once!");
  exit_thread = false;
  thread.instantiate();
  return thread->start(Callable::create(this, "server_loop"), Thread::PRIORITY_LOW);
}

void SchemeReplServer::finish() {
  if (thread.is_null()) {
    return;
  }

  exit_thread = true;
  thread->wait_to_finish();

  thread.unref();
}

SchemeReplServer *SchemeReplServer::singleton = NULL;

SchemeReplServer *SchemeReplServer::get_singleton() {
  return singleton;
}

SchemeReplServer::SchemeReplServer() {
  singleton = this;
}

void SchemeReplServer::_bind_methods() {
  ClassDB::bind_method(D_METHOD("server_loop"), &SchemeReplServer::server_loop);
}

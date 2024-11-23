#ifndef GODOT_S7_SCHEME_REPL_REQUEST_MEDIATOR_HPP
#define GODOT_S7_SCHEME_REPL_REQUEST_MEDIATOR_HPP

#include "request_compiler.hpp"
#include "connection.hpp"
#include <godot_cpp/classes/tcp_server.hpp>
#include <vector>

class ReplMediator {
public:
  ReplMediator(godot::Ref<godot::TCPServer> server) :
      server(server) {}

public:
  void mediate();

private:
  godot::Ref<godot::TCPServer> server;
  std::vector<ReplConnection> connections;
  ReplRequestCompiler request_compiler;
};
#endif

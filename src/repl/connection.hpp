#ifndef GODOT_S7_SCHEME_REPL_REQUEST_CONNECTION_HPP
#define GODOT_S7_SCHEME_REPL_REQUEST_CONNECTION_HPP

#include "request_compiler.hpp"
#include <godot_cpp/classes/stream_peer_tcp.hpp>
#include <godot_cpp/variant/string.hpp>

class ReplConnection {
public:
  ReplConnection(godot::Ref<godot::StreamPeerTCP> tcp_stream) :
      tcp_stream(tcp_stream) {}

public:
  void send_prompt();
  bool process_with(ReplRequestCompiler &compiler);
  void disconnect();

private:
  godot::String get_prompt();
  void send(const godot::CharString &s);
  void send(const char *p, size_t count);
  void send(char c);
  bool process_buffer_with(ReplRequestCompiler &compiler);

private:
  godot::Ref<godot::StreamPeerTCP> tcp_stream;
  std::string buffer;
};
#endif

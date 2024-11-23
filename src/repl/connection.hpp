#ifndef GODOT_S7_SCHEME_REPL_REQUEST_CONNECTION_HPP
#define GODOT_S7_SCHEME_REPL_REQUEST_CONNECTION_HPP

#include <godot_cpp/variant/string.hpp>
#include <godot_cpp/classes/stream_peer_tcp.hpp>
#include "request_compiler.hpp"

class ReplClient {
public:
  ReplClient(godot::Ref<godot::StreamPeerTCP> tcp_stream) :
      tcp_stream(tcp_stream) {}

public:
  void send_prompt();
  bool process(ReplRequestCompiler &compiler);
  void disconnect();

private:
  godot::String get_prompt();
  void send(const godot::CharString &s);
  bool process_buffer(ReplRequestCompiler &compiler);

private:
  godot::Ref<godot::StreamPeerTCP> tcp_stream;
  std::string buffer;
};
#endif

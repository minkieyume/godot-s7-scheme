#include "connection.hpp"
#include <godot_cpp/variant/utility_functions.hpp>

using namespace godot;
using gd = UtilityFunctions;

void ReplClient::disconnect() {
  tcp_stream->disconnect_from_host();
}

String ReplClient::get_prompt() {
  // auto owner = (target_scheme != nullptr ? (Node *)target_scheme : this)->get_owner();
  // auto path = owner != nullptr ? "" + owner->get_name() + "/" + get_name() : "" + get_name();
  //return "\ns7@(" + path + ")> ";
  return "\ns7@(:)> ";
}

void ReplClient::send_prompt() {
  send(get_prompt().utf8());
}

bool ReplClient::process(ReplRequestCompiler &compiler) {
  if (tcp_stream->get_status() != StreamPeerTCP::STATUS_CONNECTED) {
    return false;
  }

  auto available = tcp_stream->get_available_bytes();
  while (available > 0) {
    available--;
    auto ch = tcp_stream->get_8();

#if DEBUG_REPL_INTERACTIONS
    putchar(ch);
#endif

    if (ch == '\n' && available == 0) {
      if (!process_buffer(compiler)) {
        return false;
      }
    } else {
      buffer.push_back(ch);
    }
  }
  return tcp_stream->poll() == Error::OK;
}

bool ReplClient::process_buffer(ReplRequestCompiler &compiler) {
  if (buffer == ",q") {
    // disconnection from repl
    return false;
  }

  auto result = compiler.eval(buffer);
  buffer.clear();

  if (result.first) {
    gd::printerr(result.first);
    send(result.first);
    tcp_stream->put_8('\n');
  }
#if DEBUG_REPL_INTERACTIONS
  gd::print(result.second);
#endif
  send(result.second);
  tcp_stream->put_8('\n');

  send_prompt();
  return true;
}

void ReplClient::send(const CharString &s) {
  for (int i = 0; i < s.length(); ++i) {
    tcp_stream->put_8(s[i]);
  }
}

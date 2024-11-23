#include "connection.hpp"
#include <godot_cpp/variant/utility_functions.hpp>

using namespace godot;
using gd = UtilityFunctions;

void ReplConnection::disconnect() {
  tcp_stream->disconnect_from_host();
}

String ReplConnection::get_prompt() {
  // auto owner = (target_scheme != nullptr ? (Node *)target_scheme : this)->get_owner();
  // auto path = owner != nullptr ? "" + owner->get_name() + "/" + get_name() : "" + get_name();
  //return "\ns7@(" + path + ")> ";
  return "\ns7@(:)> ";
}

void ReplConnection::send(char c) {
  tcp_stream->put_8(c);
}

void ReplConnection::send(const char *p, size_t count) {
  for (int i = 0; i < count; ++i) {
    send(p[i]);
  }
}

void ReplConnection::send(const CharString &s) {
  send(s.get_data(), s.length());
}

void ReplConnection::send_prompt() {
  send(get_prompt().utf8());
}

bool ReplConnection::process_with(ReplRequestCompiler &compiler) {
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
      if (!process_buffer_with(compiler)) {
        return false;
      }
    } else {
      buffer.push_back(ch);
    }
  }
  return tcp_stream->poll() == Error::OK;
}

bool ReplConnection::process_buffer_with(ReplRequestCompiler &compiler) {
  if (buffer == ",q") {
    // disconnection from repl
    return false;
  }

  auto result = compiler.eval(buffer);
  buffer.clear();

  if (result.first) {
    gd::printerr(result.first);
    send(result.first);
    send('\n');
  }
#if DEBUG_REPL_INTERACTIONS
  gd::print(result.second);
#endif
  send(result.second);
  send('\n');

  send_prompt();
  return true;
}

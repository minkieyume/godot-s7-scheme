#include "connection.hpp"
#include "debug.hpp"

using namespace godot;

void ReplConnection::disconnect() {
  tcp_stream->disconnect_from_host();
}

String ReplConnection::get_prompt() {
  if (target_node) {
    return "\ns7@(" + target_node->node_name + ")> ";
  }
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

void ReplConnection::send(const String &s) {
  auto utf8 = s.utf8();
  send(utf8.get_data(), utf8.length());
}

void ReplConnection::send_prompt() {
  send(get_prompt());
}

void ReplConnection::on_eval_async_result(const String &s) {
  send(s);
  send_prompt();
}

ReplConnection::Status ReplConnection::process_with(Context &context) {
  if (tcp_stream->get_status() != StreamPeerTCP::STATUS_CONNECTED) {
    return ReplConnection::DISCONNECTED;
  }

  auto available = tcp_stream->get_available_bytes();
  auto originally_available = available;
  while (available > 0) {
    available--;
    auto ch = tcp_stream->get_8();

#if DEBUG_REPL_INTERACTIONS
    putchar(ch);
#endif

    if (ch == '\n' && available == 0) {
      if (!process_buffer_with(context)) {
        return ReplConnection::DISCONNECTED;
      }
      buffer.clear();
    } else {
      buffer.push_back(ch);
    }
  }
  if (tcp_stream->poll() != Error::OK) {
    return ReplConnection::DISCONNECTED;
  }
  return originally_available > 0
      ? ReplConnection::TRANSMITTING
      : ReplConnection::IDLE;
}

bool ReplConnection::process_buffer_with(Context &context) {
  if (buffer.size() == 2 && buffer.get_string_from_utf8() == ",q") {
    // disconnection from repl
    return false;
  }
  if (buffer.size() == 3 && buffer.get_string_from_utf8() == ",ls") {
    send_available_nodes();
    return true;
  }

  process_eval_request_with(context);
  return true;
}

void ReplConnection::send_available_nodes() {
  auto node_paths = node_registry->get_available_node_names();
  for (const auto &path : node_paths) {
    send(path);
    send('\n');
  }
  send_prompt();
}

void ReplConnection::send_output(const String &output) {
  if (output.is_empty())
    return;
  send(output);
  send('\n');
}

void ReplConnection::process_eval_request_with(Context &context) {
  auto [output, code] = context.compiler().compile_request(buffer);
  send_output(output);

  if (!code.is_empty()) {
    if (target_node) {
      if (context.eval_async(code, target_node->node_id)) {
        // TODO: enter waiting state
        return;
      } else {
        send("Target scheme node has been disconnected");
        target_node = std::nullopt;
      }
    } else {
      auto result = context.compiler().eval(code);
      DEBUG_REPL(result);
      send(result);
    }
  }

  send_prompt();
}

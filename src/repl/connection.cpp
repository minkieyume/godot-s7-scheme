#include "connection.hpp"
#include "debug.hpp"
#include <cstring>

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

template <typename B, size_t N>
bool buffer_contains(const char (&str)[N], B buffer) {
  constexpr size_t strlen = N - 1;
  return buffer.size() == strlen && std::strncmp((const char *)buffer.ptr(), str, strlen) == 0;
}

template <typename B, size_t N>
bool buffer_starts_with(const char (&str)[N], B buffer) {
  constexpr size_t strlen = N - 1;
  return buffer.size() >= strlen && std::strncmp((const char *)buffer.ptr(), str, strlen) == 0;
}

bool ReplConnection::process_buffer_with(Context &context) {
  if (buffer_contains(",q", buffer)) {
    // disconnection from repl
    return false;
  }
  if (buffer_contains(",ls", buffer)) {
    send_available_nodes();
    return true;
  }
  if (buffer_starts_with(",enter ", buffer)) {
    enter_selected_node(buffer.get_string_from_utf8().substr(7));
    return true;
  }

  process_eval_request_with(context);
  return true;
}

void ReplConnection::send_available_nodes() {
  auto sent = 0;
  for (const auto &node_name : node_registry->get_available_node_names()) {
    if (sent++ > 0) {
      send('\n');
    }
    send(node_name);
  }
  send_prompt();
}

void ReplConnection::enter_selected_node(const String &node_name) {
  auto found = node_registry->find_node_by_name(node_name);
  if (found) {
    target_node = found;
    send("Connected to `" + node_name + "`.");
  } else {
    send("Node `" + node_name + "` not found.");
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

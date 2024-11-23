#include "mediator.hpp"
#include <godot_cpp/variant/utility_functions.hpp>

using gd = godot::UtilityFunctions;

void ReplMediator::mediate() {
  if (server->is_connection_available()) {
    // TODO: client starts with most recent Scheme node
    auto connection = ReplConnection(server->take_connection());
    gd::print("Scheme repl client connected.");
    connection.send_prompt();
    connections.emplace_back(std::move(connection));
  }

  for (auto connection = connections.begin(); connection != connections.end();) {
    if (!connection->process_with(request_compiler)) {
      connection = connections.erase(connection);
      gd::print("Scheme repl client disconnected.");
    } else {
      connection++;
    }
  }
}

#include "mediator.hpp"
#include <godot_cpp/variant/utility_functions.hpp>

using gd = godot::UtilityFunctions;

bool ReplMediator::mediate() {
  int interactions = 0;
  if (server->is_connection_available()) {
    // TODO: client starts with most recent Scheme node
    auto connection = ReplConnection(server->take_connection());
    gd::print("Scheme repl client connected.");
    connection.send_prompt();
    connections.emplace_back(std::move(connection));
    interactions++;
  }

  for (auto connection = connections.begin(); connection != connections.end();) {
    auto status = connection->process_with(request_compiler);
    if (status == ReplConnection::DISCONNECTED) {
      connection = connections.erase(connection);
      gd::print("Scheme repl client disconnected.");
    } else {
      connection++;
      interactions += (status != ReplConnection::IDLE) ? 1 : 0;
    }
  }
  return interactions > 0;
}

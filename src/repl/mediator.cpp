#include "mediator.hpp"
#include <godot_cpp/variant/utility_functions.hpp>

using gd = godot::UtilityFunctions;

void ReplMediator::mediate() {
  if (server->is_connection_available()) {
    // TODO: client starts with most recent Scheme node
    auto client = ReplClient(server->take_connection());
    gd::print("Scheme repl client connected.");
    client.send_prompt();
    clients.emplace_back(std::move(client));
  }

  for (auto client = clients.begin(); client != clients.end();) {
    if (!client->process(request_compiler)) {
      client = clients.erase(client);
      gd::print("Scheme repl client disconnected.");
    } else {
      client++;
    }
  }
}

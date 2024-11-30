#include "node_registry.hpp"
#include "debug.hpp"
#include <godot_cpp/variant/utility_functions.hpp>

using namespace godot;

std::optional<ReplNodeRegistry::NodeRecord> ReplNodeRegistry::get_most_recent() {
  if (nodes.empty()) {
    return std::nullopt;
  }
  return nodes.back();
}

std::vector<String> ReplNodeRegistry::get_available_node_names() {
  auto result = std::vector<String>();
  for (const auto &node : nodes) {
    result.push_back(node.node_name);
  }
  return result;
}

std::optional<ReplNodeRegistry::NodeRecord> ReplNodeRegistry::find_node_by_name(const String& node_name) {
  for (const auto &node : nodes) {
    if (node.node_name == node_name) {
      return node;
    }
  }
  return std::nullopt;
}

void ReplNodeRegistry::register_node(uint64_t node_id, String &&node_name) {
  DEBUG_REPL("Scheme node ", node_name, " is available for repl interaction.");
  nodes.emplace_back(NodeRecord{ node_id, std::move(node_name) });
}

void ReplNodeRegistry::unregister_node(uint64_t node_id) {
  for (auto it = nodes.cbegin(); it != nodes.cend(); ++it) {
    if (it->node_id == node_id) {
      DEBUG_REPL("Scheme node ", it->node_name, " is no longer available for repl interaction.");
      nodes.erase(it);
      break;
    }
  }
}

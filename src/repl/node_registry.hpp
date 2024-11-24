#ifndef GODOT_S7_SCHEME_REPL_NODE_REGISTRY_HPP
#define GODOT_S7_SCHEME_REPL_NODE_REGISTRY_HPP

#include <godot_cpp/variant/string.hpp>
#include <vector>

class ReplNodeRegistry {
public:
  std::vector<godot::String> get_available_node_names();
  void register_node(uint64_t node_id, godot::String node_name);
  void unregister_node(uint64_t node_id);

private:
  struct NodeRecord {
    uint64_t node_id;
    godot::String node_name;
  };

  std::vector<NodeRecord> nodes;
};
#endif

#include "scheme_repl_server.hpp"
#include "repl/s7_scheme_repl_string.hpp"
#include <godot_cpp/classes/engine.hpp>
#include <godot_cpp/classes/os.hpp>
#include <godot_cpp/classes/stream_peer_tcp.hpp>
#include <godot_cpp/variant/utility_functions.hpp>
#include <string>
#include <vector>

#define DEBUG_REPL_INTERACTIONS 0

using namespace godot;
using gd = godot::UtilityFunctions;

typedef std::basic_string<char> str;

typedef std::pair<const char *, const char *> error_output_and_response;

template <typename T>
std::pair<const char *, T> eval_with_error_output(s7_scheme *sc, std::function<T(s7_scheme *)> f) {
  auto error_port = s7_open_output_string(sc);
  auto previous_error_port = s7_set_current_error_port(sc, error_port);
  auto res = f(sc);
  s7_set_current_error_port(sc, previous_error_port);
  auto error_output = s7_get_output_string(sc, error_port);
  return std::make_pair(error_output, res);
}

inline const char *non_empty_nor_null(const char *s) {
  return s != nullptr && *s != 0 ? s : nullptr;
}

class ReplRequestCompiler {
public:
  ReplRequestCompiler();
  ~ReplRequestCompiler();

public:
  error_output_and_response eval(const str &request);

private:
  s7_protected_ptr compile_geiser_request;
  s7 scheme;
};

ReplRequestCompiler::ReplRequestCompiler() {
  compile_geiser_request = scheme.make_symbol("compile-geiser-request");
  auto result = eval_with_error_output<const char *>(scheme.get(), [](auto sc) {
#if DEBUG_REPL_INTERACTIONS
  std::cout << s7_scheme_repl_string << std::endl;
#endif
    return s7_object_to_c_string(sc, s7_load_c_string(sc, s7_scheme_repl_string, strlen(s7_scheme_repl_string)));
  });
  auto error_output = non_empty_nor_null(result.first);
  if (error_output) {
    gd::printerr(error_output);
  }
}

ReplRequestCompiler::~ReplRequestCompiler() {
  compile_geiser_request = nullptr;
}

error_output_and_response ReplRequestCompiler::eval(const str &request) {
  auto compile_geiser_request = scheme.value_of(this->compile_geiser_request);
  if (!s7_is_procedure(compile_geiser_request)) {
    return std::make_pair(
        "repl script is missing a compile-geiser-request function!",
        "'done");
  }

  auto sc = scheme.get();
  auto compile_result =
      eval_with_error_output<s7_pointer>(sc,
          [compile_geiser_request, &request](auto sc) {
            auto args = s7_cons(sc,
                s7_make_string_wrapper_with_length(
                    sc,
                    request.c_str(),
                    static_cast<s7_int>(request.length())),
                s7_nil(sc));
            return s7_call_with_location(
                sc,
                compile_geiser_request,
                args,
                __func__,
                __FILE__,
                __LINE__);
          });

  auto compile_error_output = compile_result.first;
  auto compiled_request = compile_result.second;
  if (!s7_is_string(compiled_request)) {
    return std::make_pair(
        non_empty_nor_null(compile_error_output),
        s7_object_to_c_string(sc, compiled_request));
  }

  auto code_string = s7_string(compiled_request);

#if DEBUG_REPL_INTERACTIONS
  std::cout << "```" << std::endl
            << code_string << std::endl
            << "```" << std::endl;
#endif

  auto eval_sc = /*target_scheme != nullptr ? target_scheme->get_s7().get() :*/ sc;
  auto res = eval_with_error_output<const char *>(eval_sc,
      [code_string](auto sc) {
        auto res = s7_eval_c_string(sc, code_string);
        return s7_is_string(res) ? s7_string(res) : s7_object_to_c_string(sc, res);
      });
  return std::make_pair(non_empty_nor_null(res.first), res.second);
}

class ReplClient {
public:
  ReplClient(Ref<StreamPeerTCP> tcp_stream) :
      tcp_stream(tcp_stream) {}

public:
  void send(const CharString &s);
  String get_prompt();
  bool process(ReplRequestCompiler &compiler);
  void disconnect();

private:
  Ref<StreamPeerTCP> tcp_stream;
  str buffer;
};

void ReplClient::disconnect() {
  tcp_stream->disconnect_from_host();
}

String ReplClient::get_prompt() {
  // auto owner = (target_scheme != nullptr ? (Node *)target_scheme : this)->get_owner();
  // auto path = owner != nullptr ? "" + owner->get_name() + "/" + get_name() : "" + get_name();
  //return "\ns7@(" + path + ")> ";
  return "\ns7@(:)> ";
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
      auto prompt = get_prompt().utf8();
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
      gd::print(result.second);
      send(result.second);
      tcp_stream->put_8('\n');

      send(prompt);
    } else {
      buffer.push_back(ch);
    }
  }
  return tcp_stream->poll() == Error::OK;
}

void ReplClient::send(const CharString &s) {
  for (int i = 0; i < s.length(); ++i) {
    tcp_stream->put_8(s[i]);
  }
}

class ReplController {
public:
  ReplController(Ref<TCPServer> server) :
      server(server) {}

public:
  void process();

private:
  Ref<TCPServer> server;
  ReplRequestCompiler request_compiler;
  std::vector<ReplClient> clients;
};

void ReplController::process() {
  if (server->is_connection_available()) {
    // TODO: client starts with most recent Scheme node
    auto client = ReplClient(server->take_connection());
    gd::print("Scheme repl client connected.");
    client.send(client.get_prompt().utf8());
    clients.emplace_back(std::move(client));
  }

  if (clients.empty()) {
    return;
  }

  auto client_index = 0;
  auto disconnected = std::vector<int>();
  std::for_each(
      clients.begin(),
      clients.end(),
      [&](auto &client) {
        if (!client.process(request_compiler)) {
          disconnected.push_back(client_index);
        }
        client_index++;
      });
  std::for_each(
      disconnected.rbegin(),
      disconnected.rend(),
      [&](auto i) {
        auto c = clients.begin() + i;
        c->disconnect();
        clients.erase(c);
        gd::print("Scheme repl client disconnected.");
      });
}

void SchemeReplServer::server_loop() {
  // TODO: only start server when --s7-tcp-port=<number> and/or --s7-tcp-address=<address> are present in OS::get_cmdline_args()

  String tcp_bind_address = "127.0.0.1";
  int tcp_port = 0;

  Ref<TCPServer> tcp_server;
  tcp_server.instantiate();

  auto error = tcp_server->listen(tcp_port, tcp_bind_address);
  ERR_FAIL_COND_MSG(
      error != OK,
      ("Failed to start scheme repl server: " + gd::error_string(error)));

  gd::print("Scheme repl server listening on local port ", tcp_server->get_local_port());

  auto controller = ReplController(tcp_server);
  while (!exit_thread) {
    controller.process();
    OS::get_singleton()->delay_msec(250);
  }
  tcp_server->stop();
}

Error SchemeReplServer::init() {
  ERR_FAIL_COND_V_MSG(thread.is_valid(), ERR_BUG, "Scheme repl server can only be started once!");
  exit_thread = false;
  thread.instantiate();
  return thread->start(Callable::create(this, "server_loop"), Thread::PRIORITY_LOW);
}

void SchemeReplServer::finish() {
  if (thread.is_null()) {
    return;
  }

  exit_thread = true;
  thread->wait_to_finish();

  thread.unref();
}

SchemeReplServer *SchemeReplServer::singleton = NULL;

SchemeReplServer *SchemeReplServer::get_singleton() {
  return singleton;
}

SchemeReplServer::SchemeReplServer() {
  singleton = this;
}

void SchemeReplServer::_bind_methods() {
  ClassDB::bind_method(D_METHOD("server_loop"), &SchemeReplServer::server_loop);
}

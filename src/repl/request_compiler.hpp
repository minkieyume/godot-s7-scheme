#ifndef GODOT_S7_SCHEME_REPL_REQUEST_COMPILER_HPP
#define GODOT_S7_SCHEME_REPL_REQUEST_COMPILER_HPP

#include "../s7.hpp"
#include <string>

#define DEBUG_REPL_INTERACTIONS 0

typedef std::pair<const char *, const char *> error_output_and_response;

class ReplRequestCompiler {
public:
  ReplRequestCompiler();
  ~ReplRequestCompiler();

public:
  error_output_and_response eval(const std::string &request);

private:
  s7_protected_ptr compile_geiser_request;
  s7 scheme;
};
#endif

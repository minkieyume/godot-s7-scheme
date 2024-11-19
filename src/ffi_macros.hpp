#ifndef FFI_MACROS_H
#define FFI_MACROS_H

#define BEGIN_ARGS() s7_pointer _arg; int _arg_i = 0;
#define EXPECT_ARG(named, value, check, desc) {_arg = s7_car(args); if (!check(_arg)) return s7_wrong_type_arg_error(sc, __func__, _arg_i, _arg, desc); args = s7_cdr(args); _arg_i++;}; const auto named = value
#define EXPECT_INT_ARG(n) EXPECT_ARG(n, s7_integer(_arg), s7_is_integer, "integer")
#define EXPECT_UINT32_ARG(n) EXPECT_ARG(n, s7_uint32(_arg), s7_is_integer, "uint32")
#define EXPECT_REAL_ARG(n) EXPECT_ARG(n, s7_real(_arg), s7_is_real, "real")
#define EXPECT_STRING_ARG(n) EXPECT_ARG(n, _arg, s7_is_string, "string")
#define EXPECT_SYMBOL_ARG(n) EXPECT_ARG(n, _arg, s7_is_symbol, "symbol")

#endif //FFI_MACROS_H

open Base
open Bril_instruction
open Bril_type

type bril_arg = { name : string; typ : bril_type [@key "type"] }
[@@deriving yojson, compare, hash, sexp]

val bril_arg_to_string : bril_arg -> string

type bril_function = {
  name : string;
  args : bril_arg list option; [@default None]
  typ : bril_type option; [@key "type"] [@default None]
  instrs : Bril_instruction.bril_instruction list;
}
[@@deriving yojson, compare, hash, sexp]

val bril_function_of_yojson : Yojson.Safe.t -> (bril_function, string) Result.t
val bril_function_to_string : bril_function -> string

type bril_ir_function = {
  name : string;
  args : bril_arg list option; [@default None]
  typ : bril_type option; [@key "type"] [@default None]
  instrs : Instruction.t list;
}
[@@deriving compare, hash, sexp]

val bril_ir_function_to_string : bril_ir_function -> string

open Base

type bril_program = { functions : Bril_function.bril_function list }
[@@deriving yojson]

val bril_program_to_string : bril_program -> string
val parsed_bril_json : string -> bril_program
val bril_program_of_yojson : Yojson.Safe.t -> (bril_program, string) Result.t

type bril_ir_program = { functions : Bril_function.bril_ir_function list }

val from_tokens : bril_program -> bril_ir_program
val bril_ir_program_to_string : bril_ir_program -> string

type bril_type = Yojson.Safe.t [@@deriving yojson]
type bril_value = Yojson.Safe.t [@@deriving yojson]

type bril_arg = {
  name: string;
  typ: bril_type; [@key "type"]
} [@@deriving yojson]

type bril_instruction = {
  op: string option; [@default None]
  label: string option; [@default None]
  dest: string option; [@default None]
  typ: bril_type option; [@key "type"] [@default None]
  args: string list option; [@default None]
  funcs: string list option; [@default None]
  labels: string list option; [@default None]
  value: bril_value option; [@default None]
} [@@deriving yojson]

type bril_function = {
  name: string;
  args: bril_arg list option; [@default None]
  typ: bril_type option; [@key "type"] [@default None]
  instrs: bril_instruction list
} [@@deriving yojson]

type bril_program = {
  functions: bril_function list
} [@@deriving yojson]

val parsed_bril_json : string -> bril_program
val main : unit -> unit

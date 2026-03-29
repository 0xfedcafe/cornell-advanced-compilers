open Base

type bril_label = { label : string } [@@deriving yojson]
type bril_immediate = BrilBool of bool | BrilInt of int

val bril_immediate_of_yojson :
  Yojson.Safe.t -> (bril_immediate, string) Result.t

val bril_immediate_to_yojson : bril_immediate -> Yojson.Safe.t

type bril_type =
  | BrilType of string
  | BrilStructType of (string * bril_type) list

val bril_type_of_yojson : Yojson.Safe.t -> (bril_type, string) Result.t
val bril_type_to_yojson : bril_type -> Yojson.Safe.t

type bril_const_instruction = {
  op : string;
  dest : string;
  typ : bril_type; [@key "type"]
  value : bril_immediate;
}
[@@deriving yojson]

type bril_value_instruction = {
  op : string;
  dest : string;
  typ : bril_type; [@key "type"]
  args : string list option; [@default None]
  funcs : string list option; [@default None]
  labels : string list option; [@default None]
}
[@@deriving yojson]

type bril_effect_instruction = {
  op : string;
  args : string list option; [@default None]
  funcs : string list option; [@default None]
  labels : string list option; [@default None]
}
[@@deriving yojson]

type bril_instruction =
  | BrilLabel of bril_label
  | BrilConstInstruction of bril_const_instruction
  | BrilValueInstruction of bril_value_instruction
  | BrilEffectInstruction of bril_effect_instruction

val bril_instruction_of_yojson :
  Yojson.Safe.t -> (bril_instruction, string) Result.t

val bril_instruction_to_yojson : bril_instruction -> Yojson.Safe.t

type bril_arg = { name : string; typ : bril_type [@key "type"] }
[@@deriving yojson]

type bril_function = {
  name : string;
  args : bril_arg list option; [@default None]
  typ : bril_type option; [@key "type"] [@default None]
  instrs : bril_instruction list;
}
[@@deriving yojson]

type bril_program = { functions : bril_function list } [@@deriving yojson]

type basic_block = {
  label : string option;
  instructions : bril_instruction list;
}

val parsed_bril_json : string -> bril_program
val gather_basic_blocks : bril_program -> basic_block list
val build_cfg : bril_program -> (basic_block, basic_block list) Hashtbl.t

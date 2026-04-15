open Base
open Bril_label
open Bril_immediate

type bril_const_instruction = {
  op : string;
  dest : string;
  typ : Bril_type.bril_type; [@key "type"]
  value : Bril_immediate.bril_immediate;
}
[@@deriving yojson, compare, hash, sexp]

val bril_const_instruction_of_yojson :
  Yojson.Safe.t -> (bril_const_instruction, string) Result.t

type bril_value_instruction = {
  op : string;
  dest : string;
  typ : Bril_type.bril_type; [@key "type"]
  args : string list option; [@default None]
  funcs : string list option; [@default None]
  labels : string list option; [@default None]
}
[@@deriving yojson, compare, hash, sexp]

val bril_value_instruction_of_yojson :
  Yojson.Safe.t -> (bril_value_instruction, string) Result.t

type bril_effect_instruction = {
  op : string;
  args : string list option; [@default None]
  funcs : string list option; [@default None]
  labels : string list option; [@default None]
}
[@@deriving yojson, compare, hash, sexp]

val bril_effect_instruction_of_yojson :
  Yojson.Safe.t -> (bril_effect_instruction, string) Result.t

type bril_instruction =
  | BrilLabel of Bril_label.bril_label
  | BrilConstInstruction of bril_const_instruction
  | BrilValueInstruction of bril_value_instruction
  | BrilEffectInstruction of bril_effect_instruction
[@@deriving compare, hash, sexp]

val bril_instruction_of_yojson :
  Yojson.Safe.t -> (bril_instruction, string) Result.t

val bril_instruction_to_yojson : bril_instruction -> Yojson.Safe.t
val bril_instruction_to_string : bril_instruction -> string

type bin_op = { dst : string; src1 : string; src2 : string }
[@@deriving compare, hash, sexp]

type un_op = { dst : string; src1 : string } [@@deriving compare, hash, sexp]

type const_op = { dst : string; value : bril_immediate }
[@@deriving compare, hash, sexp]

type bril_arithm_instr =
  | Add of bin_op
  | Sub of bin_op
  | Mul of bin_op
  | Div of bin_op
[@@deriving compare, hash, sexp]

val bril_arithm_of_value_instr :
  bril_value_instruction -> bril_arithm_instr option

type bril_comp_instr =
  | Eq of bin_op
  | Lt of bin_op
  | Gt of bin_op
  | Le of bin_op
  | Ge of bin_op
[@@deriving compare, hash, sexp]

val bril_comp_of_value_instr : bril_value_instruction -> bril_comp_instr option

type bril_logic_instr = Not of un_op | And of bin_op | Or of bin_op
[@@deriving compare, hash, sexp]

val bril_logic_of_value_instr :
  bril_value_instruction -> bril_logic_instr option

type bril_const_instr = Const of const_op [@@deriving compare, hash, sexp]

val bril_const_of_const_instr :
  bril_const_instruction -> bril_const_instr option

type bril_control_instr =
  | Jump of Bril_label.bril_label
  | Branch of {
      cond : string;
      iftrue : Bril_label.bril_label;
      iffalse : Bril_label.bril_label;
    }
  | Call of { name : string; arg : string list option; dst : string option }
  | Return of Bril_immediate.bril_immediate option

val bril_control_of_effect_instr :
  bril_effect_instruction -> bril_control_instr option

val bril_call_of_value_instr :
  bril_value_instruction -> bril_control_instr option

type bril_misc_instr =
  | Identity of { dst : string; src : string }
  | Print of string list
  | Nop
[@@deriving compare, hash, sexp]

val bril_misc_of_effect_instr :
  bril_effect_instruction -> bril_misc_instr option

val bril_misc_of_value_instr : bril_value_instruction -> bril_misc_instr option

type bril_ir_instruction =
  | Arithm of bril_arithm_instr
  | Comp of bril_comp_instr
  | Logic of bril_logic_instr
  | Const of bril_const_instr
  | Control of bril_control_instr
  | Label of bril_label
  | Misc of bril_misc_instr
[@@deriving compare, hash, sexp]

val bril_ir_instruction_to_string : bril_ir_instruction -> string

val bril_ir_instruction_from_instruction :
  bril_instruction -> bril_ir_instruction

val get_dest : bril_ir_instruction -> string option
val replace_dst : bril_ir_instruction -> string -> bril_ir_instruction

val replace_args :
  bril_ir_instruction -> (string -> string) -> bril_ir_instruction

val get_args : bril_ir_instruction -> string list
val string_of_op : bril_ir_instruction -> string

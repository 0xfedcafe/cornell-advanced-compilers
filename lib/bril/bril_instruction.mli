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

type bin_op = {
  dst : string;
  typ : Bril_type.bril_type;
  src1 : string;
  src2 : string;
}
[@@deriving compare, hash, sexp]

type un_op = {
  dst : string;
  typ : Bril_type.bril_type option;
  src1 : string;
}
[@@deriving compare, hash, sexp]

type const_op = {
  dst : string;
  typ : Bril_type.bril_type;
  value : bril_immediate;
}
[@@deriving compare, hash, sexp]

module Op : sig
  type binary = Add | Sub | Mul | Div | Eq | Lt | Gt | Le | Ge | And | Or
  [@@deriving compare, hash, sexp, enumerate]

  type unary = Not | Id [@@deriving compare, hash, sexp, enumerate]

  val binary_name : binary -> string
  val unary_name : unary -> string
  val binary_of_name : string -> binary option
  val unary_of_name : string -> unary option
  val binary_result_type : binary -> Bril_type.bril_type
  val unary_result_type : unary -> Bril_type.bril_type option
  val is_commutative : binary -> bool
end

val binary_of_value_instr :
  bril_value_instruction -> (Op.binary * bin_op) option

val unary_of_value_instr : bril_value_instruction -> (Op.unary * un_op) option
val unary_of_effect_instr : bril_effect_instruction -> (Op.unary * un_op) option
val const_of_const_instr : bril_const_instruction -> const_op option

type bril_control_instr =
  | Jump of Bril_label.bril_label
  | Branch of {
      cond : string;
      iftrue : Bril_label.bril_label;
      iffalse : Bril_label.bril_label;
    }
  | Call of {
      name : string;
      arg : string list option;
      dst : string option;
      typ : Bril_type.bril_type option;
    }
  | Return of Bril_immediate.bril_immediate option

val bril_control_of_effect_instr :
  bril_effect_instruction -> bril_control_instr option

val bril_call_of_value_instr :
  bril_value_instruction -> bril_control_instr option

type bril_misc_instr = Print of string list | Nop
[@@deriving compare, hash, sexp]

val bril_misc_of_effect_instr :
  bril_effect_instruction -> bril_misc_instr option

val bril_misc_of_value_instr : bril_value_instruction -> bril_misc_instr option

type bril_ssa_instr =
  | Get of { dst : string; typ : Bril_type.bril_type }
  | Set of { dst : string; src : string }
  | Undef of { dst : string; typ : Bril_type.bril_type }
[@@deriving compare, hash, sexp]

module Instruction : sig
  type t =
    | Binary of Op.binary * bin_op
    | Unary of Op.unary * un_op
    | Const of const_op
    | Control of bril_control_instr
    | Label of bril_label
    | Misc of bril_misc_instr
    | SSA of bril_ssa_instr
  [@@deriving compare, hash, sexp]

  include Base.Comparator.S with type t := t

  val to_string : t -> string
  val from_instruction : bril_instruction -> t
  val get_dest : t -> string option
  val result_type : t -> Bril_type.bril_type option
  val replace_dst : t -> string -> t
  val replace_args : t -> (string -> string) -> t
  val get_args : t -> string list
  val string_of_op : t -> string
end

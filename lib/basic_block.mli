open Base
open Bril

type id = int [@@deriving compare, equal, hash, sexp]

module Id : sig
  type t = id [@@deriving compare, equal, hash, sexp]

  include Base.Comparator.S with type t := t
  include Hashtbl.Key.S with type t := t
end

type t = { id : id; label : string option; instructions : Instruction.t list }
[@@deriving compare, hash, sexp]

val bbs_in_function : bril_ir_function -> t list
val gather_basic_blocks : bril_ir_program -> t list
val is_generated_label : func_name:string -> string -> bool
val instrs_of_blocks : func_name:string -> t list -> Instruction.t list

type basic_block = t [@@deriving compare, hash, sexp]

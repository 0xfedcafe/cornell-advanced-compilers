open Base
open Bril

type basic_block = {
  label : string option;
  instructions : bril_instruction list;
}
[@@deriving compare, hash, sexp]

module Basic_block : sig
  type t = basic_block [@@deriving compare, hash, sexp]
  type id = private int [@@deriving compare, equal, hash]

  val bbs_in_function : bril_function -> basic_block list
  val gather_basic_blocks : bril_program -> basic_block list
end

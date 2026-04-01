open Base
open Bril

type id = int [@@deriving compare, equal, hash, sexp]

module Id : sig
  type t = id [@@deriving compare, equal, hash, sexp]
  include Hashtbl.Key.S with type t := t
end

type t = {
  id : id;
  label : string option;
  instructions : bril_instruction list;
}
[@@deriving compare, hash, sexp]

val bbs_in_function : bril_function -> t list
val gather_basic_blocks : bril_program -> t list

type basic_block = t [@@deriving compare, hash, sexp]

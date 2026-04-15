open Base

type bril_immediate = BrilBool of bool | BrilInt of int
[@@deriving compare, hash, sexp]

val bril_immediate_of_yojson :
  Yojson.Safe.t -> (bril_immediate, string) Result.t

val bril_immediate_to_yojson : bril_immediate -> Yojson.Safe.t
val bril_immediate_to_string : bril_immediate -> string

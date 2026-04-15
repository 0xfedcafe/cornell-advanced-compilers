open Base

type bril_type =
  | BrilType of string
  | BrilStructType of (string * bril_type) list
[@@deriving compare, hash, sexp]

val bril_type_of_yojson : Yojson.Safe.t -> (bril_type, string) Result.t
val bril_type_to_yojson : bril_type -> Yojson.Safe.t
val bril_type_to_string : bril_type -> string

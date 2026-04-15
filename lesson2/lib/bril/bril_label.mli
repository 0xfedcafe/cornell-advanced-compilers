open Base

type bril_label = { label : string } [@@deriving yojson, compare, hash, sexp]

val bril_label_to_string : bril_label -> string

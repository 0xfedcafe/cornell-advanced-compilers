open Base

type bril_label = { label : string } [@@deriving yojson, compare, hash, sexp]

let bril_label_to_string = function { label } -> "." ^ label ^ ":"

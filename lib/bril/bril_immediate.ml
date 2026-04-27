open Base

type bril_immediate = BrilBool of bool | BrilInt of int
[@@deriving compare, hash, sexp]

let bril_immediate_of_yojson = function
  | `Bool b -> Ok (BrilBool b)
  | `Int i -> Ok (BrilInt i)
  | _ -> Error "bril_immediate"

let bril_immediate_to_yojson = function
  | BrilBool b -> `Bool b
  | BrilInt i -> `Int i

let bril_immediate_to_string = function
  | BrilBool b -> Bool.to_string b
  | BrilInt i -> Int.to_string i

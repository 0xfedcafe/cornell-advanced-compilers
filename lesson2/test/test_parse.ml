open Yojson.Safe

type bril_immediate = BrilBool of bool | BrilInt of int [@@deriving yojson]
let json = `String "int"
let () = print_endline "ok"

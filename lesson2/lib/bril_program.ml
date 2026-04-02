open Base
open Bril_function

type bril_program = { functions : Bril_function.bril_function list }
[@@deriving yojson]

let bril_program_to_string { functions } =
  String.concat ~sep:"\n\n"
    (List.map ~f:Bril_function.bril_function_to_string functions)

let parsed_bril_json bril_string =
  let bril_json = Yojson.Safe.from_string bril_string in
  let bril_program = bril_program_of_yojson bril_json in
  match bril_program with
  | Ok program -> program
  | Error err -> failwith ("Error parsing Bril JSON: " ^ err)

type bril_ir_program = { functions : Bril_function.bril_ir_function list }

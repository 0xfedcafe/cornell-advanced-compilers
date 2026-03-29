open Base
open Stdio

(* Bril Type Definitions *)

(* Bril Type can be a string "int" or a JSON object {"ptr": <Type>} *)
type bril_type = Yojson.Safe.t [@@deriving yojson]

(* Bril Value can be an int or a bool *)
type bril_value = Yojson.Safe.t [@@deriving yojson]

type bril_arg = {
  name: string;
  typ: bril_type; [@key "type"]
} [@@deriving yojson]

(* Bril Instruction can be a Label OR an Instruction *)
type bril_instruction = {
  op: string option; [@default None]
  label: string option; [@default None]
  dest: string option; [@default None]
  typ: bril_type option; [@key "type"] [@default None]
  args: string list option; [@default None]
  funcs: string list option; [@default None]
  labels: string list option; [@default None]
  value: bril_value option; [@default None]
} [@@deriving yojson]

type bril_function = {
  name: string;
  args: bril_arg list option; [@default None]
  typ: bril_type option; [@key "type"] [@default None]
  instrs: bril_instruction list
} [@@deriving yojson]

type bril_program = {
  functions: bril_function list
} [@@deriving yojson]


let parsed_bril_json bril_string =
  let bril_json = Yojson.Safe.from_string bril_string in
  let bril_program = bril_program_of_yojson bril_json in
  match bril_program with
  | Ok program -> program
  | Error err -> failwith ("Error parsing Bril JSON: " ^ err)

let bbs_in_function func =
  let rec build_bbs instrs current_bb bbs =
    match instrs with
    | x::xs ->

let gather_basic_blocks program =
  ()



let main () =
  let bril_string = In_channel.read_all "input.bril" in
  let bril_program = parsed_bril_json bril_string in
  printf "Parsed Bril Program: %s\n" (Yojson.Safe.pretty_to_string (bril_program_to_yojson bril_program))

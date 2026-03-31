open Base
open Yojson.Safe

type bril_label = { label : string } [@@deriving yojson]

type bril_immediate = BrilBool of bool | BrilInt of int

let bril_immediate_of_yojson = function
  | `Bool b -> Ok (BrilBool b)
  | `Int i -> Ok (BrilInt i)
  | _ -> Error "bril_immediate"

let bril_immediate_to_yojson = function
  | BrilBool b -> `Bool b
  | BrilInt i -> `Int i

type bril_type =
  | BrilType of string
  | BrilStructType of (string * bril_type) list

let rec bril_type_of_yojson = function
  | `String s -> Ok (BrilType s)
  | `Assoc fields ->
      let rec parse_fields acc = function
        | [] -> Ok (BrilStructType (List.rev acc))
        | (k, v) :: rest ->
            (match bril_type_of_yojson v with
            | Ok t -> parse_fields ((k, t) :: acc) rest
            | Error e -> Error e)
      in
      parse_fields [] fields
  | _ -> Error "bril_type"

let rec bril_type_to_yojson = function
  | BrilType s -> `String s
  | BrilStructType fields ->
      `Assoc (List.map ~f:(fun (k, t) -> (k, bril_type_to_yojson t)) fields)

type bril_const_instruction = {
  dest : string;
  typ : bril_type; [@key "type"]
  value : bril_immediate;
}
[@@deriving yojson]

type bril_value_instruction = {
  op : string;
  dest : string;
  typ : bril_type; [@key "type"]
  args : string list option; [@default None]
  funcs : string list option; [@default None]
  labels : string list option; [@default None]
}
[@@deriving yojson]

type bril_effect_instruction = {
  op : string;
  args : string list option; [@default None]
  funcs : string list option; [@default None]
  labels : string list option; [@default None]
}
[@@deriving yojson]

type bril_instruction =
  | BrilLabel of bril_label
  | BrilConstInstruction of bril_const_instruction
  | BrilValueInstruction of bril_value_instruction
  | BrilEffectInstruction of bril_effect_instruction

let bril_instruction_of_yojson json =
  match json with
  | `Assoc fields ->
      if List.exists ~f:(fun (k, _) -> String.equal k "label") fields then
        match bril_label_of_yojson json with
        | Ok l -> Ok (BrilLabel l)
        | Error e -> Error e
      else if List.exists ~f:(fun (k, _) -> String.equal k "op") fields then
        let op = List.Assoc.find_exn fields ~equal:String.equal "op" in
        match op with
        | `String "const" ->
            (match bril_const_instruction_of_yojson json with
            | Ok c -> Ok (BrilConstInstruction c)
            | Error e -> Error e)
        | `String _ ->
            if List.exists ~f:(fun (k, _) -> String.equal k "dest") fields then
              match bril_value_instruction_of_yojson json with
              | Ok v -> Ok (BrilValueInstruction v)
              | Error e -> Error e
            else
              match bril_effect_instruction_of_yojson json with
              | Ok e -> Ok (BrilEffectInstruction e)
              | Error e -> Error e
        | _ -> Error "op must be a string"
      else Error "Instruction must have 'label' or 'op'"
  | _ -> Error "Instruction must be an object"

let bril_instruction_to_yojson = function
  | BrilLabel l -> bril_label_to_yojson l
  | BrilConstInstruction c -> bril_const_instruction_to_yojson c
  | BrilValueInstruction v -> bril_value_instruction_to_yojson v
  | BrilEffectInstruction e -> bril_effect_instruction_to_yojson e

type bril_arg = { name : string; typ : bril_type [@key "type"] }
[@@deriving yojson]

type bril_function = {
  name : string;
  args : bril_arg list option; [@default None]
  typ : bril_type option; [@key "type"] [@default None]
  instrs : bril_instruction list;
}
[@@deriving yojson]

type bril_program = { functions : bril_function list } [@@deriving yojson]

let () =
  let json_str = {|
  {
  "functions": [
    {
      "instrs": [
        {
          "dest": "v0",
          "op": "const",
          "type": "int",
          "value": 1
        }
      ],
      "name": "main"
    }
  ]
}
|} in
  let json = Yojson.Safe.from_string json_str in
  match bril_program_of_yojson json with
  | Ok _ -> Stdio.print_endline "Success"
  | Error e -> Stdio.print_endline ("Error: " ^ e)

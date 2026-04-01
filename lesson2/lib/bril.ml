open Base

type bril_label = { label : string } [@@deriving yojson, compare, hash, sexp]

let bril_label_to_string = function { label } -> "." ^ label ^ ":"

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

type bril_type =
  | BrilType of string
  | BrilStructType of (string * bril_type) list
[@@deriving compare, hash, sexp]

let rec bril_type_of_yojson = function
  | `String s -> Ok (BrilType s)
  | `Assoc fields ->
      let rec parse_fields acc = function
        | [] -> Ok (BrilStructType (List.rev acc))
        | (k, v) :: rest -> (
            match bril_type_of_yojson v with
            | Ok t -> parse_fields ((k, t) :: acc) rest
            | Error e -> Error e)
      in
      parse_fields [] fields
  | _ -> Error "bril_type"

let rec bril_type_to_yojson = function
  | BrilType s -> `String s
  | BrilStructType fields ->
      `Assoc (List.map ~f:(fun (k, t) -> (k, bril_type_to_yojson t)) fields)

let rec bril_type_to_string = function
  | BrilType s -> s
  | BrilStructType fields ->
      let field_strs =
        List.map ~f:(fun (k, t) -> k ^ ": " ^ bril_type_to_string t) fields
      in
      "{" ^ String.concat ~sep:", " field_strs ^ "}"

type bril_const_instruction = {
  op : string;
  dest : string;
  typ : bril_type; [@key "type"]
  value : bril_immediate;
}
[@@deriving yojson, compare, hash, sexp]

let bril_const_instruction_to_string { op; dest; typ; value } =
  Printf.sprintf "%s %s: %s = %s" op dest (bril_type_to_string typ)
    (bril_immediate_to_string value)

type bril_value_instruction = {
  op : string;
  dest : string;
  typ : bril_type; [@key "type"]
  args : string list option; [@default None]
  funcs : string list option; [@default None]
  labels : string list option; [@default None]
}
[@@deriving yojson, compare, hash, sexp]

type bril_effect_instruction = {
  op : string;
  args : string list option; [@default None]
  funcs : string list option; [@default None]
  labels : string list option; [@default None]
}
[@@deriving yojson, compare, hash, sexp]

let format_args_funcs_labels op args funcs labels =
  let funcs_str =
    match funcs with
    | Some f when not (List.is_empty f) ->
        " " ^ String.concat ~sep:" " (List.map ~f:(fun s -> "@" ^ s) f)
    | _ -> ""
  in
  let args_str =
    match args with
    | Some a when not (List.is_empty a) -> " " ^ String.concat ~sep:" " a
    | _ -> ""
  in
  let labels_str =
    match labels with
    | Some l when not (List.is_empty l) ->
        " " ^ String.concat ~sep:" " (List.map ~f:(fun s -> "." ^ s) l)
    | _ -> ""
  in
  Printf.sprintf "%s%s%s%s" op funcs_str args_str labels_str

let bril_value_instruction_to_string { op; dest; typ; args; funcs; labels } =
  let rhs = format_args_funcs_labels op args funcs labels in
  Printf.sprintf "%s: %s = %s" dest (bril_type_to_string typ) rhs

let bril_effect_instruction_to_string { op; args; funcs; labels } =
  format_args_funcs_labels op args funcs labels

type bril_instruction =
  | BrilLabel of bril_label
  | BrilConstInstruction of bril_const_instruction
  | BrilValueInstruction of bril_value_instruction
  | BrilEffectInstruction of bril_effect_instruction
[@@deriving compare, hash, sexp]

let bril_instruction_of_yojson (json : Yojson.Safe.t) :
    (bril_instruction, string) Result.t =
  match json with
  | `Assoc fields ->
      if List.exists ~f:(fun (k, _) -> String.equal k "label") fields then
        match bril_label_of_yojson json with
        | Ok l -> Ok (BrilLabel l)
        | Error e -> Error e
      else if List.exists ~f:(fun (k, _) -> String.equal k "op") fields then
        match List.Assoc.find ~equal:String.equal fields "op" with
        | Some (`String "const") -> (
            match bril_const_instruction_of_yojson json with
            | Ok c -> Ok (BrilConstInstruction c)
            | Error e -> Error e)
        | Some (`String _) -> (
            if List.exists ~f:(fun (k, _) -> String.equal k "dest") fields then
              match bril_value_instruction_of_yojson json with
              | Ok v -> Ok (BrilValueInstruction v)
              | Error e -> Error e
            else
              match bril_effect_instruction_of_yojson json with
              | Ok e -> Ok (BrilEffectInstruction e)
              | Error e -> Error e)
        | Some _ -> Error "op must be a string"
        | None -> Error "Missing op"
      else Error "Instruction must have 'label' or 'op'"
  | _ -> Error "Instruction must be an object"

let bril_instruction_to_yojson = function
  | BrilLabel l -> bril_label_to_yojson l
  | BrilConstInstruction c -> bril_const_instruction_to_yojson c
  | BrilValueInstruction v -> bril_value_instruction_to_yojson v
  | BrilEffectInstruction e -> bril_effect_instruction_to_yojson e

let bril_instruction_to_string = function
  | BrilLabel l -> bril_label_to_string l
  | BrilConstInstruction c -> bril_const_instruction_to_string c
  | BrilValueInstruction v -> bril_value_instruction_to_string v
  | BrilEffectInstruction e -> bril_effect_instruction_to_string e

type bril_arg = { name : string; typ : bril_type [@key "type"] }
[@@deriving yojson, compare, hash, sexp]

let bril_arg_to_string { name; typ } =
  Printf.sprintf "%s: %s" name (bril_type_to_string typ)

type bril_function = {
  name : string;
  args : bril_arg list option; [@default None]
  typ : bril_type option; [@key "type"] [@default None]
  instrs : bril_instruction list;
}
[@@deriving yojson, compare, hash, sexp]

let bril_function_to_string { name; args; typ; instrs } =
  let _ = match typ with Some t -> bril_type_to_string t | None -> "void" in

  let args_str =
    match args with
    | Some a -> String.concat ~sep:", " (List.map ~f:bril_arg_to_string a)
    | None -> ""
  in
  (* let typ_str = *)
  (*   match typ with Some t -> bril_type_to_string t | None -> "void" *)
  (* in *)
  let instrs_str =
    String.concat ~sep:"\n"
      (List.map
         ~f:(function
           | BrilLabel _ as l -> bril_instruction_to_string l
           | instr -> "  " ^ bril_instruction_to_string instr ^ ";")
         instrs)
  in
  (* Printf.sprintf "@%s(%s) -> %s {\n%s\n}" name args_str typ_str instrs_str *)
  Printf.sprintf "@%s(%s) {\n%s\n}" name args_str instrs_str

type bril_program = { functions : bril_function list } [@@deriving yojson]

let bril_program_to_string { functions } =
  String.concat ~sep:"\n\n" (List.map ~f:bril_function_to_string functions)

let parsed_bril_json bril_string =
  let bril_json = Yojson.Safe.from_string bril_string in
  let bril_program = bril_program_of_yojson bril_json in
  match bril_program with
  | Ok program -> program
  | Error err -> failwith ("Error parsing Bril JSON: " ^ err)

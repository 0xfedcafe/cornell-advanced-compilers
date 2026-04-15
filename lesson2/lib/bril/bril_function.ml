open Base
open Bril_type
open Bril_instruction

type bril_arg = { name : string; typ : Bril_type.bril_type [@key "type"] }
[@@deriving yojson, compare, hash, sexp]

let bril_arg_to_string { name; typ } =
  Printf.sprintf "%s: %s" name (Bril_type.bril_type_to_string typ)

type bril_function = {
  name : string;
  args : bril_arg list option; [@default None]
  typ : Bril_type.bril_type option; [@key "type"] [@default None]
  instrs : Bril_instruction.bril_instruction list;
}
[@@deriving yojson, compare, hash, sexp]

let bril_function_to_string { name; args; typ; instrs } =
  let _ =
    match typ with Some t -> Bril_type.bril_type_to_string t | None -> "void"
  in

  let args_str =
    match args with
    | Some a -> String.concat ~sep:", " (List.map ~f:bril_arg_to_string a)
    | None -> ""
  in
  let instrs_str =
    String.concat ~sep:"\n"
      (List.map
         ~f:(function
           | Bril_instruction.BrilLabel _ as l ->
               Bril_instruction.bril_instruction_to_string l
           | instr ->
               "  " ^ Bril_instruction.bril_instruction_to_string instr ^ ";")
         instrs)
  in
  Printf.sprintf "@%s(%s) {\n%s\n}" name args_str instrs_str

type bril_ir_function = {
  name : string;
  args : bril_arg list option;
  typ : bril_type option;
  instrs : bril_ir_instruction list;
}
[@@deriving compare, hash, sexp]

let bril_ir_function_to_string { name; args; typ; instrs } =
  let _ =
    match typ with Some t -> Bril_type.bril_type_to_string t | None -> "void"
  in

  let args_str =
    match args with
    | Some a -> String.concat ~sep:", " (List.map ~f:bril_arg_to_string a)
    | None -> ""
  in
  let instrs_str =
    String.concat ~sep:"\n"
      (List.map
         ~f:(function
           | Bril_instruction.Label _ as l ->
               Bril_instruction.bril_ir_instruction_to_string l
           | instr ->
               "  " ^ Bril_instruction.bril_ir_instruction_to_string instr ^ ";")
         instrs)
  in
  Printf.sprintf "@%s(%s) {\n%s\n}" name args_str instrs_str

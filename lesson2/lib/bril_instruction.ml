open Base
open Bril_label
open Bril_type
open Bril_immediate

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

type bin_op = { dst : string; src1 : string; src2 : string }
[@@deriving compare, hash, sexp]

type un_op = { dst : string; src1 : string } [@@deriving compare, hash, sexp]

type bril_arithm_instr =
  | Add of bin_op
  | Sub of bin_op
  | Mul of bin_op
  | Div of bin_op
[@@deriving compare, hash, sexp]

let bril_arithm_of_value_instr = function
  (* TODO: Add typ checks *)
  | { op = "add"; dest; typ = _; args = Some [ src1; src2 ]; _ } ->
      Some (Add { dst = dest; src1; src2 })
  | { op = "sub"; dest; typ = _; args = Some [ src1; src2 ]; _ } ->
      Some (Sub { dst = dest; src1; src2 })
  | { op = "mul"; dest; typ = _; args = Some [ src1; src2 ]; _ } ->
      Some (Mul { dst = dest; src1; src2 })
  | { op = "div"; dest; typ = _; args = Some [ src1; src2 ]; _ } ->
      Some (Div { dst = dest; src1; src2 })
  | _ -> None

type bril_comp_instr =
  | Eq of bin_op
  | Lt of bin_op
  | Gt of bin_op
  | Le of bin_op
  | Ge of bin_op
[@@deriving compare, hash, sexp]

let bril_comp_of_value_instr = function
  (* TODO: Add typ checks *)
  | { op = "eq"; dest; typ = _; args = Some [ src1; src2 ]; _ } ->
      Some (Eq { dst = dest; src1; src2 })
  | { op = "lt"; dest; typ = _; args = Some [ src1; src2 ]; _ } ->
      Some (Lt { dst = dest; src1; src2 })
  | { op = "gt"; dest; typ = _; args = Some [ src1; src2 ]; _ } ->
      Some (Gt { dst = dest; src1; src2 })
  | { op = "le"; dest; typ = _; args = Some [ src1; src2 ]; _ } ->
      Some (Le { dst = dest; src1; src2 })
  | { op = "ge"; dest; typ = _; args = Some [ src1; src2 ]; _ } ->
      Some (Ge { dst = dest; src1; src2 })
  | _ -> None

type bril_logic_instr = Not of un_op | And of bin_op | Or of bin_op
[@@deriving compare, hash, sexp]

let bril_logic_of_value_instr = function
  | { op = "not"; dest; typ = _; args = Some [ src1 ]; _ } ->
      Some (Not { dst = dest; src1 })
  | { op = "and"; dest; typ = _; args = Some [ src1; src2 ]; _ } ->
      Some (And { dst = dest; src1; src2 })
  | { op = "or"; dest; typ = _; args = Some [ src1; src2 ]; _ } ->
      Some (Or { dst = dest; src1; src2 })
  | _ -> None

type bril_control_instr =
  | Jump of bril_label
  | Branch of { cond : string; iftrue : bril_label; iffalse : bril_label }
  | Call of { name : string; arg : string list option }
  | Return of bril_immediate option
[@@deriving compare, hash, sexp]

let bril_control_of_effect_instr = function
  | { op = "jmp"; labels = Some [ lbl ]; _ } -> Some (Jump { label = lbl })
  | { op = "br"; args = Some [ cond ]; labels = Some [ iftrue; iffalse ]; _ } ->
      Some
        (Branch
           { cond; iftrue = { label = iftrue }; iffalse = { label = iffalse } })
  | { op = "call"; funcs = Some [ name ]; args; _ } ->
      Some (Call { name; arg = args })
  | { op = "ret"; args = _; _ } -> Some (Return None)
  | _ -> None

type bril_misc_instr =
  | Identity of { dst : string; src : string }
  | Print of string list
  | Nop
[@@deriving compare, hash, sexp]

let bril_misc_of_effect_instr = function
  | { op = "id"; args = Some [ src ]; funcs = None; labels = None } ->
      Some (Identity { dst = src; src })
  | { op = "print"; args; funcs = None; labels = None } ->
      Some (Print (Option.value args ~default:[]))
  | { op = "nop"; args = None; funcs = None; labels = None } -> Some Nop
  | _ -> None

type bril_ir_instruction =
  | Arithm of bril_arithm_instr
  | Comp of bril_comp_instr
  | Logic of bril_logic_instr
  | Control of bril_control_instr
  | Misc of bril_misc_instr
[@@deriving compare, hash, sexp]

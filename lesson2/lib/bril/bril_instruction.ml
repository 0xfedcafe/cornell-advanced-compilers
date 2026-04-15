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

type const_op = { dst : string; value : bril_immediate }
[@@deriving compare, hash, sexp]

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

type bril_const_instr = Const of const_op [@@deriving compare, hash, sexp]

let bril_const_of_const_instr = function
  | { op = "const"; dest; typ = _; value } -> Some (Const { dst = dest; value })
  | _ -> None

type bril_control_instr =
  | Jump of bril_label
  | Branch of { cond : string; iftrue : bril_label; iffalse : bril_label }
  | Call of { name : string; arg : string list option; dst : string option }
  | Return of bril_immediate option
[@@deriving compare, hash, sexp]

let bril_control_of_effect_instr = function
  | { op = "jmp"; labels = Some [ lbl ]; _ } -> Some (Jump { label = lbl })
  | { op = "br"; args = Some [ cond ]; labels = Some [ iftrue; iffalse ]; _ } ->
      Some
        (Branch
           { cond; iftrue = { label = iftrue }; iffalse = { label = iffalse } })
  | { op = "call"; funcs = Some [ name ]; args; _ } ->
      Some (Call { name; arg = args; dst = None })
  | { op = "ret"; args = _; _ } -> Some (Return None)
  | _ -> None

let bril_call_of_value_instr = function
  | { op = "call"; funcs = Some [ name ]; args; dest; _ } ->
      Some (Call { name; arg = args; dst = Some dest })
  | _ -> None

type bril_misc_instr =
  | Identity of { dst : string; src : string }
  | Print of string list
  | Nop
[@@deriving compare, hash, sexp]

let bril_misc_of_value_instr = function
  | { op = "id"; dest = dest_v; typ = _; args = Some [ src ]; _ } ->
      Some (Identity { dst = dest_v; src })
  | { op = "print"; dest = _; typ = _; args; _ } ->
      Some (Print (Option.value args ~default:[]))
  | _ -> None

let bril_misc_of_effect_instr = function
  | { op = "id"; args = Some [ src ]; funcs = None; labels = None } ->
      Some (Identity { dst = src; src })
  | { op = "print"; args; funcs = None; labels = None } ->
      Some (Print (Option.value args ~default:[]))
  | { op = "nop"; args = None; funcs = None; labels = None } -> Some Nop
  | _ -> None

module Instruction = struct
  module T = struct
    type t =
      | Arithm of bril_arithm_instr
      | Comp of bril_comp_instr
      | Logic of bril_logic_instr
      | Const of bril_const_instr
      | Control of bril_control_instr
      | Label of bril_label
      | Misc of bril_misc_instr
    [@@deriving compare, hash, sexp]
  end

  include T
  include Base.Comparator.Make (T)

  let to_string = function
  | Arithm a -> (
      match a with
      | Add { dst; src1; src2 } ->
          Printf.sprintf "%s: int = add %s %s" dst src1 src2
      | Sub { dst; src1; src2 } ->
          Printf.sprintf "%s: int = sub %s %s" dst src1 src2
      | Mul { dst; src1; src2 } ->
          Printf.sprintf "%s: int = mul %s %s" dst src1 src2
      | Div { dst; src1; src2 } ->
          Printf.sprintf "%s: int = div %s %s" dst src1 src2)
  | Comp c -> (
      match c with
      | Eq { dst; src1; src2 } ->
          Printf.sprintf "eq %s = %s == %s" dst src1 src2
      | Lt { dst; src1; src2 } -> Printf.sprintf "lt %s = %s < %s" dst src1 src2
      | Gt { dst; src1; src2 } -> Printf.sprintf "gt %s = %s > %s" dst src1 src2
      | Le { dst; src1; src2 } ->
          Printf.sprintf "le %s = %s <= %s" dst src1 src2
      | Ge { dst; src1; src2 } ->
          Printf.sprintf "ge %s = %s >= %s" dst src1 src2)
  | Logic l -> (
      match l with
      | Not { dst; src1 } -> Printf.sprintf "not %s = !%s" dst src1
      | And { dst; src1; src2 } ->
          Printf.sprintf "and %s = %s && %s" dst src1 src2
      | Or { dst; src1; src2 } ->
          Printf.sprintf "or %s = %s || %s" dst src1 src2)
  | Const (Const { dst; value }) ->
      let typ = match value with BrilInt _ -> "int" | BrilBool _ -> "bool" in
      let imm_str = bril_immediate_to_string value in
      Printf.sprintf "%s: %s = const %s" dst typ imm_str
  | Control c -> (
      match c with
      | Jump lbl -> Printf.sprintf "jmp .%s" lbl.label
      | Branch { cond; iftrue; iffalse } ->
          Printf.sprintf "br %s .%s . %s" cond iftrue.label iffalse.label
      | Call { name; arg; dst } ->
          let args_str =
            match arg with
            | Some args when not (List.is_empty args) ->
                " " ^ String.concat ~sep:" " args
            | _ -> ""
          in
          let dst_str =
            match dst with Some d -> Printf.sprintf " %s = " d | None -> ""
          in
          Printf.sprintf "call %s@%s(%s)" dst_str name args_str
      | Return None -> " ret "
      | Return (Some arg) ->
          Printf.sprintf " ret %s" (bril_immediate_to_string arg))
  | Label l -> Printf.sprintf ".%s" l.label
  | Misc m -> (
      match m with
      | Identity { dst; src } -> Printf.sprintf "id %s = %s" dst src
      | Print args ->
          let args_str =
            if not (List.is_empty args) then " " ^ String.concat ~sep:" " args
            else ""
          in
          Printf.sprintf "print%s" args_str
      | Nop -> "nop")

let from_instruction = function
  | BrilValueInstruction v -> (
      match bril_arithm_of_value_instr v with
      | Some a -> Arithm a
      | None -> (
          match bril_comp_of_value_instr v with
          | Some c -> Comp c
          | None -> (
              match bril_logic_of_value_instr v with
              | Some l -> Logic l
              | None -> (
                  match bril_call_of_value_instr v with
                  | Some c -> Control c
                  | None -> (
                      match bril_misc_of_value_instr v with
                      | Some m -> Misc m
                      | None -> failwith "Invalid value instruction")))))
  | BrilEffectInstruction e -> (
      match bril_control_of_effect_instr e with
      | Some c -> Control c
      | None -> (
          match bril_misc_of_effect_instr e with
          | Some m -> Misc m
          | None -> failwith "Invalid effect instruction"))
  | BrilConstInstruction c -> (
      let converted = bril_const_of_const_instr c in
      match converted with
      | Some const -> Const const
      | None -> failwith "Invalid const instruction")
  | BrilLabel l -> Label l

let get_dest = function
  | Arithm (Add { dst; _ } | Sub { dst; _ } | Mul { dst; _ } | Div { dst; _ })
    ->
      Some dst
  | Comp
      ( Eq { dst; _ }
      | Lt { dst; _ }
      | Gt { dst; _ }
      | Le { dst; _ }
      | Ge { dst; _ } ) ->
      Some dst
  | Logic (Not { dst; _ }) -> Some dst
  | Logic (And { dst; _ } | Or { dst; _ }) -> Some dst
  | Const (Const { dst; _ }) -> Some dst
  | Misc (Identity { dst; _ }) -> Some dst
  | Control (Call { dst; _ }) -> dst
  | _ -> None

let replace_dst instr new_dst =
  match instr with
  | Arithm (Add op) -> Arithm (Add { op with dst = new_dst })
  | Arithm (Sub op) -> Arithm (Sub { op with dst = new_dst })
  | Arithm (Mul op) -> Arithm (Mul { op with dst = new_dst })
  | Arithm (Div op) -> Arithm (Div { op with dst = new_dst })
  | Comp (Eq op) -> Comp (Eq { op with dst = new_dst })
  | Comp (Lt op) -> Comp (Lt { op with dst = new_dst })
  | Comp (Gt op) -> Comp (Gt { op with dst = new_dst })
  | Comp (Le op) -> Comp (Le { op with dst = new_dst })
  | Comp (Ge op) -> Comp (Ge { op with dst = new_dst })
  | Logic (Not op) -> Logic (Not { op with dst = new_dst })
  | Logic (And op) -> Logic (And { op with dst = new_dst })
  | Logic (Or op) -> Logic (Or { op with dst = new_dst })
  | Const (Const op) -> Const (Const { op with dst = new_dst })
  | Misc (Identity id) -> Misc (Identity { id with dst = new_dst })
  | Control (Call call) -> Control (Call { call with dst = Some new_dst })
  | _ -> instr

let get_args = function
  | Arithm
      ( Add { src1; src2; _ }
      | Sub { src1; src2; _ }
      | Mul { src1; src2; _ }
      | Div { src1; src2; _ } ) ->
      [ src1; src2 ]
  | Comp
      ( Eq { src1; src2; _ }
      | Lt { src1; src2; _ }
      | Gt { src1; src2; _ }
      | Le { src1; src2; _ }
      | Ge { src1; src2; _ } ) ->
      [ src1; src2 ]
  | Logic (Not { src1; _ }) -> [ src1 ]
  | Logic (And { src1; src2; _ } | Or { src1; src2; _ }) -> [ src1; src2 ]
  | Control (Branch { cond; _ }) -> [ cond ]
  | Control (Call { arg = Some args; _ }) -> args
  | Misc (Identity { src; _ }) -> [ src ]
  | Misc (Print args) -> args
  | _ -> []

let replace_args (instr : t) (f : string -> string) : t =
  match instr with
  | Arithm (Add op) ->
      Arithm (Add { op with src1 = f op.src1; src2 = f op.src2 })
  | Arithm (Sub op) ->
      Arithm (Sub { op with src1 = f op.src1; src2 = f op.src2 })
  | Arithm (Mul op) ->
      Arithm (Mul { op with src1 = f op.src1; src2 = f op.src2 })
  | Arithm (Div op) ->
      Arithm (Div { op with src1 = f op.src1; src2 = f op.src2 })
  | Comp (Eq op) -> Comp (Eq { op with src1 = f op.src1; src2 = f op.src2 })
  | Comp (Lt op) -> Comp (Lt { op with src1 = f op.src1; src2 = f op.src2 })
  | Comp (Gt op) -> Comp (Gt { op with src1 = f op.src1; src2 = f op.src2 })
  | Comp (Le op) -> Comp (Le { op with src1 = f op.src1; src2 = f op.src2 })
  | Comp (Ge op) -> Comp (Ge { op with src1 = f op.src1; src2 = f op.src2 })
  | Logic (Not op) -> Logic (Not { op with src1 = f op.src1 })
  | Logic (And op) -> Logic (And { op with src1 = f op.src1; src2 = f op.src2 })
  | Logic (Or op) -> Logic (Or { op with src1 = f op.src1; src2 = f op.src2 })
  | Misc (Identity id) -> Misc (Identity { id with src = f id.src })
  | Misc (Print args) -> Misc (Print (List.map args ~f))
  | Control (Branch br) -> Control (Branch { br with cond = f br.cond })
  | Control (Call call) ->
      Control (Call { call with arg = Option.map call.arg ~f:(List.map ~f) })
  | _ -> instr

let string_of_op = function
  | Arithm (Add _) -> "add"
  | Arithm (Sub _) -> "sub"
  | Arithm (Mul _) -> "mul"
  | Arithm (Div _) -> "div"
  | Comp (Eq _) -> "eq"
  | Comp (Lt _) -> "lt"
  | Comp (Gt _) -> "gt"
  | Comp (Le _) -> "le"
  | Comp (Ge _) -> "ge"
  | Logic (Not _) -> "not"
  | Logic (And _) -> "and"
  | Logic (Or _) -> "or"
  | Const (Const _) -> "const"
  | Control (Jump _) -> "jmp"
  | Control (Branch _) -> "br"
  | Control (Call _) -> "call"
  | Control (Return _) -> "ret"
  | Misc (Identity _) -> "id"
  | Misc (Print _) -> "print"
  | Misc Nop -> "nop"
  | Label _ -> failwith "Labels don't have ops"

end

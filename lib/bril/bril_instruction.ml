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

type bin_op = { dst : string; typ : bril_type; src1 : string; src2 : string }
[@@deriving compare, hash, sexp]

type un_op = { dst : string; typ : bril_type option; src1 : string }
[@@deriving compare, hash, sexp]

type const_op = { dst : string; typ : bril_type; value : bril_immediate }
[@@deriving compare, hash, sexp]

module Op = struct
  type binary = Add | Sub | Mul | Div | Eq | Lt | Gt | Le | Ge | And | Or
  [@@deriving compare, hash, sexp, enumerate]

  type unary = Not | Id [@@deriving compare, hash, sexp, enumerate]

  let binary_name = function
    | Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | Div -> "div"
    | Eq -> "eq"
    | Lt -> "lt"
    | Gt -> "gt"
    | Le -> "le"
    | Ge -> "ge"
    | And -> "and"
    | Or -> "or"

  let unary_name = function Not -> "not" | Id -> "id"

  let by_name name all =
    List.map all ~f:(fun o -> (name o, o)) |> Map.of_alist_exn (module String)

  let binary_by_name = by_name binary_name all_of_binary
  let unary_by_name = by_name unary_name all_of_unary
  let binary_of_name s = Map.find binary_by_name s
  let unary_of_name s = Map.find unary_by_name s

  let binary_result_type = function
    | Add | Sub | Mul | Div -> BrilType "int"
    | Eq | Lt | Gt | Le | Ge | And | Or -> BrilType "bool"

  let unary_result_type = function Not -> Some (BrilType "bool") | Id -> None

  let is_commutative = function
    | Add | Mul | Eq | And | Or -> true
    | Sub | Div | Lt | Gt | Le | Ge -> false
end

let binary_of_value_instr = function
  (* TODO: Add typ checks *)
  | { op; dest; typ; args = Some [ src1; src2 ]; _ } ->
      Op.binary_of_name op
      |> Option.map ~f:(fun o -> (o, { dst = dest; typ; src1; src2 }))
  | _ -> None

let unary_of_value_instr = function
  | { op; dest; typ; args = Some [ src1 ]; _ } ->
      Op.unary_of_name op
      |> Option.map ~f:(fun o -> (o, { dst = dest; typ = Some typ; src1 }))
  | _ -> None

let unary_of_effect_instr = function
  | { op = "id"; args = Some [ src1 ]; funcs = None; labels = None } ->
      Some (Op.Id, { dst = src1; typ = None; src1 })
  | _ -> None

let const_of_const_instr = function
  | { op = "const"; dest; typ; value } -> Some { dst = dest; typ; value }
  | _ -> None

type bril_control_instr =
  | Jump of bril_label
  | Branch of { cond : string; iftrue : bril_label; iffalse : bril_label }
  | Call of {
      name : string;
      arg : string list option;
      dst : string option;
      typ : bril_type option;
    }
  | Return of bril_immediate option
[@@deriving compare, hash, sexp]

let bril_control_of_effect_instr = function
  | { op = "jmp"; labels = Some [ lbl ]; _ } -> Some (Jump { label = lbl })
  | { op = "br"; args = Some [ cond ]; labels = Some [ iftrue; iffalse ]; _ } ->
      Some
        (Branch
           { cond; iftrue = { label = iftrue }; iffalse = { label = iffalse } })
  | { op = "call"; funcs = Some [ name ]; args; _ } ->
      Some (Call { name; arg = args; dst = None; typ = None })
  | { op = "ret"; args = _; _ } -> Some (Return None)
  | _ -> None

let bril_call_of_value_instr = function
  | { op = "call"; funcs = Some [ name ]; args; dest; typ; _ } ->
      Some (Call { name; arg = args; dst = Some dest; typ = Some typ })
  | _ -> None

type bril_misc_instr = Print of string list | Nop
[@@deriving compare, hash, sexp]

let bril_misc_of_value_instr = function
  | { op = "print"; dest = _; typ = _; args; _ } ->
      Some (Print (Option.value args ~default:[]))
  | _ -> None

let bril_misc_of_effect_instr = function
  | { op = "print"; args; funcs = None; labels = None } ->
      Some (Print (Option.value args ~default:[]))
  | { op = "nop"; args = None; funcs = None; labels = None } -> Some Nop
  | _ -> None

type bril_ssa_instr =
  | Get of { dst : string; typ : bril_type }
  | Set of { dst : string; src : string }
  | Undef of { dst : string; typ : bril_type }
[@@deriving compare, hash, sexp]

module Instruction = struct
  module T = struct
    type t =
      | Binary of Op.binary * bin_op
      | Unary of Op.unary * un_op
      | Const of const_op
      | Control of bril_control_instr
      | Label of bril_label
      | Misc of bril_misc_instr
      | SSA of bril_ssa_instr
    [@@deriving compare, hash, sexp]
  end

  include T
  include Base.Comparator.Make (T)

  let to_string = function
    | Binary (o, { dst; typ; src1; src2 }) ->
        Printf.sprintf "%s: %s = %s %s %s" dst (bril_type_to_string typ)
          (Op.binary_name o) src1 src2
    | Unary (o, { dst; typ; src1 }) ->
        let typ_str =
          match typ with Some t -> ": " ^ bril_type_to_string t | None -> ""
        in
        Printf.sprintf "%s%s = %s %s" dst typ_str (Op.unary_name o) src1
    | Const { dst; typ; value } ->
        let imm_str = bril_immediate_to_string value in
        Printf.sprintf "%s: %s = const %s" dst (bril_type_to_string typ) imm_str
    | Control c -> (
        match c with
        | Jump lbl -> Printf.sprintf "jmp .%s" lbl.label
        | Branch { cond; iftrue; iffalse } ->
            Printf.sprintf "br %s .%s . %s" cond iftrue.label iffalse.label
        | Call { name; arg; dst; typ = _ } ->
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
    | Misc (Print args) ->
        let args_str =
          if not (List.is_empty args) then " " ^ String.concat ~sep:" " args
          else ""
        in
        Printf.sprintf "print%s" args_str
    | Misc Nop -> "nop"
    | SSA s -> (
        match s with
        | Get { dst; typ } ->
            Printf.sprintf "%s: %s = get" dst (bril_type_to_string typ)
        | Set { dst; src } -> Printf.sprintf "set %s %s" dst src
        | Undef { dst; typ } ->
            Printf.sprintf "%s: %s = undef" dst (bril_type_to_string typ))

  let first_parse parsers instr what =
    match List.find_map parsers ~f:(fun parse -> parse instr) with
    | Some i -> i
    | None -> failwith ("Invalid " ^ what ^ " instruction")

  let from_instruction = function
    | BrilValueInstruction v ->
        first_parse
          [
            (fun v ->
              binary_of_value_instr v
              |> Option.map ~f:(fun (o, args) -> Binary (o, args)));
            (fun v ->
              unary_of_value_instr v
              |> Option.map ~f:(fun (o, args) -> Unary (o, args)));
            (fun v ->
              bril_call_of_value_instr v |> Option.map ~f:(fun c -> Control c));
            (fun v ->
              bril_misc_of_value_instr v |> Option.map ~f:(fun m -> Misc m));
          ]
          v "value"
    | BrilEffectInstruction e ->
        first_parse
          [
            (fun e ->
              bril_control_of_effect_instr e
              |> Option.map ~f:(fun c -> Control c));
            (fun e ->
              bril_misc_of_effect_instr e |> Option.map ~f:(fun m -> Misc m));
            (fun e ->
              unary_of_effect_instr e
              |> Option.map ~f:(fun (o, args) -> Unary (o, args)));
          ]
          e "effect"
    | BrilConstInstruction c ->
        first_parse
          [
            (fun c ->
              const_of_const_instr c |> Option.map ~f:(fun k -> Const k));
          ]
          c "const"
    | BrilLabel l -> Label l

  let get_dest = function
    | Binary (_, { dst; _ }) | Unary (_, { dst; _ }) | Const { dst; _ } ->
        Some dst
    | Control (Call { dst; _ }) -> dst
    | SSA (Get { dst; _ } | Undef { dst; _ }) -> Some dst
    | Control (Jump _ | Branch _ | Return _) | Label _ | Misc _ | SSA (Set _) ->
        None

  let result_type = function
    | Binary (_, { typ; _ }) | Const { typ; _ } -> Some typ
    | Unary (_, { typ; _ }) | Control (Call { typ; _ }) -> typ
    | SSA (Get { typ; _ } | Undef { typ; _ }) -> Some typ
    | Control (Jump _ | Branch _ | Return _) | Label _ | Misc _ | SSA (Set _) ->
        None

  let replace_dst instr dst =
    match instr with
    | Binary (o, op) -> Binary (o, { op with dst })
    | Unary (o, op) -> Unary (o, { op with dst })
    | Const op -> Const { op with dst }
    | Control (Call call) -> Control (Call { call with dst = Some dst })
    | SSA (Get s) -> SSA (Get { s with dst })
    | SSA (Undef s) -> SSA (Undef { s with dst })
    | Control (Jump _ | Branch _ | Return _) | Label _ | Misc _ | SSA (Set _) ->
        instr

  let get_args = function
    | Binary (_, { src1; src2; _ }) -> [ src1; src2 ]
    | Unary (_, { src1; _ }) -> [ src1 ]
    | Control (Branch { cond; _ }) -> [ cond ]
    | Control (Call { arg; _ }) -> Option.value arg ~default:[]
    | Misc (Print args) -> args
    | SSA (Set { src; _ }) -> [ src ]
    | Const _ | Label _
    | Misc Nop
    | Control (Jump _ | Return _)
    | SSA (Get _ | Undef _) ->
        []

  let replace_args (instr : t) (f : string -> string) : t =
    match instr with
    | Binary (o, op) ->
        Binary (o, { op with src1 = f op.src1; src2 = f op.src2 })
    | Unary (o, op) -> Unary (o, { op with src1 = f op.src1 })
    | Misc (Print args) -> Misc (Print (List.map args ~f))
    | Control (Branch br) -> Control (Branch { br with cond = f br.cond })
    | Control (Call call) ->
        Control (Call { call with arg = Option.map call.arg ~f:(List.map ~f) })
    | SSA (Set s) -> SSA (Set { s with src = f s.src })
    | Const _ | Label _
    | Misc Nop
    | Control (Jump _ | Return _)
    | SSA (Get _ | Undef _) ->
        instr

  let string_of_op = function
    | Binary (o, _) -> Op.binary_name o
    | Unary (o, _) -> Op.unary_name o
    | Const _ -> "const"
    | Control (Jump _) -> "jmp"
    | Control (Branch _) -> "br"
    | Control (Call _) -> "call"
    | Control (Return _) -> "ret"
    | Misc (Print _) -> "print"
    | Misc Nop -> "nop"
    | SSA (Get _) -> "get"
    | SSA (Set _) -> "set"
    | SSA (Undef _) -> "undef"
    | Label _ -> failwith "Labels don't have ops"
end

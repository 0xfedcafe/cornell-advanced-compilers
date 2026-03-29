open Base

(* Bril Type Definitions *)

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

type bril_const_instruction = {
  op : string;
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

type basic_block = {
  label : string option;
  instructions : bril_instruction list;
}

let parsed_bril_json bril_string =
  let bril_json = Yojson.Safe.from_string bril_string in
  let bril_program = bril_program_of_yojson bril_json in
  match bril_program with
  | Ok program -> program
  | Error err -> failwith ("Error parsing Bril JSON: " ^ err)

let is_effect = function BrilEffectInstruction _ -> true | _ -> false

let has_label = function
  | BrilLabel _ -> true
  | BrilValueInstruction v -> Option.is_some v.labels
  | BrilEffectInstruction e -> Option.is_some e.labels
  | _ -> false

let is_label = function BrilLabel _ -> true | _ -> false

let bbs_in_function func =
  let rec build_bbs instrs current_bb bbs =
    match instrs with
    | x :: xs -> (
        let is_lbl = is_label x in
        let is_new_label = is_lbl && Option.is_none current_bb.label in
        let label_seq = is_lbl && not is_new_label in

        let upd_instrs, upd_label =
          match x with
          | BrilLabel l -> (current_bb.instructions, Some l.label)
          | _ -> (x :: current_bb.instructions, current_bb.label)
        in

        let empty_instrs = List.is_empty upd_instrs in

        ((is_effect x && has_label x) || label_seq) |> function
        | true ->
            let keep_label = (is_new_label && empty_instrs) || label_seq in
            build_bbs xs
              {
                label = (if keep_label then upd_label else None);
                instructions = [];
              }
              ({
                 label = (if keep_label then current_bb.label else upd_label);
                 instructions = List.rev upd_instrs;
               }
              :: bbs)
        | false ->
            build_bbs xs { label = upd_label; instructions = upd_instrs } bbs)
    | [] -> (
        match current_bb with
        | { label = None; instructions = [] } -> bbs
        | _ -> current_bb :: bbs)
  in
  build_bbs func.instrs { label = None; instructions = [] } []

let gather_basic_blocks program =
  let rec bbs_iter funcs bbs =
    match funcs with
    | f :: fs -> bbs_iter fs (List.append (bbs_in_function f) bbs)
    | [] -> bbs
  in
  List.rev (bbs_iter program.functions [])

let build_cfg program =
  let bbs = gather_basic_blocks program in
  (* hashset add edge from bb1 to bb2 if bb1 has a jump to bb2 *)
  let add_edge cfg bb1 bb2 =
    let edges = match Hashtbl.find cfg bb1 with Some es -> es | None -> [] in
    Hashtbl.set cfg ~key:bb1 ~data:(bb2 :: edges)
  in
  (* Get the basic block corresponding to a label *)
  let label_to_bb =
    Hashtbl.create
      (module struct
        type t = string

        let compare = String.compare
        let hash = Hashtbl.hash
        let sexp_of_t _ = Sexp.Atom "string"
      end)
  in

  let cfg =
    Hashtbl.create
      (module struct
        type t = basic_block

        let compare = Poly.compare
        let hash = Hashtbl.hash
        let sexp_of_t _ = Sexp.Atom "basic_block"
      end)
  in

  let handle_instr_edge bb instr =
    match instr with
    | BrilEffectInstruction e -> (
        match e.labels with
        | None -> ()
        | Some ls -> (
            match e.op with
            | "jmp" -> (
                match List.hd ls with
                | Some v ->
                    Hashtbl.find label_to_bb v
                    |> Option.iter ~f:(add_edge cfg bb)
                | None -> failwith "wrong jmp instruction")
            | "br" -> (
                match ls with
                | br1 :: br2 :: _ ->
                    Hashtbl.find label_to_bb br1
                    |> Option.iter ~f:(add_edge cfg bb);
                    Hashtbl.find label_to_bb br2
                    |> Option.iter ~f:(add_edge cfg bb)
                | _ -> failwith "wrong br instruction")
            | "ret" -> ()
            | _ -> ()))
    | _ -> ()
  in

  List.iter bbs ~f:(fun bb ->
      match bb.instructions with
      | [] -> ()
      | instrs -> handle_instr_edge bb (List.last_exn instrs));

  cfg

(* let main () = *)
(*   let bril_string = In_channel.read_all "input.bril" in *)
(*   let bril_program = parsed_bril_json bril_string in *)
(*   printf "Parsed Bril Program: %s\n" (Yojson.Safe.pretty_to_string (bril_program_to_yojson bril_program)) *)

open Base
open Bril

type basic_block = {
  label : string option;
  instructions : bril_instruction list;
}

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

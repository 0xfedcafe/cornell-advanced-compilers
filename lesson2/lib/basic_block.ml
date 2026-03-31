open Base
open Bril

type basic_block = {
  label : string option;
  instructions : bril_instruction list;
}

let bbs_in_function func =
  let rec build_bbs instrs curr_lbl curr_instrs bbs =
    match instrs with
    | [] ->
        if Option.is_some curr_lbl || not (List.is_empty curr_instrs) then
          { label = curr_lbl; instructions = List.rev curr_instrs } :: bbs
        else bbs
    | BrilLabel l :: rest ->
        let bbs' =
          if Option.is_some curr_lbl || not (List.is_empty curr_instrs) then
            { label = curr_lbl; instructions = List.rev curr_instrs } :: bbs
          else bbs
        in
        build_bbs rest (Some l.label) [] bbs'
    | instr :: rest ->
        let curr_instrs' = instr :: curr_instrs in
        let is_terminator =
          match instr with
          | BrilEffectInstruction { op; _ } ->
              List.exists [ "jmp"; "br"; "ret" ] ~f:(String.equal op)
          | _ -> false
        in
        if is_terminator then
          let bbs' =
            { label = curr_lbl; instructions = List.rev curr_instrs' } :: bbs
          in
          build_bbs rest None [] bbs'
        else build_bbs rest curr_lbl curr_instrs' bbs
  in
  build_bbs func.instrs None [] []

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

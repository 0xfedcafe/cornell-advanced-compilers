open Cfg
open Base
open Bril
open Basic_block

let dce_pass (cfg : CFG.t) : unit =
  let bbs = cfg.nodes in

  let handle_block ~key:bb_id ~data:(bb : basic_block) : bool =
    let defined_later = Hashtbl.create (module String) in
    let optimised_instrs =
      List.fold_left (List.rev bb.instructions) ~init:[] ~f:(fun acc instr ->
          let is_dead =
            match instr with
            | BrilValueInstruction v -> Hashtbl.mem defined_later v.dest
            | BrilConstInstruction c -> Hashtbl.mem defined_later c.dest
            | _ -> false
          in
          if is_dead then acc
          else begin
            (* Mark written: if we keep it, it becomes the 'latest' definition in reverse order *)
            (match instr with
            | BrilValueInstruction v ->
                Hashtbl.set defined_later ~key:v.dest ~data:()
            | BrilConstInstruction c ->
                Hashtbl.set defined_later ~key:c.dest ~data:()
            | _ -> ());

            (* Mark used: using a variable means earlier definitions are NO LONGER dead *)
            (match instr with
            | BrilValueInstruction v ->
                Option.iter v.args
                  ~f:(List.iter ~f:(Hashtbl.remove defined_later))
            | BrilEffectInstruction e ->
                Option.iter e.args
                  ~f:(List.iter ~f:(Hashtbl.remove defined_later))
            | _ -> ());

            instr :: acc
          end)
    in
    let changed = List.length optimised_instrs <> List.length bb.instructions in
    let new_block = { bb with instructions = optimised_instrs } in
    Hashtbl.set cfg.nodes ~key:bb_id ~data:new_block;
    changed
  in

  let rec handle_block_iter ~key:bb_id =
    let bb = Hashtbl.find_exn cfg.nodes bb_id in
    let changed = handle_block ~key:bb_id ~data:bb in
    if changed then (handle_block_iter [@tailcall]) ~key:bb_id else ()
  in

  let bb_keys = Hashtbl.keys bbs in
  List.iter bb_keys ~f:(fun bb_id -> handle_block_iter ~key:bb_id)

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
            match bril_ir_instr_get_dest instr with
            | Some dst -> Hashtbl.mem defined_later dst
            | None -> false
          in
          if is_dead then acc
          else begin
            (* Mark written: if we keep it, it becomes the 'latest' definition in reverse order *)
            (match bril_ir_instr_get_dest instr with
            | Some dst -> Hashtbl.set defined_later ~key:dst ~data:()
            | None -> ());

            (* Mark used: using a variable means earlier definitions are NO LONGER dead *)
            List.iter
              (bril_ir_instr_get_args instr)
              ~f:(Hashtbl.remove defined_later);

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

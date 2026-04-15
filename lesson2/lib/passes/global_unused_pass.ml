open Basic_block
open Cfg
open Base
open Bril_instruction

let mark_used (cfg : CFG.t) =
  let used_vars = Hashtbl.create (module String) in
  Hashtbl.iter cfg.nodes ~f:(fun bb ->
      List.iter bb.instructions ~f:(fun instr ->
          List.iter (Instruction.get_args instr) ~f:(fun arg ->
              Hashtbl.set used_vars ~key:arg ~data:())));
  used_vars

let remove_unused (cfg : CFG.t) (used_vars : (string, unit) Hashtbl.t) =
  let block_ids = cfg.nodes |> Hashtbl.keys in
  List.fold_left block_ids ~init:false ~f:(fun changed bb_id ->
      let bb = Hashtbl.find_exn cfg.nodes bb_id in
      let new_bb =
        List.fold_left bb.instructions ~init:[] ~f:(fun acc instr ->
            let dst = Instruction.get_dest instr in
            match dst with
            | None -> instr :: acc
            | Some i -> (
                match Hashtbl.find used_vars i with
                | None -> acc
                | Some _ -> instr :: acc))
        |> List.rev
      in
      CFG.replace_node cfg ~id:bb_id ~new_bb:{ bb with instructions = new_bb };
      List.length new_bb <> List.length bb.instructions || changed)

let rec global_unused_pass (cfg : CFG.t) : unit =
  let used_vars = mark_used cfg in
  let changed = remove_unused cfg used_vars in
  if changed then global_unused_pass cfg else ()

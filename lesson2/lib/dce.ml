open Cfg
open Base
open Bril
open Basic_block

let dce_pass (cfg : CFG.t) : unit =
  let bbs = cfg.nodes in

  let handle_block ~key:bb_id ~data:(bb : basic_block) : bool =
    let last_def = Hashtbl.create (module String) in
    let for_removal = Hashtbl.create (module Id) in

    List.iteri bb.instructions ~f:(fun idx instr ->
        (* Mark used *)
        List.iter (get_args instr) ~f:(Hashtbl.remove last_def);

        (* Mark written *)
        match get_dest instr with
        | Some dst ->
            (match Hashtbl.find last_def dst with
            | Some prev_use -> Hashtbl.set for_removal ~key:prev_use ~data:()
            | None -> ());
            Hashtbl.set last_def ~key:dst ~data:idx
        | None -> ());

    let optimised_instrs =
      List.filteri bb.instructions ~f:(fun idx _ ->
          not (Hashtbl.mem for_removal idx))
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

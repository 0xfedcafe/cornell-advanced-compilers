open Cfg
open Base
open Basic_block
open Dominators
open Lattice
open Bril_instruction

module Ssa = struct
  type var_map_t = (String.t, Basic_block.id list) Hashtbl.t

  let build_var_map (cfg : CFG.t) : var_map_t =
    let var_map = Hashtbl.create (module String) in
    Hashtbl.iter cfg.nodes ~f:(fun bb ->
        let seen = Hashtbl.create (module String) in
        List.iter bb.instructions ~f:(fun instr ->
            Instruction.get_dest instr
            |> Option.iter ~f:(fun dest ->
                if not (Hashtbl.mem seen dest) then begin
                  Hashtbl.set seen ~key:dest ~data:();
                  let curr_blocks =
                    Hashtbl.find var_map dest |> Option.value ~default:[]
                  in
                  Hashtbl.set var_map ~key:dest ~data:(bb.id :: curr_blocks)
                end)));

    var_map

  let insert_phis (cfg : CFG.t)
      (dom_front : (Basic_block.id, Dominators.dom_set) Hashtbl.t)
      (var_map : var_map_t) : unit =
    Hashtbl.iteri var_map ~f:(fun ~key:var ~data:var_defs ->
        let worklist = Queue.create () in
        let added_phi = Hashtbl.create (module Basic_block.Id) in
        let added_set = Hashtbl.create (module Basic_block.Id) in

        List.iter var_defs ~f:(fun def_bb_id ->
            Queue.enqueue worklist def_bb_id;
            if not (Hashtbl.mem added_set def_bb_id) then begin
              Hashtbl.set added_set ~key:def_bb_id ~data:();
              let def_bb = Hashtbl.find_exn cfg.nodes def_bb_id in
              let set_instr = Instruction.SSA (Set { dst = var; src = var }) in
              let new_instrs =
                match List.rev def_bb.instructions with
                | (Instruction.Control (Jump _ | Branch _ | Return _) as term)
                  :: rest ->
                    List.rev (term :: set_instr :: rest)
                | _ -> def_bb.instructions @ [ set_instr ]
              in
              CFG.replace_node cfg ~id:def_bb_id
                ~new_bb:{ def_bb with instructions = new_instrs }
            end);

        let rec process_worklist () =
          match Queue.dequeue worklist with
          | None -> ()
          | Some d ->
              let frontier_set = Hashtbl.find_exn dom_front d in
              Set.iter frontier_set ~f:(fun block ->
                  if not (Hashtbl.mem added_phi block) then begin
                    Hashtbl.set added_phi ~key:block ~data:();
                    let phi =
                      Instruction.SSA
                        (Get { dst = var; typ = Bril_type.BrilType "int" })
                    in
                    let frontier_bb = Hashtbl.find_exn cfg.nodes block in
                    let instrs' =
                      match frontier_bb.instructions with
                      | (Instruction.Label _ as l) :: rest -> l :: phi :: rest
                      | rest -> phi :: rest
                    in
                    let bb' = { frontier_bb with instructions = instrs' } in
                    CFG.replace_node cfg ~id:block ~new_bb:bb';

                    if not (Hashtbl.mem added_set block) then begin
                      Hashtbl.set added_set ~key:block ~data:();
                      let updated_bb = Hashtbl.find_exn cfg.nodes block in
                      let set_instr =
                        Instruction.SSA (Set { dst = var; src = var })
                      in
                      let new_instrs_with_set =
                        match List.rev updated_bb.instructions with
                        | (Instruction.Control (Jump _ | Branch _ | Return _) as
                           term)
                          :: rest ->
                            List.rev (term :: set_instr :: rest)
                        | _ -> updated_bb.instructions @ [ set_instr ]
                      in
                      CFG.replace_node cfg ~id:block
                        ~new_bb:
                          { updated_bb with instructions = new_instrs_with_set }
                    end;
                    Queue.enqueue worklist block
                  end);
              process_worklist ()
        in
        process_worklist ());
    ()

  let rec rename (cfg : CFG.t) (dom_tree : Dominators.doms_state)
      (var_map : var_map_t) : unit =
    Hashtbl.iter var_map ~f:(fun var -> ());
    ()

  let convert_to_ssa (cfg : CFG.t) : CFG.t =
    let dom_tree = Dominators.compute_dominators cfg 0 in
    let dom_front = Dominators.dominance_frontier cfg 0 in
    let var_map = build_var_map cfg in
    insert_phis cfg dom_front var_map;
    rename cfg dom_tree var_map;
    cfg

  let convert_from_ssa (cfg : CFG.t) : CFG.t = failwith "todo"
end

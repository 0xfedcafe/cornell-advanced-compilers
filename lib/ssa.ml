open Cfg
open Base
open Basic_block
open Dominators
open Bril_instruction

module Ssa = struct
  type var_info = {
    typ : Bril_type.bril_type option;
    defs : Basic_block.id list;
  }

  type var_map_t = (String.t, var_info) Hashtbl.t

  let build_var_map (func : Bril.bril_ir_function) (cfg : CFG.t)
      (entry_id : Basic_block.id) : var_map_t =
    let var_map = Hashtbl.create (module String) in
    let record ~var ~typ ~block =
      let existing =
        Hashtbl.find var_map var
        |> Option.value ~default:{ typ = None; defs = [] }
      in
      let defs =
        if List.mem existing.defs block ~equal:Basic_block.equal_id then
          existing.defs
        else block :: existing.defs
      in
      Hashtbl.set var_map ~key:var
        ~data:{ typ = Option.first_some existing.typ typ; defs }
    in

    Option.iter func.args
      ~f:
        (List.iter ~f:(fun (arg : Bril.bril_arg) ->
             record ~var:arg.name ~typ:(Some arg.typ) ~block:entry_id));

    List.iter (Dominators.reverse_post_order cfg entry_id) ~f:(fun bb_id ->
        let bb = Hashtbl.find_exn cfg.nodes bb_id in
        List.iter bb.instructions ~f:(fun instr ->
            Instruction.get_dest instr
            |> Option.iter ~f:(fun dest ->
                record ~var:dest
                  ~typ:(Instruction.result_type instr)
                  ~block:bb.id)));

    var_map

  let insert_gets (cfg : CFG.t)
      (dom_front : (Basic_block.id, Dominators.dom_set) Hashtbl.t)
      (var_map : var_map_t) =
    let gets_per_block = Hashtbl.create (module Basic_block.Id) in

    Hashtbl.iteri var_map ~f:(fun ~key:var ~data:var_info ->
        let worklist = Queue.create () in
        List.iter var_info.defs ~f:(fun def_block ->
            Queue.enqueue worklist def_block);

        let rec iter () =
          match Queue.dequeue worklist with
          | None -> ()
          | Some block ->
              let dom_front_v = Hashtbl.find_exn dom_front block in
              Set.iter dom_front_v ~f:(fun df_block ->
                  let gets_in_block =
                    Hashtbl.find_or_add gets_per_block df_block
                      ~default:(fun () -> Set.empty (module String))
                  in

                  if not (Set.mem gets_in_block var) then begin
                    let bb = Hashtbl.find_exn cfg.nodes df_block in
                    let typ =
                      match var_info.typ with
                      | Some t -> t
                      | None ->
                          failwith
                            (Printf.sprintf "ssa: no type known for variable %s"
                               var)
                    in
                    let get_instr = Instruction.SSA (Get { dst = var; typ }) in

                    let new_instrs = get_instr :: bb.instructions in
                    let new_bb = { bb with instructions = new_instrs } in

                    Hashtbl.set cfg.nodes ~key:df_block ~data:new_bb;
                    Queue.enqueue worklist df_block;

                    let new_gets = Set.add gets_in_block var in
                    Hashtbl.set gets_per_block ~key:df_block ~data:new_gets
                  end);
              iter ()
        in
        iter ());

    gets_per_block

  let rename (func : Bril.bril_ir_function) (cfg : CFG.t)
      (entry_id : Basic_block.id) (var_map : var_map_t) gets_per_block : unit =
    let var_stacks = Hashtbl.create (module String) in

    let var_names = Hashtbl.create (module String) in

    let get_names = Hashtbl.create (module Basic_block.Id) in

    let undefs = Hashtbl.create (module String) in

    let pending = ref [] in

    let children = Dominators.dom_tree_children cfg entry_id in

    let fresh var =
      let n = Hashtbl.find var_names var |> Option.value ~default:0 in
      Hashtbl.set var_names ~key:var ~data:(n + 1);
      Printf.sprintf "%s.%d" var n
    in

    let push_def var =
      let name = fresh var in
      let stack = Hashtbl.find_or_add var_stacks var ~default:Stack.create in
      Stack.push stack name;
      name
    in

    let undef_name var =
      Hashtbl.find_or_add undefs var ~default:(fun () ->
          Printf.sprintf "%s.init" var)
    in

    Option.iter func.args ~f:(fun args' ->
        List.iter args' ~f:(fun arg ->
            let name = arg.name in
            Hashtbl.set var_stacks ~key:name ~data:(Stack.singleton name)));

    let rec rename_block block =
      let new_instructions =
        List.map block.instructions ~f:(fun instr ->
            let replaced_args =
              Instruction.replace_args instr (fun arg ->
                  match Hashtbl.find var_stacks arg with
                  | Some stack -> Stack.top_exn stack
                  | None -> arg)
            in
            match Instruction.get_dest replaced_args with
            | Some dst ->
                let new_name = push_def dst in
                (match instr with
                | Instruction.SSA (Get _) ->
                    let names =
                      Hashtbl.find_or_add get_names block.id ~default:(fun () ->
                          Hashtbl.create (module String))
                    in
                    Hashtbl.set names ~key:dst ~data:new_name
                | _ -> ());
                Instruction.replace_dst replaced_args new_name
            | None -> replaced_args)
      in

      let pushed = List.filter_map block.instructions ~f:Instruction.get_dest in

      CFG.replace_node cfg ~id:block.id
        ~new_bb:{ block with instructions = new_instructions };

      List.iter (Hashtbl.find_exn cfg.graph block.id).succs ~f:(fun succ_id ->
          Hashtbl.find gets_per_block succ_id
          |> Option.iter ~f:(fun vars ->
              Set.iter vars ~f:(fun var ->
                  let value =
                    match Hashtbl.find var_stacks var with
                    | Some stack when not (Stack.is_empty stack) ->
                        Stack.top_exn stack
                    | _ -> undef_name var
                  in
                  pending := (block.id, succ_id, var, value) :: !pending)));

      List.iter (Hashtbl.find_exn children block.id) ~f:(fun child_id ->
          rename_block (Hashtbl.find_exn cfg.nodes child_id));

      List.iter pushed ~f:(fun var ->
          ignore (Stack.pop (Hashtbl.find_exn var_stacks var)))
    in

    rename_block (Hashtbl.find_exn cfg.nodes entry_id);

    let append_before_terminator instrs extra =
      match List.rev instrs with
      | (Instruction.Control (Jump _ | Branch _ | Return _) as term) :: rest ->
          List.rev rest @ extra @ [ term ]
      | _ -> instrs @ extra
    in

    let sets_per_block = Hashtbl.create (module Basic_block.Id) in
    List.iter !pending ~f:(fun (block_id, succ_id, var, value) ->
        let shadow =
          Hashtbl.find_exn (Hashtbl.find_exn get_names succ_id) var
        in
        let set_instr = Instruction.SSA (Set { dst = shadow; src = value }) in
        let curr =
          Hashtbl.find sets_per_block block_id |> Option.value ~default:[]
        in
        Hashtbl.set sets_per_block ~key:block_id ~data:(set_instr :: curr));

    Hashtbl.iteri sets_per_block ~f:(fun ~key:block_id ~data:sets ->
        let bb = Hashtbl.find_exn cfg.nodes block_id in
        CFG.replace_node cfg ~id:block_id
          ~new_bb:
            {
              bb with
              instructions = append_before_terminator bb.instructions sets;
            });

    if not (Hashtbl.is_empty undefs) then begin
      let entry = Hashtbl.find_exn cfg.nodes entry_id in
      let undef_instrs =
        Hashtbl.fold undefs ~init:[] ~f:(fun ~key:var ~data:name acc ->
            let typ =
              match (Hashtbl.find_exn var_map var).typ with
              | Some t -> t
              | None ->
                  failwith
                    (Printf.sprintf "ssa: no type known for variable %s" var)
            in
            Instruction.SSA (Undef { dst = name; typ }) :: acc)
      in
      CFG.replace_node cfg ~id:entry_id
        ~new_bb:{ entry with instructions = undef_instrs @ entry.instructions }
    end

  let convert_to_ssa (func : Bril.bril_ir_function) (cfg : CFG.t)
      (entry_id : Basic_block.id) : CFG.t =
    let dom_front = Dominators.dominance_frontier cfg entry_id in
    let var_map = build_var_map func cfg entry_id in
    let gets_per_block = insert_gets cfg dom_front var_map in
    rename func cfg entry_id var_map gets_per_block;
    cfg

  let convert_from_ssa (cfg : CFG.t) : CFG.t =
    let shadow_name dst = Printf.sprintf "%s.shadow" dst in

    let types = Hashtbl.create (module String) in
    let gather_types () =
      Hashtbl.iter cfg.nodes ~f:(fun bb ->
          List.iter bb.instructions ~f:(fun instr ->
              match instr with
              | Instruction.SSA (Get { dst; typ }) ->
                  Hashtbl.set types ~key:dst ~data:typ
              | _ -> ()))
    in

    let replace_sets_and_gets () =
      List.iter (Hashtbl.keys cfg.nodes) ~f:(fun bb_id ->
          let bb = Hashtbl.find_exn cfg.nodes bb_id in
          let instructions =
            List.filter_map bb.instructions ~f:(fun instr ->
                match instr with
                | Instruction.SSA (Set { dst; src }) ->
                    Hashtbl.find types dst
                    |> Option.map ~f:(fun typ ->
                        Instruction.Unary
                          ( Op.Id,
                            {
                              dst = shadow_name dst;
                              typ = Some typ;
                              src1 = src;
                            } ))
                | Instruction.SSA (Get { dst; typ }) ->
                    Some
                      (Instruction.Unary
                         (Op.Id, { dst; typ = Some typ; src1 = shadow_name dst }))
                | _ -> Some instr)
          in
          CFG.replace_node cfg ~id:bb_id ~new_bb:{ bb with instructions })
    in

    gather_types ();
    replace_sets_and_gets ();
    cfg
end

open Lesson2
open Lesson2.Basic_block
open Bril_instruction

let process_bril_file filename =
  let json = Stdio.In_channel.read_all filename in
  let bril_program = Bril.parsed_bril_json json in
  let bril_ir_program = Bril.from_tokens bril_program in

  (* Extract bbs for each function once so IDs match later *)
  let funcs_with_bbs =
    Base.List.map bril_ir_program.functions ~f:(fun func ->
        (func, Lesson2.Basic_block.bbs_in_function func))
  in
  let all_bbs = Base.List.concat_map funcs_with_bbs ~f:snd in

  let cfg = Cfg.build_cfg all_bbs in

  (* Run the Global Unused Pass *)
  Lesson2.Global_unused_pass.global_unused_pass cfg;

  (* Run the DCE pass *)
  Lesson2.Dce.dce_pass cfg;

  (* Run the LVN pass *)
  Lesson2.Lvn.lvn_pass cfg;

  (* Run the DCE pass *)
  Lesson2.Dce.dce_pass cfg;

  (* Run the Global Unused Pass *)
  Lesson2.Global_unused_pass.global_unused_pass cfg;

  let res = Lesson2.Lattice.reaching_definitions_analysis cfg in
  print_endline "Reaching Definitions Analysis Result:";
  Base.Hashtbl.iteri res.in' ~f:(fun ~key:bb_id ~data:defs ->
      let in_str =
        match defs with
        | Lesson2.Lattice.Top -> "TOP (Universal)"
        | Lesson2.Lattice.Set s ->
            Base.Set.to_list s
            |> Base.List.map ~f:(fun instr -> Instruction.to_string instr)
            |> Base.String.concat ~sep:", "
      in
      let out_str =
        match Base.Hashtbl.find res.out' bb_id with
        | Some Lesson2.Lattice.Top -> "TOP (Universal)"
        | Some (Lesson2.Lattice.Set s) ->
            Base.Set.to_list s
            |> Base.List.map ~f:(fun instr -> Instruction.to_string instr)
            |> Base.String.concat ~sep:", "
        | None -> "NONE"
      in
      Printf.printf "BB%d IN: %s\nBB%d OUT: %s\n" bb_id in_str bb_id out_str);

  let live_vars = Lesson2.Lattice.live_vars_analysis cfg in
  print_endline "\nLive Variable Analysis Result:";
  Base.Hashtbl.iteri live_vars.in' ~f:(fun ~key:bb_id ~data:vars ->
      let in_str =
        match vars with
        | Lesson2.Lattice.Top -> "TOP (Universal)"
        | Lesson2.Lattice.Set s ->
            Base.Set.to_list s |> Base.String.concat ~sep:", "
      in
      let out_str =
        match Base.Hashtbl.find live_vars.out' bb_id with
        | Some Lesson2.Lattice.Top -> "TOP (Universal)"
        | Some (Lesson2.Lattice.Set s) ->
            Base.Set.to_list s |> Base.String.concat ~sep:", "
        | None -> "NONE"
      in
      Printf.printf "BB%d IN: %s\nBB%d OUT: %s\n" bb_id out_str bb_id in_str);

  let const_prop = Lesson2.Lattice.constant_propagation_analysis cfg in
  print_endline "\nConstant Propagation Analysis Result:";
  Base.Hashtbl.iteri const_prop.in' ~f:(fun ~key:bb_id ~data:consts ->
      let in_str =
        match consts with
        | Lesson2.Lattice.Top -> "TOP (Universal)"
        | Lesson2.Lattice.Set s ->
            Base.Set.to_list s
            |> Base.List.map ~f:(fun instr -> Instruction.to_string instr)
            |> Base.String.concat ~sep:", "
      in
      let out_str =
        match Base.Hashtbl.find const_prop.out' bb_id with
        | Some Lesson2.Lattice.Top -> "TOP (Universal)"
        | Some (Lesson2.Lattice.Set s) ->
            Base.Set.to_list s
            |> Base.List.map ~f:(fun instr -> Instruction.to_string instr)
            |> Base.String.concat ~sep:", "
        | None -> "NONE"
      in
      Printf.printf "BB%d IN: %s\nBB%d OUT: %s\n" bb_id in_str bb_id out_str);

  (* Reconstruct functions from CFG *)
  let optimized_functions =
    Base.List.map funcs_with_bbs ~f:(fun (func, bbs) ->
        (* Get the potentially modified blocks from the CFG nodes *)
        let optimized_instrs =
          Base.List.concat_map bbs ~f:(fun bb ->
              match Base.Hashtbl.find cfg.nodes bb.id with
              | Some optimized_bb ->
                  let label_instr =
                    match optimized_bb.label with
                    | Some l
                      when not
                             (Base.String.is_prefix ~prefix:func.name l
                             && Base.String.is_substring ~substring:"_b" l) ->
                        [ Bril.Instruction.Label { label = l } ]
                    | Some _ | None -> []
                  in
                  label_instr @ optimized_bb.instructions
              | None -> bb.instructions)
        in
        { func with instrs = optimized_instrs })
  in
  let optimized_program = { Bril.functions = optimized_functions } in
  print_endline (Bril.bril_ir_program_to_string optimized_program)

let () =
  print_string "Enter JSON filename: ";
  Stdio.Out_channel.flush Stdio.Out_channel.stdout;
  match Stdio.In_channel.input_line Stdio.In_channel.stdin with
  | Some filename ->
      let filename = String.trim filename in
      process_bril_file filename
  | None -> ()

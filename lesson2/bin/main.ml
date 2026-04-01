open Lesson2
open Lesson2.Basic_block

let process_bril_file filename =
  let json = Stdio.In_channel.read_all filename in
  let bril_program = Bril.parsed_bril_json json in

  (* Extract bbs for each function once so IDs match later *)
  let funcs_with_bbs =
    Base.List.map bril_program.functions ~f:(fun func ->
        (func, Lesson2.Basic_block.bbs_in_function func))
  in
  let all_bbs = Base.List.concat_map funcs_with_bbs ~f:snd in

  let cfg = Cfg.build_cfg all_bbs in

  (* Run the DCE pass *)
  Lesson2.Dce.dce_Pass cfg;

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
                        [ Bril.BrilLabel { label = l } ]
                    | Some _ | None -> []
                  in
                  label_instr @ optimized_bb.instructions
              | None -> bb.instructions)
        in
        { func with instrs = optimized_instrs })
  in
  let optimized_program = { Bril.functions = optimized_functions } in
  print_endline
    (Yojson.Safe.pretty_to_string
       (Bril.bril_program_to_yojson optimized_program));
  print_endline (Bril.bril_program_to_string optimized_program)

let () =
  print_string "Enter JSON filename: ";
  Stdio.Out_channel.flush Stdio.Out_channel.stdout;
  match Stdio.In_channel.input_line Stdio.In_channel.stdin with
  | Some filename ->
      let filename = String.trim filename in
      process_bril_file filename
  | None -> ()

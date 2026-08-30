open Base
open Cornell_advanced_compilers_cs6120

type mode = Opt | To_ssa | From_ssa | Analyze

let optimize cfg =
  Global_unused_pass.global_unused_pass cfg;
  Dce.dce_pass cfg;
  Lvn.lvn_pass cfg;
  Dce.dce_pass cfg;
  Global_unused_pass.global_unused_pass cfg

let print_analysis label ~in' ~out' ~render =
  let show = function
    | Lattice.Top -> "TOP (Universal)"
    | Lattice.Set s -> render s
  in
  Stdio.printf "\n%s:\n" label;
  Hashtbl.iteri in' ~f:(fun ~key:bb_id ~data ->
      let out =
        match Hashtbl.find out' bb_id with Some v -> show v | None -> "NONE"
      in
      Stdio.printf "BB%d IN: %s\nBB%d OUT: %s\n" bb_id (show data) bb_id out)

let instrs_of_set s =
  Set.to_list s
  |> List.map ~f:Bril.Instruction.to_string
  |> String.concat ~sep:", "

let analyze cfg entry_id =
  let rd = Reaching_definitions.reaching_definitions_analysis cfg entry_id in
  print_analysis "Reaching Definitions Analysis Result" ~in':rd.in' ~out':rd.out'
    ~render:instrs_of_set;
  let lv = Live_vars.live_vars_analysis cfg entry_id in
  print_analysis "Live Variable Analysis Result" ~in':lv.in' ~out':lv.out'
    ~render:(fun s -> String.concat ~sep:", " (Set.to_list s));
  let cp = Constant_propagation.constant_propagation_analysis cfg entry_id in
  print_analysis "Constant Propagation Analysis Result" ~in':cp.in' ~out':cp.out'
    ~render:instrs_of_set

let run mode (program : Bril.bril_ir_program) =
  let funcs_with_bbs =
    List.map program.functions ~f:(fun func ->
        (func, Basic_block.bbs_in_function func))
  in
  let cfg = Cfg.build_cfg (List.concat_map funcs_with_bbs ~f:snd) in
  let entry_of bbs = Option.map (List.hd bbs) ~f:(fun bb -> bb.Basic_block.id) in
  (match mode with
  | Opt -> optimize cfg
  | From_ssa -> ignore (Ssa.Ssa.convert_from_ssa cfg)
  | To_ssa ->
      List.iter funcs_with_bbs ~f:(fun (func, bbs) ->
          Option.iter (entry_of bbs) ~f:(fun entry ->
              ignore (Ssa.Ssa.convert_to_ssa func cfg entry)))
  | Analyze ->
      List.iter funcs_with_bbs ~f:(fun (_, bbs) ->
          Option.iter (entry_of bbs) ~f:(analyze cfg)));
  {
    Bril.functions =
      List.map funcs_with_bbs ~f:(fun (func, bbs) ->
          let blocks =
            List.map bbs ~f:(fun bb ->
                Hashtbl.find_exn cfg.Cfg.CFG.nodes bb.Basic_block.id)
          in
          {
            func with
            Bril.instrs =
              Basic_block.instrs_of_blocks ~func_name:func.Bril.name blocks;
          });
  }

let usage () =
  Stdio.eprintf
    "usage: %s [opt|to-ssa|from-ssa|analyze]\n\
     Reads Bril JSON on stdin, writes Bril JSON on stdout.\n"
    (Sys.get_argv ()).(0);
  Stdlib.exit 1

let () =
  let mode =
    match Sys.get_argv () with
    | [| _ |] | [| _; "opt" |] -> Opt
    | [| _; "to-ssa" |] -> To_ssa
    | [| _; "from-ssa" |] -> From_ssa
    | [| _; "analyze" |] -> Analyze
    | _ -> usage ()
  in
  Stdio.In_channel.input_all Stdio.In_channel.stdin
  |> Bril.parsed_bril_json |> Bril.from_tokens |> run mode
  |> Bril.bril_ir_program_to_json |> Yojson.Safe.to_string
  |> Stdio.print_endline

open Base
open Cornell_advanced_compilers_cs6120

let format_bb_name bb =
  match bb.Basic_block.label with
  | Some l -> l
  | None -> "b" ^ Int.to_string bb.id

let process_file json_file =
  let json = Stdio.In_channel.read_all json_file in
  let bril_program = Bril.parsed_bril_json json in
  let bril_ir_program = Bril.from_tokens bril_program in
  let funcs = bril_ir_program.functions in
  Base.List.iter funcs ~f:(fun func ->
      let bbs = Basic_block.bbs_in_function func in
      let cfg = Cfg.build_cfg bbs in
      match bbs with
      | [] -> ()
      | entry_bb :: _ ->
          Stdio.printf "Testing dominance frontiers for %s function %s:\n"
            json_file func.name;

          let df = Dominators.Dominators.dominance_frontier cfg entry_bb.id in

          let bb_names =
            List.map bbs ~f:(fun bb -> (bb.id, format_bb_name bb))
          in
          let sorted_bbs =
            List.sort bb_names ~compare:(fun (_, n1) (_, n2) ->
                String.compare n1 n2)
          in

          List.iter sorted_bbs ~f:(fun (id, name) ->
              match Hashtbl.find df id with
              | None -> Stdio.printf "  %s: []\n" name
              | Some frontiers ->
                  let frontier_names =
                    Set.elements frontiers
                    |> List.map ~f:(fun f_id ->
                        let f_bb =
                          List.find_exn bbs ~f:(fun b -> b.id = f_id)
                        in
                        format_bb_name f_bb)
                    |> List.sort ~compare:String.compare
                  in
                  Stdio.printf "  %s: [%s]\n" name
                    (String.concat ~sep:", " frontier_names));
          Stdio.printf "\n")

let () =
  let file1 =
    if Stdlib.Sys.file_exists "tests_source/dom/loopcond.json" then
      "tests_source/dom/loopcond.json"
    else "../../../tests_source/dom/loopcond.json"
  in
  let file2 =
    if Stdlib.Sys.file_exists "tests_source/dom/while.json" then
      "tests_source/dom/while.json"
    else "../../../tests_source/dom/while.json"
  in
  process_file file1;
  process_file file2

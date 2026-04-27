open Base
open Cornell_advanced_compilers_cs6120

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
          let rpo = Dominators.Dominators.reverse_post_order cfg entry_bb.id in
          Stdio.printf "RPO for %s function %s:\n" json_file func.name;
          Base.List.iter rpo ~f:(fun id ->
              let bb = Base.List.find_exn bbs ~f:(fun b -> b.id = id) in
              let label =
                match bb.label with Some l -> l | None -> "<no-label>"
              in
              Stdio.printf "BB%d(%s) " id label);
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

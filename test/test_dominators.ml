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
          Stdio.printf "Testing dominators for %s function %s:\n" json_file
            func.name;

          (* Compute dominators *)
          let doms_state =
            Dominators.Dominators.compute_dominators cfg entry_bb.id
          in
          Stdio.printf "Dominators:\n";
          let bb_names =
            List.map bbs ~f:(fun bb -> (bb.id, format_bb_name bb))
          in
          let sorted_bbs =
            List.sort bb_names ~compare:(fun (_, n1) (_, n2) ->
                String.compare n1 n2)
          in

          List.iter sorted_bbs ~f:(fun (id_b, name_b) ->
              let dom_names =
                List.filter_map bbs ~f:(fun bb_a ->
                    if Dominators.Dominators.dominates doms_state bb_a.id id_b
                    then Some (format_bb_name bb_a)
                    else None)
              in
              let sorted_dom_names =
                List.sort dom_names ~compare:String.compare
              in
              Stdio.printf "  %s: [%s]\n" name_b
                (String.concat ~sep:", " sorted_dom_names));

          (* Compute idoms / dominator tree *)
          Stdio.printf "Dominator Tree:\n";
          let idoms = Dominators.Dominators.compute_idoms cfg entry_bb.id in

          (* Invert idoms to get children *)
          let tree = Hashtbl.create (module String) in
          List.iter bbs ~f:(fun bb ->
              Hashtbl.set tree ~key:(format_bb_name bb) ~data:[]);

          Hashtbl.iteri idoms ~f:(fun ~key:bb_id ~data:idom_opt ->
              match idom_opt with
              | Some idom_id ->
                  let bb_name =
                    format_bb_name
                      (List.find_exn bbs ~f:(fun b -> b.id = bb_id))
                  in
                  let idom_name =
                    format_bb_name
                      (List.find_exn bbs ~f:(fun b -> b.id = idom_id))
                  in
                  let current_children = Hashtbl.find_exn tree idom_name in
                  Hashtbl.set tree ~key:idom_name
                    ~data:(bb_name :: current_children)
              | None -> ());

          List.iter sorted_bbs ~f:(fun (_, name) ->
              let children = Hashtbl.find_exn tree name in
              let sorted_children =
                List.sort children ~compare:String.compare
              in
              Stdio.printf "  %s: [%s]\n" name
                (String.concat ~sep:", " sorted_children));

          (* Test dominates function *)
          Stdio.printf "Dominates checks (A dominates B?):\n";
          List.iter sorted_bbs ~f:(fun (id_a, name_a) ->
              List.iter sorted_bbs ~f:(fun (id_b, name_b) ->
                  let is_dom =
                    Dominators.Dominators.dominates doms_state id_a id_b
                  in
                  (* Only print if true to keep output clean *)
                  if is_dom then
                    Stdio.printf "  %s dominates %s\n" name_a name_b));
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

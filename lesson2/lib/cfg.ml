open Base
open Bril
open Basic_block

module CFG = struct
  type edges = {
    mutable preds : basic_block list;
    mutable succs : basic_block list;
  }

  type t = {
    label_to_bb : (string, basic_block) Hashtbl.t;
    cfg : (basic_block, edges) Hashtbl.t;
  }

  let create () =
    {
      label_to_bb =
        Hashtbl.create
          (module struct
            type t = string

            let compare = String.compare
            let hash = Hashtbl.hash
            let sexp_of_t _ = Sexp.Atom "string"
          end);
      cfg =
        Hashtbl.create
          (module struct
            type t = basic_block

            let compare = Poly.compare
            let hash = Hashtbl.hash
            let sexp_of_t _ = Sexp.Atom "basic_block"
          end);
    }

  let add_edge t ~src ~dst =
    let src_edges = Hashtbl.find_exn t.cfg src in
    let dst_edges = Hashtbl.find_exn t.cfg dst in
    src_edges.succs <- dst :: src_edges.succs;
    dst_edges.preds <- src :: dst_edges.preds

  let to_dot t =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "digraph CFG {\n";
    Hashtbl.iteri t.cfg ~f:(fun ~key:bb ~data:edges ->
        let label_str =
          match bb.label with
          | Some l -> Printf.sprintf ".%s" l
          | None -> "none"
        in
        Buffer.add_string buf (Printf.sprintf "  \"%s\";\n" label_str);
        List.iter edges.succs ~f:(fun succ ->
            let succ_label_str =
              match succ.label with
              | Some l -> Printf.sprintf ".%s" l
              | None -> "none"
            in
            Buffer.add_string buf
              (Printf.sprintf "  \"%s\" -> \"%s\";\n" label_str succ_label_str)));
    Buffer.add_string buf "}\n";
    Buffer.contents buf
end

let build_cfg program =
  let bbs = Basic_block.gather_basic_blocks program in
  let cfg = CFG.create () in

  (* First, populate the nodes in the graph and the label_to_bb mapping *)
  List.iter bbs ~f:(fun bb ->
      Hashtbl.set cfg.cfg ~key:bb ~data:{ CFG.preds = []; succs = [] };
      match bb.label with
      | Some l -> Hashtbl.set cfg.label_to_bb ~key:l ~data:bb
      | None -> ());

  let handle_instr_edge bb instr =
    match instr with
    | BrilEffectInstruction e -> (
        match e.labels with
        | None -> false
        | Some ls -> (
            match e.op with
            | "jmp" -> (
                match List.hd ls with
                | Some v ->
                    Hashtbl.find cfg.label_to_bb v
                    |> Option.iter ~f:(fun dst -> CFG.add_edge cfg ~src:bb ~dst);
                    false
                | None -> failwith "wrong jmp instruction")
            | "br" -> (
                match ls with
                | br1 :: br2 :: _ ->
                    Hashtbl.find cfg.label_to_bb br1
                    |> Option.iter ~f:(fun dst -> CFG.add_edge cfg ~src:bb ~dst);
                    Hashtbl.find cfg.label_to_bb br2
                    |> Option.iter ~f:(fun dst -> CFG.add_edge cfg ~src:bb ~dst);
                    false
                | _ -> failwith "wrong br instruction")
            | "ret" -> false
            | _ -> true))
    | _ -> true
  in

  let rec add_fallthrough_edges = function
    | [] -> ()
    | bb :: rest ->
        let needs_fallthrough =
          match bb.instructions with
          | [] -> true
          | instrs -> handle_instr_edge bb (List.last_exn instrs)
        in
        (match rest with
        | next_bb :: _ when needs_fallthrough ->
            CFG.add_edge cfg ~src:bb ~dst:next_bb
        | _ -> ());
        add_fallthrough_edges rest
  in

  add_fallthrough_edges bbs;

  cfg

let to_dot = CFG.to_dot

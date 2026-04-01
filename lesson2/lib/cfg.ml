open Base
open Bril
open Basic_block

module CFG = struct
  type edges = {
    mutable preds : Basic_block.id list;
    mutable succs : Basic_block.id list;
  }

  type t = {
    nodes : (Basic_block.id, basic_block) Hashtbl.t;
    graph : (Basic_block.id, edges) Hashtbl.t;
  }

  let create () =
    {
      nodes = Hashtbl.create (module Basic_block.Id);
      graph = Hashtbl.create (module Basic_block.Id);
    }

  let add_edge t ~src ~dst =
    let src_edges = Hashtbl.find_exn t.graph src in
    let dst_edges = Hashtbl.find_exn t.graph dst in
    src_edges.succs <- dst :: src_edges.succs;
    dst_edges.preds <- src :: dst_edges.preds

  let to_dot t =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "digraph CFG {\n";
    Hashtbl.iteri t.graph ~f:(fun ~key:id ~data:edges ->
        let bb = Hashtbl.find_exn t.nodes id in
        let label_str =
          match bb.label with
          | Some l -> Printf.sprintf ".%s" l
          | None -> "none"
        in
        Buffer.add_string buf (Printf.sprintf "  \"%s\";\n" label_str);
        List.iter edges.succs ~f:(fun succ_id ->
            let succ_bb = Hashtbl.find_exn t.nodes succ_id in
            let succ_label_str =
              match succ_bb.label with
              | Some l -> Printf.sprintf ".%s" l
              | None -> "none"
            in
            Buffer.add_string buf
              (Printf.sprintf "  \"%s\" -> \"%s\";\n" label_str succ_label_str)));
    Buffer.add_string buf "}\n";
    Buffer.contents buf
end

let build_cfg bbs =
  let cfg = CFG.create () in
  let label_to_id = Hashtbl.create (module String) in

  (* First, populate the nodes in the graph and the label_to_id mapping *)
  List.iter bbs ~f:(fun bb ->
      Hashtbl.set cfg.nodes ~key:bb.id ~data:bb;
      Hashtbl.set cfg.graph ~key:bb.id ~data:{ CFG.preds = []; succs = [] };
      match bb.label with
      | Some l -> Hashtbl.set label_to_id ~key:l ~data:bb.id
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
                    Hashtbl.find label_to_id v
                    |> Option.iter ~f:(fun dst_id ->
                        CFG.add_edge cfg ~src:bb.id ~dst:dst_id);
                    false
                | None -> failwith "wrong jmp instruction")
            | "br" -> (
                match ls with
                | br1 :: br2 :: _ ->
                    Hashtbl.find label_to_id br1
                    |> Option.iter ~f:(fun dst_id ->
                        CFG.add_edge cfg ~src:bb.id ~dst:dst_id);
                    Hashtbl.find label_to_id br2
                    |> Option.iter ~f:(fun dst_id ->
                        CFG.add_edge cfg ~src:bb.id ~dst:dst_id);
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
            CFG.add_edge cfg ~src:bb.id ~dst:next_bb.id
        | _ -> ());
        add_fallthrough_edges rest
  in

  add_fallthrough_edges bbs;

  cfg

let to_dot = CFG.to_dot

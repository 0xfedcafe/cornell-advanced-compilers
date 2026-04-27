open Base

type basic_block = { label : string option; instructions : int list }
[@@deriving show]

let () =
  let func_name = "main" in
  let raw_bbs =
    [
      { label = Some "end"; instructions = [ 3 ] };
      { label = None; instructions = [ 2 ] };
      { label = None; instructions = [ 1 ] };
    ]
  in
  let _, named_bbs =
    List.fold_left raw_bbs ~init:(1, []) ~f:(fun (idx, acc) bb ->
        match bb.label with
        | Some _ -> (idx, bb :: acc)
        | None ->
            ( idx + 1,
              { bb with label = Some (func_name ^ "_b" ^ Int.to_string idx) }
              :: acc ))
  in
  let forward_bbs = List.rev named_bbs in
  List.iter forward_bbs ~f:(fun b ->
      Stdio.printf "label: %s\n" (Option.value b.label ~default:"None"))

open Base
open Bril

type id = int [@@deriving compare, equal, hash, sexp]

module Id = struct
  type t = id [@@deriving compare, equal, hash, sexp]

  let compare = compare_id
  let equal = equal_id
  let hash = hash_id
  let sexp_of_t = sexp_of_id
  let t_of_sexp = id_of_sexp
end

type t = {
  id : id;
  label : string option;
  instructions : bril_ir_instruction list;
}
[@@deriving compare, hash, sexp]

let next_id =
  let count = ref 0 in
  fun () ->
    let id = !count in
    count := !count + 1;
    id

let bbs_in_function func =
  let rec build_bbs instrs curr_lbl curr_instrs bbs =
    match instrs with
    | [] ->
        if Option.is_some curr_lbl || not (List.is_empty curr_instrs) then
          {
            id = next_id ();
            label = curr_lbl;
            instructions = List.rev curr_instrs;
          }
          :: bbs
        else bbs
    | Label l :: rest ->
        let bbs' =
          if Option.is_some curr_lbl || not (List.is_empty curr_instrs) then
            {
              id = next_id ();
              label = curr_lbl;
              instructions = List.rev curr_instrs;
            }
            :: bbs
          else bbs
        in
        build_bbs rest (Some l.label) [] bbs'
    | instr :: rest ->
        let curr_instrs' = instr :: curr_instrs in
        let is_terminator =
          match instr with
          | Control (Jump _ | Branch _ | Return _) -> true
          | _ -> false
        in
        if is_terminator then
          let bbs' =
            {
              id = next_id ();
              label = curr_lbl;
              instructions = List.rev curr_instrs';
            }
            :: bbs
          in
          build_bbs rest None [] bbs'
        else build_bbs rest curr_lbl curr_instrs' bbs
  in
  let raw_bbs = build_bbs func.instrs None [] [] |> List.rev in
  let _, named_bbs =
    List.fold_left raw_bbs ~init:(1, []) ~f:(fun (idx, acc) bb ->
        match bb.label with
        | Some _ -> (idx, bb :: acc)
        | None ->
            ( idx + 1,
              { bb with label = Some (func.name ^ "_b" ^ Int.to_string idx) }
              :: acc ))
  in
  List.rev named_bbs

let gather_basic_blocks program =
  List.concat_map program.functions ~f:bbs_in_function

type basic_block = t [@@deriving compare, hash, sexp]

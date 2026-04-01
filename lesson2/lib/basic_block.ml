open Base
open Bril

type basic_block = {
  label : string option;
  instructions : bril_instruction list;
}
[@@deriving compare, hash, sexp]

module Basic_block = struct
  type t = basic_block [@@deriving compare, hash, sexp]
  type id = int [@@deriving compare, equal, hash]

  (* ============ Lesson 2 ============ *)

  let bbs_in_function func =
    let rec build_bbs instrs curr_lbl curr_instrs bbs =
      match instrs with
      | [] ->
          if Option.is_some curr_lbl || not (List.is_empty curr_instrs) then
            { label = curr_lbl; instructions = List.rev curr_instrs } :: bbs
          else bbs
      | BrilLabel l :: rest ->
          let bbs' =
            if Option.is_some curr_lbl || not (List.is_empty curr_instrs) then
              { label = curr_lbl; instructions = List.rev curr_instrs } :: bbs
            else bbs
          in
          build_bbs rest (Some l.label) [] bbs'
      | instr :: rest ->
          let curr_instrs' = instr :: curr_instrs in
          let is_terminator =
            match instr with
            | BrilEffectInstruction { op; _ } ->
                List.exists [ "jmp"; "br"; "ret" ] ~f:(String.equal op)
            | _ -> false
          in
          if is_terminator then
            let bbs' =
              { label = curr_lbl; instructions = List.rev curr_instrs' } :: bbs
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
    named_bbs

  let gather_basic_blocks program =
    let rec bbs_iter funcs bbs =
      match funcs with
      | f :: fs -> bbs_iter fs (List.append (bbs_in_function f) bbs)
      | [] -> bbs
    in
    List.rev (bbs_iter program.functions [])

  (* ============ End of Lesson 2  ============ *)
end

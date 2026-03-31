open Base
open Lesson2.Bril

type basic_block = {
  label : string option;
  instructions : bril_instruction list;
}

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
              List.exists ~f:(String.equal op) ["jmp"; "br"; "ret"]
          | _ -> false
        in
        if is_terminator then
          let bbs' =
            { label = curr_lbl; instructions = List.rev curr_instrs' } :: bbs
          in
          build_bbs rest None [] bbs'
        else
          build_bbs rest curr_lbl curr_instrs' bbs
  in
  build_bbs func.instrs None [] []

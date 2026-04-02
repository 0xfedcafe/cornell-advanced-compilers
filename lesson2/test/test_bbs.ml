open Base
open Lesson2.Bril

let () =
  let func = {
    name = "main";
    args = None;
    typ = None;
    instrs = [
      Misc Nop;
      Misc Nop;
    ]
  } in
  let bbs = Lesson2.Basic_block.bbs_in_function func in
  List.iter bbs ~f:(fun bb ->
    Stdio.printf "Block label: %s\n" (Option.value ~default:"None" bb.label);
    List.iter bb.instructions ~f:(fun instr ->
      match instr with
      | Misc Nop -> Stdio.printf "  Instr: nop\n"
      | _ -> ()
    )
  )
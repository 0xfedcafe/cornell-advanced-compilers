open Base
open Lesson2.Basic_block
open Lesson2.Bril

let () =
  let func = {
    name = "main";
    args = None;
    typ = None;
    instrs = [
      BrilEffectInstruction { op = "nop"; args = None; funcs = None; labels = None };
      BrilEffectInstruction { op = "nop2"; args = None; funcs = None; labels = None };
    ]
  } in
  let bbs = bbs_in_function func in
  List.iter bbs ~f:(fun bb ->
    Stdio.printf "Block label: %s\n" (Option.value ~default:"None" bb.label);
    List.iter bb.instructions ~f:(fun instr ->
      match instr with
      | BrilEffectInstruction e -> Stdio.printf "  Instr: %s\n" e.op
      | _ -> ()
    )
  )

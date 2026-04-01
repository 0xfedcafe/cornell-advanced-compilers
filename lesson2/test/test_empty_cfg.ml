open Base
open Lesson2
open Lesson2.Basic_block

let bril_single_unnamed_block =
  {|
  {
  "functions": [
    {
      "instrs": [
        {
          "dest": "v0",
          "op": "const",
          "type": "int",
          "value": 1
        }
      ],
      "name": "main"
    }
  ]
}
|}

let () =
  let program = Bril.parsed_bril_json bril_single_unnamed_block in
  let cfg = Cfg.build_cfg program in
  let dot = Cfg.to_dot cfg in
  Stdio.printf "%s\n" dot

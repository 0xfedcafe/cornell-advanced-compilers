open Lesson2

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
  let program_ir = Bril.from_tokens program in
  let bbs = Lesson2.Basic_block.gather_basic_blocks program_ir in
  let cfg = Cfg.build_cfg bbs in
  let dot = Cfg.to_dot cfg in
  Stdio.printf "%s\n" dot

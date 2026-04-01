open Lesson2
open Lesson2.Basic_block

let bril_add_json =
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
        },
        {
          "dest": "v1",
          "op": "const",
          "type": "int",
          "value": 2
        },
        {
          "args": [
            "v0",
            "v1"
          ],
          "dest": "v2",
          "op": "add",
          "type": "int"
        },
        {
          "args": [
            "v2"
          ],
          "op": "print"
        }
      ],
      "name": "main"
    }
  ]
}
|}

let bril_simple_branch =
  {|
  {
  "functions": [
    {
      "instrs": [
        {
          "dest": "v",
          "op": "const",
          "type": "int",
          "value": 4
        },
        {
          "dest": "b",
          "op": "const",
          "type": "bool",
          "value": false
        },
        {
          "args": [
            "b"
          ],
          "labels": [
            "there",
            "here"
          ],
          "op": "br"
        },
        {
          "label": "here"
        },
        {
          "dest": "v",
          "op": "const",
          "type": "int",
          "value": 2
        },
        {
          "label": "there"
        },
        {
          "args": [
            "v"
          ],
          "op": "print"
        }
      ],
      "name": "main"
    }
  ]
}
|}

let bril_non_linear_cf =
  {|
  {
  "functions": [
    {
      "args": [
        {
          "name": "b0",
          "type": "bool"
        },
        {
          "name": "b1",
          "type": "bool"
        }
      ],
      "instrs": [
        {
          "labels": [
            "start"
          ],
          "op": "jmp"
        },
        {
          "label": "end"
        },
        {
          "args": [
            "x_0_2"
          ],
          "op": "print"
        },
        {
          "args": [
            "x_1_2"
          ],
          "op": "print"
        },
        {
          "op": "ret"
        },
        {
          "label": "l_1_3"
        },
        {
          "labels": [
            "end"
          ],
          "op": "jmp"
        },
        {
          "label": "l_1_2"
        },
        {
          "dest": "x_1_2",
          "op": "const",
          "type": "int",
          "value": 0
        },
        {
          "labels": [
            "l_1_3"
          ],
          "op": "jmp"
        },
        {
          "label": "l_1_1"
        },
        {
          "dest": "x_1_1",
          "op": "const",
          "type": "int",
          "value": 1
        },
        {
          "labels": [
            "l_1_3"
          ],
          "op": "jmp"
        },
        {
          "label": "l_0_3"
        },
        {
          "args": [
            "b1"
          ],
          "labels": [
            "l_1_1",
            "l_1_2"
          ],
          "op": "br"
        },
        {
          "label": "l_0_2"
        },
        {
          "dest": "x_0_2",
          "op": "const",
          "type": "int",
          "value": 2
        },
        {
          "labels": [
            "l_0_3"
          ],
          "op": "jmp"
        },
        {
          "label": "l_0_1"
        },
        {
          "dest": "x_0_1",
          "op": "const",
          "type": "int",
          "value": 3
        },
        {
          "labels": [
            "l_0_3"
          ],
          "op": "jmp"
        },
        {
          "label": "start"
        },
        {
          "args": [
            "b0"
          ],
          "labels": [
            "l_0_1",
            "l_0_2"
          ],
          "op": "br"
        }
      ],
      "name": "main"
    }
  ]
}
  |}

let bril_non_linear_cf_label_first =
  {|
  {
  "functions": [
    {
      "args": [
        {
          "name": "b0",
          "type": "bool"
        },
        {
          "name": "b1",
          "type": "bool"
        }
      ],
      "instrs": [
        {
          "label": "start_test"
        },
        {
          "labels": [
            "start"
          ],
          "op": "jmp"
        },
        {
          "label": "end"
        },
        {
          "args": [
            "x_0_2"
          ],
          "op": "print"
        },
        {
          "args": [
            "x_1_2"
          ],
          "op": "print"
        },
        {
          "op": "ret"
        },
        {
          "label": "l_1_3"
        },
        {
          "labels": [
            "end"
          ],
          "op": "jmp"
        },
        {
          "label": "l_1_2"
        },
        {
          "dest": "x_1_2",
          "op": "const",
          "type": "int",
          "value": 0
        },
        {
          "labels": [
            "l_1_3"
          ],
          "op": "jmp"
        },
        {
          "label": "l_1_1"
        },
        {
          "dest": "x_1_1",
          "op": "const",
          "type": "int",
          "value": 1
        },
        {
          "labels": [
            "l_1_3"
          ],
          "op": "jmp"
        },
        {
          "label": "l_0_3"
        },
        {
          "args": [
            "b1"
          ],
          "labels": [
            "l_1_1",
            "l_1_2"
          ],
          "op": "br"
        },
        {
          "label": "l_0_2"
        },
        {
          "dest": "x_0_2",
          "op": "const",
          "type": "int",
          "value": 2
        },
        {
          "labels": [
            "l_0_3"
          ],
          "op": "jmp"
        },
        {
          "label": "l_0_1"
        },
        {
          "dest": "x_0_1",
          "op": "const",
          "type": "int",
          "value": 3
        },
        {
          "labels": [
            "l_0_3"
          ],
          "op": "jmp"
        },
        {
          "label": "start"
        },
        {
          "args": [
            "b0"
          ],
          "labels": [
            "l_0_1",
            "l_0_2"
          ],
          "op": "br"
        }
      ],
      "name": "main"
    }
  ]
}
  |}

let bril_non_linear_cf_label_first_dupl =
  {|
  {
  "functions": [
    {
      "args": [
        {
          "name": "b0",
          "type": "bool"
        },
        {
          "name": "b1",
          "type": "bool"
        }
      ],
      "instrs": [
        {
          "label": "start_test"
        },
        {
          "label": "start_test2"
        },
        {
          "labels": [
            "start"
          ],
          "op": "jmp"
        },
        {
          "label": "end"
        },
        {
          "args": [
            "x_0_2"
          ],
          "op": "print"
        },
        {
          "args": [
            "x_1_2"
          ],
          "op": "print"
        },
        {
          "op": "ret"
        },
        {
          "label": "l_1_3"
        },
        {
          "labels": [
            "end"
          ],
          "op": "jmp"
        },
        {
          "label": "l_1_2"
        },
        {
          "dest": "x_1_2",
          "op": "const",
          "type": "int",
          "value": 0
        },
        {
          "labels": [
            "l_1_3"
          ],
          "op": "jmp"
        },
        {
          "label": "l_1_1"
        },
        {
          "dest": "x_1_1",
          "op": "const",
          "type": "int",
          "value": 1
        },
        {
          "labels": [
            "l_1_3"
          ],
          "op": "jmp"
        },
        {
          "label": "l_0_3"
        },
        {
          "args": [
            "b1"
          ],
          "labels": [
            "l_1_1",
            "l_1_2"
          ],
          "op": "br"
        },
        {
          "label": "l_0_2"
        },
        {
          "dest": "x_0_2",
          "op": "const",
          "type": "int",
          "value": 2
        },
        {
          "labels": [
            "l_0_3"
          ],
          "op": "jmp"
        },
        {
          "label": "l_0_1"
        },
        {
          "dest": "x_0_1",
          "op": "const",
          "type": "int",
          "value": 3
        },
        {
          "labels": [
            "l_0_3"
          ],
          "op": "jmp"
        },
        {
          "label": "start"
        },
        {
          "args": [
            "b0"
          ],
          "labels": [
            "l_0_1",
            "l_0_2"
          ],
          "op": "br"
        }
      ],
      "name": "main"
    }
  ]
}
  |}

let process_bril_program json =
  let bril_program = Bril.parsed_bril_json json in
  (* Print the parsed Bril program *)
  print_endline "Parsed Bril Program:";
  print_endline
    (Yojson.Safe.pretty_to_string (Bril.bril_program_to_yojson bril_program));
  (* Print all basic blocks *)
  let basic_blocks = Lesson2.Basic_block.gather_basic_blocks bril_program in
  print_endline "\nBasic Blocks:";
  List.iteri
    (fun i (block : Lesson2.Basic_block.t) ->
      let label_str = match block.label with Some s -> s | None -> "none" in
      print_endline
        (Printf.sprintf "Basic Block %d, label .%s:" (i + 1) label_str);
      List.iter
        (fun instr ->
          print_endline
            (Yojson.Safe.pretty_to_string
               (Bril.bril_instruction_to_yojson instr)))
        block.instructions)
    basic_blocks;

  Cfg.build_cfg bril_program |> Cfg.to_dot |> print_endline

let () =
  process_bril_program bril_add_json;
  process_bril_program bril_simple_branch;
  process_bril_program bril_non_linear_cf;
  process_bril_program bril_non_linear_cf_label_first;
  process_bril_program bril_non_linear_cf_label_first_dupl

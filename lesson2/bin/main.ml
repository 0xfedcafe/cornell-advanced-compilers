open Lesson2

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

let main () =
  let bril_program = Basic_block.parsed_bril_json bril_add_json in
  (* Print the parsed Bril program *)
  print_endline "Parsed Bril Program:";
  print_endline (Yojson.Safe.pretty_to_string (Basic_block.bril_program_to_yojson bril_program))

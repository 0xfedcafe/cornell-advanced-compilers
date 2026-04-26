open Base
open Bril_function
open Bril_instruction

type bril_program = { functions : bril_function list } [@@deriving yojson]

let bril_program_to_string { functions } =
  String.concat ~sep:"\n\n"
    (List.map ~f:Bril_function.bril_function_to_string functions)

let parsed_bril_json bril_string =
  let bril_json = Yojson.Safe.from_string bril_string in
  let bril_program = bril_program_of_yojson bril_json in
  match bril_program with
  | Ok program -> program
  | Error err -> failwith ("Error parsing Bril JSON: " ^ err)

type bril_ir_program = { functions : bril_ir_function list }

let from_tokens (prog : bril_program) : bril_ir_program =
  let ir_functions =
    List.map prog.functions ~f:(fun f ->
        let ir_instrs =
          List.map f.instrs ~f:(fun instr -> Instruction.from_instruction instr)
        in
        { name = f.name; args = f.args; typ = f.typ; instrs = ir_instrs })
  in
  { functions = ir_functions }

let bril_ir_program_to_string { functions } =
  String.concat ~sep:"\n\n"
    (List.map ~f:Bril_function.bril_ir_function_to_string functions)

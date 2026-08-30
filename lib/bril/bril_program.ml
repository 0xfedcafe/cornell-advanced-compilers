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

let to_tokens (prog : bril_ir_program) : bril_program =
  let functions =
    List.map prog.functions ~f:(fun f ->
        let instrs = List.map f.instrs ~f:Instruction.to_instruction in
        ({ name = f.name; args = f.args; typ = f.typ; instrs } : bril_function))
  in
  { functions }

let rec drop_nulls (json : Yojson.Safe.t) : Yojson.Safe.t =
  match json with
  | `Assoc fields ->
      `Assoc
        (List.filter_map fields ~f:(fun (k, v) ->
             match v with `Null -> None | _ -> Some (k, drop_nulls v)))
  | `List items -> `List (List.map items ~f:drop_nulls)
  | other -> other

let bril_ir_program_to_json (prog : bril_ir_program) : Yojson.Safe.t =
  drop_nulls (bril_program_to_yojson (to_tokens prog))

let bril_ir_program_to_string { functions } =
  String.concat ~sep:"\n\n"
    (List.map ~f:Bril_function.bril_ir_function_to_string functions)

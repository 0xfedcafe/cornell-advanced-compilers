open Cfg
open Base
open Basic_block
open Bril_instruction
open Instruction

type lvn_value = LvnConst of string | LvnOp of string * string list
[@@deriving compare, hash, sexp]

let canonicalize_lvn_value (instr : Instruction.t) : lvn_value option =
  match instr with
  | Const op ->
      Some (LvnConst (Bril_immediate.bril_immediate_to_string op.value))
  | Unary (Op.Id, _) -> None
  | Binary (o, _) ->
      let args = get_args instr in
      let args =
        if Op.is_commutative o then List.sort args ~compare:String.compare
        else args
      in
      Some (LvnOp (string_of_op instr, args))
  | Unary _ -> Some (LvnOp (string_of_op instr, get_args instr))
  | _ -> None

let lvn_pass (cfg : CFG.t) : unit =
  let next_num = ref 1 in

  let handle_block ~data:(bb : basic_block) : unit =
    (* var name -> canonical var name, updated as we handle identity assignments and clobbering *)
    (* Same as table entry id, but with strings *)
    let env = Hashtbl.create (module String) in

    (* expression -> canonical var name *)
    let table = Hashtbl.Poly.create () in

    (* variable name -> times written *)
    let def_counts = Hashtbl.create (module String) in

    (*
       Can be significantly improved by using a hashtable where an entry contains table entry as a key and
       a list of variable names, where first is canonical name and others are "synonyms", it's useful when
       doing copy propagation due to clobbering, this **hint** was taken from bril/examples/lvn.py repo.
       Also renaming all variables to "lvn.n" format is not necessary, we can just keep the original name
       for canonical variables and only rename clobbered ones, but it makes debugging easier if all variables
       are renamed to a consistent format. I'm just dumb and it's impossible to program in this language.
    *)
    List.iter bb.instructions ~f:(fun instr ->
        match get_dest instr with
        | Some dest -> Hashtbl.incr def_counts dest
        | None -> ());

    let get_canon var =
      match Hashtbl.find env var with Some c -> c | None -> var
    in

    let new_instrs =
      List.fold_left bb.instructions ~init:[] ~f:(fun acc instr ->
          let canon_instr = replace_args instr get_canon in

          match get_dest canon_instr with
          | Some dest -> (
              (* Decrement def count to see if this definition is overwritten later *)
              Hashtbl.decr def_counts dest;
              let will_be_clobbered = Hashtbl.find_exn def_counts dest > 0 in

              let dest_name =
                if will_be_clobbered then begin
                  let n = !next_num in
                  Int.incr next_num;
                  Printf.sprintf "lvn.%d" n
                end
                else dest
              in

              match canon_instr with
              | Unary (Op.Id, { src1; typ; _ }) ->
                  Hashtbl.set env ~key:dest ~data:src1;
                  let id_instr =
                    Unary (Op.Id, { dst = dest_name; typ; src1 })
                  in
                  id_instr :: acc
              | _ -> (
                  match canonicalize_lvn_value canon_instr with
                  | Some val_ -> (
                      match Hashtbl.find table val_ with
                      | Some canon ->
                          Hashtbl.set env ~key:dest ~data:canon;
                          let id_instr =
                            Unary
                              (Op.Id,
                                {
                                  dst = dest_name;
                                  typ = result_type canon_instr;
                                  src1 = canon;
                                })
                          in
                          id_instr :: acc
                      | None ->
                          Hashtbl.set table ~key:val_ ~data:dest_name;
                          Hashtbl.set env ~key:dest ~data:dest_name;
                          let final_instr = replace_dst canon_instr dest_name in
                          final_instr :: acc)
                  | None ->
                      Hashtbl.set env ~key:dest ~data:dest_name;
                      let final_instr = replace_dst canon_instr dest_name in
                      final_instr :: acc))
          | None -> canon_instr :: acc)
      |> List.rev
    in
    CFG.replace_node cfg ~id:bb.id ~new_bb:{ bb with instructions = new_instrs }
  in

  let bb_ids = Hashtbl.keys cfg.nodes in
  List.iter bb_ids ~f:(fun bb_id ->
      match Hashtbl.find cfg.nodes bb_id with
      | Some bb -> handle_block ~data:bb
      | None -> ())

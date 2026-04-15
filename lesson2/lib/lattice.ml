open Cfg
open Base
open Bril_instruction
open Basic_block

module type Lattice = sig
  type t

  val ( <= ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val leq : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val top : t
  val bottom : t
end

type direction = Forward | Backward

module type DataflowBase = sig
  type t

  val d : direction
  val transfer : Basic_block.t -> t -> t
end

module DataflowSolver (L : Lattice) (B : DataflowBase with type t = L.t) =
struct
  type t = L.t

  type state = {
    in' : (Basic_block.id, t) Hashtbl.t;
    out' : (Basic_block.id, t) Hashtbl.t;
  }

  let create_state () =
    {
      in' = Hashtbl.create (module Basic_block.Id);
      out' = Hashtbl.create (module Basic_block.Id);
    }

  let update_in state bb_id new_in =
    Hashtbl.set state.in' ~key:bb_id ~data:new_in

  let update_out state bb_id new_out =
    Hashtbl.set state.out' ~key:bb_id ~data:new_out

  let preds (cfg : CFG.t) bb_id =
    match B.d with
    | Forward -> (Hashtbl.find_exn cfg.graph bb_id).preds
    | Backward -> (Hashtbl.find_exn cfg.graph bb_id).succs

  let succs (cfg : CFG.t) bb_id =
    match B.d with
    | Forward -> (Hashtbl.find_exn cfg.graph bb_id).succs
    | Backward -> (Hashtbl.find_exn cfg.graph bb_id).preds

  let analyze (s : state) (cfg : CFG.t) : state =
    let ns = cfg.nodes in
    let workgroup = Hashtbl.keys ns in
    (* Initialize with inputs *)

    List.iter workgroup ~f:(fun n ->
        if not (Hashtbl.mem s.in' n) then
          Hashtbl.set s.in' ~key:n ~data:L.bottom;
        if not (Hashtbl.mem s.out' n) then
          Hashtbl.set s.out' ~key:n ~data:L.bottom);

    let compute_in (n : Basic_block.id) =
      List.fold_left (preds cfg n) ~init:L.bottom ~f:(fun acc pred ->
          let p_out = Hashtbl.find_exn s.out' pred in
          L.meet p_out acc)
    in

    let rec analyze_iter workgroup =
      match workgroup with
      | [] -> ()
      | x :: xs ->
          let in' = compute_in x in
          let bb = Hashtbl.find_exn ns x in
          let out' = B.transfer bb in' in

          let out = Hashtbl.find_exn s.out' x in
          if not L.(out = out') then (
            let succs' = succs cfg x in

            update_in s x in';
            update_out s x out';
            analyze_iter (xs @ succs'))
          else analyze_iter xs
    in

    analyze_iter workgroup;
    s
end

type reaching_def_set = (Instruction.t, Instruction.comparator_witness) Set.t
type reaching_def_lattice_t = Top | Set of reaching_def_set

module ReachingDefinitionsLattice :
  Lattice with type t = reaching_def_lattice_t = struct
  (* set of definitions reaching current point: Set of var_name * val*)
  type t = reaching_def_lattice_t

  let ( <= ) s1 s2 =
    match (s1, s2) with
    | _, Top -> true
    | Top, _ -> false
    | Set s1', Set s2' -> Set.is_subset s1' ~of_:s2'

  (* to be entirely precise we can do s1 <= s2 && s2 <= s1 *)
  let ( = ) s1 s2 =
    match (s1, s2) with
    | Top, Top -> true
    | Set s1', Set s2' -> Set.equal s1' s2'
    | _ -> false

  let leq = ( <= )

  let join s1 s2 =
    match (s1, s2) with
    | Top, _ | _, Top -> Top
    | Set s1', Set s2' -> Set (Set.inter s1' s2')

  let meet s1 s2 =
    match (s1, s2) with
    | Top, x | x, Top -> x
    | Set s1', Set s2' -> Set (Set.union s1' s2')

  (* set with all elements *)
  let top = Top

  (* set with no elements *)
  let bottom = Set (Set.empty (module Instruction))
end

module ReachingDefinitions :
  DataflowBase with type t = ReachingDefinitionsLattice.t = struct
  type t = ReachingDefinitionsLattice.t

  let d = Forward

  let transfer (bb : Basic_block.t) (in' : t) : t =
    match in' with
    | Top -> Top
    | Set acc ->
        let gen_kill =
          List.fold_left bb.instructions ~init:acc ~f:(fun acc instr ->
              match Instruction.get_dest instr with
              | Some dst ->
                  let filter =
                    Set.filter acc ~f:(fun i ->
                        let got_dst = Instruction.get_dest i in
                        match got_dst with
                        | Some d -> String.(d <> dst)
                        | None -> true)
                  in
                  Set.add filter instr
              | None -> acc)
        in
        Set gen_kill
end

module RDSolver =
  DataflowSolver (ReachingDefinitionsLattice) (ReachingDefinitions)

let reaching_definitions_analysis (cfg : CFG.t) : RDSolver.state =
  let module Solver =
    DataflowSolver (ReachingDefinitionsLattice) (ReachingDefinitions)
  in
  let state = Solver.create_state () in
  RDSolver.analyze state cfg

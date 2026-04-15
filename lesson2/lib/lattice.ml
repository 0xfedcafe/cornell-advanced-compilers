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

module Instruction = struct
  type t = bril_ir_instruction

  let compare = compare_bril_ir_instruction
  let hash = hash_bril_ir_instruction
  let sexp_of_t = sexp_of_bril_ir_instruction
  let t_of_sexp = bril_ir_instruction_of_sexp
end

module PolymorphicComparator = struct
  include Instruction
  include Base.Comparator.Make (Instruction)
end

module ReachingDefinitionsLattice :
  Lattice
    with type t =
      (Instruction.t, PolymorphicComparator.comparator_witness) Set.t =
struct
  (* set of definitions reaching current point: Set of var_name * val*)
  type t = (Instruction.t, PolymorphicComparator.comparator_witness) Set.t

  let ( <= ) s1 s2 = Set.is_subset s1 ~of_:s2

  (* to be entirely precise we can do s1 <= s2 && s2 <= s1 *)
  let ( = ) = Set.equal
  let leq = ( <= )
  let join = Set.inter
  let meet = Set.union

  (* set with all elements *)
  let top = Set.empty (module PolymorphicComparator)

  (* set with no elements *)
  let bottom = Set.empty (module PolymorphicComparator)
end

module ReachingDefinitions :
  DataflowBase with type t = ReachingDefinitionsLattice.t = struct
  type t = ReachingDefinitionsLattice.t

  let d = Forward

  let transfer (bb : Basic_block.t) (in' : t) : t =
    let gen_kill =
      List.fold_left bb.instructions ~init:in' ~f:(fun acc instr ->
          match get_dest instr with
          | Some dst ->
              let filter =
                Set.filter acc ~f:(fun i ->
                    let got_dst = get_dest i in
                    match got_dst with
                    | Some d -> String.(d <> dst)
                    | None -> true)
              in
              Set.add filter instr
          | None -> acc)
    in
    gen_kill
end

module RDSolver =
  DataflowSolver (ReachingDefinitionsLattice) (ReachingDefinitions)

let reaching_definitions_analysis (cfg : CFG.t) : RDSolver.state =
  let module Solver =
    DataflowSolver (ReachingDefinitionsLattice) (ReachingDefinitions)
  in
  let state = Solver.create_state () in
  RDSolver.analyze state cfg

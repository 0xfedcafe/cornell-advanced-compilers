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

module type Analysis = sig
  include Lattice

  val d : direction
  val init_state : t
  val boundary_state : t
  val transfer : Basic_block.t -> t -> t
end

module type SolverState = sig
  type t

  type state = {
    in' : (Basic_block.id, t) Hashtbl.t;
    out' : (Basic_block.id, t) Hashtbl.t;
  }

  val create_state : unit -> state
  val update_in : state -> Basic_block.id -> t -> unit
  val update_out : state -> Basic_block.id -> t -> unit
  val preds : CFG.t -> Basic_block.id -> Basic_block.id list
  val succs : CFG.t -> Basic_block.id -> Basic_block.id list
end

module BaseSolver (A : Analysis) = struct
  type t = A.t

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
    match A.d with
    | Forward -> (Hashtbl.find_exn cfg.graph bb_id).preds
    | Backward -> (Hashtbl.find_exn cfg.graph bb_id).succs

  let succs (cfg : CFG.t) bb_id =
    match A.d with
    | Forward -> (Hashtbl.find_exn cfg.graph bb_id).succs
    | Backward -> (Hashtbl.find_exn cfg.graph bb_id).preds

  let init_nodes s nodes =
    List.iter nodes ~f:(fun n ->
        if not (Hashtbl.mem s.in' n) then
          Hashtbl.set s.in' ~key:n ~data:A.init_state;
        if not (Hashtbl.mem s.out' n) then
          Hashtbl.set s.out' ~key:n ~data:A.init_state)

  let process_node s cfg x =
    let p = preds cfg x in
    let in' =
      if List.is_empty p then A.boundary_state
      else
        List.fold_left p ~init:A.top ~f:(fun acc pred ->
            let p_out = Hashtbl.find_exn s.out' pred in
            A.meet p_out acc)
    in
    let bb = Hashtbl.find_exn cfg.nodes x in

    let out' = A.transfer bb in' in
    let out = Hashtbl.find_exn s.out' x in

    update_in s x in';
    update_out s x out';

    not (A.( = ) out out')
end

module DataflowSolver (A : Analysis) = struct
  include BaseSolver (A)

  let analyze (s : state) (cfg : CFG.t) : state =
    let ns = cfg.nodes in
    let workgroup = Hashtbl.keys ns in
    init_nodes s workgroup;

    let rec analyze_iter workgroup =
      match workgroup with
      | [] -> ()
      | x :: xs ->
          let changed = process_node s cfg x in
          if changed then
            let succs' = succs cfg x in
            analyze_iter (xs @ succs')
          else analyze_iter xs
    in
    analyze_iter workgroup;
    s
end

module OrderedSolver (A : Analysis) = struct
  include BaseSolver (A)

  let analyze (s : state) (cfg : CFG.t) (ordered_nodes : Basic_block.id list) :
      state =
    init_nodes s ordered_nodes;

    let rec analyze_iter () =
      let changed =
        List.fold_left ordered_nodes ~init:false ~f:(fun changed_acc x ->
            let node_changed = process_node s cfg x in
            changed_acc || node_changed)
      in
      if changed then analyze_iter ()
    in
    analyze_iter ();
    s
end

type 'a set_lattice_t = Top | Set of 'a

module type SetLatticeParams = sig
  type set_t

  val empty : set_t
  val is_subset : set_t -> of_:set_t -> bool
  val equal : set_t -> set_t -> bool
  val meet_sets : set_t -> set_t -> set_t
  val join_sets : set_t -> set_t -> set_t
end

module MakeSetLattice (P : SetLatticeParams) :
  Lattice with type t = P.set_t set_lattice_t = struct
  type t = P.set_t set_lattice_t

  let ( <= ) s1 s2 =
    match (s1, s2) with
    | _, Top -> true
    | Top, _ -> false
    | Set s1', Set s2' -> P.is_subset s1' ~of_:s2'

  let leq = ( <= )

  let ( = ) s1 s2 =
    match (s1, s2) with
    | Top, Top -> true
    | Set s1', Set s2' -> P.equal s1' s2'
    | _ -> false

  let join s1 s2 =
    match (s1, s2) with
    | Top, _ | _, Top -> Top
    | Set s1', Set s2' -> Set (P.join_sets s1' s2')

  let meet s1 s2 =
    match (s1, s2) with
    | Top, x | x, Top -> x
    | Set s1', Set s2' -> Set (P.meet_sets s1' s2')

  let top = Top
  let bottom = Set P.empty
end

type reaching_def_set = (Instruction.t, Instruction.comparator_witness) Set.t
type reaching_def_lattice_t = reaching_def_set set_lattice_t

module ReachingDefinitionsLattice = MakeSetLattice (struct
  type set_t = reaching_def_set

  let empty = Set.empty (module Instruction)
  let is_subset = Set.is_subset
  let equal = Set.equal
  let meet_sets = Set.union
  let join_sets = Set.inter
end)

module ReachingDefinitionsAnalysis :
  Analysis with type t = ReachingDefinitionsLattice.t = struct
  include ReachingDefinitionsLattice

  let d = Forward
  let init_state = bottom
  let boundary_state = bottom

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

type live_vars_set = (String.t, String.comparator_witness) Set.t
type live_vars_lattice_t = live_vars_set set_lattice_t

module LiveVarsLattice = MakeSetLattice (struct
  type set_t = live_vars_set

  let empty = Set.empty (module String)
  let is_subset = Set.is_subset
  let equal = Set.equal
  let meet_sets = Set.union
  let join_sets = Set.inter
end)

module LiveVarsAnalysis : Analysis with type t = LiveVarsLattice.t = struct
  include LiveVarsLattice

  let d = Backward
  let init_state = bottom
  let boundary_state = bottom

  let transfer (bb : Basic_block.t) (in' : t) : t =
    match in' with
    | Top -> Top
    | Set out_b ->
        let use_b, kill_b =
          List.fold_left bb.instructions
            ~init:(Set.empty (module String), Set.empty (module String))
            ~f:(fun (use_acc, kill_acc) instr ->
              let gen =
                Instruction.get_args instr |> Set.of_list (module String)
              in
              let kill =
                match Instruction.get_dest instr with
                | Some dst -> Set.singleton (module String) dst
                | None -> Set.empty (module String)
              in
              let use_b = Set.union use_acc (Set.diff gen kill_acc) in
              let kill_b = Set.union kill_acc kill in
              (use_b, kill_b))
        in
        let without_kill = Set.diff out_b kill_b in
        Set (Set.union use_b without_kill)
end

type constant_propagation_set =
  (Instruction.t, Instruction.comparator_witness) Set.t

type constant_propagation_lattice_t = constant_propagation_set set_lattice_t

module ConstantPropagationLattice = MakeSetLattice (struct
  type set_t = constant_propagation_set

  let empty = Set.empty (module Instruction)
  let is_subset = Set.is_subset
  let equal = Set.equal

  let meet_sets s1' s2' =
    let union_set = Set.union s1' s2' in
    let dest_counts = Hashtbl.create (module String) in
    Set.iter union_set ~f:(fun instr ->
        match Instruction.get_dest instr with
        | Some dst ->
            let count =
              Hashtbl.find dest_counts dst |> Option.value ~default:0
            in
            Hashtbl.set dest_counts ~key:dst ~data:(count + 1)
        | None -> ());
    Set.filter union_set ~f:(fun instr ->
        match Instruction.get_dest instr with
        | Some dst -> Int.equal (Hashtbl.find_exn dest_counts dst) 1
        | None -> true)

  let join_sets = Set.union
end)

module ConstantPropagationAnalysis :
  Analysis with type t = ConstantPropagationLattice.t = struct
  include ConstantPropagationLattice

  let d = Forward
  let init_state = bottom
  let boundary_state = bottom

  let transfer (bb : Basic_block.t) (in' : ConstantPropagationLattice.t) :
      ConstantPropagationLattice.t =
    match in' with
    | Top -> Top
    | Set acc ->
        let gen_kill =
          List.fold_left bb.instructions ~init:acc ~f:(fun acc instr ->
              match Instruction.get_dest instr with
              | Some dst -> (
                  let filter =
                    Set.filter acc ~f:(fun i ->
                        let got_dst = Instruction.get_dest i in
                        match got_dst with
                        | Some d -> String.(d <> dst)
                        | None -> true)
                  in
                  match instr with
                  | Instruction.Const _ -> Set.add filter instr
                  | _ -> filter)
              | None -> acc)
        in
        Set gen_kill
end

module RDSolver = DataflowSolver (ReachingDefinitionsAnalysis)
module LVSolver = DataflowSolver (LiveVarsAnalysis)
module ConstantPropagationSolver = DataflowSolver (ConstantPropagationAnalysis)

let reaching_definitions_analysis (cfg : CFG.t) : RDSolver.state =
  let module Solver = DataflowSolver (ReachingDefinitionsAnalysis) in
  let state = Solver.create_state () in
  RDSolver.analyze state cfg

let live_vars_analysis (cfg : CFG.t) : LVSolver.state =
  let module Solver = DataflowSolver (LiveVarsAnalysis) in
  let state = Solver.create_state () in
  LVSolver.analyze state cfg

let constant_propagation_analysis (cfg : CFG.t) :
    ConstantPropagationSolver.state =
  let module Solver = DataflowSolver (ConstantPropagationAnalysis) in
  let state = Solver.create_state () in
  ConstantPropagationSolver.analyze state cfg

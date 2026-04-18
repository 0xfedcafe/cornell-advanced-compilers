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

          update_in s x in';
          update_out s x out';

          if not L.(out = out') then
            let succs' = succs cfg x in
            analyze_iter (xs @ succs')
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

type live_vars_set = (String.t, String.comparator_witness) Set.t
type live_vars_lattice_t = Top | Set of live_vars_set

module LiveVarsLattice : Lattice with type t = live_vars_lattice_t = struct
  type t = live_vars_lattice_t

  let ( <= ) s1 s2 =
    match (s1, s2) with
    | _, Top -> true
    | Top, _ -> false
    | Set s1', Set s2' -> Set.is_subset s1' ~of_:s2'

  let leq = ( <= )

  let ( = ) s1 s2 =
    match (s1, s2) with
    | Top, Top -> true
    | Set s1', Set s2' -> Set.equal s1' s2'
    | _ -> false

  let join s1 s2 =
    match (s1, s2) with
    | Top, _ | _, Top -> Top
    | Set s1', Set s2' -> Set (Set.inter s1' s2')

  let meet s1 s2 =
    match (s1, s2) with
    | Top, x | x, Top -> x
    | Set s1', Set s2' -> Set (Set.union s1' s2')

  let top = Top
  let bottom = Set (Set.empty (module String))
end

module LiveVars : DataflowBase with type t = LiveVarsLattice.t = struct
  type t = LiveVarsLattice.t

  let d = Backward

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

type constant_propagation_lattice_t = Top | Set of constant_propagation_set

module ConstantPropagationLattice :
  Lattice with type t = constant_propagation_lattice_t = struct
  type t = constant_propagation_lattice_t

  let ( <= ) s1 s2 =
    match (s1, s2) with
    | _, Top -> true
    | Top, _ -> false
    | Set s1', Set s2' -> Set.is_subset s1' ~of_:s2'

  let leq = ( <= )

  let ( = ) s1 s2 =
    match (s1, s2) with
    | Top, Top -> true
    | Set s1', Set s2' -> Set.equal s1' s2'
    | _ -> false

  let join s1 s2 =
    match (s1, s2) with
    | Top, _ | _, Top -> Top
    | Set s1', Set s2' -> Set (Set.union s1' s2')

  let meet s1 s2 =
    match (s1, s2) with
    | Top, x | x, Top -> x
    | Set s1', Set s2' ->
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
        let filtered_set =
          Set.filter union_set ~f:(fun instr ->
              match Instruction.get_dest instr with
              | Some dst -> Int.equal (Hashtbl.find_exn dest_counts dst) 1
              | None -> true)
        in
        Set filtered_set

  let top = Top
  let bottom = Set (Set.empty (module Instruction))
end

module ConstantPropagation :
  DataflowBase with type t = ConstantPropagationLattice.t = struct
  type t = ConstantPropagationLattice.t

  let d = Forward

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

module RDSolver =
  DataflowSolver (ReachingDefinitionsLattice) (ReachingDefinitions)

module LVSolver = DataflowSolver (LiveVarsLattice) (LiveVars)

module ConstantPropagationSolver =
  DataflowSolver (ConstantPropagationLattice) (ConstantPropagation)

let reaching_definitions_analysis (cfg : CFG.t) : RDSolver.state =
  let module Solver =
    DataflowSolver (ReachingDefinitionsLattice) (ReachingDefinitions)
  in
  let state = Solver.create_state () in
  RDSolver.analyze state cfg

let live_vars_analysis (cfg : CFG.t) : LVSolver.state =
  let module Solver = DataflowSolver (LiveVarsLattice) (LiveVars) in
  let state = Solver.create_state () in
  LVSolver.analyze state cfg

let constant_propagation_analysis (cfg : CFG.t) :
    ConstantPropagationSolver.state =
  let module Solver =
    DataflowSolver (ConstantPropagationLattice) (ConstantPropagation)
  in
  let state = Solver.create_state () in
  ConstantPropagationSolver.analyze state cfg

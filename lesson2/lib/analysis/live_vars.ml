open Base
open Bril_instruction
open Basic_block
open Cfg
open Lattice

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

module LVSolver = DataflowSolver (LiveVarsAnalysis)

let live_vars_analysis (cfg : CFG.t) (entry_id : Basic_block.id) :
    LVSolver.state =
  let module Solver = DataflowSolver (LiveVarsAnalysis) in
  let state = Solver.create_state () in
  LVSolver.analyze state cfg entry_id

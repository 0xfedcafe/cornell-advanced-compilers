open Base
open Bril_instruction
open Basic_block
open Cfg
open Lattice

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

module RDSolver = DataflowSolver (ReachingDefinitionsAnalysis)

let reaching_definitions_analysis (cfg : CFG.t) (entry_id : Basic_block.id) :
    RDSolver.state =
  let module Solver = DataflowSolver (ReachingDefinitionsAnalysis) in
  let state = Solver.create_state () in
  RDSolver.analyze state cfg entry_id

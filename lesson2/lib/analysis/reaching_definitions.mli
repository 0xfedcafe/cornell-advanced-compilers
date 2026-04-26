open Base
open Bril_instruction
open Cfg
open Lattice

type reaching_def_set = (Instruction.t, Instruction.comparator_witness) Set.t
type reaching_def_lattice_t = reaching_def_set set_lattice_t

module ReachingDefinitionsLattice : Lattice with type t = reaching_def_lattice_t

module ReachingDefinitionsAnalysis :
  Analysis with type t = ReachingDefinitionsLattice.t

module RDSolver : module type of DataflowSolver (ReachingDefinitionsAnalysis)

val reaching_definitions_analysis : CFG.t -> Basic_block.id -> RDSolver.state

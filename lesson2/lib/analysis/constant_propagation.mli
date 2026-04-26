open Base
open Bril_instruction
open Cfg
open Lattice

type constant_propagation_set =
  (Instruction.t, Instruction.comparator_witness) Set.t

type constant_propagation_lattice_t = constant_propagation_set set_lattice_t

module ConstantPropagationLattice :
  Lattice with type t = constant_propagation_lattice_t

module ConstantPropagationAnalysis :
  Analysis with type t = ConstantPropagationLattice.t

module ConstantPropagationSolver :
    module type of DataflowSolver (ConstantPropagationAnalysis)

val constant_propagation_analysis :
  CFG.t -> Basic_block.id -> ConstantPropagationSolver.state

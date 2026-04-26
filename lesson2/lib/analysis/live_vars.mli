open Base
open Cfg
open Lattice

type live_vars_set = (String.t, String.comparator_witness) Set.t
type live_vars_lattice_t = live_vars_set set_lattice_t

module LiveVarsLattice : Lattice with type t = live_vars_lattice_t
module LiveVarsAnalysis : Analysis with type t = LiveVarsLattice.t
module LVSolver : module type of DataflowSolver (LiveVarsAnalysis)

val live_vars_analysis : CFG.t -> Basic_block.id -> LVSolver.state

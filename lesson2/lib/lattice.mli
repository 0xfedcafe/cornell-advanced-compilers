open Cfg
open Base
open Bril_instruction

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

module DataflowSolver : functor
  (A : Analysis)
  -> sig
  include SolverState with type t = A.t
  val analyze : state -> CFG.t -> state
end

module OrderedSolver : functor
  (A : Analysis)
  -> sig
  include SolverState with type t = A.t
  val analyze : state -> CFG.t -> Basic_block.id list -> state
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

module MakeSetLattice : functor (P : SetLatticeParams) -> Lattice with type t = P.set_t set_lattice_t

type reaching_def_set = (Instruction.t, Instruction.comparator_witness) Set.t
type reaching_def_lattice_t = reaching_def_set set_lattice_t

module ReachingDefinitionsLattice : Lattice with type t = reaching_def_lattice_t

module ReachingDefinitionsAnalysis :
  Analysis with type t = ReachingDefinitionsLattice.t

type live_vars_set = (String.t, String.comparator_witness) Set.t
type live_vars_lattice_t = live_vars_set set_lattice_t

module LiveVarsLattice : Lattice with type t = live_vars_lattice_t
module LiveVarsAnalysis : Analysis with type t = LiveVarsLattice.t

type constant_propagation_set =
  (Instruction.t, Instruction.comparator_witness) Set.t

type constant_propagation_lattice_t = constant_propagation_set set_lattice_t

module ConstantPropagationLattice :
  Lattice with type t = constant_propagation_lattice_t

module ConstantPropagationAnalysis :
  Analysis with type t = ConstantPropagationLattice.t

module RDSolver :
    module type of
      DataflowSolver (ReachingDefinitionsAnalysis)

module LVSolver : module type of DataflowSolver (LiveVarsAnalysis)

module ConstantPropagationSolver :
    module type of
      DataflowSolver (ConstantPropagationAnalysis)

val reaching_definitions_analysis : CFG.t -> RDSolver.state
val live_vars_analysis : CFG.t -> LVSolver.state
val constant_propagation_analysis : CFG.t -> ConstantPropagationSolver.state

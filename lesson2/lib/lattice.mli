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

module type DataflowBase = sig
  type t

  val d : direction
  val transfer : Basic_block.t -> t -> t
end

module DataflowSolver : functor
  (L : Lattice)
  (_ : DataflowBase with type t = L.t)
  -> sig
  type t = L.t

  type state = {
    in' : (Basic_block.id, t) Hashtbl.t;
    out' : (Basic_block.id, t) Hashtbl.t;
  }

  val create_state : unit -> state
  val update_in : state -> Basic_block.id -> t -> unit
  val update_out : state -> Basic_block.id -> t -> unit
  val preds : CFG.t -> Basic_block.id -> Basic_block.id list
  val succs : CFG.t -> Basic_block.id -> Basic_block.id list
  val analyze : state -> CFG.t -> state
end

type reaching_def_set = (Instruction.t, Instruction.comparator_witness) Set.t
type reaching_def_lattice_t = Top | Set of reaching_def_set

module ReachingDefinitionsLattice : Lattice with type t = reaching_def_lattice_t

module ReachingDefinitions :
  DataflowBase with type t = ReachingDefinitionsLattice.t

type live_vars_set = (String.t, String.comparator_witness) Set.t
type live_vars_lattice_t = Top | Set of live_vars_set

module LiveVarsLattice : Lattice with type t = live_vars_lattice_t
module LiveVars : DataflowBase with type t = LiveVarsLattice.t

type constant_propagation_set =
  (Instruction.t, Instruction.comparator_witness) Set.t

type constant_propagation_lattice_t = Top | Set of constant_propagation_set

module ConstantPropagationLattice :
  Lattice with type t = constant_propagation_lattice_t

module ConstantPropagation :
  DataflowBase with type t = ConstantPropagationLattice.t

module RDSolver :
    module type of
      DataflowSolver (ReachingDefinitionsLattice) (ReachingDefinitions)

module LVSolver : module type of DataflowSolver (LiveVarsLattice) (LiveVars)

module ConstantPropagationSolver :
    module type of
      DataflowSolver (ConstantPropagationLattice) (ConstantPropagation)

val reaching_definitions_analysis : CFG.t -> RDSolver.state
val live_vars_analysis : CFG.t -> LVSolver.state
val constant_propagation_analysis : CFG.t -> ConstantPropagationSolver.state

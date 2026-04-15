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

module Instruction : sig
  type t = bril_ir_instruction

  val compare : t -> t -> int
  val hash : t -> int
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

module PolymorphicComparator : sig
  include module type of Instruction
  include Base.Comparator.S with type t := t
end

module ReachingDefinitionsLattice :
  Lattice
    with type t =
      (Instruction.t, PolymorphicComparator.comparator_witness) Set.t

module ReachingDefinitions :
  DataflowBase with type t = ReachingDefinitionsLattice.t

module RDSolver :
    module type of
      DataflowSolver (ReachingDefinitionsLattice) (ReachingDefinitions)

val reaching_definitions_analysis : CFG.t -> RDSolver.state

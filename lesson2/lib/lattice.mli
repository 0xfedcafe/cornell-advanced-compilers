open Cfg
open Base

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

module DataflowSolver : functor (A : Analysis) -> sig
  include SolverState with type t = A.t

  val analyze : state -> CFG.t -> Basic_block.id -> state
end

module OrderedSolver : functor (A : Analysis) -> sig
  include SolverState with type t = A.t

  val analyze : state -> CFG.t -> Basic_block.id -> Basic_block.id list -> state
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

module MakeSetLattice : functor (P : SetLatticeParams) ->
  Lattice with type t = P.set_t set_lattice_t

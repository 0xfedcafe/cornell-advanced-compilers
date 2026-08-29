open Base
open Cfg

module Dominators : sig
  val reverse_post_order : CFG.t -> Basic_block.id -> Basic_block.id list

  type doms_state
  type dom_set = (Basic_block.id, Basic_block.Id.comparator_witness) Set.t

  val compute_dominators : CFG.t -> Basic_block.id -> doms_state

  val compute_idoms :
    CFG.t -> Basic_block.id -> (Basic_block.id, Basic_block.id option) Hashtbl.t

  val dom_tree_children :
    CFG.t -> Basic_block.id -> (Basic_block.id, Basic_block.id list) Hashtbl.t

  val dominates : doms_state -> Basic_block.id -> Basic_block.id -> bool

  val strictly_dominates :
    doms_state -> Basic_block.id -> Basic_block.id -> bool

  val dominance_frontier :
    CFG.t ->
    Basic_block.id ->
    ( Basic_block.id,
      (Basic_block.id, Basic_block.Id.comparator_witness) Set.t )
    Hashtbl.t
end

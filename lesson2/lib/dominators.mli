open Base
open Cfg

module Dominators : sig
  val reverse_post_order : CFG.t -> Basic_block.id -> Basic_block.id list

  type doms_state

  val compute_dominators : CFG.t -> Basic_block.id -> doms_state
  val compute_idoms : CFG.t -> Basic_block.id -> (Basic_block.id, Basic_block.id option) Hashtbl.t
  val dominates : doms_state -> Basic_block.id -> Basic_block.id -> bool
end

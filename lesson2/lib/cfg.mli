open Base
open Bril
open Basic_block

module CFG : sig
  type edges = {
    mutable preds : Basic_block.id list;
    mutable succs : Basic_block.id list;
  }

  type t = {
    nodes : (Basic_block.id, basic_block) Hashtbl.t;
    graph : (Basic_block.id, edges) Hashtbl.t;
  }

  val create : unit -> t
  val add_edge : t -> src:Basic_block.id -> dst:Basic_block.id -> unit
  val to_dot : t -> string
end

val build_cfg : bril_program -> CFG.t
val to_dot : CFG.t -> string

open Base
open Bril
open Basic_block

module CFG : sig
  type edges = {
    mutable preds : basic_block list;
    mutable succs : basic_block list;
  }

  type t = {
    label_to_bb : (string, basic_block) Hashtbl.t;
    cfg : (basic_block, edges) Hashtbl.t;
  }

  val create : unit -> t
  val add_edge : t -> src:basic_block -> dst:basic_block -> unit
  val to_dot : t -> string
end

val build_cfg : bril_program -> CFG.t
val to_dot : CFG.t -> string

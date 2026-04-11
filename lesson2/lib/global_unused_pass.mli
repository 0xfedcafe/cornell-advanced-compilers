open Base
open Cfg

val mark_used : CFG.t -> (string, unit) Hashtbl.t
val remove_unused : CFG.t -> (string, unit) Hashtbl.t -> bool
val global_unused_pass : CFG.t -> unit

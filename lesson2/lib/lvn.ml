open Cfg
open Base
open Bril
open Basic_block

let lvn_pass (cfg : CFG.t) : unit =
  (* LVN with copies, identities and then DCE *)
  let expr_correspondence = Hashtbl.create (module String) in
  ()

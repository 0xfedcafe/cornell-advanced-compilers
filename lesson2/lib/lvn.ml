open Cfg
open Base
open Basic_block

let lvn_pass (_cfg : CFG.t) : unit =
  let _handle_block ~key:_bb_id ~data:(_bb : basic_block) : bool =
    let _expr_correspondence = Hashtbl.create (module String) in
    false
  in

  (* LVN with copies, identities and then DCE *)
  ()

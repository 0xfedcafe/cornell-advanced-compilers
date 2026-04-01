open Base
open Bril
open Basic_block

let () =
  let t = Hashtbl.create (module String) in
  let b = Hashtbl.create (module Basic_block) in
  ()

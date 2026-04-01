open Base
open Lesson2.Bril

let () =
  let _ = Hashtbl.create (module String) in
  let _ = Hashtbl.create (module Lesson2.Basic_block) in
  ()

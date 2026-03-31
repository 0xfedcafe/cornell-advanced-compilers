open Base
open Bril

type basic_block = {
  label : string option;
  instructions : bril_instruction list;
}

val gather_basic_blocks : bril_program -> basic_block list
val build_cfg : bril_program -> (basic_block, basic_block list) Hashtbl.t
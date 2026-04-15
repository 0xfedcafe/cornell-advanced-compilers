open Base
open Bril_instruction
open Bril_immediate

val eval_arithm : bril_arithm_instr -> (string -> bril_immediate option) -> bril_immediate option
val eval_comp : bril_comp_instr -> (string -> bril_immediate option) -> bril_immediate option
val eval_logic : bril_logic_instr -> (string -> bril_immediate option) -> bril_immediate option
val evaluate_instruction : bril_ir_instruction -> (string -> bril_immediate option) -> bril_ir_instruction option
